## Track-to-Treat Phase 2 Data Cleaning, Correct Raw Parent Qualtrics Data
# R version 4.4.3

####  Startup  ####
## Load packages
library(groundhog) # 3.2.2
groundhog_date <- "2025-03-28"
meta.groundhog(groundhog_date)
groundhog.library(
  pkg = c("tidyverse", "tidylog", "lubridate", "qualtRics", "openxlsx", "here", "digest"),
  date = groundhog_date
)


## Load helper functions
source(here("Qualtrics Data Cleaning Helper Functions.R"))
source(here("Version Control Helper Functions.R"))


## Load data into list
# Get directories using helper function
dirs <- get_p2_qualtrics_dirs(c("raw_data", "clean_data_staging_intermediate"))
raw_data_dir <- dirs$raw_data

# Load raw Qualtrics datasets (storing paths) in this format: [respondent][wave]
# - Note: Use "timeZone" specified for date columns (e.g., "StartDate") in third row of raw CSV
raw_data_paths <- lst(
  pb = raw_data_dir %+% "DP5+Phase+2+-+Parent+-+Baseline_January+21,+2026_11.17_n.csv",
  p3m = raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+1+-+3M_January+21,+2026_11.18_n.csv",
  p6m = raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+2+-+6M_January+21,+2026_11.18_n.csv",
  p12m = raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+3+-+12M_January+21,+2026_11.18_n.csv",
  p18m = raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+4+-+18M_January+21,+2026_11.18_n.csv",
  p24m = raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+5+-+24M_January+21,+2026_11.19_n.csv"
)

dat_ls_raw <- lapply(raw_data_paths, read_survey, time_zone = "America/Chicago")


## Load corrected item-level codebook
codebook <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Qualtrics Corrected Codebook.rds")


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
p_data_types <- "p" %+% c("b", "i", c(3, 6, 12, 18, 24) %+% "m") %+% "_qualtrics"
check_raw_data_ver(raw_metadata, raw_data_paths, dat_ls_raw, p_data_types)



####  Fix Column Names in Data  ####
## In "pb", use unique "ImportId" to rename columns both named "test" in Qualtrics
# - "read_survey()" contingently named these by their column indices upon import to R
pb_col_map <- attr(dat_ls_raw$pb, "column_map")

test_col1_qname <- pb_col_map$qname[pb_col_map$ImportId == "test_CED8iraacy"]
test_col2_qname <- pb_col_map$qname[pb_col_map$ImportId == "test_CEDzbsbl7w"]

dat_ls_renamed <- dat_ls_raw %>%
  modify_in("pb", ~ rename(
      .x,
      test_col1 = !!sym(test_col1_qname),
      test_col2 = !!sym(test_col2_qname)
    )
  )


## Rename other columns
dat_ls_renamed <- dat_ls_renamed %>%
  # In various waves, fix names of "accommodations_2" items
  modify_in("p3m", ~ rename(.x, p3m_accommodations_2 = `p3m\020_accom_2`)) %>%
  modify_in("p12m", ~ rename(.x, p12m_accommodations_2 = p12m_accommodations_)) %>%
  modify_in("p18m", ~ rename(.x, p18m_accommodations_2 = p18m_accommodations_)) %>%
  modify_in("p24m", ~ rename(.x, p24m_accommodations_2 = p24m_accom_2)) %>%
  
  # In "p6m", fix prefix of "scared_b" and "scared_c" items
  modify_in("p6m", ~ rename_with(
      .x,
      .cols = contains(c("scared_b", "scared_c")),
      .fn = ~ sub("^p3m_", "p6m_", .x)
    )
  )



####  Move Data Rows to Correct Waves  ####
### Remove extraneous columns
dat_ls_selected <- dat_ls_renamed %>%
  # In all waves, remove click, page time variables with helper function
  map(rm_click_page_time_vars) %>%
  
  # In all waves, remove Qualtrics-computed scores
  map(~ .x %>% select(-matches("^SC\\d+$"))) %>%
  
  # In "pb" and "p12m", remove empty test columns
  modify_at(c("pb", "p12m"), ~ .x %>% select(-any_of(c("test", "Test", "test_col1", "test_col2"))))


### Create column labeling each row's original survey dataset
dat_ls_labeled <- lapply(names(dat_ls_selected), function(survey_prefix) {
  dat <- dat_ls_selected[[survey_prefix]]
  
  dat[[paste0(survey_prefix, "_original_dataset")]] <- survey_prefix
  
  return(dat)
})
names(dat_ls_labeled) <- names(dat_ls_selected)


### Recode items that interfere with binding rows across waves
# None
dat_ls_recoded <- dat_ls_labeled


### Manually move rows to correct waves
## Extract rows and rename/select columns to align with columns of correct waves
# Per "README_ttt_p2_data_collection.docx", LSMH01571 completed their only 3m survey at 6m
p3m_row_for_p6m_LSMH01571 <- dat_ls_recoded$p3m %>%
  filter(lsmh_id == "LSMH01571" & ResponseId == "R_1dN4KF6WFt4uIq9") %>%
  rename_with(~ sub("^p3m_", "p6m_", .x)) %>%
  select(any_of(names(dat_ls_labeled$p6m)))

# Per "README_ttt_p2_data_collection.docx", LSMH01089 completed a 3m survey at 12m
p3m_row_for_p12m_LSMH01089 <- dat_ls_recoded$p3m %>%
  filter(lsmh_id == "LSMH01089" & ResponseId == "R_2tzpxE06r8zeI4A") %>%
  rename_with(~ sub("^p3m_", "p12m_", .x)) %>%
  select(any_of(names(dat_ls_labeled$p12m)))

# Per "README_ttt_p2_data_collection.docx", LSMH02311 completed a 12m survey at 18m
p12m_row_for_p18m_LSMH02311 <- dat_ls_recoded$p12m %>%
  filter(lsmh_id == "LSMH02311" & ResponseId == "R_1qlBsRM1x8N23t2") %>%
  rename_with(~ sub("^p12m_", "p18m_", .x)) %>%
  select(any_of(names(dat_ls_labeled$p18m)))


## Move rows
dat_ls_corrected <- dat_ls_recoded %>%
  # For LSMH01571, move the row from 3m to 6m
  modify_in("p3m", ~ filter(.x, !(lsmh_id == "LSMH01571" & ResponseId == "R_1dN4KF6WFt4uIq9"))) %>%
  modify_in("p6m", ~ bind_rows(.x, p3m_row_for_p6m_LSMH01571)) %>%
  
  # For LSMH01089, move the row from 3m to 12m
  modify_in("p3m", ~ filter(.x, !(lsmh_id == "LSMH01089" & ResponseId == "R_2tzpxE06r8zeI4A"))) %>%
  modify_in("p12m", ~ bind_rows(.x, p3m_row_for_p12m_LSMH01089)) %>%

  # For LSMH02311, move the row from 12m to 18m
  modify_in("p12m", ~ filter(.x, !(lsmh_id == "LSMH02311" & ResponseId == "R_1qlBsRM1x8N23t2"))) %>%
  modify_in("p18m", ~ bind_rows(.x, p12m_row_for_p18m_LSMH02311))
  


####  Save Data  ####
# Corrected data (named list by wave)
saveRDS(dat_ls_corrected, dirs$clean_data_staging_intermediate %+% "Phase 2 Parent Qualtrics Corrected Data - List by Wave.rds")
