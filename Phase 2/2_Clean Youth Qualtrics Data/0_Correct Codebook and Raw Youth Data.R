## Track-to-Treat Phase 2 Data Cleaning, Correct Codebook and Raw Youth Data
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

# Load raw Qualtrics datasets (storing paths) in this format: [respondent][wave]_[administration]_raw
# - Note: Use "timeZone" specified for date columns (e.g., "StartDate") in third row of raw CSV
raw_data_paths <- lst(
  yb = raw_data_dir %+% "DP5+Phase+2+-+Youth+-+Baseline_January+21,+2026_11.24_n.csv",
  yi = raw_data_dir %+% "DP5+Phase+2+-+Youth+-+Interventions_January+21,+2026_11.25_n.csv",
  y3m = raw_data_dir %+% "DP5+Phase+2+-+Youth+-+FU+1+-+3M_January+21,+2026_11.24_n.csv",
  y6m = raw_data_dir %+% "DP5+Phase+2+-+Youth+-+FU+2+-+6M_January+21,+2026_11.24_n.csv",
  y12m = raw_data_dir %+% "DP5+Phase+2+-+Youth+-+FU+3+-+12M_January+21,+2026_11.24_n.csv",
  y18m = raw_data_dir %+% "DP5+Phase+2+-+Youth+-+FU+4+-+18M_January+21,+2026_11.25_n.csv",
  y24m = raw_data_dir %+% "DP5+Phase+2+-+Youth+-+FU+5+-+24M_January+29,+2026_10.59_n.csv"
)

dat_ls_raw <- lapply(raw_data_paths, read_survey, time_zone = "America/Chicago")


## Load ID lookup and (using helper function) item-level codebook
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))
codebook <- load_p2_codebook(here("Phase 2", "2026.02.12 Track to Treat P2 Codebook.xlsx"))


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
y_data_types <- "y" %+% c("b", "i", c(3, 6, 12, 18, 24) %+% "m") %+% "_qualtrics"
check_raw_data_ver(raw_metadata, raw_data_paths, dat_ls_raw, y_data_types)



####  Fix Item Prefixes in Codebook and Data  ####
# For "yb", add prefix to SRET items
yb_sret_items_raw <- c("SRET", "SRET.keys", "SRET.time", "SRET.words", "tlcond")

codebook$item[codebook$item %in% yb_sret_items_raw] <-
  paste0("yb_", codebook$item[codebook$item %in% yb_sret_items_raw])

dat_ls_renamed <- dat_ls_raw %>%
  modify_in("yb", ~ rename_with(
      .x,
      .cols = all_of(yb_sret_items_raw),
      .fn = ~ paste0("yb_", .x)
    )
  )

# For "yi", fix BADS-SF items' prefixes from "b_" to "yi_"
yi_bads_items_raw <- paste0("b_bads_", 1:9)

codebook$item[codebook$item %in% yi_bads_items_raw] <-
  sub("^b_", "yi_", codebook$item[codebook$item %in% yi_bads_items_raw])

dat_ls_renamed <- dat_ls_renamed %>%
  modify_in("yi", ~ rename_with(
      .x,
      .cols = all_of(yi_bads_items_raw),
      .fn = ~ sub("^b_", "yi_", .x)
    )
  )



####  Fix Column Names in Data  ####
## In "yi", use unique "ImportId" to rename columns both named "lsmh_id" in Qualtrics
# - "read_survey()" contingently named these by their column indices upon import to R
yi_col_map <- attr(dat_ls_raw$yi, "column_map")

lsmh_id_col1_qname <- yi_col_map$qname[yi_col_map$ImportId == "QID186_TEXT"]
lsmh_id_col2_qname <- yi_col_map$qname[yi_col_map$ImportId == "lsmh_id"]

dat_ls_renamed <- dat_ls_renamed %>%
  modify_in("yi", ~ rename(
      .x,
      lsmh_id_col1 = !!sym(lsmh_id_col1_qname),
      lsmh_id_col2 = !!sym(lsmh_id_col2_qname)
    )
  )


## Rename other columns
dat_ls_renamed <- dat_ls_renamed %>%
  # In all waves except "yi", rename "mvps" to "mpvs" with helper function
  modify_at(setdiff(names(.), "yi"), rename_mvps_to_mpvs) %>%
  
  # In various waves, fix other item names
  modify_in("y12m", ~ rename(.x, y12m_pds_7 = y312_pds_7)) %>%
  modify_in("y18m", ~ rename(.x, y18m_scsc_20 = y18n_scsc_20))



####  Move Data Rows to Correct Waves  ####
### Remove extraneous columns
dat_ls_selected <- dat_ls_renamed %>%
  # In all waves, remove click, page time variables with helper function
  map(rm_click_page_time_vars) %>%
  
  # In all waves, remove Qualtrics-computed scores (though not present in "yi")
  map(~ .x %>% select(-matches("^SC\\d+$"))) %>%

  # In "yb", remove empty "status" column
  modify_in("yb", ~ .x %>% select(-status))


### Create column labeling each row's original survey dataset
dat_ls_labeled <- lapply(names(dat_ls_selected), function(survey_prefix) {
  dat <- dat_ls_selected[[survey_prefix]]
  
  dat[[paste0(survey_prefix, "_original_dataset")]] <- survey_prefix
  
  return(dat)
})
names(dat_ls_labeled) <- names(dat_ls_selected)


### Recode items that interfere with binding rows across waves
dat_ls_recoded <- dat_ls_labeled %>%
  ## Recode the following SITBI items, which should be numeric
  # "sitbi_3b_2"
  modify_in("y3m", ~ mutate(.x, y3m_sitbi_3b_2 = as.numeric(na_if(y3m_sitbi_3b_2, "p")))) %>%
  modify_in("y6m", ~ mutate(.x, y6m_sitbi_3b_2 = as.numeric(na_if(y6m_sitbi_3b_2, "P")))) %>%
  
  # "sitbi_3b_3"
  modify_in("yb", ~ mutate(.x, yb_sitbi_3b_3 = as.numeric(na_if(yb_sitbi_3b_3, "0not sure")))) %>%
  
  # "sitbi_3b_4"
  modify_in("yb", ~ mutate(.x, yb_sitbi_3b_4 = as.numeric(recode(
      yb_sitbi_3b_4,
      "0 not sure" = NA_character_,
      "idk" = NA_character_,
      "1,708" = "1708"
    ))
  ))%>%
  
  # "sitbi_4b_3"
  modify_in("yb", ~ mutate(.x, yb_sitbi_4b_3 = as.numeric(na_if(yb_sitbi_4b_3, "i lost count")))) %>%
  
  # "sitbi_4b_4"
  modify_in("yb", ~ mutate(.x, yb_sitbi_4b_4 = as.numeric(recode(
      yb_sitbi_4b_4,
      "a lot" = NA_character_,
      "i dont know" = NA_character_,
      "maybe 5" = "5"
    ))
  )) %>%
  modify_in("y6m", ~ mutate(.x, y6m_sitbi_4b_4 = as.numeric(na_if(y6m_sitbi_4b_4, "Yes")))) %>%
  modify_in("y18m", ~ mutate(.x, y18m_sitbi_4b_4 = as.numeric(recode(
      y18m_sitbi_4b_4,
      "6ish times" = "6"
    ))
  ))


### Manually move rows to correct waves
## Extract rows and rename/select columns to align with columns of correct waves
# Per "README_ttt_p2_data_collection.docx", LSMH02311 completed a 6m survey at 12m
y6m_row_for_y12m_LSMH02311 <- dat_ls_recoded$y6m %>%
  filter(lsmh_id == "LSMH02311" & ResponseId == "R_7dAv3167l6mahX5") %>%
  rename_with(~ sub("^y6m_", "y12m_", .x)) %>%
  select(any_of(names(dat_ls_labeled$y12m)))

# Per data, LSMH02889 completed a 12m survey at 18m
y12m_row_for_y18m_LSMH02889 <- dat_ls_recoded$y12m %>%
  filter(lsmh_id == "LSMH02889" & ResponseId == "R_3O3yrzQk69BZEWT") %>%
  rename_with(~ sub("^y12m_", "y18m_", .x)) %>%
  select(any_of(names(dat_ls_labeled$y18m)))


## Move rows
dat_ls_corrected <- dat_ls_recoded %>%
  # For LSMH02311, move the row from 6m to 12m
  modify_in("y6m", ~ filter(.x, !(lsmh_id == "LSMH02311" & ResponseId == "R_7dAv3167l6mahX5"))) %>%
  modify_in("y12m", ~ bind_rows(.x, y6m_row_for_y12m_LSMH02311)) %>%

  # For LSMH02889, move the row from 12m to 18m
  modify_in("y12m", ~ filter(.x, !(lsmh_id == "LSMH02889" & ResponseId == "R_3O3yrzQk69BZEWT"))) %>%
  modify_in("y18m", ~ bind_rows(.x, y12m_row_for_y18m_LSMH02889))



####  Save Data  ####
# Corrected data (named list by wave)
saveRDS(dat_ls_corrected, dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Corrected Data - List by Wave.rds")

# Corrected codebook
saveRDS(codebook, dirs$clean_data_staging_intermediate %+% "Phase 2 Qualtrics Corrected Codebook.rds")
