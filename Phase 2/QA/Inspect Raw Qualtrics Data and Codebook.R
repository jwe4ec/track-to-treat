####  Startup  ####
# R version 4.4.3

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
dirs <- get_p2_qualtrics_dirs("raw_data") 
raw_data_dir <-  dirs$raw_data

# Load raw Qualtrics datasets (storing paths) in this format: [respondent][wave]_[administration]_raw
# - Note: Use "timeZone" specified for date columns (e.g., "StartDate") in third row of raw CSVs
# - Use file.path() to build paths independent of the operating system
raw_data_paths <- lst(
  yb_raw = file.path(raw_data_dir, "DP5+Phase+2+-+Youth+-+Baseline_January+21,+2026_11.24_n.csv"),
  yi_raw = file.path(raw_data_dir, "DP5+Phase+2+-+Youth+-+Interventions_January+21,+2026_11.25_n.csv"),
  y3m_raw = file.path(raw_data_dir, "DP5+Phase+2+-+Youth+-+FU+1+-+3M_January+21,+2026_11.24_n.csv"),
  y6m_raw = file.path(raw_data_dir, "DP5+Phase+2+-+Youth+-+FU+2+-+6M_January+21,+2026_11.24_n.csv"),
  y12m_raw = file.path(raw_data_dir, "DP5+Phase+2+-+Youth+-+FU+3+-+12M_January+21,+2026_11.24_n.csv"),
  y18m_raw = file.path(raw_data_dir, "DP5+Phase+2+-+Youth+-+FU+4+-+18M_January+21,+2026_11.25_n.csv"),
  y24m_raw = file.path(raw_data_dir, "DP5+Phase+2+-+Youth+-+FU+5+-+24M_January+29,+2026_10.59_n.csv"),
  
  pb_raw = file.path(raw_data_dir, "DP5+Phase+2+-+Parent+-+Baseline_January+21,+2026_11.17_n.csv"),
  p3m_raw = file.path(raw_data_dir, "DP5+Phase+2+-+Parent+-+FU+1+-+3M_January+21,+2026_11.18_n.csv"),
  p6m_raw = file.path(raw_data_dir, "DP5+Phase+2+-+Parent+-+FU+2+-+6M_January+21,+2026_11.18_n.csv"),
  p12m_raw = file.path(raw_data_dir, "DP5+Phase+2+-+Parent+-+FU+3+-+12M_January+21,+2026_11.18_n.csv"),
  p18m_raw = file.path (raw_data_dir, "DP5+Phase+2+-+Parent+-+FU+4+-+18M_January+21,+2026_11.18_n.csv"),
  p24m_raw = file.path(raw_data_dir, "DP5+Phase+2+-+Parent+-+FU+5+-+24M_January+21,+2026_11.19_n.csv")
)

dat_ls <- lapply(raw_data_paths, read_survey, time_zone = "America/Chicago")


## Load ID lookup and (using helper function) item-level codebook
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))
codebook <- load_p2_codebook(here("Phase 2", "2026.02.12 Track to Treat P2 Codebook.xlsx"))



####  Check that all "_n" files are indeed numeric (based on example columns)  ####
ex_col_classes <- lapply(names(dat_ls), function(name) {
  dat <- dat_ls[[name]]
  
  if (name == "yi_raw") {
    ex_col <- grep("bads_1$", names(dat), value = TRUE)
  } else {
    ex_col <- grep("cdi_1$", names(dat), value = TRUE)
  }
  
  return(class(dat[[ex_col]]))
})

stopifnot(all(ex_col_classes == "numeric"))



####  Identify and inspect data columns by type at each wave  ####
# Identify columns by type
dat_ls_cols <- lapply(dat_ls, function(dat) {
  # Identify various types of non-item columns
  q_meta_cols <- intersect(names(dat), c("StartDate", "EndDate", "Status", "IPAddress", "Progress", 
                                         "Duration (in seconds)", "Finished", "RecordedDate", "ResponseId", 
                                         "RecipientLastName", "RecipientFirstName", "RecipientEmail", 
                                         "ExternalReference", "LocationLatitude", "LocationLongitude", 
                                         "DistributionChannel", "UserLanguage"))
  
  click_submit_cols <- names(dat)[grepl("Click Count|First Click|Last Click|Page Submit", names(dat))]
  
  id_cols <- setdiff(names(dat)[grepl("lsmh_id|lifepak|lifepak_check", names(dat))],
                     click_submit_cols)
  
  date_cols <- setdiff(names(dat)[grepl("date", names(dat))],
                       click_submit_cols)
  
  test_cols <- intersect(names(dat), c("test", "Test", "test...511", "test...512"))
  
  other_patterns <- c("teen_name", "childname",
                      "password_child", "password_parent",
                      "assent_name", "assent_signature_Id", "assent_signature_Name",
                      "assent_signature_Size", "assent_signature_Type",
                      "consent_name", "consent_signature_Id", "consent_signature_Name",
                      "consent_signature_Size", "consent_signature_Type",
                      "phone", "phone_check", "homephone", "workphone", "childcell", "parentcell",
                      "childemail", "parent_email_1", "parent_email_2", "email_id", "parentemail",
                      "address",
                      paste0("SC", 0:12), "status", "interview",
                      "condition",
                      "summary_report")
  other_cols <- names(dat)[grepl(paste0(other_patterns, collapse = "|"), names(dat))]
  
  non_item_cols <- c(q_meta_cols, click_submit_cols, id_cols, date_cols, test_cols, other_cols)
  
  # Identify SSI item columns
  ssi_item_cols <- setdiff(names(dat)[grepl("shar_feel_|proj_pers_|abc_", names(dat))],
                           click_submit_cols)
  
  # Identify measure item columns
  meas_item_cols <- setdiff(names(dat), c(non_item_cols, ssi_item_cols))
  
  # Store columns in list
  dat_cols <- list(q_meta_cols       = q_meta_cols,
                   click_submit_cols = click_submit_cols,
                   id_cols           = id_cols,
                   date_cols         = date_cols,
                   test_cols         = test_cols,
                   other_cols        = other_cols,
                   ssi_item_cols     = ssi_item_cols,
                   meas_item_cols    = meas_item_cols)
  
  return(dat_cols)
})

# Inspect columns by type
lapply(dat_ls_cols, \(x) x$q_meta_cols)
lapply(dat_ls_cols, \(x) x$click_submit_cols)
lapply(dat_ls_cols, \(x) x$id_cols)   # Note: ID column names vary by wave
lapply(dat_ls_cols, \(x) x$date_cols) # Note: Youth data lack "date" columns
lapply(dat_ls_cols, \(x) x$test_cols)
lapply(dat_ls_cols, \(x) x$other_cols)
lapply(dat_ls_cols, \(x) x$ssi_item_cols)
lapply(dat_ls_cols, \(x) x$meas_item_cols)



####  Check if prefixes and stems of measure item column names are same across all waves  ####
## Get prefixes and stems of measure items for youth and parent surveys 
meas_item_cols_prefixes_stems <- lapply(dat_ls_cols, function(dat_cols) {
  meas_item_cols <- dat_cols$meas_item_cols
  
  meas_item_prefixes <- sub("_.*", "", meas_item_cols[grepl("_", meas_item_cols)])
  meas_item_cols_stems <- sub("^[^_]+_", "", meas_item_cols) 

  return(list(
    prefixes = meas_item_prefixes,
    stems = meas_item_cols_stems)
  )
})

y_meas_item_cols_prefixes_stems <- meas_item_cols_prefixes_stems[grepl("^y", names(meas_item_cols_prefixes_stems))]
p_meas_item_cols_prefixes_stems <- meas_item_cols_prefixes_stems[grepl("^p", names(meas_item_cols_prefixes_stems))]

### Confirm that all of the prefixes removed within each wave are what we would expect 
## TODO: Alyssa will replicate this check for parents, currently only checks youth prefixes

y_prefixes_by_wave <- lapply(y_meas_item_cols_prefixes_stems, \(x) unique(x$prefixes))
y_prefixes <- unique(unlist(y_prefixes_by_wave))

p_prefixes_by_wave <- lapply(p_meas_item_cols_prefixes_stems, \(x) unique(x$prefixes))
p_prefixes <- unique(unlist(p_prefixes_by_wave))

expected_y_prefixes <- c("yb", "yi", paste0("y", c(3, 6, 12, 18, 24), "m"))
unexpected_y_prefixes <- setdiff(y_prefixes, expected_y_prefixes)

stopifnot(setequal(unexpected_y_prefixes, c("b", "y312", "y18n")))

expected_p_prefixes <- c("pb", paste0("p", c(3, 6, 12, 18, 24), "m"))
unexpected_p_prefixes <-  setdiff(p_prefixes, expected_p_prefixes) #scared items in p6m are mislabled, may want to adjust the below check.

stopifnot(setequal(unexpected_p_prefixes, c("p3m\020"))) 

### Confirm that all measure items within a given wave are unique- both youth and parents pass
stopifnot(
  all(sapply(y_meas_item_cols_prefixes_stems, \(x) length(x$stems) == length(unique(x$stems)))),
  all(sapply(p_meas_item_cols_prefixes_stems, \(x) length(x$stems) == length(unique(x$stems))))
)

### Confirm that measure item stems are same across waves

y_waves <- names(y_meas_item_cols_prefixes_stems)
total_y_waves <- length(y_meas_item_cols_prefixes_stems)

p_waves <- names(p_meas_item_cols_prefixes_stems)
total_p_waves <- length(p_meas_item_cols_prefixes_stems)

y_stems_by_wave <- lapply(y_meas_item_cols_prefixes_stems, \(x) unique(x$stems))
y_stems <- unique(unlist(y_stems_by_wave))

p_stems_by_wave <-  lapply(p_meas_item_cols_prefixes_stems, \(x) unique(x$stems))
p_stems <- unique(unlist(p_stems_by_wave)) 
# Creating a data frame that organizes the info we want about each stem based on the waves it is or is not present
y_stem_wave_count_dfs <- lapply(y_stems, function(stem) {
  present_y_waves <- y_waves[sapply(y_stems_by_wave, \(stems) stem %in% stems)]
  missing_y_waves <- setdiff(y_waves, present_y_waves)
  
  y_stem_df <- data.frame(
    stem = stem,
    n_present = length(present_y_waves),
    total_y_waves = total_y_waves,
    present_y_waves = paste(present_y_waves, collapse = ", "),
    missing_y_waves = paste(missing_y_waves, collapse = ", ")
  )
  
  return(y_stem_df)
})
names(y_stem_wave_count_dfs) <- y_stems

y_stem_wave_counts_df <- bind_rows(y_stem_wave_count_dfs)

    # Parents
stem_wave_count_dfs <- lapply(p_stems, function(stem) {
  present_p_waves <- p_waves[sapply(p_stems_by_wave, \(stems) stem %in% stems)]
  missing_p_waves <- setdiff(p_waves, present_p_waves)
  
  p_stem_df <- data.frame(
    stem = stem,
    n_present = length(present_p_waves),
    total_p_waves = total_p_waves,
    present_p_waves = paste(present_p_waves, collapse = ", "),
    missing_p_waves = paste(missing_p_waves, collapse = ", ")
  )
  
  return(p_stem_df)
})
names(p_stem_wave_count_dfs) <- p_stems

p_stem_wave_counts_df <- bind_rows(p_stem_wave_count_dfs)
#Now we have a dataframe that includes information about which stems are present in which youth waves
y_stem_wave_counts_df 
p_stem_wave_counts_df

# Quick summary of item stem distribution this can be taken out when complete.
y_stem_wave_counts_df %>% 
  group_by(n_present) %>% 
  count(n_present)

  # More variation in parent stems across waves
p_stem_wave_counts_df %>% 
  group_by(n_present) %>% 
  count(n_present)

####  Check if labels and classes of measure items are same across follow-up waves  ####
### TODO: Alyssa to generalize this section to check across all waves


### Restrict to 3- to 24-month follow-ups
dat_ls_fu <- dat_ls[grepl("\\d+m_raw$", names(dat_ls))]


### Get labels and classes of measure items for youth and parent surveys and name by measure item stems
fu_meas_item_col_lbls_clss <- lapply(names(dat_ls_fu), function(dat_name) {
  dat                 <- dat_ls_fu[[dat_name]]
  meas_item_cols      <- dat_ls_cols[[dat_name]]$meas_item_cols
  meas_item_col_stems <- str_split_fixed(meas_item_cols, "_", 2)[, 2]
  
  out <- list()
  
  out$lbls <- lapply(meas_item_cols, \(col) attr(dat[[col]], "label"))
  out$clss <- lapply(meas_item_cols, \(col) class(dat[[col]]))
  
  names(out$lbls) <- meas_item_col_stems
  names(out$clss) <- meas_item_col_stems
  
  return(out)
})
names(fu_meas_item_col_lbls_clss) <- names(dat_ls_fu)

y_fu_meas_item_col_lbls_clss <- fu_meas_item_col_lbls_clss[grepl("^y", names(fu_meas_item_col_lbls_clss))]
p_fu_meas_item_col_lbls_clss <- fu_meas_item_col_lbls_clss[grepl("^p", names(fu_meas_item_col_lbls_clss))]


### Confirm that all measure item labels within a given follow-up survey are unique
stopifnot(
  all(sapply(y_fu_meas_item_col_lbls_clss, \(wave) length(wave$lbls) == length(unique(wave$lbls)))),
  all(sapply(p_fu_meas_item_col_lbls_clss, \(wave) length(wave$lbls) == length(unique(wave$lbls))))
)


### Remove survey-specific prefixes from certain item labels
# In youth data (SITBI Item 3b, SCARED items)
y_fu_meas_item_col_lbls_clss_sans_prefix <- lapply(y_fu_meas_item_col_lbls_clss, function(wave) {
  wave_lbls       <- wave$lbls
  wave_item_stems <- names(wave_lbls)
  
  sitbi_3b_item_stems <- grep("sitbi_3b_", wave_item_stems, value = TRUE)
  scared_item_stems   <- grep("scared_", wave_item_stems, value = TRUE)
  
  wave_lbls[sitbi_3b_item_stems] <- sub("^y\\d+m_", "", wave_lbls[sitbi_3b_item_stems])
  wave_lbls[scared_item_stems]   <- sub("^y\\d+m_", "", wave_lbls[scared_item_stems])
  
  wave$lbls <- wave_lbls
  
  return(wave)
})

# In parent data (SCARED items)
p_fu_meas_item_col_lbls_clss_sans_prefix <- lapply(p_fu_meas_item_col_lbls_clss, function(wave) {
  wave_lbls <- wave$lbls
  wave_item_stems <- names(wave_lbls)
  
  scared_item_stems <- grep("scared_", wave_item_stems, value = TRUE)
  
  wave_lbls[scared_item_stems] <- sub("^p\\d+m_", "", wave_lbls[scared_item_stems])
  
  wave$lbls <- wave_lbls
  
  return(wave)
})


### Check whether measure item labels are same across follow-up surveys
## Define function to check labels 
check_fu_meas_item_lbls <- function(fu_meas_item_col_lbs_clss) {
  # Get all unique measure item columns (i.e., their stems) across follow-up surveys
  all_cols <- unique(unlist(lapply(fu_meas_item_col_lbs_clss, \(wave) names(wave$lbls))))
  
  # Find measure item stems that have different labels across follow-up surveys
  diff_cols <- character()
  
  for (col in all_cols) {
    
    #TODO: Alyssa to add a check for vars repeated across surveys, rather than just unique
    
    # Get labels for column across all follow-up surveys
    lbls <- sapply(fu_meas_item_col_lbs_clss, \(wave) wave$lbls[[col]])
    
    # Check if more than one unique label
    if (length(unique(lbls)) > 1) diff_cols <- c(diff_cols, col)
  }
  
  return(diff_cols)
}


## Run function to check labels for youth and parent data
y_cols_with_diff_lbls <- check_fu_meas_item_lbls(y_fu_meas_item_col_lbls_clss_sans_prefix)
p_cols_with_diff_lbls <- check_fu_meas_item_lbls(p_fu_meas_item_col_lbls_clss_sans_prefix)

stopifnot(
  y_cols_with_diff_lbls == c("scared_a_2", "scared_c_9", "pcsc_1", "pcsc_7", "pcsc_13"),
  p_cols_with_diff_lbls == c("accom_2", "accommodations_2", "accommodations_")
)


## Inspect items with different labels across follow-up surveys
# Define function
get_lbls_diff_cols <- function(col_lbls_clss, cols_with_diff_lbls) {
  diff_lbls <- lapply(cols_with_diff_lbls, \(col) lapply(col_lbls_clss, \(wave) wave$lbls[[col]]))
  names(diff_lbls) <- cols_with_diff_lbls
  
  return(diff_lbls)
}

# Differences for youth items are due to (a) minor typos (SCARED items) or (b) referring to
# "grades" versus "marks" at some time points (PCSC items)
# - Noted this in README and documented details in raw codebook
y_cols_with_diff_lbls_due_to_typos           <- c("scared_a_2", "scared_c_9")
y_cols_with_diff_lbls_due_to_grades_vs_marks <- c("pcsc_1", "pcsc_7", "pcsc_13")

get_lbls_diff_cols(y_fu_meas_item_col_lbls_clss_sans_prefix, y_cols_with_diff_lbls_due_to_typos)
lapply(dat_ls$yb_raw[paste0("yb_", y_cols_with_diff_lbls_due_to_typos)], attr, which = "label")  # At baseline

get_lbls_diff_cols(y_fu_meas_item_col_lbls_clss_sans_prefix, y_cols_with_diff_lbls_due_to_grades_vs_marks)
lapply(dat_ls$yb_raw[paste0("yb_", y_cols_with_diff_lbls_due_to_grades_vs_marks)], attr, which = "label")  # At baseline

# Differences for parent items are due only to different names for "accommodations_2" item

get_lbls_diff_cols(p_fu_meas_item_col_lbls_clss_sans_prefix, p_cols_with_diff_lbls)


### Check whether measure item classes are same across follow-up waves
## Define function to check classes 
check_fu_meas_item_clss <- function(fu_meas_item_col_lbs_clss) {
  # Get all unique measure item columns (i.e., their stems) across follow-up surveys
  all_cols <- unique(unlist(lapply(fu_meas_item_col_lbs_clss, \(wave) names(wave$clss))))
  
  # Find measure item stems that have different classes across follow-up surveys
  diff_cols <- character()
  
  for (col in all_cols) {
    # Get classes for column across all follow-up surveys
    clss <- sapply(fu_meas_item_col_lbs_clss, \(wave) wave$clss[[col]])
    
    # Check if more than one unique class
    if (length(unique(clss)) > 1) diff_cols <- c(diff_cols, col)
  }
  
  return(diff_cols)
}


## Run function to check classes for youth and parent data
y_cols_with_diff_clss <- check_fu_meas_item_clss(y_fu_meas_item_col_lbls_clss_sans_prefix)
p_cols_with_diff_clss <- check_fu_meas_item_clss(p_fu_meas_item_col_lbls_clss_sans_prefix)

stopifnot(
  setequal(y_cols_with_diff_clss, c("sitbi_3b_2", "sitbi_4b_4")),
  setequal(p_cols_with_diff_clss, c("caregiver1_3_10_TEXT", "accom_2", "accommodations_2", "accommodations_"))
)


## Inspect items with different classes across follow-up surveys
# Define function
get_clss_diff_cols <- function(col_lbls_clss, cols_with_diff_clss) {
  diff_clss <- lapply(cols_with_diff_clss, \(col) lapply(col_lbls_clss, \(wave) wave$clss[[col]]))
  names(diff_clss) <- cols_with_diff_clss
  
  return(diff_clss)
}

# Differences for youth items are due to some character responses (SITBI items)
# - Recoded responses in "Correct Codebook and Raw Youth Data.R"

get_clss_diff_cols(y_fu_meas_item_col_lbls_clss_sans_prefix, y_cols_with_diff_clss)
lapply(dat_ls$yb_raw[paste0("yb_", y_cols_with_diff_clss)], class)  # At baseline

# Differences for parent items are due to (a) all NAs at some waves for "caregiver1_3_10_TEXT"
# (not an issue for binding rows) and (b) different names for "accommodations_2" item
p_cols_with_diff_clss_due_to_char             <- "caregiver1_3_10_TEXT"
p_cols_with_diff_clss_due_to_accommodations_2 <- c("accom_2", "accommodations_2", "accommodations_")

get_clss_diff_cols(p_fu_meas_item_col_lbls_clss_sans_prefix, p_cols_with_diff_clss_due_to_char)
lapply(dat_ls$pb_raw[paste0("pb_", p_cols_with_diff_clss_due_to_char)], class)  # At baseline

get_clss_diff_cols(p_fu_meas_item_col_lbls_clss_sans_prefix, p_cols_with_diff_clss_due_to_accommodations_2)
class(dat_ls$pb_raw$pb_accommodations_2)  # At baseline



####  Check for measure items missing from codebook  ####
lapply(dat_ls_cols, function(dat_cols) {
  setdiff(dat_cols$meas_item_cols, codebook$item)
})

# MPVS items in codebook are named "mvps_" in youth data across waves (renamed in clean data)
# In "y12m_raw", prefix for 1 PDS item ("y312_pds_7") is incorrect (renamed in clean data)
# In "y18m_raw", prefix for 1 SCSC item ("y18n_scsc_20") is incorrect (renamed in clean data)
# In "p3m_raw", "020_accom_2", for "p3m_accommodations_2", is incorrectly named (renamed in clean data)
# In "p12m_raw", "p12m_accommodations_", for "p12m_accommodations_2", is incorrectly named (renamed in clean data)
# In "p18m_raw", "p18m_accommodations_", for "p18m_accommodations_2", is incorrectly named (renamed in clean data)
# In "p24m_raw", "p24m_accom_2", for "p24m_accommodations_2", is incorrectly named (renamed in clean data)


####  Check for codebook items not in data  ####
all_meas_item_cols <- unlist(lapply(dat_ls_cols, function(dat_cols) {
  dat_cols$meas_item_cols
}), use.names = FALSE)

# "accommodations_2" columns resolved above
diff_accommodations_cols <- sort(setdiff(codebook$item[codebook$measure == "demographic" &
                                           grepl("accommodations", codebook$item)], all_meas_item_cols))

# MPVS columns resolved above
diff_mpvs_cols <- sort(setdiff(codebook$item[codebook$measure == "mpvs"], all_meas_item_cols))

# In "p6m_raw", "scared_b" and "scared_c" items have incorrect prefix "p3m" (renamed in clean data)
diff_scared_cols <- sort(setdiff(codebook$item[codebook$measure == "scared"], all_meas_item_cols))
names(dat_ls$p6m_raw)[grepl("scared_b|scared_c", names(dat_ls$p6m_raw))]

# 2 other PDS and SCSC columns resolved above
ignore_measures <- c("demographic", "ace_p", "ace_y", "mpvs", "scared", "other")
diff_other_cols <- sort(setdiff(codebook$item[!(codebook$measure %in% ignore_measures) &
                                                codebook$item != "condition"], all_meas_item_cols))
stopifnot(diff_other_cols == c("y12m_pds_7", "y18m_scsc_20"))



####  Check item prefixes  ####
## In data
lapply(dat_ls_cols, function(dat_cols) {
  meas_item_col_prefixes <- str_split_fixed(dat_cols$meas_item_cols, "_", 2)[, 1]
  
  table(meas_item_col_prefixes, useNA = "always")
})

# "yb_raw" lacks prefixes for SRET items (fixed in codebook and cleaning script)
# "yi_raw" contains prefixes 9 columns ("bads" items) with prefix "b" (changed to "yi" in codebook and data)
# "y12m_raw" contains 1 column ("y312_pds_7") with incorrect prefix "y312" (fixed in clean data)
# "y18m_raw" contains 1 column (y18n_scsc_20) with incorrect prefix "y18n" (fixed in clean data)
# "p3m_raw" contains 1 column ("p3m\020") with incorrect prefix "p3m\020" (fixed in clean data)
# "p6m_raw" contains 26 columns ("scared_b" and "scared_c" items) with incorrect prefix "p3m" (fixed in clean data)

## In codebook
codebook_prefixes <- str_split_fixed(codebook$item, "_", 2)[, 1]
table(codebook_prefixes, useNA = "always")

# SRET items ("SRET", "SRET.keys", "SRET.time", "SRET.words", "tlcond") lack prefixes (fixed in codebook and cleaning script)
lapply(dat_ls_cols, function(dat_cols) {
  dat_cols$meas_item_cols[grepl("SRET|tlcond", dat_cols$meas_item_cols)]
})

# 9 items (for "bads" items at "yi") in codebook have prefix "b" (changed to "yi" in codebook and data)



####  Check other codebook columns  ####
## Check "wave"
table(codebook$wave, useNA = "always")
# View(codebook[codebook$wave == "", ])


## Check "measure" and "subscale"
table(codebook$measure, useNA = "always")
table(codebook$subscale, useNA = "always")


## Check "minimum" and "maximum"
free_response_col_patterns <- paste(
  c("_TEXT", "pds_4", "pfs_like", "pfs_dislike", "pfs_other",
  "pb_siblings_2", "pb_siblings_1", "pb_dependent", "pb_caregiver2_8", 
  "pb_caregiver2_1", "pb_caregiver1_8", "pb_birthorder"), collapse = "|")
free_response_cols <- codebook$item[grepl(free_response_col_patterns, codebook$item)]

select_multiple_cols <- setdiff(codebook$item[grepl("ppd_1", codebook$item)], free_response_cols)
numeric_cols <- codebook$item[grepl("pds_2|pds_3|pds_11|pds_12", codebook$item)]
demographic_cols <- codebook$item[codebook$measure == "demographic"]
sret_cols <- codebook$item[codebook$measure == "sret"]

ignore_cols <- c(free_response_cols, select_multiple_cols, numeric_cols, 
                 demographic_cols, sret_cols, "condition")

rows_missing_min_max <- codebook[!(codebook$item %in% ignore_cols) &
                                   (is.na(codebook$maximum) | is.na(codebook$minimum)), ]
stopifnot(nrow(rows_missing_min_max) == 0)

# Check that "minimum" and "maximum" are same across time for each measure
# - BHS was on 0-3 scale at all time points except "yi", where it was on 1-4 scale

ignore_measures <- c("condition", "demographic", "pds", "sitbi", "sret")
target_measures <- setdiff(unique(codebook$measure), ignore_measures)

for (measure in target_measures) {
  codebook_measure <- codebook[codebook$measure == measure &
                                 !(codebook$item %in% ignore_cols), ]
  
  min_values <- unique(codebook_measure$minimum)
  max_values <- unique(codebook_measure$maximum)
  
  if (length(min_values) > 1 | length(max_values) > 1) {
    cat("In codebook, '", measure, "' has min values of ", min_values, 
          " and max values of ", max_values, "\n")
  }
}

# View(codebook[codebook$measure == "bhs", ])


## Check "reversed" and "reverse_base"
table(codebook$reversed, codebook$reverse_base, useNA = "always")



####  Check for items in data across waves  ####
## Define function to check for item pattern in data across waves, with option
## to restrict to columns of a given type
check_item_pattern <- function(dat_ls_cols, pattern, col_type = "all") {
  if (col_type == "all") {
    cat("All columns:\n\n")
  } else {
    cat("Columns of type '", col_type, "':\n\n", sep = "")
  }
  
  lapply(dat_ls_cols, function(dat_cols) {
    if (col_type == "all") {
      cols <- unlist(dat_cols, use.names = FALSE)
    } else {
      cols <- dat_cols[[col_type]]
    }
    
    cols[grepl(pattern, cols)]
  })
}


## Define function to check label for item pattern in data across waves
check_item_pattern_label <- function(dat_ls, pattern) {
  lapply(dat_ls, function(dat) {
    target_cols <- names(dat)[grepl(pattern, names(dat))]
    
    sapply(target_cols, function(target_col) {
      attr(dat[[target_col]], "label")
    }, USE.NAMES = FALSE)
  })
}


## SCARED item "scared_c_11", which was absent from youth baseline survey in Phase 1
# It's present in Phase 2 as "I am shy" at "yb", "y3m", "y6m", "y12m", "y18m", and "y24m"
check_item_pattern(dat_ls_cols, "scared_c_11", "meas_item_cols")
check_item_pattern_label(dat_ls, "scared_c_11")


## Check for columns named identically in Qualtrics and thus named contingently
## by column index upon export into R
check_item_pattern(dat_ls_cols, "\\.\\.\\.")

# - "yi_raw" contains "lsmh_id...18" and "lsmh_id...601" (renamed in code)
# - "pb_raw" contains "test...511" and "test...512" (renamed in code)