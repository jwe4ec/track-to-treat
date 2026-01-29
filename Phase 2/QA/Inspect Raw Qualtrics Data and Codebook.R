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


## Load Qualtrics data at all waves
dirs <- get_p2_qualtrics_dirs("raw_data")
raw_data_dir <- dirs$raw_data

yb_path <- raw_data_dir %+% "DP5+Phase+2+-+Youth+-+Baseline_January+21,+2026_11.24_n.csv"
yi_path <- raw_data_dir %+% "DP5+Phase+2+-+Youth+-+Interventions_January+21,+2026_11.25_n.csv"
y3m_path <- raw_data_dir %+% "DP5+Phase+2+-+Youth+-+FU+1+-+3M_January+21,+2026_11.24_n.csv"
y6m_path <- raw_data_dir %+% "DP5+Phase+2+-+Youth+-+FU+2+-+6M_January+21,+2026_11.24_n.csv"
y12m_path <- raw_data_dir %+% "DP5+Phase+2+-+Youth+-+FU+3+-+12M_January+21,+2026_11.24_n.csv"
y18m_path <- raw_data_dir %+% "DP5+Phase+2+-+Youth+-+FU+4+-+18M_January+21,+2026_11.25_n.csv"
y24m_path <- raw_data_dir %+% "DP5+Phase+2+-+Youth+-+FU+5+-+24M_January+29,+2026_10.59_n.csv"

pb_path <- raw_data_dir %+% "DP5+Phase+2+-+Parent+-+Baseline_January+21,+2026_11.17_n.csv"
p3m_path <- raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+1+-+3M_January+21,+2026_11.18_n.csv"
p6m_path <- raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+2+-+6M_January+21,+2026_11.18_n.csv"
p12m_path <- raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+3+-+12M_January+21,+2026_11.18_n.csv"
p18m_path <- raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+4+-+18M_January+21,+2026_11.18_n.csv"
p24m_path <- raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+5+-+24M_January+21,+2026_11.19_n.csv"

yb_raw <- read_survey(yb_path, time_zone = "America/Chicago")
yi_raw <- read_survey(yi_path, time_zone = "America/Chicago")
y3m_raw <- read_survey(y3m_path, time_zone = "America/Chicago")
y6m_raw <- read_survey(y6m_path, time_zone = "America/Chicago")
y12m_raw <- read_survey(y12m_path, time_zone = "America/Chicago")
y18m_raw <- read_survey(y18m_path, time_zone = "America/Chicago")
y24m_raw <- read_survey(y24m_path, time_zone = "America/Chicago")

pb_raw <- read_survey(pb_path, time_zone = "America/Chicago")
p3m_raw <- read_survey(p3m_path, time_zone = "America/Chicago")
p6m_raw <- read_survey(p6m_path, time_zone = "America/Chicago")
p12m_raw <- read_survey(p12m_path, time_zone = "America/Chicago")
p18m_raw <- read_survey(p18m_path, time_zone = "America/Chicago")
p24m_raw <- read_survey(p24m_path, time_zone = "America/Chicago")

# Collect waves in list
dat_ls <- list(yb_raw = yb_raw,
               yi_raw = yi_raw,
               y3m_raw = y3m_raw,
               y6m_raw = y6m_raw,
               y12m_raw = y12m_raw,
               y18m_raw = y18m_raw,
               y24m_raw = y24m_raw,
               pb_raw = pb_raw,
               p3m_raw = p3m_raw,
               p6m_raw = p6m_raw,
               p12m_raw = p12m_raw,
               p18m_raw = p18m_raw,
               p24m_raw = p24m_raw)


## Load ID lookup
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))


## Load item-level codebook file using helper function
codebook <- load_p2_codebook(here("Phase 2", "2025.07.02 Track to Treat P2 Codebook.xlsx"))



#### Check that all "_n" files are indeed numeric (based on example columns)
ex_col_classes <- lapply(names(dat_ls), function(name) {
  dat <- dat_ls[[name]]
  
  if (name == "yi_raw") {
    ex_col <- grep("bads_1$", names(dat), value = TRUE)
  } else {
    ex_col <- grep("cdi_1$", names(dat), value = TRUE)
  }
  
  return(class(dat[[ex_col]]))
})

all(ex_col_classes == "numeric")



#### Identify and inspect data columns by type at each wave ####
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
lapply(dat_ls_cols, function(x) x$q_meta_cols)
lapply(dat_ls_cols, function(x) x$click_submit_cols)
lapply(dat_ls_cols, function(x) x$id_cols)   # Note: ID column names vary by wave
lapply(dat_ls_cols, function(x) x$date_cols) # Note: Youth data lack "date" columns
lapply(dat_ls_cols, function(x) x$test_cols)
lapply(dat_ls_cols, function(x) x$other_cols)
lapply(dat_ls_cols, function(x) x$ssi_item_cols)
lapply(dat_ls_cols, function(x) x$meas_item_cols)



#### Check for measure items missing from codebook ####
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


#### Check for codebook items not in data ####
all_meas_item_cols <- unlist(lapply(dat_ls_cols, function(dat_cols) {
  dat_cols$meas_item_cols
}), use.names = FALSE)

# "accommodations_2" columns resolved above
diff_accommodations_cols <- sort(setdiff(codebook$item[codebook$measure == "demographic" &
                                           grepl("accommodations", codebook$item)], all_meas_item_cols))

# MPVS columns resolved above
diff_mpvs_cols        <- sort(setdiff(codebook$item[codebook$measure == "mpvs"], all_meas_item_cols))

# In "p6m_raw", "scared_b" and "scared_c" items have incorrect prefix "p3m" (renamed in clean data)
diff_scared_cols      <- sort(setdiff(codebook$item[codebook$measure == "scared"], all_meas_item_cols))
names(dat_ls$p6m_raw)[grepl("scared_b|scared_c", names(dat_ls$p6m_raw))]

# 2 other PDS and SCSC columns resolved above
ignore_measures <- c("demographic", "ace_p", "ace_y", "mpvs", "scared", "other")
diff_other_cols       <- sort(setdiff(codebook$item[!(codebook$measure %in% ignore_measures) &
                                                        codebook$item != "condition"], all_meas_item_cols))
diff_other_cols == c("y12m_pds_7", "y18m_scsc_20")



#### Check item prefixes ####
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



#### Check other codebook columns ####
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
nrow(rows_missing_min_max) == 0

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



#### Check for items in data across waves ####
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