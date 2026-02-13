## Track-to-Treat Phase 2 Data Cleaning, Parent Qualtrics, Move Rows to Correct Waves
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
  pb = raw_data_dir %+% "DP5+Phase+2+-+Parent+-+Baseline_January+21,+2026_11.17_n.csv",
  p3m = raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+1+-+3M_January+21,+2026_11.18_n.csv",
  p6m = raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+2+-+6M_January+21,+2026_11.18_n.csv",
  p12m = raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+3+-+12M_January+21,+2026_11.18_n.csv",
  p18m = raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+4+-+18M_January+21,+2026_11.18_n.csv",
  p24m = raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+5+-+24M_January+21,+2026_11.19_n.csv"
)

dat_ls_raw <- lapply(raw_data_paths, read_survey, time_zone = "America/Chicago")


## Load ID lookup and (using helper function) item-level codebook
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))
codebook <- load_p2_codebook(here("Phase 2", "2026.02.12 Track to Treat P2 Codebook.xlsx"))


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
p_data_types <- "p" %+% c("b", "i", c(3, 6, 12, 18, 24) %+% "m") %+% "_qualtrics"
check_raw_data_ver(raw_metadata, raw_data_paths, dat_ls_raw, p_data_types)



####  Fix Column Names  ####
dat_ls_renamed <- dat_ls_raw %>%
  # Fix names of "accommodations_2" items
  modify_in("p3m", ~ rename(.x, p3m_accommodations_2 = `p3m\020_accom_2`)) %>%
  modify_in("p12m", ~ rename(.x, p12m_accommodations_2 = p12m_accommodations_)) %>%
  modify_in("p18m", ~ rename(.x, p18m_accommodations_2 = p18m_accommodations_)) %>%
  
  # Fix prefix of "scared_b" and "scared_c" items
  modify_in("p6m", ~ rename_with(
    .x,
    .cols = contains(c("scared_b", "scared_c")),
    .fn = ~ sub("^p3m_", "p6m_", .x)
  ))

# TODO: Remove above renaming from individual scripts



####  Move Rows to Correct Waves  ####
# TODO



####  Save Data  ####
# TODO


