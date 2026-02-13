## Track-to-Treat Phase 2 Data Cleaning, Youth Qualtrics, Move Rows to Correct Waves
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



####  Fix Column Names  ####
dat_ls_renamed <- dat_ls_raw %>%
  # Rename "mvps" to "mpvs" throughout with helper function
  modify_at(
    setdiff(names(.), "yi"),
    rename_mvps_to_mpvs
  ) %>%
  
  # Fix other column names
  modify_in("y12m", ~ rename(.x, y12m_pds_7 = y312_pds_7)) %>%
  modify_in("y18m", ~ rename(.x, y18m_scsc_20 = y18n_scsc_20))

# TODO: Add renaming from youth intervention script



# TODO: Remove above renaming from individual scripts



####  Move Rows to Correct Waves  ####
# TODO



####  Save Data  ####
# TODO


