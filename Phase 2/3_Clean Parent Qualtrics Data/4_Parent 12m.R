## Track-to-Treat Phase 2 Data Cleaning, Parent Qualtrics, 12-Month Follow-Up
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


## Load Qualtrics data
# Get directories using helper function
dirs <- get_p2_qualtrics_dirs(c("interim_raw_data", "clean_data_staging", "clean_data_staging_intermediate"))
raw_data_dir <- dirs$interim_raw_data

# Load raw Qualtrics datasets (storing paths) in this format: [respondent][wave]_[administration]_raw
# - Note: Use "timeZone" specified for date columns (e.g., "StartDate") in third row of raw CSV
p12m_path <- raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+3+-+12M_May+6,+2025_09.48_n.csv"
p12m_raw <- read_survey(p12m_path, time_zone = "America/Chicago")


## Load ID lookup and (using helper function) item-level codebook
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))
codebook <- load_p2_codebook(here("Phase 2", "2025.07.02 Track to Treat P2 Codebook.xlsx"))


## Load assessment windows
ax_windows <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Assessment Windows.rds")


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
check_raw_data_ver(raw_metadata, list(p12m_path), list(p12m_raw), "p12m_qualtrics")