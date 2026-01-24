## Track-to-Treat Phase 2 Data Cleaning, Parent Qualtrics, 18-Month Follow-Up
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
dirs <- get_p2_qualtrics_dirs(c("raw_data", "clean_data_staging", "clean_data_staging_intermediate"))

# Load raw Qualtrics datasets (storing paths) in this format: [respondent][wave]_[administration]_raw
# - Note: Use "timeZone" specified for date columns (e.g., "StartDate") in third row of raw CSV
p18m_path <- dirs$raw_data %+% "DP5+Phase+2+-+Parent+-+FU+4+-+18M_January+21,+2026_11.18_n.csv"
p18m_raw <- read_survey(p18m_path, time_zone = "America/Chicago")


## Load ID lookup and (using helper function) item-level codebook
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))
codebook <- load_p2_codebook(here("Phase 2", "2025.07.02 Track to Treat P2 Codebook.xlsx"))


## Load assessment windows
ax_windows <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Assessment Windows.rds")


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
check_raw_data_ver(raw_metadata, list(p18m_path), list(p18m_raw), "p18m_qualtrics")



####  Clean Data  ####
### Create log
# Create lists for logging (a) items used to compute item completion rate below via
# compute_item_completion_rate() and (b) items used to compute means via mean_across()
log <- list(
  item_completion_rate = list(),
  mean_items = list()
)