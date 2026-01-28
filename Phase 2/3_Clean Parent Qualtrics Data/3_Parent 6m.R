## Track-to-Treat Phase 2 Data Cleaning, Parent Qualtrics, 6-Month Follow-Up
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
p6m_path <- dirs$raw_data %+% "DP5+Phase+2+-+Parent+-+FU+2+-+6M_January+21,+2026_11.18_n.csv"
p6m_raw <- read_survey(p6m_path, time_zone = "America/Chicago")


## Load ID lookup and (using helper function) item-level codebook
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))
codebook <- load_p2_codebook(here("Phase 2", "2025.07.02 Track to Treat P2 Codebook.xlsx"))


## Load assessment windows
ax_windows <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Assessment Windows.rds")


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
check_raw_data_ver(raw_metadata, list(p6m_path), list(p6m_raw), "p6m_qualtrics")



####  Clean Data  ####
### Create log
# Create lists for logging (a) items used to compute item completion rate below via
# compute_item_completion_rate() and (b) items used to compute means via mean_across()
log <- list(
  item_completion_rate = list(),
  mean_items = list()
)


### Correct LSMH IDs (manually as necessary)
p6m_corrected_ids <- p6m_raw %>%
  rowwise() %>%
  mutate(
    lsmh_id = case_when(
      
      # TODO (any others?): Cases to be manually recoded
      lsmh_id == "LMSH00886" & p6m_lsmh_id == "LSMH00886" ~ "LSMH00886",
      
      
      

      
      # All others (helper function for cases in which IDs are same or one/both IDs are missing)
      TRUE ~ resolve_id_pair(lsmh_id, p6m_lsmh_id)
      
    )
  ) %>%
  ungroup()

# Check LSMH ID format
warn_invalid_id_format(p6m_corrected_ids$lsmh_id)


### Remove invalid responses
# Filter to known valid LSMH IDs (marked "keep" in id_lookup) using helper function
p6m_valid_ids <- remove_invalid_p2_qualtrics_responses(p6m_corrected_ids, id_lookup)


### Identify duplicates and compute item completion rate for removing duplicates
# Identify duplicates using helper function
identify_duplicates(p6m_valid_ids, lsmh_id, phase = 2)

# Compute item completion rate using helper function (given that Qualtrics's "Progress"
# and "Finished" variables reflect only clicking through survey, not completing items)
p6m_valid_ids <- compute_item_completion_rate(p6m_valid_ids, "p6m", phase = 2)


### Remove any surveys (a) outside assessment window (or for parents of youth who 
### did not complete intervention survey in window) or (b) duplicated in window
# Compute indicators of survey completion in window using helper function
p6m_valid_ids <- mark_fu_done_in_ax_window(p6m_valid_ids, "6m", ax_windows)

# Print (using helper function) and remove any surveys outside window          # TODO: Finalize windows
p6m_valid_ids_out_window <- get_surveys_outside_window_3m_onward(p6m_valid_ids, "6m") %>% print()

p6m_valid_ids <- p6m_valid_ids %>%
  filter(in_window_6m_ext)

# Remove duplicates using helper function
p6m_deduplicated <- remove_duplicates(p6m_valid_ids, lsmh_id)

# Double-check deduplication
identify_duplicates(p6m_deduplicated, lsmh_id, phase = 2)




