## Track-to-Treat Phase 2 Data Cleaning, Youth Qualtrics, 18-Month Follow-Up
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
y18m_path <- dirs$raw_data %+% "DP5+Phase+2+-+Youth+-+FU+4+-+18M_January+21,+2026_11.25_n.csv"
y18m_raw <- read_survey(y18m_path, time_zone = "America/Chicago")


## Load ID lookup and (using helper function) item-level codebook
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))
codebook <- load_p2_codebook(here("Phase 2", "2025.07.02 Track to Treat P2 Codebook.xlsx"))


## Load assessment windows
ax_windows <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Assessment Windows.rds")


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
check_raw_data_ver(raw_metadata, list(y18m_path), list(y18m_raw), "y18m_qualtrics")



####  Clean Data  ####
### Create log
# Create lists for logging (a) items used to compute item completion rate below via
# compute_item_completion_rate() and (b) items used to compute means via mean_across()
log <- list(
  item_completion_rate = list(),
  mean_items = list()
)


### Correct LSMH IDs (manually as necessary)
y18m_corrected_ids <- y18m_raw %>%
  rowwise() %>%
  mutate(
    lsmh_id = case_when(
      
      # TODO (any others?): Cases to be manually recoded
      
      
      


      # All others (helper function for cases in which IDs are same or one/both IDs are missing)
      TRUE ~ resolve_id_pair(lsmh_id, y18m_lsmh_id)
      
    )
  ) %>%
  ungroup()

# Check LSMH ID format
warn_invalid_id_format(y18m_corrected_ids$lsmh_id)


### Remove invalid responses
# Filter to known valid LSMH IDs (marked "keep" in id_lookup) using helper function
y18m_valid_ids <- remove_invalid_p2_qualtrics_responses(y18m_corrected_ids, id_lookup)


### Identify duplicates and compute item completion rate for removing duplicates
# Identify duplicates using helper function
identify_duplicates(y18m_valid_ids, lsmh_id, phase = 2)

# Compute item completion rate using helper function (given that Qualtrics's "Progress"
# and "Finished" variables reflect only clicking through survey, not completing items)
y18m_valid_ids <- compute_item_completion_rate(y18m_valid_ids, "y18m", phase = 2)


### Remove any surveys (a) outside assessment window (or for youth who did not 
### complete intervention survey in window) or (b) duplicated in window
# Compute indicators of survey completion in window using helper function
y18m_valid_ids <- mark_fu_done_in_ax_window(y18m_valid_ids, "18m", ax_windows)

# Print (using helper function) and remove any surveys outside window          # TODO: Finalize windows
y18m_valid_ids_out_window <- get_surveys_outside_window_3m_onward(y18m_valid_ids, "18m") %>% print()

y18m_valid_ids <- y18m_valid_ids %>%
  filter(in_window_18m_ext)

# Remove duplicates using helper function
y18m_deduplicated <- remove_duplicates(y18m_valid_ids, lsmh_id)

# Double-check deduplication
identify_duplicates(y18m_deduplicated, lsmh_id, phase = 2)


### Clean columns
y18m_recoded <- y18m_deduplicated %>%
  
  # Remove click, page time variables with helper function
  rm_click_page_time_vars() %>%
  
  # Rename "mvps" to "mpvs" throughout with helper function
  rename_mvps_to_mpvs() %>%
  
  # Fix column name
  rename(y18m_scsc_20 = y18n_scsc_20) %>%
  
  # Un-reverse code items with helper function
  unreverse_code_items(codebook) %>%
  
  # Clean remaining columns by row and create composites using helper functions
  rowwise() %>%
  mutate(
    
    ## Metadata
    # ID ("lsmh_id" cleaned above)
    
    # Survey completion
    y18m_complete = !is.na(EndDate),
    
    # Survey datetime and duration
    y18m_datetime = EndDate,
    y18m_date = date(y18m_datetime),
    y18m_duration = EndDate - StartDate,
    
    # Follow-up survey completion in original and extended assessment 
    # windows and days survey was completed before/after original window
    y18m_in_window_org = in_window_18m_org,
    y18m_in_window_ext = in_window_18m_ext,
    y18m_days_before_start_window_18m_org = days_before_start_window_18m_org,
    y18m_days_after_end_window_18m_org = days_after_end_window_18m_org)




