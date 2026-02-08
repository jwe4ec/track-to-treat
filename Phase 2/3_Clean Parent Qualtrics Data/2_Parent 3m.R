## Track-to-Treat Phase 2 Data Cleaning, Parent Qualtrics, 3-Month Follow-Up
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
p3m_path <- dirs$raw_data %+% "DP5+Phase+2+-+Parent+-+FU+1+-+3M_January+21,+2026_11.18_n.csv"
p3m_raw <- read_survey(p3m_path, time_zone = "America/Chicago")


## Load ID lookup and (using helper function) item-level codebook
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))
codebook <- load_p2_codebook(here("Phase 2", "2025.07.02 Track to Treat P2 Codebook.xlsx"))


## Load assessment windows
ax_windows <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Assessment Windows.rds")


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
check_raw_data_ver(raw_metadata, list(p3m_path), list(p3m_raw), "p3m_qualtrics")



####  Clean Data  ####
### Create log
# Create lists for logging (a) items used to compute item completion rate below via
# compute_item_completion_rate() and (b) items used to compute means via mean_across()
log <- list(
  item_completion_rate = list(),
  mean_items = list()
)


### Correct LSMH IDs (manually as necessary)
p3m_corrected_ids <- p3m_raw %>%
  rowwise() %>%
  mutate(
    lsmh_id = case_when(
      
      # Cases to be manually recoded
      lsmh_id == "LMSH00886" ~ "LSMH00886",
      
      # All others (helper function for cases in which IDs are same or one/both IDs are missing)
      TRUE ~ resolve_id_pair(lsmh_id, p3m_lsmh_id)
      
    )
  ) %>%
  ungroup()

# Check LSMH ID format
warn_invalid_id_format(p3m_corrected_ids$lsmh_id)


### Remove invalid responses
# Filter to known valid LSMH IDs (marked "keep" in id_lookup) using helper function
p3m_valid_ids <- remove_invalid_p2_qualtrics_responses(p3m_corrected_ids, id_lookup)


### Identify duplicates and compute item completion rate for removing duplicates
# Identify duplicates using helper function
identify_duplicates(p3m_valid_ids, lsmh_id, phase = 2)

# Compute item completion rate using helper function (given that Qualtrics's "Progress"
# and "Finished" variables reflect only clicking through survey, not completing items)
p3m_valid_ids <- compute_item_completion_rate(p3m_valid_ids, "p3m", phase = 2)


### Remove any surveys (a) outside assessment window (or for parents of youth who 
### did not complete intervention survey in window) or (b) duplicated in window
# Compute indicators of survey completion in window using helper function
p3m_valid_ids <- mark_fu_done_in_ax_window(p3m_valid_ids, "3m", ax_windows)

# Print (using helper function) and remove any surveys outside window
p3m_valid_ids_out_window <- get_surveys_outside_window_3m_onward(p3m_valid_ids, "3m") %>% print()

p3m_valid_ids <- p3m_valid_ids %>%
  filter(in_window_3m_ext)

# Remove duplicates using helper function
p3m_deduplicated <- remove_duplicates(p3m_valid_ids, lsmh_id)

# Double-check deduplication
identify_duplicates(p3m_deduplicated, lsmh_id, phase = 2)


### Clean columns
p3m_recoded <- p3m_deduplicated %>%
  
  # Remove click, page time variables with helper function
  rm_click_page_time_vars() %>%
  
  # Fix column name
  rename(p3m_accommodations_2 = `p3m\020_accom_2`) %>%
  
  # Un-reverse code items with helper function
  unreverse_code_items(codebook) %>%
  
  # Clean remaining columns by row and create composites using helper functions
  rowwise() %>%
  mutate(
    
    ## Metadata
    # ID ("lsmh_id" cleaned above)
    
    # Survey completion
    p3m_complete = !is.na(EndDate),
    
    # Survey datetime and duration
    p3m_datetime = EndDate,
    p3m_date = date(p3m_datetime),
    p3m_duration = EndDate - StartDate,
    
    # Follow-up survey completion in original and extended assessment 
    # windows and days survey was completed before/after original window
    p3m_in_window_org = in_window_3m_org,
    p3m_in_window_ext = in_window_3m_ext,
    p3m_days_before_start_window_3m_org = days_before_start_window_3m_org,
    p3m_days_after_end_window_3m_org = days_after_end_window_3m_org,
    
    
    ## Child treatment history (assessed at follow-ups only if "childtx_change" is Yes)
    # Current and lifetime treatment
    p3m_childtx_lifetime = p3m_childtx_1 == 1 | p3m_childtx_3 == 1,
    p3m_childtx_current = p3m_childtx_3 == 1,
    
    
    ## BACE (Barriers to Accessing Care Evaluation) overall mean score and subscale
    !!!bace_means("p3m"),
    
    
    ## BFAMG (Brief Family Assessment Measure - General Scale) overall mean score
    p3m_bfamg_mean = mean_across("p3m", "bfamg", name = "p3m_bfamg_mean"),
    
    
    ## BHS-4 (Beck Hopelessness Scale - 4-item) overall mean score
    p3m_bhs_mean = mean_across("p3m", "bhs", name = "p3m_bhs_mean"),
    
    
    ## 17 items from BSI-18 (Brief Symptom Inventory-18): overall mean score and subscales
    # Overall mean score and depression subscale lack suicidal thoughts item
    !!!bsi_means("p3m"),
    
    
    ## CDI-2-P (Children's Depression Inventory - 2 - Parent Report) overall mean score and subscales
    !!!cdi_p_means("p3m"),
    
    
    ## SCARED-Parent (Screen for Child Anxiety and Related Disorders - Parent) overall mean score and subscales
    !!!scared_means("p3m")

  ) %>%
  ungroup() %>%
  
  # Select variables
  select(
    
    # Metadata
    lsmh_id,
    p3m_complete,
    p3m_date,
    p3m_datetime,
    p3m_duration,
    ax_window_3m_start_org,
    ax_window_3m_end_org,
    ax_window_3m_start_ext,
    ax_window_3m_end_ext,
    p3m_in_window_org,
    p3m_in_window_ext,
    p3m_days_before_start_window_3m_org,
    p3m_days_after_end_window_3m_org,

    # Child treatment history
    matches("childtx_change"),
    matches("childtx_lifetime"),
    matches("childtx_current"),
    
    # Measures
    matches("_bace_"),
    matches("_bfamg_"),
    matches("_bhs_"),
    matches("_bsi_"),
    matches("_cdi_"),
    matches("_scared_")
    
  )


### Check that values are in expected range
items_to_check <- p3m_recoded %>%
  select(
    matches("_bace_"),
    matches("_bfamg_"),
    matches("_bhs_"),
    matches("_bsi_"),
    matches("_cdi_"),
    matches("_scared_"),
    -ends_with("mean")
  ) %>%
  names()

walk(items_to_check, check_values, p3m_recoded) # check_values() helper function



####  Save Data  ####
# Save clean Qualtrics data
saveRDS(p3m_recoded, dirs$clean_data_staging %+% "Phase 2 Parent Qualtrics Clean Data - 3m.rds")

# Save log
saveRDS(log, dirs$clean_data_staging_intermediate %+% "Phase 2 Parent Qualtrics Clean Data Log - 3m.rds")
