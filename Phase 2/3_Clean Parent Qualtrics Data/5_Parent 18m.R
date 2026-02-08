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


### Correct LSMH IDs (manually as necessary)
p18m_corrected_ids <- p18m_raw %>%
  rowwise() %>%
  mutate(
    lsmh_id = case_when(
      
      # TODO (any others?): Cases to be manually recoded
      lsmh_id == "LSMH01710" & p18m_lsmh_id == "LSMH17100" ~ "LSMH01710",
      lsmh_id == "LSMH02307" & p18m_lsmh_id == "LSMH02265" ~ "LSMH02265",
      
      
      
      
      
      # All others (helper function for cases in which IDs are same or one/both IDs are missing)
      TRUE ~ resolve_id_pair(lsmh_id, p18m_lsmh_id)
      
    )
  ) %>%
  ungroup()

# Check LSMH ID format
warn_invalid_id_format(p18m_corrected_ids$lsmh_id)


### Remove invalid responses
# Filter to known valid LSMH IDs (marked "keep" in id_lookup) using helper function
p18m_valid_ids <- remove_invalid_p2_qualtrics_responses(p18m_corrected_ids, id_lookup)


### Identify duplicates and compute item completion rate for removing duplicates
# Identify duplicates using helper function
identify_duplicates(p18m_valid_ids, lsmh_id, phase = 2)

# Compute item completion rate using helper function (given that Qualtrics's "Progress"
# and "Finished" variables reflect only clicking through survey, not completing items)
p18m_valid_ids <- compute_item_completion_rate(p18m_valid_ids, "p18m", phase = 2)


### Remove any surveys (a) outside assessment window (or for parents of youth who 
### did not complete intervention survey in window) or (b) duplicated in window
# Compute indicators of survey completion in window using helper function
p18m_valid_ids <- mark_fu_done_in_ax_window(p18m_valid_ids, "18m", ax_windows)

# Print (using helper function) and remove any surveys outside window
p18m_valid_ids_out_window <- get_surveys_outside_window_3m_onward(p18m_valid_ids, "18m") %>% print()

p18m_valid_ids <- p18m_valid_ids %>%
  filter(in_window_18m_ext)

# Remove duplicates using helper function
p18m_deduplicated <- remove_duplicates(p18m_valid_ids, lsmh_id)

# Double-check deduplication
identify_duplicates(p18m_deduplicated, lsmh_id, phase = 2)


### Clean columns
p18m_recoded <- p18m_deduplicated %>%
  
  # Remove click, page time variables with helper function
  rm_click_page_time_vars() %>%
  
  # Fix column name
  rename(p18m_accommodations_2 = p18m_accommodations_) %>%
  
  # Un-reverse code items with helper function
  unreverse_code_items(codebook) %>%
  
  # Clean remaining columns by row and create composites using helper functions
  rowwise() %>%
  mutate(
    
    ## Metadata
    # ID ("lsmh_id" cleaned above)
    
    # Survey completion
    p18m_complete = !is.na(EndDate),
    
    # Survey datetime and duration
    p18m_datetime = EndDate,
    p18m_date = date(p18m_datetime),
    p18m_duration = EndDate - StartDate,
    
    # Follow-up survey completion in original and extended assessment 
    # windows and days survey was completed before/after original window
    p18m_in_window_org = in_window_18m_org,
    p18m_in_window_ext = in_window_18m_ext,
    p18m_days_before_start_window_18m_org = days_before_start_window_18m_org,
    p18m_days_after_end_window_18m_org = days_after_end_window_18m_org,
    
    
    ## Child treatment history (assessed at follow-ups only if "childtx_change" is Yes)
    # Current and lifetime treatment
    p18m_childtx_lifetime = p18m_childtx_1 == 1 | p18m_childtx_3 == 1,
    p18m_childtx_current = p18m_childtx_3 == 1,
    
    
    ## BACE (Barriers to Accessing Care Evaluation) overall mean score and subscale
    !!!bace_means("p18m"),
    
    
    ## BFAMG (Brief Family Assessment Measure - General Scale) overall mean score
    p18m_bfamg_mean = mean_across("p18m", "bfamg", name = "p18m_bfamg_mean"),
    
    
    ## BHS-4 (Beck Hopelessness Scale - 4-item) overall mean score
    p18m_bhs_mean = mean_across("p18m", "bhs", name = "p18m_bhs_mean"),
    
    
    ## 17 items from BSI-18 (Brief Symptom Inventory-18): overall mean score and subscales
    # Overall mean score and depression subscale lack suicidal thoughts item
    !!!bsi_means("p18m"),
    
    
    ## CDI-2-P (Children's Depression Inventory - 2 - Parent Report) overall mean score and subscales
    !!!cdi_p_means("p18m"),
    
    
    ## SCARED-Parent (Screen for Child Anxiety and Related Disorders - Parent) overall mean score and subscales
    !!!scared_means("p18m")
    
  ) %>%
  ungroup() %>%
  
  # Select variables
  select(
    
    # Metadata
    lsmh_id,
    p18m_complete,
    p18m_date,
    p18m_datetime,
    p18m_duration,
    ax_window_18m_start_org,
    ax_window_18m_end_org,
    ax_window_18m_start_ext,
    ax_window_18m_end_ext,
    p18m_in_window_org,
    p18m_in_window_ext,
    p18m_days_before_start_window_18m_org,
    p18m_days_after_end_window_18m_org,
    
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
items_to_check <- p18m_recoded %>%
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

walk(items_to_check, check_values, p18m_recoded) # check_values() helper function



####  Save Data  ####
# Save clean Qualtrics data
saveRDS(p18m_recoded, dirs$clean_data_staging %+% "Phase 2 Parent Qualtrics Clean Data - 18m.rds")

# Save log
saveRDS(log, dirs$clean_data_staging_intermediate %+% "Phase 2 Parent Qualtrics Clean Data Log - 18m.rds")
