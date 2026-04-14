## Track-to-Treat Phase 2 Data Cleaning, Parent Qualtrics, 24-Month Follow-Up
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
dirs <- get_p2_qualtrics_dirs("clean_data_staging_intermediate")

# Load corrected Qualtrics data
p24m_corrected <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Parent Qualtrics Corrected Data - List by Wave.rds") %>%
  pluck("p24m")


## Load ID lookup and corrected item-level codebook
id_lookup <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 ID Lookup.rds")
codebook <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Qualtrics Corrected Codebook.rds")


## Load assessment windows
ax_windows <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Assessment Windows.rds")



####  Clean Data  ####
### Create log
# Create lists for logging (a) items used to compute item completion rate below via
# compute_item_completion_rate() and (b) items used to compute means via mean_across()
log <- list(
  item_completion_rate = list(),
  mean_items = list()
)


### Fix LSMH IDs (manually as necessary)
p24m_fixed_ids <- p24m_corrected %>%
  rowwise() %>%
  mutate(
    lsmh_id = case_when(
      
      # Cases to be manually recoded (none)
      
      # All others (helper function for cases in which IDs are same or one/both IDs are missing)
      TRUE ~ resolve_id_pair(lsmh_id, p24m_lsmh_id)
      
    )
  ) %>%
  ungroup()

# Check LSMH ID format
warn_invalid_id_format(p24m_fixed_ids$lsmh_id)


### Remove invalid responses
# Filter to known valid LSMH IDs (marked "keep" in id_lookup) using helper function
p24m_valid_ids <- remove_invalid_p2_qualtrics_responses(p24m_fixed_ids, id_lookup)


### Identify duplicates and compute item completion rate for removing duplicates
# Identify duplicates using helper function
identify_duplicates(p24m_valid_ids, lsmh_id, phase = 2)

# Compute item completion rate using helper function (given that Qualtrics's "Progress"
# and "Finished" variables reflect only clicking through survey, not completing items)
p24m_valid_ids <- compute_item_completion_rate(p24m_valid_ids, "p24m", phase = 2)


### Remove any surveys (a) outside assessment window (or for parents of youth who 
### did not complete intervention survey in window) or (b) duplicated in window
# Compute indicators of survey completion in window using helper function
p24m_valid_ids <- mark_done_in_ax_window(p24m_valid_ids, "24m", ax_windows)

# Print (using helper function) and remove any surveys outside window
p24m_valid_ids_out_window <- get_surveys_outside_window_3m_onward(p24m_valid_ids, "24m") %>% print()

p24m_valid_ids <- p24m_valid_ids %>%
  filter(in_window_24m_ext)

# Remove duplicates using helper function
p24m_deduplicated <- remove_duplicates(p24m_valid_ids, lsmh_id)

# Double-check deduplication
identify_duplicates(p24m_deduplicated, lsmh_id, phase = 2)


### Clean columns
p24m_recoded <- p24m_deduplicated %>%
  
  # Un-reverse code items with helper function
  unreverse_code_items(codebook) %>%
  
  # Clean remaining columns by row and create composites using helper functions
  rowwise() %>%
  mutate(
    
    ## Metadata
    # ID ("lsmh_id" cleaned above)
    
    # Survey completion
    p24m_complete = !is.na(EndDate),
    
    # Survey datetime and duration
    p24m_datetime = EndDate,
    p24m_date = date(p24m_datetime),
    p24m_duration = EndDate - StartDate,
    
    # Follow-up survey completion in original and extended assessment 
    # windows and days survey was completed before/after original window
    p24m_in_window_org = in_window_24m_org,
    p24m_in_window_ext = in_window_24m_ext,
    p24m_days_before_start_window_24m_org = days_before_start_window_24m_org,
    p24m_days_after_end_window_24m_org = days_after_end_window_24m_org,
    
    
    ## Child treatment history (assessed at follow-ups only if "childtx_change" is Yes)
    # Current and lifetime treatment
    p24m_childtx_lifetime = p24m_childtx_1 == 1 | p24m_childtx_3 == 1,
    p24m_childtx_current = p24m_childtx_3 == 1,
    
    
    ## BACE (Barriers to Accessing Care Evaluation) overall mean score and subscale
    !!!bace_means("p24m"),
    
    
    ## BFAMG (Brief Family Assessment Measure - General Scale) overall mean score
    p24m_bfamg_mean = mean_across("p24m", "bfamg", name = "p24m_bfamg_mean"),
    
    
    ## BHS-4 (Beck Hopelessness Scale - 4-item) overall mean score
    p24m_bhs_mean = mean_across("p24m", "bhs", name = "p24m_bhs_mean"),
    
    
    ## 17 items from BSI-18 (Brief Symptom Inventory-18): overall mean score and subscales
    # Overall mean score and depression subscale lack suicidal thoughts item
    !!!bsi_means("p24m"),
    
    
    ## CDI-2-P (Children's Depression Inventory - 2 - Parent Report) overall mean score and subscales
    !!!cdi_p_means("p24m"),
    
    
    ## SCARED-Parent (Screen for Child Anxiety and Related Disorders - Parent) overall mean score and subscales
    !!!scared_means("p24m")
    
  ) %>%
  ungroup() %>%
  
  # Select variables
  select(
    
    # Metadata
    lsmh_id,
    p24m_complete,
    p24m_date,
    p24m_datetime,
    p24m_duration,
    ax_window_24m_start_org,
    ax_window_24m_end_org,
    ax_window_24m_start_ext,
    ax_window_24m_end_ext,
    p24m_in_window_org,
    p24m_in_window_ext,
    p24m_days_before_start_window_24m_org,
    p24m_days_after_end_window_24m_org,
    
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
items_to_check <- p24m_recoded %>%
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

walk(items_to_check, check_values, p24m_recoded) # check_values() helper function



####  Save Data  ####
# Save clean Qualtrics data
saveRDS(p24m_recoded, dirs$clean_data_staging_intermediate %+% "Phase 2 Parent Qualtrics Clean Data - 24m.rds")

# Save log
saveRDS(log, dirs$clean_data_staging_intermediate %+% "Phase 2 Parent Qualtrics Clean Data Log - 24m.rds")
