## Track-to-Treat Phase 2 Data Cleaning, Youth Qualtrics, 24-Month Follow-Up
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
y24m_corrected <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Corrected Data - List by Wave.rds") %>%
  pluck("y24m")


## Load ID lookup and corrected item-level codebook
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))
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
y24m_fixed_ids <- y24m_corrected %>%
  rowwise() %>%
  mutate(
    lsmh_id = case_when(
      
      # Cases to be manually recoded (none)
      
      # All others (helper function for cases in which IDs are same or one/both IDs are missing)
      TRUE ~ resolve_id_pair(lsmh_id, y24m_lsmh_id)
      
    )
  ) %>%
  ungroup()

# Check LSMH ID format
warn_invalid_id_format(y24m_fixed_ids$lsmh_id)


### Remove invalid responses
# Filter to known valid LSMH IDs (marked "keep" in id_lookup) using helper function
y24m_valid_ids <- remove_invalid_p2_qualtrics_responses(y24m_fixed_ids, id_lookup)


### Identify duplicates and compute item completion rate for removing duplicates
# Identify duplicates using helper function
identify_duplicates(y24m_valid_ids, lsmh_id, phase = 2)

# Compute item completion rate using helper function (given that Qualtrics's "Progress"
# and "Finished" variables reflect only clicking through survey, not completing items)
y24m_valid_ids <- compute_item_completion_rate(y24m_valid_ids, "y24m", phase = 2)


### Remove any surveys (a) outside assessment window (or for youth who did not 
### complete intervention survey in window) or (b) duplicated in window
# Compute indicators of survey completion in window using helper function
y24m_valid_ids <- mark_fu_done_in_ax_window(y24m_valid_ids, "24m", ax_windows)

# Print (using helper function) and remove any surveys outside window
y24m_valid_ids_out_window <- get_surveys_outside_window_3m_onward(y24m_valid_ids, "24m") %>% print()

y24m_valid_ids <- y24m_valid_ids %>%
  filter(in_window_24m_ext)

# Remove duplicates using helper function
y24m_deduplicated <- remove_duplicates(y24m_valid_ids, lsmh_id)

# Double-check deduplication
identify_duplicates(y24m_deduplicated, lsmh_id, phase = 2)


### Clean columns
y24m_recoded <- y24m_deduplicated %>%
  
  # Un-reverse code items with helper function
  unreverse_code_items(codebook) %>%
  
  # Clean remaining columns by row and create composites using helper functions
  rowwise() %>%
  mutate(
    
    ## Metadata
    # ID ("lsmh_id" cleaned above)
    
    # Survey completion
    y24m_complete = !is.na(EndDate),
    
    # Survey datetime and duration
    y24m_datetime = EndDate,
    y24m_date = date(y24m_datetime),
    y24m_duration = EndDate - StartDate,
    
    # Follow-up survey completion in original and extended assessment 
    # windows and days survey was completed before/after original window
    y24m_in_window_org = in_window_24m_org,
    y24m_in_window_ext = in_window_24m_ext,
    y24m_days_before_start_window_24m_org = days_before_start_window_24m_org,
    y24m_days_after_end_window_24m_org = days_after_end_window_24m_org,
    
    
    ## BADS (Behavioral Activation for Depression Scale) subscales
    !!!bads_means("y24m"),
    
    
    ## BFAMG (Brief Family Assessment Measure - General Scale) overall mean score
    y24m_bfamg_mean = mean_across("y24m", "bfamg", name = "y24m_bfamg_mean"),
    
    
    ## BHS-4 (Beck Hopelessness Scale - 4-item) overall mean score
    y24m_bhs_mean = mean_across("y24m", "bhs", name = "y24m_bhs_mean"),
    
    
    ## CDI-2-SR (Children's Depression Inventory - 2 - Self-Report) overall mean score and subscales
    !!!cdi_sr_means("y24m"),
    
    
    ## DRS (Dietary Restriction Screener)
    # Two items that do not need to be recoded or combined
    
    
    ## IDAS-II (Inventory of Depression and Anxiety Symptoms - II)
    # Given that scoring likely depends on intended use, we output items but do 
    # not score them (see Table 1 of https://doi.org/f4b85p for scale info)
    
    
    ## IPTQ (Implicit Personality Theory Questionnaire) overall mean score
    y24m_iptq_mean = mean_across("y24m", "iptq", name = "y24m_iptq_mean"),
    
    
    ## MPVS (Multidimensional Peer Victimization Scale) overall mean score and subscales
    !!!mpvs_means("y24m"),
    
    
    ## PCSC (Primary Control Scale for Children) overall mean score and subscales
    !!!pcsc_means("y24m"),
    
    
    ## SCARED-Child (Screen for Child Anxiety and Related Disorders - Child) overall mean score and subscales
    !!!scared_means("y24m"),
    
    
    ## SCSC (Secondary Control Scale for Children) overall mean score
    y24m_scsc_mean = mean_across("y24m", "scsc", name = "y24m_scsc_mean"),
    
    
    ## SHAPS (Snaith-Hamilton Pleasure Scale) overall mean score
    y24m_shaps_mean = mean_across("y24m", "shaps", name = "y24m_shaps_mean"),
    
    
    ## SHS (Self-Hate Scale) overall mean score
    y24m_self_hate_mean = mean_across("y24m", "self_hate_scale", name = "y24m_self_hate_mean"),
    
    
    ## SITBI-SF (Self-Injurious Thoughts and Behaviors Interview - Short Form)
    # Many items but no combining
    # Some recoding in "Correct Codebook and Raw Youth Data.R", but ranges still need to be checked
    
    
    ## UCLA (UCLA Loneliness Scale, aka ULS) overall mean score
    y24m_ucla_mean = mean_across("y24m", "ucla", name = "y24m_ucla_mean")
    
  ) %>%
  ungroup() %>%
  
  # Select variables
  select(
    
    # Metadata
    lsmh_id,
    y24m_complete,
    y24m_date,
    y24m_datetime,
    y24m_duration,
    ax_window_24m_start_org,
    ax_window_24m_end_org,
    ax_window_24m_start_ext,
    ax_window_24m_end_ext,
    y24m_in_window_org,
    y24m_in_window_ext,
    y24m_days_before_start_window_24m_org,
    y24m_days_after_end_window_24m_org,
    
    # Measures
    matches("_bads_"),
    matches("_bfamg_"),
    matches("_bhs_"),
    matches("_cdi_"),
    matches("_drs_"),
    matches("_idas_"),
    matches("_iptq_"),
    matches("_mpvs_"),
    matches("_pcsc_"),
    matches("_scared_"),
    matches("_scsc_"),
    matches("_shaps_"),
    matches("_shs_"),
    matches("_self_hate_"),
    matches("_sitbi_"), - matches("sitbi_.*_TEXT"),
    matches("_ucla_")
    
  )


### Check that values are in expected range
items_to_check <- y24m_recoded %>%
  select(
    matches("_bads_"),
    matches("_bfamg_"),
    matches("_bhs_"),
    matches("_cdi_"),
    matches("_drs_"),
    matches("_idas_"),
    matches("_iptq_"),
    matches("_mpvs_"),
    matches("_pcsc_"),
    matches("_scared_"),
    matches("_scsc_"),
    matches("_shaps_"),
    matches("_shs_"),
    matches("_ucla_"),
    -ends_with("mean")
  ) %>%
  names()

walk(items_to_check, check_values, y24m_recoded) # check_values() helper function



####  Save Data  ####
# Save clean Qualtrics data
# - Note: LSMH IDs meeting exclusion criteria are dropped later (in "Merge Youth Qualtrics Data.R")
saveRDS(y24m_recoded, dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data - 24m.rds")

# Save log
saveRDS(log, dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data Log - 24m.rds")
