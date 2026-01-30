## Track-to-Treat Phase 2 Data Cleaning, Youth Qualtrics, 12-Month Follow-Up
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
y12m_path <- dirs$raw_data %+% "DP5+Phase+2+-+Youth+-+FU+3+-+12M_January+21,+2026_11.24_n.csv"
y12m_raw <- read_survey(y12m_path, time_zone = "America/Chicago")


## Load ID lookup and (using helper function) item-level codebook
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))
codebook <- load_p2_codebook(here("Phase 2", "2025.07.02 Track to Treat P2 Codebook.xlsx"))


## Load assessment windows
ax_windows <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Assessment Windows.rds")


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
check_raw_data_ver(raw_metadata, list(y12m_path), list(y12m_raw), "y12m_qualtrics")



####  Clean Data  ####
### Create log
# Create lists for logging (a) items used to compute item completion rate below via
# compute_item_completion_rate() and (b) items used to compute means via mean_across()
log <- list(
  item_completion_rate = list(),
  mean_items = list()
)


### Correct LSMH IDs (manually as necessary)
y12m_corrected_ids <- y12m_raw %>%
  rowwise() %>%
  mutate(
    lsmh_id = case_when(
      
      # TODO (any others?): Cases to be manually recoded
      lsmh_id == "LSM" & y12m_lsmh_id == "LSMH01370" ~ "LSMH01370",
      
      
    
      
      
      # All others (helper function for cases in which IDs are same or one/both IDs are missing)
      TRUE ~ resolve_id_pair(lsmh_id, y12m_lsmh_id)
      
    )
  ) %>%
  ungroup()

# Check LSMH ID format
warn_invalid_id_format(y12m_corrected_ids$lsmh_id)


### Remove invalid responses
# Filter to known valid LSMH IDs (marked "keep" in id_lookup) using helper function
y12m_valid_ids <- remove_invalid_p2_qualtrics_responses(y12m_corrected_ids, id_lookup)


### Identify duplicates and compute item completion rate for removing duplicates
# Identify duplicates using helper function
identify_duplicates(y12m_valid_ids, lsmh_id, phase = 2)

# Compute item completion rate using helper function (given that Qualtrics's "Progress"
# and "Finished" variables reflect only clicking through survey, not completing items)
y12m_valid_ids <- compute_item_completion_rate(y12m_valid_ids, "y12m", phase = 2)


### Remove any surveys (a) outside assessment window (or for youth who did not 
### complete intervention survey in window) or (b) duplicated in window
# Compute indicators of survey completion in window using helper function
y12m_valid_ids <- mark_fu_done_in_ax_window(y12m_valid_ids, "12m", ax_windows)

# Print (using helper function) and remove any surveys outside window          # TODO: Finalize windows
y12m_valid_ids_out_window <- get_surveys_outside_window_3m_onward(y12m_valid_ids, "12m") %>% print()

y12m_valid_ids <- y12m_valid_ids %>%
  filter(in_window_12m_ext)

# Remove duplicates using helper function
y12m_deduplicated <- remove_duplicates(y12m_valid_ids, lsmh_id)

# Double-check deduplication
identify_duplicates(y12m_deduplicated, lsmh_id, phase = 2)


### Clean columns
y12m_recoded <- y12m_deduplicated %>%
  
  # Remove click, page time variables with helper function
  rm_click_page_time_vars() %>%
  
  # Rename "mvps" to "mpvs" throughout with helper function
  rename_mvps_to_mpvs() %>%
  
  # Fix column name
  rename(y12m_pds_7 = y312_pds_7) %>%
  
  # Un-reverse code items with helper function
  unreverse_code_items(codebook) %>%
  
  # Clean remaining columns by row and create composites using helper functions
  rowwise() %>%
  mutate(
    
    ## Metadata
    # ID ("lsmh_id" cleaned above)
    
    # Survey completion
    y12m_complete = !is.na(EndDate),
    
    # Survey datetime and duration
    y12m_datetime = EndDate,
    y12m_date = date(y12m_datetime),
    y12m_duration = EndDate - StartDate,
    
    # Follow-up survey completion in original and extended assessment 
    # windows and days survey was completed before/after original window
    y12m_in_window_org = in_window_12m_org,
    y12m_in_window_ext = in_window_12m_ext,
    y12m_days_before_start_window_12m_org = days_before_start_window_12m_org,
    y12m_days_after_end_window_12m_org = days_after_end_window_12m_org,
    
    
    ## BADS (Behavioral Activation for Depression Scale) subscales
    !!!bads_means("y12m"),
    
    
    ## BFAMG (Brief Family Assessment Measure - General Scale) overall mean score
    y12m_bfamg_mean = mean_across("y12m", "bfamg", name = "y12m_bfamg_mean"),
    
    
    ## BHS-4 (Beck Hopelessness Scale - 4-item) overall mean score
    y12m_bhs_mean = mean_across("y12m", "bhs", name = "y12m_bhs_mean"),
    
    
    ## CDI-2-SR (Children's Depression Inventory - 2 - Self-Report) overall mean score and subscales
    !!!cdi_sr_means("y12m"),
    
    
    ## DRS (Dietary Restriction Screener)
    # Two items that do not need to be recoded or combined
    
    
    ## IDAS-II (Inventory of Depression and Anxiety Symptoms - II)
    # Given that scoring likely depends on intended use, we output items but do 
    # not score them (see Table 1 of https://doi.org/f4b85p for scale info)
    
    
    ## IPTQ (Implicit Personality Theory Questionnaire) overall mean score
    y12m_iptq_mean = mean_across("y12m", "iptq", name = "y12m_iptq_mean"),
    
    
    ## MPVS (Multidimensional Peer Victimization Scale) overall mean score and subscales
    !!!mpvs_means("y12m"),
    
    
    ## PCSC (Primary Control Scale for Children) overall mean score and subscales
    !!!pcsc_means("y12m"),
    
    
    ## SCARED-Child (Screen for Child Anxiety and Related Disorders - Child) overall mean score and subscales
    !!!scared_means("y12m"),
    
    
    ## SCSC (Secondary Control Scale for Children) overall mean score
    y12m_scsc_mean = mean_across("y12m", "scsc", name = "y12m_scsc_mean"),
    
    
    ## SHAPS (Snaith-Hamilton Pleasure Scale) overall mean score
    y12m_shaps_mean = mean_across("y12m", "shaps", name = "y12m_shaps_mean"),
    
    
    ## SHS (Self-Hate Scale) overall mean score
    y12m_self_hate_mean = mean_across("y12m", "self_hate_scale", name = "y12m_self_hate_mean"),
    
    
    ## SITBI-SF (Self-Injurious Thoughts and Behaviors Interview - Short Form)
    # Many items but no recoding or combining (but ranges need to be checked)
    
    
    ## UCLA (UCLA Loneliness Scale, aka ULS) overall mean score
    y12m_ucla_mean = mean_across("y12m", "ucla", name = "y12m_ucla_mean")
    
  ) %>%
  ungroup() %>%
  
  # Select variables
  select(
    
    # Metadata
    lsmh_id,
    y12m_complete,
    y12m_date,
    y12m_datetime,
    y12m_duration,
    ax_window_12m_start_org,
    ax_window_12m_end_org,
    ax_window_12m_start_ext,
    ax_window_12m_end_ext,
    y12m_in_window_org,
    y12m_in_window_ext,
    y12m_days_before_start_window_12m_org,
    y12m_days_after_end_window_12m_org,
    
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
items_to_check <- y12m_recoded %>%
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

walk(items_to_check, check_values, y12m_recoded) # check_values() helper function



####  Save Data  ####
# Save clean Qualtrics data
saveRDS(y12m_recoded, dirs$clean_data_staging %+% "Phase 2 Youth Qualtrics Clean Data - 12m.rds")

# Save log
saveRDS(log, dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data Log - 12m.rds")
