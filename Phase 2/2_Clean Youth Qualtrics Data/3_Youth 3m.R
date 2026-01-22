## Track-to-Treat Phase 2 Data Cleaning, Youth Qualtrics, 3-Month Follow-Up
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
y3m_path <- raw_data_dir %+% "DP5+Phase+2+-+Youth+-+FU+1+-+3M_May+6,+2025_09.45_n.csv"
y3m_raw <- read_survey(y3m_path, time_zone = "America/Chicago")


## Load ID lookup and (using helper function) item-level codebook
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))
codebook <- load_p2_codebook(here("Phase 2", "2025.07.02 Track to Treat P2 Codebook.xlsx"))


## Load assessment windows computed in "Youth Intervention.R"
ax_windows <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Assessment Windows.rds")


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
check_raw_data_ver(raw_metadata, list(y3m_path), list(y3m_raw), "y3m_qualtrics")



####  Clean Data  ####
### Create log
# Create lists for logging (a) items used to compute item completion rate below via
# compute_item_completion_rate() and (b) items used to compute means via mean_across()
log <- list(
  item_completion_rate = list(),
  mean_items = list()
)


### Correct LSMH IDs (manually as necessary)
y3m_corrected_ids <- y3m_raw %>%
  rowwise() %>%
  mutate(
    lsmh_id = case_when(
      
      # Cases to be manually recoded
      lsmh_id == "LMSH00886" & is.na(y3m_lsmh_id) ~ "LSMH00886",
      lsmh_id == "LMSH00886" & y3m_lsmh_id == "LSMH00886" ~ "LSMH00886",
      lsmh_id == "LSMH" & y3m_lsmh_id == "LSMH02265" ~ "LSMH02265",
      lsmh_id == "LSMH01829" & y3m_lsmh_id == "LSMH08129" ~ "LSMH01829",
      lsmh_id == "LSMH02416" & y3m_lsmh_id == "LSMH ID, LSMH02416" ~ "LSMH02416",
      lsmh_id == "LSMH02471" & y3m_lsmh_id == "LSMH02471" & y3m_lsmh_id_check == "LSMH02571" ~ "LSMH02471",
      
      # Cases where both match
      lsmh_id == y3m_lsmh_id ~ lsmh_id,
      
      # Cases where one is missing (keep the non-missing value)
      is.na(lsmh_id) & !is.na(y3m_lsmh_id) ~ y3m_lsmh_id,
      is.na(y3m_lsmh_id) & !is.na(lsmh_id) ~ lsmh_id,
      
      # Cases where both are missing
      is.na(lsmh_id) & is.na(y3m_lsmh_id) ~ NA_character_,
      
      # Additional cases are flagged for cleaning
      T ~ "ID Combination Unaccounted For (lsmh_id '" %+% lsmh_id %+% "', y3m_lsmh_id '" %+% y3m_lsmh_id %+% "')"
      
    )
  ) %>%
  ungroup()


### Remove invalid responses
# Known valid LSMH IDs
valid_ids <- id_lookup %>%
  filter(action == "keep") %>%
  distinct(lsmh_id)

invalid_ids <- setdiff(y3m_corrected_ids$lsmh_id, valid_ids$lsmh_id)

y3m_valid_ids <- y3m_corrected_ids %>%
  inner_join(
    valid_ids,
    by = "lsmh_id",
    relationship = "many-to-one"
  )

# Just FYI: This is how many IDs/rows included known LSMH IDs matched for removal
y3m_corrected_ids %>%
  filter(lsmh_id %in% id_lookup$lsmh_id[id_lookup$action == "drop"]) %>%
  count(lsmh_id)

# Just FYI: No rows contained unknown LSMH IDs (good!)
# If these rows indicate typos or other errors in the IDs, fix them in the mutate() above
y3m_corrected_ids %>%
  filter(!lsmh_id %in% id_lookup$lsmh_id) %>%
  count(lsmh_id)


### Identify duplicates and compute item completion rate for removing duplicates
# Identify duplicates using helper function
identify_duplicates(y3m_valid_ids, lsmh_id)

# Compute item completion rate using helper function (given that Qualtrics's "Progress"
# and "Finished" variables reflect only clicking through survey, not completing items)
y3m_valid_ids <- compute_item_completion_rate(y3m_valid_ids, "y3m", phase = 2)


### Remove any surveys (a) outside assessment window (or for youth who did not 
### complete intervention survey in window) or (b) duplicated in window
# Compute indicators of survey completion in window using helper function
y3m_valid_ids <- mark_fu_done_in_ax_window(y3m_valid_ids, "3m", ax_windows)

# Print and remove any surveys outside window          # TODO: Finalize windows
y3m_valid_ids %>%
  filter(!in_window_3m_ext | is.na(in_window_3m_ext)) %>%
  select(lsmh_id, StartDate, EndDate, ax_window_3m_start_org, ax_window_3m_end_org, 
         in_window_3m_org, days_before_start_window_3m_org, days_after_end_window_3m_org, 
         ax_window_3m_start_ext, ax_window_3m_end_ext, in_window_3m_ext, item_completion_rate) %>%
  arrange(lsmh_id, EndDate)

y3m_valid_ids <- y3m_valid_ids %>%
  filter(in_window_3m_ext)

# Remove duplicates using helper function
y3m_deduplicated <- remove_duplicates(y3m_valid_ids, lsmh_id)

# Double-check deduplication
identify_duplicates(y3m_deduplicated, lsmh_id)


### Clean columns
y3m_recoded <- y3m_deduplicated %>%
  
  # Remove click, page time variables with helper function
  rm_click_page_time_vars() %>%
  
  # Rename "mvps" to "mpvs" throughout with helper function
  rename_mvps_to_mpvs() %>%
  
  # Un-reverse code items with helper function
  unreverse_code_items(codebook) %>%
  
  # Clean remaining columns by row and create composites using helper functions
  rowwise() %>%
  mutate(
    
    ## Metadata
    # ID ("lsmh_id" cleaned above)
    
    # Survey completion
    y3m_complete = !is.na(EndDate),
    
    # Survey datetime and duration
    y3m_datetime = EndDate,
    y3m_date = date(y3m_datetime),
    y3m_duration = EndDate - StartDate,
    
    # Follow-up survey completion in original and extended assessment 
    # windows and days survey was completed before/after original window
    y3m_in_window_org = in_window_3m_org,
    y3m_in_window_ext = in_window_3m_ext,
    y3m_days_before_start_window_3m_org = days_before_start_window_3m_org,
    y3m_days_after_end_window_3m_org = days_after_end_window_3m_org,
    
    
    ## CDI-2-SR (Children's Depression Inventory - 2 - Self-Report) overall mean score and subscales
    !!!cdi_sr_means("y3m"),
    
    
    ## BHS-4 (Beck Hopelessness Scale - 4-item) overall mean score
    y3m_bhs_mean = mean_across("y3m", "bhs", name = "y3m_bhs_mean"),
    
    
    ## PCSC (Primary Control Scale for Children) overall mean score and subscales
    !!!pcsc_means("y3m"),
    
    
    ## SCSC (Secondary Control Scale for Children) overall mean score
    y3m_scsc_mean = mean_across("y3m", "scsc", name = "y3m_scsc_mean"),
    
    
    ## BADS (Behavioral Activation for Depression Scale) subscales
    !!!bads_means("y3m"),

    
    ## SHS (Self-Hate Scale) overall mean score
    y3m_self_hate_mean = mean_across("y3m", "self_hate_scale", name = "y3m_self_hate_mean"),
    
    
    ## IDAS-II (Inventory of Depression and Anxiety Symptoms - II)
    # Given that scoring likely depends on intended use, we output items but do 
    # not score them (see Table 1 of https://doi.org/f4b85p for scale info)
    
    
    ## SCARED-Child (Screen for Child Anxiety and Related Disorders - Child) overall mean score and subscales
    !!!scared_means("y3m"),
    
    
    ## SHAPS (Snaith-Hamilton Pleasure Scale) overall mean score
    y3m_shaps_mean = mean_across("y3m", "shaps", name = "y3m_shaps_mean"),
    
    
    ## DRS (Dietary Restriction Screener)
    # Two items that do not need to be recoded or combined
    
    
    ## SITBI-SF (Self-Injurious Thoughts and Behaviors Interview - Short Form)
    # Many items but no recoding or combining (but ranges need to be checked)
    
    
    ## IPTQ (Implicit Personality Theory Questionnaire) overall mean score
    y3m_iptq_mean = mean_across("y3m", "iptq", name = "y3m_iptq_mean"),
    
    
    ## BFAMG (Brief Family Assessment Measure - General Scale) overall mean score
    y3m_bfamg_mean = mean_across("y3m", "bfamg", name = "y3m_bfamg_mean"),
    
    
    ## MPVS (Multidimensional Peer Victimization Scale) overall mean score and subscales
    !!!mpvs_means("y3m"),
    
    
    ## UCLA (UCLA Loneliness Scale, aka ULS) overall mean score
    y3m_ucla_mean = mean_across("y3m", "ucla", name = "y3m_ucla_mean")
    
  ) %>%
  ungroup() %>%
  
  # Select variables
  select(
    
    # Metadata
    lsmh_id,
    y3m_complete,
    y3m_date,
    y3m_datetime,
    y3m_duration,
    ax_window_3m_start_org,
    ax_window_3m_end_org,
    ax_window_3m_start_ext,
    ax_window_3m_end_ext,
    y3m_in_window_org,
    y3m_in_window_ext,
    y3m_days_before_start_window_3m_org,
    y3m_days_after_end_window_3m_org,
    
    # Measures
    matches("_cdi_"),
    matches("_bhs_"),
    matches("_pcsc_"),
    matches("_scsc_"),
    matches("_bads_"),
    matches("_shs_"),
    matches("_self_hate_"),
    matches("_idas_"),
    matches("_scared_"),
    matches("_shaps_"),
    matches("_drs_"),
    matches("_sitbi_"), - matches("sitbi_.*_TEXT"),
    matches("_iptq_"),
    matches("_bfamg_"),
    matches("_mpvs_"),
    matches("_ucla_")
    
  )


### Check that values are in expected range
items_to_check <- y3m_recoded %>%
  select(
    matches("_cdi_"),
    matches("_bhs_"),
    matches("_pcsc_"),
    matches("_scsc_"),
    matches("_bads_"),
    matches("_shs_"),
    matches("_idas_"),
    matches("_scared_"),
    matches("_shaps_"),
    matches("_drs_"),
    matches("_iptq_"),
    matches("_bfamg_"),
    matches("_mpvs_"),
    matches("_ucla_"),
    -ends_with("mean")
  ) %>%
  names()

walk(items_to_check, check_values, y3m_recoded) # check_values() helper function



####  Save Data  ####
# Save clean Qualtrics data
saveRDS(y3m_recoded, dirs$clean_data_staging %+% "Phase 2 Youth Qualtrics Clean Data - 3m.rds")

# Save log
saveRDS(log, dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data Log - 3m.rds")
