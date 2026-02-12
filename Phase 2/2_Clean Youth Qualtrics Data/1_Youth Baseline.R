## Track-to-Treat Phase 2 Data Cleaning, Youth Qualtrics, Baseline
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


## Load data
# Get directories using helper function
dirs <- get_p2_qualtrics_dirs(c("raw_data", "clean_data_staging", "clean_data_staging_intermediate"))

# Load raw Qualtrics datasets (storing paths) in this format: [respondent][wave]_[administration]_raw
# - Note: Use "timeZone" specified for date columns (e.g., "StartDate") in third row of raw CSV
yb_path <- dirs$raw_data %+% "DP5+Phase+2+-+Youth+-+Baseline_January+21,+2026_11.24_n.csv"
yb_raw <- read_survey(yb_path, time_zone = "America/Chicago")

# Load clean LifePak data without free-response items (until these are deidentified)
nis_clean_wout_free <- readRDS(dirs$clean_data_staging %+% "Phase 2 LifePak Clean Data Without Free Responses.rds")


## Load ID lookup and (using helper function) item-level codebook
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))
codebook <- load_p2_codebook(here("Phase 2", "2026.02.12 Track to Treat P2 Codebook.xlsx"))


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
check_raw_data_ver(raw_metadata, list(yb_path), list(yb_raw), "yb_qualtrics")



####  Clean Data  ####
### Create log
# Create lists for logging (a) items used to compute item completion rate below via
# compute_item_completion_rate() and (b) items used to compute means via mean_across()
log <- list(
  item_completion_rate = list(),
  mean_items = list()
)


### Fix item prefixes in data and codebook
# Add prefix to SRET items

yb_sret_items_raw <- c("SRET", "SRET.keys", "SRET.time", "SRET.words", "tlcond")

yb_renamed <- yb_raw %>%
  rename_with(
    .cols = all_of(yb_sret_items_raw),
    .fn   = ~ paste0("yb_", .x)
  )

codebook$item[codebook$item %in% yb_sret_items_raw] <-
  paste0("yb_", codebook$item[codebook$item %in% yb_sret_items_raw])


### Add codebook with clean youth baseline items to log
log$yb_codebook_clean <- codebook


### Correct LSMH IDs (manually as necessary)
yb_corrected_ids <- yb_renamed %>%
  rowwise() %>%
  mutate(
    lsmh_id = case_when(
      
      # Cases to be manually recoded
      lsmh_id == "LMSH00886" & yb_lsmh_id == "LSMH00886" ~ "LSMH00886",
      lsmh_id == "LMSH00886" & is.na(yb_lsmh_id) ~ "LSMH00886",
      lsmh_id == "LSMH02264?Redirect=0" & is.na(yb_lsmh_id) ~ "LSMH02264",
      lsmh_id == "Baseline" & yb_lsmh_id == "LSMH02533" ~ "LSMH02533",
      lsmh_id == "Baseline" & is.na(yb_lsmh_id) & StartDate == "2023-04-17 17:28:45" ~ "LSMH02533",
      is.na(lsmh_id) & is.na(yb_lsmh_id) & StartDate == "2022-03-01 12:33:20" ~ "LSMH01791",
      
      # All others (helper function for cases in which IDs are same or one/both IDs are missing)
      TRUE ~ resolve_id_pair(lsmh_id, yb_lsmh_id)
      
    )
  ) %>%
  ungroup()

# Check LSMH ID format
warn_invalid_id_format(yb_corrected_ids$lsmh_id)


### Remove invalid responses
# Filter to known valid LSMH IDs (marked "keep" in id_lookup) using helper function
yb_valid_ids <- remove_invalid_p2_qualtrics_responses(yb_corrected_ids, id_lookup)


### Identify duplicates and compute item completion rate for removing duplicates
# Identify duplicates using helper function
identify_duplicates(yb_valid_ids, lsmh_id, phase = 2)

# Compute item completion rate using helper function (given that Qualtrics's "Progress"
# and "Finished" variables reflect only clicking through survey, not completing items)
yb_valid_ids <- compute_item_completion_rate(yb_valid_ids, "yb", phase = 2)


### Remove any baseline surveys (a) outside assessment window (or for youth who 
### didn't start EMA) or (b) duplicated in window
# Obtain EMA notification dates from LifePak data and compute end of EMA period
ema_notif_dates <- nis_clean_wout_free %>%
  group_by(lsmh_id) %>%
  summarize(
    lsmh_id = unique(lsmh_id),
    first_ema_notif_date = min(notification_date),
    last_ema_notif_date = max(notification_date),
    end_ema_period = first_ema_notif_date + days(21),
    .groups = "drop"
  )

# Compute indicator of baseline survey completion in window using helper function
yb_valid_ids <- mark_b_done_in_ax_window(yb_valid_ids, "lsmh_id", ema_notif_dates)

# Print and remove any baseline surveys outside window
# - LSMH01677's "EndDate" is "2022-02-18 14:56:15" (in "America/Chicago"), after
# "first_ema_notif_date" of "2022-02-17" ("notification_datetime" is "2022-02-17 
# 08:52:31"; likely in "America/Los_Angeles" per parent-reported address in baseline
# Qualtrics survey, but exact time zone is unknown as participant's LifePak data lack
# GPS data). Still, youth likely completed most of survey before EMA, as "StartDate"
# is "2022-02-16 13:45:47" and "Progress" is 97; the RA likely confirmed near-100% 
# progress before administering EMA (and parent completed baseline on 2022-02-16).
#   - Thus, manually deem the survey to be within the window
yb_valid_ids$in_window_b[yb_valid_ids$lsmh_id == "LSMH01677" & yb_valid_ids$EndDate == "2022-02-18 14:56:15"] <- TRUE

yb_valid_ids %>%
  filter(!in_window_b | is.na(in_window_b)) %>%
  select(lsmh_id, "StartDate", "EndDate", "first_ema_notif_date", "in_window_b", "item_completion_rate") %>%
  arrange(lsmh_id, EndDate)

yb_valid_ids <- yb_valid_ids %>%
  filter(in_window_b)

# Remove duplicates using helper function
yb_deduplicated <- remove_duplicates(yb_valid_ids, lsmh_id)

# Double-check deduplication
identify_duplicates(yb_deduplicated, lsmh_id, phase = 2)

# Save dates for baseline survey and EMA for use in later scripts
yb_ema_dates <- yb_deduplicated %>%
  select(
    lsmh_id = lsmh_id,
    StartDate_yb = StartDate, 
    EndDate_yb = EndDate, 
    first_ema_notif_date, last_ema_notif_date, end_ema_period
  ) %>%
  ungroup()


### Clean columns
yb_recoded <- yb_deduplicated %>%

  # Remove click, page time variables with helper function
  rm_click_page_time_vars() %>%
  
  # Rename "mvps" to "mpvs" throughout with helper function
  rename_mvps_to_mpvs() %>%
  
  # Un-reverse code items with helper function
  unreverse_code_items(codebook) %>%
  
  # Clean remaining columns by row and create composites (using helper functions)
  rowwise() %>%
  mutate(
    
    ## Metadata
    # ID ("lsmh_id" cleaned above)

    # Survey completion
    yb_complete = !is.na(EndDate),
    
    # Survey datetime and duration
    yb_datetime = EndDate,
    yb_date = date(yb_datetime),
    yb_duration = EndDate - StartDate,
    
    
    ## BADS (Behavioral Activation for Depression Scale) subscales
    !!!bads_means("yb"),
    
    
    ## BFAMG (Brief Family Assessment Measure - General Scale) overall mean score
    yb_bfamg_mean = mean_across("yb", "bfamg", name = "yb_bfamg_mean"),
    
    
    ## BHS-4 (Beck Hopelessness Scale - 4-item) overall mean score
    yb_bhs_mean = mean_across("yb", "bhs", name = "yb_bhs_mean"),
    
    
    ## CDI-2-SR (Children's Depression Inventory - 2 - Self-Report) overall mean score and subscales
    !!!cdi_sr_means("yb"),

    
    ## DRS (Dietary Restriction Screener)
    # Two items that do not need to be recoded or combined
    
    
    ## IDAS-II (Inventory of Depression and Anxiety Symptoms - II)
    # Given that scoring likely depends on intended use, we output items but do 
    # not score them (see Table 1 of https://doi.org/f4b85p for scale info)
    
    
    ## IPTQ (Implicit Personality Theory Questionnaire) overall mean score
    yb_iptq_mean = mean_across("yb", "iptq", name = "yb_iptq_mean"),
    
    
    ## MPVS (Multidimensional Peer Victimization Scale) overall mean score and subscales
    !!!mpvs_means("yb"),
    
    
    ## PCSC (Primary Control Scale for Children) overall mean score and subscales
    !!!pcsc_means("yb"),
    
    
    ## SCARED-Child (Screen for Child Anxiety and Related Disorders - Child) overall mean score and subscales
    !!!scared_means("yb"),
    

    ## SCSC (Secondary Control Scale for Children) overall mean score
    yb_scsc_mean = mean_across("yb", "scsc", name = "yb_scsc_mean"),

    
    ## SHAPS (Snaith-Hamilton Pleasure Scale) overall mean score
    yb_shaps_mean = mean_across("yb", "shaps", name = "yb_shaps_mean"),
    

    ## SHS (Self-Hate Scale) overall mean score
    yb_self_hate_mean = mean_across("yb", "self_hate_scale", name = "yb_self_hate_mean"),

    
    ## SITBI-SF (Self-Injurious Thoughts and Behaviors Interview - Short Form)
    # Many items but no recoding or combining (but ranges need to be checked)
    
    
    ## SRET (Self-Referential Encoding Task)
    # (Not currently outputted and a low priority to code given how time-intensive 
    # this is; see Dainer-Best et al., 2018)
    # Items: "yb_SRET", "yb_SRET.keys", "yb_SRET.time", "yb_SRET.words", "yb_tlcond"
    
    
    ## UCLA (UCLA Loneliness Scale, aka ULS) overall mean score
    yb_ucla_mean = mean_across("yb", "ucla", name = "yb_ucla_mean")
    
  ) %>%
  ungroup() %>%
  
  # Select variables
  select(
    
    # Metadata
    lsmh_id,
    yb_complete,
    yb_date,
    yb_datetime,
    yb_duration,

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
items_to_check <- yb_recoded %>%
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

walk(items_to_check, check_values, yb_recoded) # check_values() helper function



####  Save Data  ####
# Save clean Qualtrics data
# - Note: LSMH IDs meeting exclusion criteria are dropped later (in "Merge Youth Qualtrics Data.R")
saveRDS(yb_recoded, dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data - Baseline.rds")

# Save log
saveRDS(log, dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data Log - Baseline.rds")

# Save dates for baseline survey and EMA for use in later scripts
saveRDS(yb_ema_dates, dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Baseline and EMA Dates.rds")
