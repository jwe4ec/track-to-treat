## Track-to-Treat Phase 2 Data Cleaning, Youth Qualtrics, Intervention
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
# Save directories
raw_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT P2\\Data\\Qualtrics\\Raw\\2025.05.22_interim\\"
clean_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT P2\\Data\\Clean Data (Isaac)\\"
clean_data_staging_dir <- clean_data_dir %+% "staging\\"
clean_data_staging_intermediate_dir <- clean_data_staging_dir %+% "intermediate\\"

# Load raw Qualtrics datasets (storing paths) in this format: [respondent][wave]_[administration]_raw
# - Note: Use "timeZone" specified for date columns (e.g., "StartDate") in third row of raw CSV
yi_path <- raw_data_dir %+% "DP5 Phase 2 - Youth - Interventions_May 22, 2025_12.17_n.csv"
yi_raw <- read_survey(yi_path, time_zone = "America/Denver")

# Load dates for baseline Qualtrics survey and EMA computed when cleaning baseline survey
yb_ema_dates <- readRDS(clean_data_staging_intermediate_dir %+% "Phase 2 Youth Qualtrics Baseline and EMA Dates.rds")


## Load ID lookup
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))


## Load item-level codebook file using helper function
codebook <- load_p2_codebook(here("Phase 2", "2025.07.02 Track to Treat P2 Codebook.xlsx"))


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
check_raw_data_ver(raw_metadata, list(yi_path), list(yi_raw), "yi_qualtrics")



####  Clean Data  ####
### Create log
# Create lists for logging (a) items used to compute item completion rate below via
# compute_item_completion_rate(), (b) items used to compute means via mean_across(),
# and (c) clean codebook (edited and added to log below)
log <- list(
  item_completion_rate = list(),
  mean_items = list()
)


### Use unique "ImportId" to rename columns both named "lsmh_id" in Qualtrics
# - "read_survey()" contingently named these by their column indices upon import to R
col_map <- attr(yi_raw, "column_map")

lsmh_id_col1_qname <- col_map$qname[col_map$ImportId == "QID186_TEXT"]
lsmh_id_col2_qname <- col_map$qname[col_map$ImportId == "lsmh_id"]

yi_renamed <- yi_raw

names(yi_renamed)[names(yi_renamed) == lsmh_id_col1_qname] <- "lsmh_id_col1"
names(yi_renamed)[names(yi_renamed) == lsmh_id_col2_qname] <- "lsmh_id_col2"


### Correct item prefixes in data and codebook
# For BADS-SF items with prefixes "b_" instead of "yi_"

yi_bads_items_raw <- paste0("b_bads_", 1:9)

names(yi_renamed)[names(yi_renamed) %in% yi_bads_items_raw] <-
  sub("b_", "yi_", names(yi_renamed)[names(yi_renamed) %in% yi_bads_items_raw])

codebook$item[codebook$item %in% yi_bads_items_raw] <-
  sub("b_", "yi_", codebook$item[codebook$item %in% yi_bads_items_raw])


### Change "minimum"-"maximum" values in codebook for BHS-4 from 1-4 to 0-3 for consistency 
### with other time points (despite different anchors; data values are recoded below)

yi_bhs_items <- c("yi_pre_bhs_1", "yi_pre_bhs_2", "yi_pre_bhs_3", "yi_pre_bhs_4", 
                  "yi_post_bhs_1", "yi_post_bhs_2", "yi_post_bhs_3", "yi_post_bhs_4")

codebook$minimum[codebook$item %in% yi_bhs_items] <- 0
codebook$maximum[codebook$item %in% yi_bhs_items] <- 3


### Add codebook with clean youth intervention items to log
log$yi_codebook_clean <- codebook


### Correct LSMH IDs (manually as necessary)
yi_corrected_ids <- yi_renamed %>%
  rowwise() %>%
  mutate(
    lsmh_id = case_when(
      
      # Cases to be manually recoded
      lsmh_id_col1 == "LSMH01297" & lsmh_id_col2 == "LSMH0129" ~ "LSMH01297",
      lsmh_id_col1 == "LsmH00886" & lsmh_id_col2 == "LMSH00886" ~ "LSMH00886",
      lsmh_id_col1 == "lsmh01826" & lsmh_id_col2 == "LSMH01826" ~ "LSMH01826",
      
      # Cases where both match
      lsmh_id_col1 == lsmh_id_col2 ~ lsmh_id_col1,
      
      # Cases where one is missing (keep the non-missing value)
      is.na(lsmh_id_col1) & !is.na(lsmh_id_col2) ~ lsmh_id_col2,
      is.na(lsmh_id_col2) & !is.na(lsmh_id_col1) ~ lsmh_id_col1,
      
      # Cases where both are missing
      is.na(lsmh_id_col1) & is.na(lsmh_id_col2) ~ NA_character_,
      
      # Additional cases are flagged for cleaning
      T ~ "ID Combination Unaccounted For (" %+% lsmh_id_col1 %+% ", " %+% lsmh_id_col2 %+% ")"
      
    )
  ) %>%
  ungroup()


### Remove invalid responses
# Known valid LSMH IDs
valid_ids <- id_lookup %>%
  filter(action == "keep") %>%
  distinct(lsmh_id)

invalid_ids <- setdiff(yi_corrected_ids$lsmh_id, valid_ids$lsmh_id)

yi_valid_ids <- yi_corrected_ids %>%
  inner_join(
    valid_ids,
    by = "lsmh_id",
    relationship = "many-to-one"
  )

# Just FYI: This is how many IDs/rows included known LSMH IDs matched for removal
yi_corrected_ids %>%
  filter(lsmh_id %in% id_lookup$lsmh_id[id_lookup$action == "drop"]) %>%
  count(lsmh_id)

# Just FYI: No rows contained unknown LSMH IDs (good!)
# If these rows indicate typos or other errors in the IDs, fix them in the mutate() above
yi_corrected_ids %>%
  filter(!lsmh_id %in% id_lookup$lsmh_id) %>%
  count(lsmh_id)


### Manually check selected free-text columns for the following exclusion criteria
# - Check all columns below for (a) lack of English fluency and (b) random text responses
# - Check all columns below except "abc_q_6" and "abc_q_20" for (c) responses < 3 words
# Export selected columns to check
cols_to_check <- c(paste0("shar_feel_q_", 1:3), paste0("proj_pers_q_", 1:3), "abc_q_6", "abc_q_7_b", "abc_q_20")
filename_to_check <- "2025.08.02 Phase 2 Youth Qualtrics Valid Data - Intervention Free-Responses to Check.csv"

yi_valid_ids %>%
  select(lsmh_id, condition, EndDate, all_of(cols_to_check)) %>%
  arrange(condition, lsmh_id, EndDate) %>%
  mutate(exclude = NA, # Mark as 0 or 1
         exclude_not_fluent = NA, # If "exclude" is 1, mark reason(s) as 1 (otherwise leave as NA)
         exclude_random_text = NA, 
         exclude_too_short = NA,
         note = NA) %>% # Make note if needed
  write.csv(clean_data_staging_intermediate_dir %+% filename_to_check, row.names = FALSE)

# Manually copy exported file and rename as follows for Alyssa Gorkin to complete "exclude" columns
filename_checked <- "2025.08.02 Phase 2 Youth Qualtrics Valid Data - Intervention Free-Responses Checked.csv"

# Load checked responses and exclude surveys that meet exclusion criteria
yi_valid_ids_free_text_checked <- read_csv(clean_data_staging_intermediate_dir %+% filename_checked) %>%
  mutate(EndDate = force_tz(EndDate, tzone = "America/Denver"))

yi_valid_ids <- yi_valid_ids %>%
  left_join(
    yi_valid_ids_free_text_checked[c("lsmh_id", "EndDate", "exclude")],
    by = c("lsmh_id", "EndDate"),
    relationship = "one-to-one"
  ) %>%
  filter(exclude != 1) %>%
  select(-exclude)


### Identify duplicates and compute item completion rate for removing duplicates
# Identify duplicates using helper function
identify_duplicates(yi_valid_ids, lsmh_id)

# Compute item completion rate using helper function (given that Qualtrics's "Progress"
# and "Finished" variables reflect only clicking through survey, not completing items)
yi_valid_ids <- compute_item_completion_rate(yi_valid_ids, "yi", phase = 2)


### Remove any surveys (a) outside assessment window or (b) duplicated in window
# Obtain EMA dates and compute intervention survey window
# - Intervention surveys were manually scheduled (not using Qualtrics workflows) and 
# completed with an RA on Zoom, in general starting 4 weeks after baseline completion 
# and ending 3 weeks later (per Arielle Smith on 7/29/25-7/31/25). (Note: Even though
# an Excel formula [see follow-up assessment windows below] was used to compute the 
# intervention window start dates as 1 month after baseline completion, most RAs seem
# to have computed the start dates as 4 weeks from the baseline completion date.)
# - However, some may have been scheduled early (e.g., due to RA error) or late (e.g., 
# due to youth availability). Thus, also compute an extended window whose start date is 
# the day after the end of the EMA period and whose end date is 6 weeks later (i.e., ~7 
# weeks after baseline, if EMA started right after baseline, plus a 14-day grace period).
ax_windows_yi <- yb_ema_dates %>%
  rowwise() %>%
  mutate(
    ax_window_yi_start_org = as_date(EndDate_yb) + weeks(4),
    ax_window_yi_end_org = ax_window_yi_start_org + weeks(3),
    
    ax_window_yi_start_ext = end_ema_period + days(1),
    ax_window_yi_end_ext = ax_window_yi_start_ext + weeks(6),
  ) %>%
  ungroup()

# Compute indicator of intervention completion in window using helper function
yi_valid_ids <- mark_fu_done_in_ax_window(yi_valid_ids, "yi", ax_windows_yi)

# Print and remove any intervention surveys outside window
yi_valid_ids %>%
  filter(!in_window_yi_ext) %>%
  select(lsmh_id, StartDate, EndDate, ax_window_yi_start_org, ax_window_yi_end_org, 
         in_window_yi_org, days_before_start_window_yi_org, days_after_end_window_yi_org, 
         ax_window_yi_start_ext, ax_window_yi_end_ext, in_window_yi_ext, item_completion_rate) %>%
  arrange(lsmh_id, EndDate)

yi_valid_ids <- yi_valid_ids %>%
  filter(in_window_yi_ext)

# Remove duplicates using helper function
yi_deduplicated <- remove_duplicates(yi_valid_ids, lsmh_id)

# Double-check deduplication
identify_duplicates(yi_deduplicated, lsmh_id)


### Use deduplicated data to define assessment windows for follow-up surveys in later scripts
# Compute potential assessment windows based on intervention completion date
# - In Phase I, 3-month assessment window start dates were computed manually by adding 3
# to the month number and then rolling to the last real date of the prior month when this
# yields a date that does not exist. In R, this is "as_date(EndDate) %m+% months(3)".
# - In Phase II, follow-up window start dates were computed using Qualtrics Workflows such that 
# once the intervention survey was completed, Qualtrics sent the 3-, 6-, 12-, 18-, and 24-month 
# surveys to youth and parents after those number of months had passed since the intervention 
# survey completion date stored in Qualtrics (per Arielle Smith on 7/31/25). Per Qualtrics Support, 
# each month is defined as 30 days. Follow-up end dates were determined via two steps:
#   - First, an Excel formula was used to approximate the start date (e.g., 3-month start date 
#   based on intervention completion date in Cell A1: "=DATE(YEAR(A1), MONTH(A1) + 3, DAY(A1))"),
#   which rolls forward to the closest real date (not necessarily the first date of the next month).
#   In R, this is "seq(as_date(EndDate), by = "3 months", length.out = 2)[2]".
#   - Second, 1 month from the approximated start date, the RA sent a final email and changed the
#   participant's status to the next phase (e.g., from "3M" to "6M"). Given that window end dates
#   were not recorded, have leeway and use the same Excel formula to approximate the end date that
#   the RA might have had in mind based on the approximated start date.
# - Because some surveys were completed late, also compute an extended window that
# extends the original window's end date by a reasonable 14 days.
ax_windows <- yi_deduplicated %>%
  # Add intervention dates (when applicable) for participants with baseline surveys
  select(
    lsmh_id,
    StartDate_yi = StartDate,
    EndDate_yi = EndDate
  ) %>%
  right_join(ax_windows_yi, by = "lsmh_id", relationship = "one-to-one") %>%
  relocate(all_of(names(ax_windows_yi))) %>%
  arrange(lsmh_id) %>%
  
  # Compute follow-up windows (using helper function for seq() method)
  rowwise() %>%
  mutate(
    
    # 3-month follow-up
    ax_window_3m_start_org = as_date(EndDate_yi) + days(3*30),
    ax_window_3m_end_org = compute_date_w_seq(as_date(EndDate_yi), "3 months"),
    ax_window_3m_end_org = compute_date_w_seq(ax_window_3m_end_org, "1 month"),
    
    ax_window_3m_start_ext = ax_window_3m_start_org,        # TODO: Consider extending earlier by 1-7 days
    ax_window_3m_end_ext = ax_window_3m_end_org + days(14),
    
    # 6-month follow-up
    ax_window_6m_start_org = as_date(EndDate_yi) + days(6*30),
    ax_window_6m_end_org = compute_date_w_seq(as_date(EndDate_yi), "6 months"),
    ax_window_6m_end_org = compute_date_w_seq(ax_window_6m_end_org, "1 month"),
    
    ax_window_6m_start_ext = ax_window_6m_start_org,
    ax_window_6m_end_ext = ax_window_6m_end_org + days(14),
    
    # 12-month follow-up
    ax_window_12m_start_org = as_date(EndDate_yi) + days(12*30),
    ax_window_12m_end_org = compute_date_w_seq(as_date(EndDate_yi), "12 months"),
    ax_window_12m_end_org = compute_date_w_seq(ax_window_12m_end_org, "1 month"),
    
    ax_window_12m_start_ext = ax_window_12m_start_org,
    ax_window_12m_end_ext = ax_window_12m_end_org + days(14),
    
    # 18-month follow-up
    ax_window_18m_start_org = as_date(EndDate_yi) + days(18*30),
    ax_window_18m_end_org = compute_date_w_seq(as_date(EndDate_yi), "18 months"),
    ax_window_18m_end_org = compute_date_w_seq(ax_window_18m_end_org, "1 month"),
    
    ax_window_18m_start_ext = ax_window_18m_start_org,
    ax_window_18m_end_ext = ax_window_18m_end_org + days(14),
    
    # 24-month follow-up
    ax_window_24m_start_org = as_date(EndDate_yi) + days(24*30),
    ax_window_24m_end_org = compute_date_w_seq(as_date(EndDate_yi), "24 months"),
    ax_window_24m_end_org = compute_date_w_seq(ax_window_24m_end_org, "1 month"),
    
    ax_window_24m_start_ext = ax_window_24m_start_org,
    ax_window_24m_end_ext = ax_window_24m_end_org + days(14),
    
  ) %>%
  ungroup()


### Clean columns
yi_recoded <- yi_deduplicated %>%
  
  # Remove click, page time variables
  select(
    
    -matches("Click Count"),
    -matches("First Click"),
    -matches("Last Click"),
    -matches("Page Submit")
    
  ) %>%
  
  # Un-reverse code items
  mutate(
    across(
      .cols = any_of(codebook$item[codebook$reversed %in% 1]),
      .fns = ~ codebook$reverse_base[codebook$item == cur_column()] - .x
    )
  ) %>%
  
  # Clean remaining columns by row and create composites
  rowwise() %>%
  mutate(
    
    ## Metadata
    # ID ("lsmh_id" cleaned above)
    
    # Survey completion
    yi_complete = !is.na(EndDate),
    
    # Survey datetime and duration
    yi_datetime = EndDate,
    yi_date = date(yi_datetime),
    yi_duration = EndDate - StartDate,
    
    # Follow-up survey completion in original and extended assessment 
    # windows and days survey was completed before/after original window
    yi_in_window_org = in_window_yi_org,
    yi_in_window_ext = in_window_yi_ext,
    yi_days_before_start_window_yi_org = days_before_start_window_yi_org,
    yi_days_after_end_window_yi_org = days_after_end_window_yi_org,
    
    
    ## BADS-SF (Behavioral Activation for Depression Scale - Short Form)
    # Activation subscale
    yi_pre_bads_sf_ac_mean = mean_across("yi", "bads-sf", "activation", name = "yi_pre_bads_sf_ac_mean"),
    
    # Avoidance subscale
    yi_pre_bads_sf_av_mean = mean_across("yi", "bads-sf", "avoidance", name = "yi_pre_bads_sf_av_mean"),
    
    # Overall score can also be computed (for instructions, see Note column of raw codebook)
    # - Subscales are not recommended (per 5/21/25 email from Jonathan Kanter to Alyssa/Jeremy)
    
    
    ## BHS-4 (Beck Hopelessness Scale - 4-item)
    # Recode items from 1-4 scale to 0-3 scale used at other time points (despite different anchors)
    across(
      .cols = all_of(yi_bhs_items),
      .fns = ~ .x - 1
    ),
    
    # Overall mean score
    yi_pre_bhs_mean = mean_across("yi_pre", "bhs", name = "yi_pre_bhs_mean"),
    yi_post_bhs_mean = mean_across("yi_post", "bhs", name = "yi_post_bhs_mean"),
    
    
    ## SHS (*State Hope* Scale)
    # Pathways subscale
    yi_pre_pathways_mean = mean_across("yi_pre", "state_hope_scale", "pathways", name = "yi_pre_pathways_mean"),
    yi_post_pathways_mean = mean_across("yi_post", "state_hope_scale", "pathways", name = "yi_post_pathways_mean"),
    
    # Agency subscale
    yi_pre_agency_mean = mean_across("yi_pre", "state_hope_scale", "agency", name = "yi_pre_agency_mean"),
    yi_post_agency_mean = mean_across("yi_post", "state_hope_scale", "agency", name = "yi_post_agency_mean"),
    
    # Overall score can also be computed (see https://doi.org/fwc2xc; p. 334)
    
    
    ## IPTQ (Implicit Personality Theory Questionnaire)
    yi_pre_iptq_mean = mean_across("yi_pre", "iptq", name = "yi_pre_iptq_mean"),
    yi_post_iptq_mean = mean_across("yi_post", "iptq", name = "yi_post_iptq_mean"),
    
    
    ## PFS (Program Feedback Scale; developed by LSMH)
    # - "subscale" for ordinal PFS items is "pfs" in codebook (vs. NA for free-text PFS items)
    yi_post_pfs_mean = mean_across("yi", "pfs", "pfs", name = "yi_post_pfs_mean")
    
    
    ## Perceived Change
    # Two post-intervention items that do not need to be recoded or combined
    
  ) %>%
  ungroup() %>%
  
  # Select variables
  select(
    
    # Metadata
    lsmh_id,
    yi_complete,
    yi_date,
    yi_datetime,
    yi_duration,
    condition,
    ax_window_yi_start_org,
    ax_window_yi_end_org,
    ax_window_yi_start_ext,
    ax_window_yi_end_ext,
    yi_in_window_org,
    yi_in_window_ext,
    yi_days_before_start_window_yi_org,
    yi_days_after_end_window_yi_org,
    
    # Measures
    matches("_bads_"),
    matches("_bhs_"),
    matches("_shs_"),
    matches("_pathways_"),
    matches("_agency_"),
    matches("_iptq_"),
    matches("_pfs_"),
    yi_perc_change_hope,
    yi_perc_change_prob
    
  )


### Check that values are in expected range
items_to_check <- yi_recoded %>%
  select(
    matches("_bads_"),
    matches("_bhs_"),
    matches("_shs_"),
    matches("_iptq_"),
    matches("_pfs_[1-7]"),
    yi_perc_change_hope,
    yi_perc_change_prob,
    -ends_with("mean")
  ) %>%
  names()

walk(
  items_to_check,
  ~ check_values( # Helper function
    .data = yi_recoded,
    .item = .x
  )
)



####  Save Data  ####
# Save clean Qualtrics data
saveRDS(yi_recoded, clean_data_staging_dir %+% "Phase 2 Youth Qualtrics Clean Data - Intervention.rds")

# Save assessment windows
saveRDS(ax_windows, clean_data_staging_intermediate_dir %+% "Phase 2 Assessment Windows.rds")

# Save log
saveRDS(log, clean_data_staging_intermediate_dir %+% "Phase 2 Youth Qualtrics Clean Data Log - Intervention.rds")
