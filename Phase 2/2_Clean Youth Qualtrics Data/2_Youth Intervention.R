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
`%+%` <- paste0


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


## Load ID lookup
id_lookup <- read_csv(here("Phase 2", "2025.05.26 Track to Treat P2 ID Lookup.csv"))


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


### Identify duplicates and compute item completion rate for removing duplicates
# Identify duplicates using helper function
identify_duplicates(yi_valid_ids, lsmh_id)

# Compute item completion rate using helper function (given that Qualtrics's "Progress"
# and "Finished" variables reflect only clicking through survey, not completing items)
yi_valid_ids <- compute_item_completion_rate(yi_valid_ids, "yi", phase = 2)


### TODO: Remove any surveys (a) outside assessment window or (b) duplicated in window
# TODO: Obtain baseline survey dates and compute intervention survey window

# TODO: Compute indicator of intervention completion in window using helper function

# TODO: Print and remove any intervention surveys outside window

# Remove duplicates using helper function
yi_deduplicated <- remove_duplicates(yi_valid_ids, lsmh_id)

# Double-check deduplication
identify_duplicates(yi_deduplicated, lsmh_id)


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


### TODO (check and move up?): Use deduplicated data to establish assessment windows for follow-up surveys
# Compute potential assessment windows based on intervention completion date
# - In Phase I, 3-month assessment window start dates were computed manually by adding 3
# to the month number and then rolling to the last real date of the prior month when this
# yields a date that does not exist. In R, this is "as_date(EndDate) %m+% months(3)".
# - In Phase II, follow-up start dates were computed using an Excel formula (e.g.,
# 3-month start date based on Cell A1: "=DATE(YEAR(A1), MONTH(A1) + 3, DAY(A1))"),
# which rolls forward to the closest real date (not necessarily the first date of
# the next month). In R: "seq(as_date(EndDate), by = "3 months", length.out = 2)[2]".
# - End dates for windows were not recorded. Thus, have leeway and use Phase II formula
# above (more forgiving) to compute end dates from start dates for the original window.
# - Because some surveys were completed late, also compute an extended window that
# extends the original window's end date by a reasonable 14 days.
ax_windows <- yi_recoded %>%
  select(
    lsmh_id,
    yi_date
  ) %>%
  rowwise() %>%        # TODO: Investigate ax_windows and extend
  mutate(
    
    # 3-month follow-up
    ax_window_3m_start = seq(yi_date, by = "3 months", length.out = 2)[2],
    ax_window_3m_end = seq(ax_window_3m_start, by = "1 month", length.out = 2)[2],

    # 6-month follow-up
    ax_window_6m_start = seq(yi_date, by = "6 months", length.out = 2)[2],
    ax_window_6m_end = seq(ax_window_6m_start, by = "1 month", length.out = 2)[2],

    # 12-month follow-up
    ax_window_12m_start = seq(yi_date, by = "12 months", length.out = 2)[2],
    ax_window_12m_end = seq(ax_window_12m_start, by = "1 month", length.out = 2)[2],

    # 18-month follow-up
    ax_window_18m_start = seq(yi_date, by = "18 months", length.out = 2)[2],
    ax_window_18m_end = seq(ax_window_18m_start, by = "1 month", length.out = 2)[2],

    # 24-month follow-up
    ax_window_24m_start = seq(yi_date, by = "24 months", length.out = 2)[2],
    ax_window_24m_end = seq(ax_window_24m_start, by = "1 month", length.out = 2)[2]
    
  ) %>%
  ungroup()


### TODO: Check for exclusion criteria in free-response items



####  Save Data  ####
# Save clean Qualtrics data
saveRDS(yi_deduplicated, clean_data_staging_dir %+% "Phase 2 Youth Qualtrics Clean Data - Intervention.rds")

# Save assessment windows
saveRDS(ax_windows, clean_data_staging_intermediate_dir %+% "Phase 2 Assessment Windows.rds")

# Save log
saveRDS(log, clean_data_staging_intermediate_dir %+% "Phase 2 Youth Qualtrics Clean Data Log - Intervention.rds")
