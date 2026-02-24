## Track-to-Treat Phase 2 Data Cleaning, Parent Qualtrics, Baseline
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
pb_corrected <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Parent Qualtrics Corrected Data - List by Wave.rds") %>%
  pluck("pb")


## Load ID lookup and corrected item-level codebook
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))
codebook <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Qualtrics Corrected Codebook.rds")


## Load assessment windows
ax_windows <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Assessment Windows.rds")



####  Clean Data  ####
### Create log
# Create lists for logging (a) items used to compute item completion rate below via
# compute_item_completion_rate(), (b) items used to compute means via mean_across(),
# and (c) items used to compute counts via count_across()
log <- list(
  item_completion_rate = list(),
  mean_items = list(),
  count_items = list()
)



### Fix LSMH IDs (manually as necessary)
pb_fixed_ids <- pb_corrected %>%
  rowwise() %>%
  mutate(
    lsmh_id = case_when(
      
      # Cases to be manually recoded
      lsmh_id == "Baseline" & pb_lsmh_id == "LSMH02533" ~ "LSMH02533",
      lsmh_id == "LMSH00886" & pb_lsmh_id == "LSMH00886" ~ "LSMH00886",
      lsmh_id == "LSMH01836 Password: 3tp2_parent" ~ "LSMH01836",
      
      # All others (helper function for cases in which IDs are same or one/both IDs are missing)
      TRUE ~ resolve_id_pair(lsmh_id, pb_lsmh_id)
      
    )
  ) %>%
  ungroup()

# Check LSMH ID format
warn_invalid_id_format(pb_fixed_ids$lsmh_id)


### Remove invalid responses
# Filter to known valid LSMH IDs (marked "keep" in id_lookup) using helper function
pb_valid_ids <- remove_invalid_p2_qualtrics_responses(pb_fixed_ids, id_lookup)


### Identify duplicates and compute item completion rate for removing duplicates
# Identify duplicates using helper function
identify_duplicates(pb_valid_ids, lsmh_id, phase = 2)

# Compute item completion rate using helper function (given that Qualtrics's "Progress"
# and "Finished" variables reflect only clicking through survey, not completing items)
pb_valid_ids <- compute_item_completion_rate(pb_valid_ids, "pb", phase = 2)


### Remove any surveys (a) outside assessment window (or for parents of youth who 
### didn't start EMA) or (b) duplicated in window
# Compute indicator of baseline completion in window using helper function
pb_valid_ids <- mark_done_in_ax_window(pb_valid_ids, "b", ax_windows)

# Print and remove any baseline surveys outside window
# - Note: Filtering on baseline window already keeps latter survey for LSMH02077, which 
# README_ttt_p2_data_collection says to keep as it was done on the same day as the child
pb_valid_ids %>%
  filter(!in_window_b_ext | is.na(in_window_b_ext)) %>%
  select(lsmh_id, StartDate, EndDate, first_ema_notif_date,
         ax_window_b_start_org, ax_window_b_end_org, in_window_b_org, 
         days_before_start_window_b_org, days_after_end_window_b_org, 
         ax_window_b_start_ext, ax_window_b_end_ext, in_window_b_ext, item_completion_rate) %>%
  arrange(lsmh_id, EndDate)

pb_valid_ids <- pb_valid_ids %>%
  filter(in_window_b_ext)

# Remove duplicates using helper function
pb_deduplicated <- remove_duplicates(pb_valid_ids, lsmh_id)

# Double-check deduplication
identify_duplicates(pb_deduplicated, lsmh_id, phase = 2)


### Clean columns
pb_recoded <- pb_deduplicated %>%
  
  # Un-reverse code items with helper function
  unreverse_code_items(codebook) %>%
  
  # Clean remaining columns by row and create composites using helper functions
  rowwise() %>%
  mutate(
    
    ## Metadata
    # ID ("lsmh_id" cleaned above)
    
    # Survey completion
    pb_complete = !is.na(EndDate),
    
    # Survey datetime and duration
    pb_datetime = EndDate,
    pb_date = date(pb_datetime),
    pb_duration = EndDate - StartDate,
    
    # Baseline survey completion in original and extended assessment 
    # windows and days survey was completed before/after original window
    pb_in_window_org = in_window_b_org,
    pb_in_window_ext = in_window_b_ext,
    pb_days_before_start_window_b_org = days_before_start_window_b_org,
    pb_days_after_end_window_b_org = days_after_end_window_b_org,
    
    
    ## Demographics at baseline
    # Child age
    pb_child_age = pb_childage,
    
    # Child sex
    pb_child_sex = case_match(
      pb_childsex,
      1 ~ "Male",
      2 ~ "Female"
    ),
    
    # Child gender
    pb_child_gender = case_match(
      pb_childgender,
      1 ~ "Agender",
      2 ~ "Androgyne",
      3 ~ "Demigender",
      4 ~ "Genderqueer or genderfluid",
      5 ~ "Man",
      6 ~ "Questioning or unsure",
      7 ~ "Trans man",
      8 ~ "Trans woman",
      9 ~ "Woman",
      10 ~ "Other"
    ),
    pb_child_gender_other = pb_childgender_10_TEXT,

    # Child ethnicity
    pb_child_ethnicity = case_match(
      pb_childethnicity,
      1 ~ "AI/AN",
      2 ~ "Asian",
      3 ~ "Black/African American",
      4 ~ "Hispanic or Latino/a/x",
      5 ~ "NH/PI",
      6 ~ "White non-Hispanic",
      8 ~ "Other",
      9 ~ "Multiple"
    ),
    pb_child_ethnicity_other = pb_childethnicity_8_TEXT,
    
    # Child n/siblings (these are manually corrected below)
    pb_n_sisters = as.numeric(pb_siblings_1),
    pb_n_brothers = as.numeric(pb_siblings_2),
    pb_n_siblings = pb_n_sisters + pb_n_brothers,
    
    # Child grade
    pb_grade = case_when(
      pb_grade %in% 5:12 ~ as.character(pb_grade),
      pb_grade_13_TEXT == "4th" ~ "4",
      pb_grade_13_TEXT == "CCP student third year college" ~ "Other"
    ),
    pb_grade_other = pb_grade_13_TEXT,
    
    # Child school type
    pb_school = case_when(
      pb_school == 1 ~ "Public School",
      pb_school == 2 ~ "Private School",
      pb_school == 3 ~ "Parochial School",
      pb_school == 4 ~ "Magnet School",
      pb_school == 5 ~ "Special Education",
      pb_school == 6 ~ "Combination of Special Education and Regular School",
      pb_school_7_TEXT %in% c(
        "Public School via Blended Learning Program which is online; testing at physical location",
        "Public School Virtual",
        "Public virtual",
        "Virtual Public School"
      ) ~ "Public School",
      pb_school_7_TEXT %in% c(
        "home school", "Home School", "Home/unschool", "homeschool", "Homeschool", "homeschooled", "Homeschooled",
        "Charter Homeschool",
        "Online homeschooling",
        "Was homeschooled, will be starting public HS in Fall 2022"
      ) ~ "Other (Specified Homeschool)",
      pb_school_7_TEXT %in% c(
        "charter", "Charter", "Charter school", "Charter School",
        "Online Charter School",
        "Public Charter"
      ) ~ "Other (Specified Charter School)",
      pb_school_7_TEXT %in% c(
        "Academy",
        "Catholic Sector",
        "CCP College",
        "College Prep",
        "Charter Homeschool",
        "Currently in PHP w/remote home school component",
        "Department of defense education school military base",
        "has an IEP",
        "online school", "virtual", "Virtual", "Virtual School"
      ) ~ "Other",
    ),
    pb_school_other = pb_school_7_TEXT,
    
    # Family income
    pb_income = ordered(
      pb_income, 
      levels = 1:8,
      labels = c(
        "$0-$19,000",
        "$20,000-$39,000",
        "$40,000-$59,000",
        "$60,000-$79,000",
        "$80,000-$99,000",
        "$100,000-$119,000",
        "$120,000-$140,000",
        "$140,000+"
      )
    ),
    
    
    ## Parent characteristics
    # Parent age
    pb_parent_age = if_else(
      pb_caregiver1_1 == 2, # Set invalid responses to NA (these are manually corrected below)
      NA_real_,
      pb_caregiver1_1
    ),
    
    # Parent sex
    pb_parent_sex = case_match(
      pb_caregiver1_2,
      1 ~ "Male",
      2 ~ "Female"
    ),
    
    # Parent gender
    pb_parent_gender = case_match(
      pb_caregiver1_3,
      1 ~ "Agender",
      2 ~ "Androgyne",
      3 ~ "Demigender",
      4 ~ "Genderqueer or genderfluid",
      5 ~ "Man",
      6 ~ "Questioning or unsure",
      7 ~ "Trans man",
      8 ~ "Trans woman",
      9 ~ "Woman",
      10 ~ "Other"
    ),
    pb_parent_gender_other = pb_caregiver1_3_10_TEXT,
    
    # Parent ethnicity
    pb_parent_ethnicity = case_match(
      pb_caregiver1_4,
      1 ~ "AI/AN",
      2 ~ "Asian",
      3 ~ "Black/African American",
      4 ~ "Hispanic or Latino/a/x",
      5 ~ "NH/PI",
      6 ~ "White non-Hispanic",
      8 ~ "Other",
      9 ~ "Multiple"
    ),
    pb_parent_ethnicity_other = pb_caregiver1_4_8_TEXT,
    
    # Parent relationship to child
    pb_parent_relationship_to_child = case_match(
      pb_caregiver1_5,
      1 ~ "Biological Parent",
      2 ~ "Step-Parent",
      3 ~ "Adoptive Parent",
      4 ~ "Foster Parent",
      5 ~ "Other"
    ),
    pb_parent_relationship_to_child_other = pb_caregiver1_5_5_TEXT,
    
    # Parent relationship status
    pb_parent_relationship_status = case_match(
      pb_caregiver1_6,
      1 ~ "Married",
      2 ~ "Widowed",
      3 ~ "Divorced",
      4 ~ "Separated",
      5 ~ "Never Married",
      6 ~ "Living with Partner"
    ),
    
    # Single parent status
    pb_parent_single_parent = case_match(
      pb_single_parent,
      1 ~ "Yes",
      2 ~ "No"
    ),
    
    # Parent educational attainment
    pb_parent_education = ordered(
      pb_caregiver1_7, 
      levels = 1:6,
      labels = c(
        "Less than high school",
        "Attended high school",
        "Graduated high school",
        "Attended college",
        "Bachelor's degree",
        "Graduate/professional degree"
      )
    ),

    
    ## Child treatment history
    # Current and lifetime treatment
    pb_childtx_lifetime = pb_childtx_1 == 1 | pb_childtx_3 == 1,
    pb_childtx_current = pb_childtx_3 == 1,

    
    ## Child ACES overall count
    pb_child_aces_count = count_across("pb", "ace_y", name = "pb_child_aces_count"),

    
    ## Parent ACES overall count
    pb_parent_aces_count = count_across("pb", "ace_p", name = "pb_parent_aces_count"),

    
    ## BACE (Barriers to Accessing Care Evaluation) overall mean score and subscale
    !!!bace_means("pb"),
    
    
    ## BFAMG (Brief Family Assessment Measure - General Scale) overall mean score
    pb_bfamg_mean = mean_across("pb", "bfamg", name = "pb_bfamg_mean"),
    
    
    ## BHS-4 (Beck Hopelessness Scale - 4-item) overall mean score
    pb_bhs_mean = mean_across("pb", "bhs", name = "pb_bhs_mean"),
    
    
    ## 17 items from BSI-18 (Brief Symptom Inventory-18): overall mean score and subscales
    # Overall mean score and depression subscale lack suicidal thoughts item
    !!!bsi_means("pb"),
    
    
    ## CDI-2-P (Children's Depression Inventory - 2 - Parent Report) overall mean score and subscales
    !!!cdi_p_means("pb"),
    
    
    ## SCARED-Parent (Screen for Child Anxiety and Related Disorders - Parent) overall mean score and subscales
    !!!scared_means("pb")

  ) %>%
  ungroup() %>%
  
  # Select variables
  select(
    
    # Metadata
    lsmh_id,
    pb_complete,
    pb_date,
    pb_datetime,
    pb_duration,
    ax_window_b_start_org,
    ax_window_b_end_org,
    ax_window_b_start_ext,
    ax_window_b_end_ext,
    pb_in_window_org,
    pb_in_window_ext,
    pb_days_before_start_window_b_org,
    pb_days_after_end_window_b_org,
    
    # Parent characteristics
    pb_parent_age,
    pb_parent_sex,
    pb_parent_gender,
    pb_parent_gender_other,
    pb_parent_ethnicity,
    pb_parent_ethnicity_other,
    pb_parent_relationship_to_child,
    pb_parent_relationship_to_child_other,
    pb_parent_relationship_status,
    pb_parent_single_parent,
    pb_parent_education,
    
    # Child demographics
    pb_child_age,
    pb_child_sex,
    pb_child_gender,
    pb_child_gender_other,
    pb_child_ethnicity,
    pb_child_ethnicity_other,
    pb_n_sisters,
    pb_n_brothers,
    pb_n_siblings,
    pb_grade,
    pb_grade_other,
    pb_school,
    pb_school_other,
    pb_income,

    # Child treatment history
    matches("childtx_lifetime"),
    matches("childtx_current"),
    
    # Measures
    matches("_child_aces_"),
    matches("_parent_aces_"),
    matches("_bace_"),
    matches("_bfamg_"),
    matches("_bhs_"),
    matches("_bsi_"),
    matches("_cdi_"),
    matches("_scared_")
    
  )


### Manual corrections, per README_ttt_p2_data_collection
pb_recoded$pb_parent_age[pb_recoded$lsmh_id == "LSMH00666"] <- 42
pb_recoded$pb_parent_age[pb_recoded$lsmh_id == "LSMH00787"] <- 41
pb_recoded$pb_n_sisters[pb_recoded$lsmh_id == "LSMH00899"] <- 0 # n_siblings is still ok, but sibling is non-binary
pb_recoded$pb_n_sisters[pb_recoded$lsmh_id == "LSMH00905"] <- 0 # 0, not 4 sisters, and therefore...
pb_recoded$pb_n_siblings[pb_recoded$lsmh_id == "LSMH00905"] <- 4 # ...4, not 8 siblings


### Check that values are in expected range
items_to_check <- pb_recoded %>%
  select(
    matches("_child_aces_"),
    matches("_parent_aces_"),
    matches("_bace_"),
    matches("_bfamg_"),
    matches("_bhs_"),
    matches("_bsi_"),
    matches("_cdi_"),
    matches("_scared_"),
    -ends_with("mean"), -ends_with("count")
  ) %>%
  names()

walk(items_to_check, check_values, pb_recoded) # check_values() helper function



####  Save Data  ####
# Save clean Qualtrics data
# - Note: LSMH IDs meeting exclusion criteria are dropped later (in "Merge Parent Qualtrics Data.R")
saveRDS(pb_recoded, dirs$clean_data_staging_intermediate %+% "Phase 2 Parent Qualtrics Clean Data - Baseline.rds")

# Save log
saveRDS(log, dirs$clean_data_staging_intermediate %+% "Phase 2 Parent Qualtrics Clean Data Log - Baseline.rds")
