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
pb_path <- raw_data_dir %+% "DP5+Phase+2+-+Parent+-+Baseline_May+6,+2025_09.42_n.csv"
pb_raw <- read_survey(pb_path, time_zone = "America/Chicago")


## Load ID lookup
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))


## Load item-level codebook file using helper function
codebook <- load_p2_codebook(here("Phase 2", "2025.07.02 Track to Treat P2 Codebook.xlsx"))


## Load assessment windows
ax_windows <- readRDS(clean_data_staging_intermediate_dir %+% "Phase 2 Assessment Windows.rds")


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
check_raw_data_ver(raw_metadata, list(pb_path), list(pb_raw), "pb_qualtrics")



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


### Use unique "ImportId" to rename columns both named "test" in Qualtrics
# - "read_survey()" contingently named these by their column indices upon import to R
col_map <- attr(pb_raw, "column_map")

test_col1_qname <- col_map$qname[col_map$ImportId == "test_CED8iraacy"]
test_col2_qname <- col_map$qname[col_map$ImportId == "test_CEDzbsbl7w"]

pb_renamed <- pb_raw

names(pb_renamed)[names(pb_renamed) == test_col1_qname] <- "test_col1"
names(pb_renamed)[names(pb_renamed) == test_col2_qname] <- "test_col2"


### Correct LSMH IDs (manually as necessary)
pb_corrected_ids <- pb_renamed %>%
  rowwise() %>%
  mutate(
    lsmh_id = case_when(
      
      # Cases to be manually recoded
      lsmh_id == "Baseline" & pb_lsmh_id == "LSMH02533" ~ "LSMH02533",
      lsmh_id == "LMSH00886" & pb_lsmh_id == "LSMH00886" ~ "LSMH00886",
      lsmh_id == "LSMH01836 Password: 3tp2_parent" ~ "LSMH01836",
      
      # Cases where both match
      lsmh_id == pb_lsmh_id ~ lsmh_id,
      
      # Cases where one is missing (keep the non-missing value)
      is.na(lsmh_id) & !is.na(pb_lsmh_id) ~ pb_lsmh_id,
      is.na(pb_lsmh_id) & !is.na(lsmh_id) ~ lsmh_id,
      
      # Cases where both are missing
      is.na(lsmh_id) & is.na(pb_lsmh_id) ~ NA_character_,
      
      # Additional cases are flagged for cleaning
      T ~ "ID Combination Unaccounted For (lsmh_id '" %+% lsmh_id %+% "', pb_lsmh_id '" %+% pb_lsmh_id %+% "')"
      
    )
  ) %>%
  ungroup()


### Remove invalid responses
# Known valid LSMH IDs
valid_ids <- id_lookup %>%
  filter(action == "keep") %>%
  distinct(lsmh_id)

invalid_ids <- setdiff(pb_corrected_ids$lsmh_id, valid_ids$lsmh_id)

pb_valid_ids <- pb_corrected_ids %>%
  inner_join(
    valid_ids,
    by = "lsmh_id",
    relationship = "many-to-one"
  )

# Just FYI: This is how many IDs/rows included known LSMH IDs matched for removal
pb_corrected_ids %>%
  filter(lsmh_id %in% id_lookup$lsmh_id[id_lookup$action == "drop"]) %>%
  count(lsmh_id)

# Just FYI: No rows contained unknown LSMH IDs (good!)
# If these rows indicate typos or other errors in the IDs, fix them in the mutate() above
pb_corrected_ids %>%
  filter(!lsmh_id %in% id_lookup$lsmh_id) %>%
  count(lsmh_id)


### Identify duplicates and compute item completion rate for removing duplicates
# Identify duplicates using helper function
identify_duplicates(pb_valid_ids, lsmh_id)

# Compute item completion rate using helper function (given that Qualtrics's "Progress"
# and "Finished" variables reflect only clicking through survey, not completing items)
pb_valid_ids <- compute_item_completion_rate(pb_valid_ids, "pb", phase = 2)


### Remove any surveys (a) outside assessment window (or for parents of youth who 
### didn't start EMA) or (b) duplicated in window
# Obtain EMA notification dates from assessment windows computed when cleaning youth Qualtrics data
ema_notif_dates <- ax_windows[, c("lsmh_id", "first_ema_notif_date", "last_ema_notif_date", "end_ema_period")]

# Compute indicator of baseline survey completion in window using helper function
# - LSMH00920 has two responses, one with the most data before "first_ema_notif_date" 
# and one with no data months later 
pb_valid_ids <- mark_b_done_in_ax_window(pb_valid_ids, "lsmh_id", ema_notif_dates)

# Print and remove any baseline surveys outside window
pb_valid_ids %>%
  filter(!in_window_b | is.na(in_window_b)) %>%
  select(lsmh_id, "StartDate", "EndDate", "first_ema_notif_date", "in_window_b", "item_completion_rate") %>%
  arrange(lsmh_id, EndDate)

pb_valid_ids <- pb_valid_ids %>%
  filter(in_window_b)

# For LSMH02077, manually keep the latter survey, as this was done on the same day
# as the child, per README_ttt_p2_data_collection
pb_manual_filter_02077 <- pb_valid_ids %>%
  filter(
    !(lsmh_id == "LSMH02077" & as_date(EndDate) == mdy("8/27/2022"))
  )

# Remove duplicates using helper function
pb_deduplicated <- remove_duplicates(pb_manual_filter_02077, lsmh_id)

# Double-check deduplication
identify_duplicates(pb_deduplicated, lsmh_id)


### Clean columns
pb_recoded <- pb_deduplicated %>%
  
  # Remove click, page time variables
  select(
    
    -matches("Click Count"),
    -matches("First Click"),
    -matches("Last Click"),
    -matches("Page Submit")
    
  ) %>%
  
  # Rename "mvps" to "mpvs" throughout
  rename_with(
    .cols = contains("mvps"),
    .fn = ~ gsub("mvps", "mpvs", .x)
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
    pb_complete = !is.na(EndDate),
    
    # Survey datetime and duration
    pb_datetime = EndDate,
    pb_date = date(pb_datetime),
    pb_duration = EndDate - StartDate,
    
    
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

    
    ## Child ACES
    # Overall count
    pb_child_aces_count = count_across("pb", "ace_y", name = "pb_child_aces_count"), # count_across() from helper function script

    
    ## Parent ACES
    # Overall count
    pb_parent_aces_count = count_across("pb", "ace_p", name = "pb_parent_aces_count"),

    
    ## CDI-2 (Children's Depression Inventory - 2)
    # Overall mean score
    pb_cdi_mean = mean_across("pb", "CDI-2 P", name = "pb_cdi_mean"),

    # Emotional problems subscale
    pb_cdi_emotional_mean = mean_across("pb", "CDI-2 P", "Emotional Problems", name = "pb_cdi_emo_mean"),

    # Functional problems subscale
    pb_cdi_functional_mean = mean_across("pb", "CDI-2 P", "Functional Problems", name = "pb_cdi_fun_mean"),

    
    ## BHS-4 (Beck Hopelessness Scale - 4-item)
    # Overall mean score
    pb_bhs_mean = mean_across("pb", "bhs", name = "pb_bhs_mean"),

    
    ## BFAMG (Brief Family Assessment Measure - General Scale)
    # Overall mean score
    pb_bfamg_mean = mean_across("pb", "bfamg", name = "pb_bfamg_mean"),

    
    ## 17 items from BSI-18 (Brief Symptom Inventory-18)
    # Overall mean score (without suicidal thoughts item)
    pb_bsi_mean = mean_across("pb", "bsi", name = "pb_bsi_mean"),

    # Somatization subscale
    pb_bsi_s_mean = mean_across("pb", "bsi", "S", name = "pb_bsi_s_mean"),

    # Depression subscale (without suicidal thoughts item)
    pb_bsi_d_mean = mean_across("pb", "bsi", "D", name = "pb_bsi_d_mean"),

    # Anxiety subscale
    pb_bsi_a_mean = mean_across("pb", "bsi", "A", name = "pb_bsi_a_mean"),

    
    ## BACE (Barriers to Accessing Care Evaluation)
    # Overall mean score
    pb_bace_mean = mean_across("pb", "bace", name = "pb_bace_mean"),

    # Treatment stigma subscale
    pb_bace_stigma_mean = mean_across("pb", "bace", "Treatment Stigma", name = "pb_bace_stigma_mean"),

    
    ## SCARED (Screen for Child Anxiety and Related Disorders)
    # Overall mean score
    pb_scared_mean = mean_across("pb", "scared", name = "pb_scared_mean"),

    # Panic disorder/significant somatic symptoms subscale
    pb_scared_paso_mean = mean_across("pb", "scared", "PA/SO", name = "pb_scared_paso_mean"),

    # Generalized anxiety disorder subscale
    pb_scared_ga_mean = mean_across("pb", "scared", "GA", name = "pb_scared_ga_mean"),

    # Separation anxiety disorder subscale
    pb_scared_sep_mean = mean_across("pb", "scared", "SEP", name = "pb_scared_sep_mean"),

    # Social phobic disorder subscale
    pb_scared_soc_mean = mean_across("pb", "scared", "SOC", name = "pb_scared_soc_mean"),

    # Significant school avoidance symptoms
    pb_scared_sch_mean = mean_across("pb", "scared", "SCH", name = "pb_scared_sch_mean"),

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
    matches("_cdi_"),
    matches("_bhs_"),
    matches("_bfamg_"),
    matches("_bsi_"),
    matches("_bace_"),
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
    matches("_cdi_"),
    matches("_bhs_"),
    matches("_bfamg_"),
    matches("_bsi_"),
    matches("_bace_"),
    matches("_scared_"),
    -ends_with("mean"), -ends_with("count")
  ) %>%
  names()

walk(
  items_to_check,
  ~ check_values( # Helper function
    .data = pb_recoded,
    .item = .x
  )
)



####  Save Data  ####
# Save clean Qualtrics data
saveRDS(pb_recoded, clean_data_staging_dir %+% "Phase 2 Parent Qualtrics Clean Data - Baseline.rds")

# Save log
saveRDS(log, clean_data_staging_intermediate_dir %+% "Phase 2 Parent Qualtrics Clean Data Log - Baseline.rds")
