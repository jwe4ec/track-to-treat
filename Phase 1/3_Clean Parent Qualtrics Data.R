## Track-to-Treat Phase 1 Data Cleaning
## Clean parent Qualtrics data
# R version 4.4.3

####  Startup  ####
## Load packages
library(groundhog) # 3.2.2
groundhog_date <- "2025-03-28"
meta.groundhog(groundhog_date)
groundhog.library(
  pkg = c("tidyverse", "qualtRics", "here", "openxlsx"),
  date = groundhog_date
)
`%+%` <- paste0


## Load helper functions
source(here("Qualtrics Data Cleaning Helper Functions.R"))


## Load data
# Save directory
raw_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\Qualtrics Data\\Raw Data\\"
clean_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\Clean Data (Isaac)\\"
clean_data_staging_dir <- clean_data_dir %+% "staging\\"
clean_data_staging_intermediate_dir <- clean_data_staging_dir %+% "intermediate\\"

# Load raw parent Qualtrics datasets in this format: [respondent][wave]_[administration]_raw
# - Note: Use "timeZone" specified for date columns (e.g., "StartDate") in third row of raw CSV
pb_in_person_raw <- read_survey(raw_data_dir %+% "dp5_b_parent_p1_numeric.csv", time_zone = "America/Denver")
pb_remote_raw <- read_survey(raw_data_dir %+% "dp5_b_parent_remote_p1_numeric.csv", time_zone = "America/Denver")
p3m_raw <- read_survey(raw_data_dir %+% "dp5_3m_parent_p1_numeric.csv", time_zone = "America/Denver")

# Load assessment windows computed when cleaning youth Qualtrics data
ax_windows <- readRDS(clean_data_staging_intermediate_dir %+% "Phase 1 Assessment Windows.rds")

# Load item-level codebook file
codebook_path <- here("Phase 1", "Track to Treat P1 Codebook.xlsx")
sheet_name <- "Individual Variables"
(sheet_last_row <- nrow(openxlsx::read.xlsx(codebook_path, sheet_name)) + 1) # Add 1 for header row

codebook <- openxlsx::read.xlsx(
  codebook_path,
  sheet_name,
  rows = c(1, 3:sheet_last_row) # Skip column description row
) %>%
  # Select only necessary variables
  select(
    item = Variable.Name,
    measure = Measure,
    subscale = Subscale,
    minimum = Minimum,
    maximum = Maximum,
    reversed = `Is.the.variable.reverse.coded?`
  ) %>%
  mutate(
    # Make `reversed` logical
    reversed = reversed == 1,
    # Create `reverse_base`: the number a response should be subtracted from to reverse it
    reverse_base = if_else(
      reversed,
      maximum + minimum,
      NA_real_
    )
  )



####  Clean Data  ####
### Combine in-person and remote administrations
# First, need to recode pb_siblings_2 as character in the remote dataset, to 
# conform with the in-person dataset
pb_remote_raw$pb_siblings_2 <- as.character(pb_remote_raw$pb_siblings_2)

# Note on variable overlap: 
# - No variables appear in the in-person dataset only
# - Variables that appear in the remote dataset only include "password_parent",
#   COVID-related variables, "pb_teletherapy1" and "pb_teletherapy2" (interest in 
#   teletherapy), "pb_online_tx1" and "pb_online_tx2" (interest in self-guided 
#   mental health programs), "pb_interview" (interest in being interviewed by a 
#   journalist), and click and time on page information
pb_raw <- bind_rows(
  list(
    "in-person" = pb_in_person_raw, 
    "remote" = pb_remote_raw
  ),
  .id = "administration"
)


### Confirm that all IDs match "validate" columns and then remove "validate" columns
all(pb_raw$pb_lsmh_id == pb_raw$`pb_lsmh_id_validate`, na.rm = TRUE)
all(p3m_raw$p3m_lsmh_id == p3m_raw$p3m_lsmh_id_validate, na.rm = TRUE)

pb_raw[, "pb_lsmh_id_validate"] <- NULL
p3m_raw[, "p3m_lsmh_id_validate"] <- NULL


### Remove invalid responses
# Invalid IDs: Tests or survey previews
invalid_ids <- c("LSMH00000", "LSMH00000000111", "LSMH00001", "LSMH00062", "LSMH000", "LSMH000000")

# Remove invalid responses using helper function
pb_valid_ids <- remove_invalid_responses(pb_raw, pb_lsmh_id)
p3m_valid_ids <- remove_invalid_responses(p3m_raw, p3m_lsmh_id)


### Create lists for logging (a) items used to compute item completion rates below via
### compute_item_completion_rate(), items used to compute (b) means via mean_across() and
### (c) counts via count_across(), and (d) clean codebook (edited and added to log below)
log <- list(item_completion_rate = list(),
            mean_items = list(),
            count_items = list())


### Identify duplicates and compute item completion rate for removing duplicates
# Identify duplicates using helper function
identify_duplicates(pb_valid_ids, pb_lsmh_id)
identify_duplicates(p3m_valid_ids, p3m_lsmh_id)

# Compute item completion rate using helper function (given that Qualtrics's "Progress" 
# and "Finished" variables reflect only clicking through survey, not completing items)
pb_valid_ids <- compute_item_completion_rate(pb_valid_ids, "pb")
p3m_valid_ids <- compute_item_completion_rate(p3m_valid_ids, "p3m")


### Remove any baseline surveys (a) outside assessment window or (b) duplicated in window
# Obtain EMA notification dates from assessment windows computed when cleaning youth Qualtrics data
ema_notif_dates <- ax_windows[, c("lsmh_id", "first_ema_notif_date", "last_ema_notif_date", "end_ema_period")]

# Compute indicator of baseline survey completion in window using helper function
pb_valid_ids <- mark_b_done_in_ax_window(pb_valid_ids, "pb_lsmh_id", ema_notif_dates)

# TODO: Remove any baseline surveys outside window (0) using helper function





# Remove any baseline duplicates (0) using helper function
pb_deduplicated <- remove_duplicates(pb_valid_ids, pb_lsmh_id)

# Double-check baseline deduplication
identify_duplicates(pb_deduplicated, pb_lsmh_id)


### Remove any follow-up surveys (a) outside assessment window or (b) duplicated in window
# Note: Assessment windows were computed when cleaning youth Qualtrics data

# Compute indicators of 3-month survey completion in window using helper function
p3m_valid_ids <- mark_3m_done_in_ax_window(p3m_valid_ids, "p3m_lsmh_id", ax_windows)

# Remove 3-month surveys outside extended window (v3) using helper function
p3m_valid_ids <- remove_out_of_ax_window(p3m_valid_ids, "p3m_lsmh_id", "p3m")

# Remove 3-month duplicates using helper function
p3m_deduplicated <- remove_duplicates(p3m_valid_ids, p3m_lsmh_id)

# Double-check follow-up deduplication
identify_duplicates(p3m_deduplicated, p3m_lsmh_id)

# Remove columns redundant with baseline dataset
p3m_deduplicated <- p3m_deduplicated %>%
  select(-first_ema_notif_date, -last_ema_notif_date, -end_ema_period)


### Merge data by LSMH ID
# IDs in baseline not 3m
setdiff(pb_deduplicated$pb_lsmh_id, p3m_deduplicated$p3m_lsmh_id)

# IDs in 3m not baseline
setdiff(p3m_deduplicated$p3m_lsmh_id, pb_deduplicated$pb_lsmh_id)

# Full join
p_merged <- full_join(
  pb_deduplicated,
  p3m_deduplicated,
  by = c("pb_lsmh_id" = "p3m_lsmh_id"),
  relationship = "one-to-one",
  suffix = c(".pb", ".p3m")
)


### Clean columns
## Correct misspelled item prefixes in the data and codebook
# Data: Before
prefixes_data <- str_extract(colnames(p_merged), "^.*?(?=_)")
table(prefixes_data)

# Data: Fixing
colnames(p_merged) <- gsub("^p3_", "p3m_", colnames(p_merged))

# Data: After
prefixes_data <- str_extract(colnames(p_merged), "^.*?(?=_)")
table(prefixes_data)

# Codebook: Before
prefixes_codebook <- str_extract(codebook$item, "^.*?(?=_)")
table(prefixes_codebook)

# Codebook: Fixing
codebook$item <- gsub("^p3_", "p3m_", codebook$item)

# Codebook: After
prefixes_codebook <- str_extract(codebook$item, "^.*?(?=_)")
table(prefixes_codebook)

# Add clean codebook to log
log$codebook_clean <- codebook

# Data collected but not included here: 
# - Data regarding child's current medications
# - Data regarding child's school accommodations
# - Further details regarding child's current and lifetime treatment (e.g., type, provider)
# - Parent's treatment history
# - Data regarding parent's care taking responsibilities
# - Additional caregiver demographics
# - Child demographics at 3 months
# - COVID-19-related variables
# - Parent attitudes towards therapy
# - Child birth order (requires manual coding)
# - Parent Prognostic Pessimism for Depression scale (PPD)
# These can be cleaned if needed but I'm not sure we have plans for them...
p_clean <- p_merged %>%
  
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
      .cols = any_of(codebook$item[codebook$reversed]),
      .fns = ~ codebook$reverse_base[codebook$item == cur_column()] - .x
    )
  ) %>%
  
  # Clean columns and create composites
  rowwise() %>%
  mutate(
    
    ## Metadata
    # ID
    lsmh_id = pb_lsmh_id,

    # Administration mode
    pb_administration = administration,
    
    # Survey completion
    pb_complete = !is.na(EndDate.pb),
    p3m_complete = !is.na(EndDate.p3m),
    
    # Survey datetime and duration
    pb_date = EndDate.pb,
    pb_duration = EndDate.pb - StartDate.pb,
    p3m_date = EndDate.p3m,
    p3m_duration = EndDate.p3m - StartDate.p3m,
    
    # Follow-up survey completion in original (v2) and extended (v3) assessment 
    # windows and days survey was completed before/after original window
    p3m_in_window_v2 = in_window_3m_v2,
    p3m_in_window_v3 = in_window_3m_v3,
    p3m_days_before_start_window_3m_v2 = days_before_start_window_3m_v2,
    p3m_days_after_end_window_3m_v2 = days_after_end_window_3m_v2,
    
    
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

    # Child ethnicity
    pb_child_ethnicity = case_match(
      pb_childethnicity,
      1 ~ "AI/AN",
      2 ~ "Asian",
      3 ~ "Black/African American",
      4 ~ "Hispanic or Latino/a/x",
      5 ~ "NH/PI",
      6 ~ "White non-Hispanic",
      8 ~ "Multiple", # This is actually "Other", but the only response indicates "multiple"
      9 ~ "Multiple"
    ),
    
    # Child n/siblings
    pb_n_sisters = case_when(
      pb_siblings_1 == "1-3" ~ 2,
      T ~ as.numeric(pb_siblings_1)
    ),
    pb_n_brothers = as.numeric(pb_siblings_2),
    pb_n_siblings = pb_n_sisters + pb_n_brothers,
    
    # Child grade: Does not need further cleaning
    
    # Child school type
    pb_school = case_when(
      pb_school == 1 ~ "Public School",
      pb_school == 2 ~ "Private School",
      pb_school == 3 ~ "Parochial School",
      pb_school == 4 ~ "Magnet School",
      pb_school == 5 ~ "Special Education",
      pb_school == 6 ~ "Combination of Special Education and Regular School",
      pb_school_7_TEXT == "Public, Integrated classes, but also Home instruction at times due to health issues" ~ "Public School",
      pb_school_7_TEXT %in% c(
        "home instruction",
        "Homeschool",
        "homeschool",
        "homeschooled",
        "Public Home Charter"
      ) ~ "Other (Specified Homeschool)",
      pb_school_7_TEXT %in% c(
        "Public Charter School",
        "Public Charter",
        "Charter"
      ) ~ "Other (Specified Charter School)"
    ),
    
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
    pb_caregiver1_age = pb_caregiver1_1,
    
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
    
    # Parent relationship to child
    pb_caregiver1_relationship_to_child = case_match(
      pb_caregiver1_5,
      1 ~ "Biological Parent",
      2 ~ "Step-Parent",
      3 ~ "Adoptive Parent",
      4 ~ "Foster Parent",
      5 ~ "Other"
    ),
    
    # Parent relationship status
    pb_caregiver1_relationship_status = case_match(
      pb_caregiver1_6,
      1 ~ "Married",
      2 ~ "Widowed",
      3 ~ "Divorced",
      4 ~ "Separated",
      5 ~ "Never Married",
      6 ~ "Living with Partner"
    ),
    
    # Parent educational attainment
    pb_caregiver1_education = case_match(
      pb_caregiver1_7,
      1 ~ "Less than high school",
      2 ~ "Attended high school",
      3 ~ "Graduated high school",
      4 ~ "Attended college",
      5 ~ "Bachelor's degree",
      6 ~ "Graduate/professional degree"
    ),

    
    ## Child treatment history
    # Current and lifetime treatment
    pb_childtx_lifetime = pb_childtx_1 == 1 | pb_childtx_3 == 1,
    pb_childtx_current = pb_childtx_3 == 1,
    p3m_childtx_lifetime = p3m_childtx_1 == 1 | p3m_childtx_3 == 1,
    p3m_childtx_current = p3m_childtx_3 == 1,

    
    ## Child ACES
    # Overall mean score
    pb_child_aces_count = count_across("pb", "ace_y", name = "pb_child_aces_count"), # count_across() from helper function script
    p3m_child_aces_count = count_across("p3m", "ace_y", name = "p3m_child_aces_count"),
    
    
    ## Parent ACES
    # Overall mean score
    pb_parent_aces_count = count_across("pb", "ace_p", name = "pb_parent_aces_count"),
    p3m_parent_aces_count = count_across("p3m", "ace_p", name = "p3m_parent_aces_count"),
    
    
    ## CDI-2 (Children's Depression Inventory - 2)
    # Overall mean score
    pb_cdi_mean = mean_across("pb", "CDI-2 P", name = "pb_cdi_mean"),
    p3m_cdi_mean = mean_across("p3m", "CDI-2 P", name = "p3m_cdi_mean"),
    
    # Emotional problems subscale
    pb_cdi_emotional_mean = mean_across("pb", "CDI-2 P", "Emotional Problems", name = "pb_cdi_emo_mean"),
    p3m_cdi_emotional_mean = mean_across("p3m", "CDI-2 P", "Emotional Problems", name = "p3m_cdi_emo_mean"),
    
    # Functional problems subscale
    pb_cdi_functional_mean = mean_across("pb", "CDI-2 P", "Functional Problems", name = "pb_cdi_fun_mean"),
    p3m_cdi_functional_mean = mean_across("p3m", "CDI-2 P", "Functional Problems", name = "p3m_cdi_fun_mean"),
    
    
    ## BHS-4 (Beck Hopelessness Scale - 4-item)
    # Overall mean score
    pb_bhs_mean = mean_across("pb", "bhs", name = "pb_bhs_mean"),
    p3m_bhs_mean = mean_across("p3m", "bhs", name = "p3m_bhs_mean"),
    
    
    ## BFAMG (Brief Family Assessment Measure - General Scale)
    # Overall mean score
    pb_bfamg_mean = mean_across("pb", "bfamg", name = "pb_bfamg_mean"),
    p3m_bfamg_mean = mean_across("p3m", "bfamg", name = "p3m_bfamg_mean"),
    
    
    ## BSI (Brief Symptom Inventory)
    # Overall mean score
    pb_bsi_mean = mean_across("pb", "bsi", name = "pb_bsi_mean"),
    p3m_bsi_mean = mean_across("p3m", "bsi", name = "p3m_bsi_mean"),
    
    # Somatization subscale
    pb_bsi_s_mean = mean_across("pb", "bsi", "S", name = "pb_bsi_s_mean"),
    p3m_bsi_s_mean = mean_across("p3m", "bsi", "S", name = "p3m_bsi_s_mean"),
    
    # Depression subscale
    pb_bsi_d_mean = mean_across("pb", "bsi", "D", name = "pb_bsi_d_mean"),
    p3m_bsi_d_mean = mean_across("p3m", "bsi", "D", name = "p3m_bsi_d_mean"),
    
    # Anxiety subscale
    pb_bsi_a_mean = mean_across("pb", "bsi", "A", name = "pb_bsi_a_mean"),
    p3m_bsi_a_mean = mean_across("p3m", "bsi", "A", name = "p3m_bsi_a_mean"),
    
    
    ## BACE (Barriers to Accessing Care Evaluation)
    # Overall mean score
    pb_bace_mean = mean_across("pb", "bace", name = "pb_bace_mean"),
    p3m_bace_mean = mean_across("p3m", "bace", name = "p3m_bace_mean"),
    
    # Treatment stigma subscale
    pb_bace_stigma_mean = mean_across("pb", "bace", "Treatment Stigma", name = "pb_bace_stigma_mean"),
    p3m_bace_stigma_mean = mean_across("p3m", "bace", "Treatment Stigma", name = "p3m_bace_stigma_mean"),
    
    
    ## SCARED (Screen for Child Anxiety and Related Disorders)
    # Overall mean score
    pb_scared_mean = mean_across("pb", "scared", name = "pb_scared_mean", exclude = "pb_scared_c_1"),
    p3m_scared_mean = mean_across("p3m", "scared", name = "p3m_scared_mean", exclude = "p3m_scared_c_1"),
    
    # Panic disorder/significant somatic symptoms subscale
    pb_scared_paso_mean = mean_across("pb", "scared", "PA/SO", name = "pb_scared_paso_mean"),
    p3m_scared_paso_mean = mean_across("p3m", "scared", "PA/SO", name = "p3m_scared_paso_mean"),
    
    # Generalized anxiety disorder subscale
    pb_scared_ga_mean = mean_across("pb", "scared", "GA", name = "pb_scared_ga_mean"),
    p3m_scared_ga_mean = mean_across("p3m", "scared", "GA", name = "p3m_scared_ga_mean"),
    
    # Separation anxiety disorder subscale
    pb_scared_sep_mean = mean_across("pb", "scared", "SEP", name = "pb_scared_sep_mean"),
    p3m_scared_sep_mean = mean_across("p3m", "scared", "SEP", name = "p3m_scared_sep_mean"),
    
    # Social phobic disorder subscale
    pb_scared_soc_mean = mean_across("pb", "scared", "SOC", name = "pb_scared_soc_mean"),
    p3m_scared_soc_mean = mean_across("p3m", "scared", "SOC", name = "p3m_scared_soc_mean"),
    
    # Significant school avoidance symptoms
    pb_scared_sch_mean = mean_across("pb", "scared", "SCH", name = "pb_scared_sch_mean"),
    p3m_scared_sch_mean = mean_across("p3m", "scared", "SCH", name = "p3m_scared_sch_mean"),
    
  ) %>%
  
  ungroup() %>%
  
  select(
    
    # Metadata
    lsmh_id,
    pb_administration,
    pb_complete,
    p3m_complete,
    pb_date,
    pb_duration,
    p3m_date,
    p3m_duration,
    start_window_3m_v2,
    end_window_3m_v2,
    start_window_3m_v3,
    end_window_3m_v3,
    p3m_in_window_v2,
    p3m_in_window_v3,
    p3m_days_before_start_window_3m_v2,
    p3m_days_after_end_window_3m_v2,
    
    # Parent characteristics
    
    # Child demographics
    pb_child_age,
    pb_child_sex,
    pb_child_gender,
    pb_child_ethnicity,
    pb_n_sisters,
    pb_n_brothers,
    pb_n_siblings,
    pb_grade,
    pb_school,
    pb_income,

    # Child treatment history
    matches("childtx_lifetime"),
    matches("childtx_current"),
    
    # Measures
    matches("_child_aces_"),
    matches("_parent_aces_"),
    matches("_cdi_"),
    matches("_bhs_"),
    matches("_bsi_"),
    matches("_bace_"),
    matches("_bfamg_"),
    matches("_scared_")
    
  )


### Check that values are in expected range
items_to_check <- p_clean %>%
  select(
    matches("_child_aces_"),
    matches("_parent_aces_"),
    matches("_cdi_"),
    matches("_bhs_"),
    matches("_bsi_"),
    matches("_bace_"),
    matches("_bfamg_"),
    matches("_scared_"),
    -ends_with("mean"), -ends_with("count")
  ) %>%
  names()

walk(
  items_to_check,
  ~ check_values(
    .data = p_clean,
    .item = .x
  )
)



####  Save Clean Parent Qualtrics Data and Log  ####
saveRDS(p_clean, clean_data_staging_dir %+% "Phase 1 Parent Qualtrics Clean Data.rds")
saveRDS(log, clean_data_staging_dir %+% "Phase 1 Parent Qualtrics Clean Data Log.rds")