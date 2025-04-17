## Track-to-Treat Phase 1 Data Cleaning
## Qualtrics data (parents)
# R version 4.4.3

####  Startup  ####
## Load packages
library(groundhog) # 3.2.2
groundhog.library(
  pkg = c("tidyverse", "qualtRics", "here", "openxlsx"),
  date = "2025-03-28"
)
`%+%` <- paste0


## Load helper functions
source(here("Qualtrics Data Cleaning Helper Functions.R"))


## Load data
# Save directory
raw_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\Qualtrics Data\\Raw Data\\"
clean_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\Clean Data (Isaac)\\"

# Load datasets in the following format: [respondent][wave]_[administration]_raw
pb_in_person_raw <- read_survey(raw_data_dir %+% "dp5_b_parent_p1_numeric.csv")
pb_remote_raw <- read_survey(raw_data_dir %+% "dp5_b_parent_remote_p1_numeric.csv")
p3m_raw <- read_survey(raw_data_dir %+% "dp5_3m_parent_p1_numeric.csv")

# Load item-level codebook file
codebook <- openxlsx::read.xlsx(
  here("Phase 1", "Track to Treat P1 Codebook.xlsx"),
  sheet = "Individual Variables",
  rows = c(1, 3:1214)
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
## Combine in-person and remote administrations
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


## Remove invalid responses
# Invalid IDs: Tests or survey previews
invalid_ids <- c("LSMH00000", "LSMH00000000111", "LSMH00001", "LSMH00062", "LSMH000", "LSMH000000")

# Remove invalid responses using helper function
pb_valid_ids <- remove_invalid_responses(pb_raw, pb_lsmh_id)
p3m_valid_ids <- remove_invalid_responses(p3m_raw, p3m_lsmh_id)


## Create lists for logging (a) items used to compute item completion rates below via
## compute_item_completion_rate() and (b) items used to compute means via mean_across()
log <- list(item_completion_rate = list(),
            mean_items = list())


### Deduplicate
# Compute item completion rate using helper function (given that Qualtrics's "Progress" 
# and "Finished" variables reflect only clicking through survey, not completing items)
pb_valid_ids <- compute_item_completion_rate(pb_valid_ids, "pb")
p3m_valid_ids <- compute_item_completion_rate(p3m_valid_ids, "p3m")

# Identify duplicates using helper function
identify_duplicates(pb_valid_ids, pb_lsmh_id)
identify_duplicates(p3m_valid_ids, p3m_lsmh_id)

# Remove duplicates using helper function
pb_deduplicated <- remove_duplicates(pb_valid_ids, pb_lsmh_id)
p3m_deduplicated <- remove_duplicates(p3m_valid_ids, p3m_lsmh_id)

# Double-check work
identify_duplicates(pb_deduplicated, pb_lsmh_id)
identify_duplicates(p3m_deduplicated, p3m_lsmh_id)


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


## Clean columns
# Data collected but not included here: 
# - Data regarding child's current medications
# - Data regarding child's school accommodations
# - Further details regarding child's current and lifetime treatment (e.g., type, provider)
# - Parent's treatment history
# - Data regarding parent's care taking responsibilities
# - Parental demographics
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
    
    
    ## Demographics at baseline
    # Child age: Does not need further cleaning
    
    # Child sex
    pb_childsex = case_match(
      pb_childsex,
      1 ~ "Male",
      2 ~ "Female"
    ),
    
    # Child gender
    pb_childgender = case_match(
      pb_childgender,
      5 ~ "Boy",
      9 ~ "Girl",
      c(1, 2, 3, 4, 6, 7, 8, 10) ~ "TGD" # n = 5 total, so collapsing
    ),
    
    # Child ethnicity
    pb_childethnicity = case_match(
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
    
    # Child n/siblings
    pb_n_sisters = as.numeric(pb_siblings_1),
    pb_n_brothers = as.numeric(pb_siblings_2),
    pb_n_siblings = pb_n_sisters + pb_n_brothers,
    
    # Child grade: Does not need further cleaning
    
    # Child school type
    pb_school = case_match(
      pb_school,
      1 ~ "Public School",
      2 ~ "Private School",
      3 ~ "Parochial School",
      4 ~ "Magnet School",
      5 ~ "Special Education",
      6 ~ "Combination of Special Education and Regular School",
      7 ~ "Other"
    ),
    
    # Family income
    pb_income = case_match(
      pb_income, 
      1 ~ "$0-$19,000",
      2 ~ "$20,000-$39,000",
      3 ~ "$40,000-$59,000",
      4 ~ "$60,000-$79,000",
      5 ~ "$80,000-$99,000",
      6 ~ "$100,000-$119,000",
      7 ~ "$120,000-$140,000",
      8 ~ "$140,000+"
    ),
    
    
    ## Child treatment history
    # Current and lifetime treatment
    pb_childtx_lifetime = pb_childtx_1 == 1,
    pb_childtx_current = pb_childtx_3 == 1,
    p3m_childtx_lifetime = p3m_childtx_1 == 1,
    p3m_childtx_current = p3m_childtx_3 == 1,

    
    ## Child ACES
    # Overall mean score
    pb_child_aces_mean = mean_across("pb", "ace_y", name = "pb_child_aces_mean"), # mean_across() from helper function script
    p3m_child_aces_mean = mean_across("p3m", "ace_y", name = "p3m_child_aces_mean"),
    
    
    ## Parent ACES
    # Overall mean score
    pb_parent_aces_mean = mean_across("pb", "ace_p", name = "pb_parent_aces_mean"),
    p3m_parent_aces_mean = mean_across("p3m", "ace_p", name = "p3m_parent_aces_mean"),
    
    
    ## CDI-2 (Children's Depression Inventory - 2)
    # Overall mean score
    pb_cdi_mean = mean_across("pb", "CDI-2 P", name = "pb_cdi_mean"),
    p3m_cdi_mean = mean_across("p3m", "CDI-2 P", name = "p3m_cdi_mean"),
    
    # Emotional problems subscale
    pb_cdi_emo_mean = mean_across("pb", "CDI-2 P", "Emotional Problems", name = "pb_cdi_emo_mean"),
    p3m_cdi_emo_mean = mean_across("p3m", "CDI-2 P", "Emotional Problems", name = "p3m_cdi_emo_mean"),
    
    # Functional problems subscale
    pb_cdi_fun_mean = mean_across("pb", "CDI-2 P", "Functional Problems", name = "pb_cdi_fun_mean"),
    p3m_cdi_fun_mean = mean_across("p3m", "CDI-2 P", "Functional Problems", name = "p3m_cdi_fun_mean"),
    
    
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
    pb_scared_mean = mean_across("pb", "scared", name = "pb_scared_mean"),
    p3m_scared_mean = mean_across("p3m", "scared", name = "p3m_scared_mean"),
    
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
    
    # Demographics
    pb_childage,
    pb_childsex,
    pb_childgender,
    pb_childethnicity,
    pb_birthorder,
    pb_n_sisters = pb_siblings_1,
    pb_n_brothers = pb_siblings_2,
    pb_grade,
    pb_school,
    pb_income,
    pb_dependent,
    
    # Child treatment history
    matches("childtx_lifetime"),
    matches("pb_childtx_current"),
    
    # Measures
    matches("_child_aces_"),
    matches("_parent_aces_"),
    matches("_cdi_"),
    matches("_bhs_"),
    matches("_bsi_"),
    matches("_bace_"),
    matches("_scared_")
    
  )


## Check that values are in expected range
items_to_check <- p_clean %>%
  select(
    matches("_child_aces_"),
    matches("_parent_aces_"),
    matches("_cdi_"),
    matches("_bhs_"),
    matches("_bsi_"),
    matches("_bace_"),
    matches("_scared_"),
    -ends_with("mean")
  ) %>%
  names()

walk(
  items_to_check,
  ~ check_values(
    .data = p_clean,
    .item = .x
  )
)


####  Save Data and Log  ####
saveRDS(p_clean, clean_data_dir %+% "Phase 1 Parent Qualtrics Data.rds")
saveRDS(log,     clean_data_dir %+% "Phase 1 Parent Qualtrics Log.rds")