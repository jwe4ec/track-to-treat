## Track-to-Treat Phase 1 Data Cleaning
## Qualtrics data (parents)
# R version 4.1.2

####  Startup  ####
## Load packages
library(tidyverse) # 2.0.0
library(qualtRics) # 3.2.0
library(here) # 1.0.1
library(openxlsx) # 4.2.5.2
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

# Item-level codebook file
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
    # Crate `reverse_base`: the number a response should be subtracted from to reverse it
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
#   click information, and COVID-related variables)
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


### Deduplicate
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
# - COVID-19-related variables
# - Parent attitudes towards therapy
# - Child birth order (requires manual coding)
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
    
    
    ## Demographics
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
    pb_child_aces_mean = mean_across("pb", "ace_y"),
    p3m_child_aces_mean = mean_across("p3m", "ace_y"),
    
    
    ## Parent ACES
    # Overall mean score
    pb_parent_aces_mean = mean_across("pb", "ace_p"),
    p3m_parent_aces_mean = mean_across("p3m", "ace_p"),
    
    
    ## CDI-2 (Children's Depression Inventory - 2)
    # Overall mean score
    pb_cdi_mean = mean_across("pb", "CDI-2 P"),
    p3m_cdi_mean = mean_across("p3m", "CDI-2 P"),
    
    # Emotional problems subscale
    pb_cdi_emo_mean = mean_across("pb", "CDI-2 P", "Emotional Problems"),
    p3m_cdi_emo_mean = mean_across("p3m", "CDI-2 P", "Emotional Problems"),
    
    # Functional problems subscale
    pb_cdi_fun_mean = mean_across("pb", "CDI-2 P", "Functional Problems"),
    p3m_cdi_fun_mean = mean_across("p3m", "CDI-2 P", "Functional Problems"),
    
    
    ## BHS-4 (Beck Hopelessness Scale - 4-item)
    # Overall mean score
    pb_bhs_mean = mean_across("pb", "bhs"),
    p3m_bhs_mean = mean_across("p3m", "bhs"),
    
    
    ## BFAMG (Brief Family Assessment Measure - General Scale)
    # Overall mean score
    pb_bfamg_mean = mean_across("pb", "bfamg"),
    p3m_bfamg_mean = mean_across("p3m", "bfamg"),
    
    
    ## BSI (Brief Symptom Inventory)
    # Overall mean score
    pb_bsi_mean = mean_across("pb", "bsi"),
    p3m_bsi_mean = mean_across("p3m", "bsi"),
    
    # Somatization subscale
    pb_bsi_s_mean = mean_across("pb", "bsi", "S"),
    p3m_bsi_s_mean = mean_across("p3m", "bsi", "S"),
    
    # Depression subscale
    pb_bsi_d_mean = mean_across("pb", "bsi", "D"),
    p3m_bsi_d_mean = mean_across("p3m", "bsi", "D"),
    
    # Anxiety subscale
    pb_bsi_a_mean = mean_across("pb", "bsi", "A"),
    p3m_bsi_a_mean = mean_across("p3m", "bsi", "A"),
    
    
    ## BACE (Barriers to Accessing Care Evaluation)
    # Overall mean score
    pb_bace_mean = mean_across("pb", "bace"),
    p3m_bace_mean = mean_across("p3m", "bace"),
    
    # Treatment stigma subscale
    pb_bace_stigma_mean = mean_across("pb", "bace", "Treatment Stigma"),
    p3m_bace_stigma_mean = mean_across("p3m", "bace", "Treatment Stigma"),
    
    
    ## SCARED (Screen for Child Anxiety and Related Disorders)
    # Overall mean score
    pb_scared_mean = mean_across("pb", "scared"),
    p3m_scared_mean = mean_across("p3m", "scared"),
    
    # Panic disorder/significant somatic symptoms subscale
    pb_scared_paso_mean = mean_across("pb", "scared", "PA/SO"),
    p3m_scared_paso_mean = mean_across("p3m", "scared", "PA/SO"),
    
    # Generalized anxiety disorder subscale
    pb_scared_ga_mean = mean_across("pb", "scared", "GA"),
    p3m_scared_ga_mean = mean_across("p3m", "scared", "GA"),
    
    # Separation anxiety disorder subscale
    pb_scared_sep_mean = mean_across("pb", "scared", "SEP"),
    p3m_scared_sep_mean = mean_across("p3m", "scared", "SEP"),
    
    # Social phobic disorder subscale
    pb_scared_soc_mean = mean_across("pb", "scared", "SOC"),
    p3m_scared_soc_mean = mean_across("p3m", "scared", "SOC"),
    
    # Significant school avoidance symptoms
    pb_scared_sch_mean = mean_across("pb", "scared", "SCH"),
    p3m_scared_sch_mean = mean_across("p3m", "scared", "SCH"),
    
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



####  Save Data  ####
saveRDS(p_clean, clean_data_dir %+% "Phase 1 Parent Qualtrics Data.rds")
