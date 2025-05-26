## Track-to-Treat Phase 2 Data Cleaning, Parent Qualtrics, 3-Month Follow-Up
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
p3m_path <- raw_data_dir %+% "DP5+Phase+2+-+Parent+-+FU+1+-+3M_May+6,+2025_09.44_n.csv"
p3m_raw <- read_survey(p3m_path, time_zone = "America/Chicago")


## Load ID lookup
id_lookup <- read_csv(here("Phase 2", "2025.05.26 Track to Treat P2 ID Lookup.csv"))


## Load item-level codebook file
codebook <- load_p2_codebook(here("Phase 2", "2025.05.26 Track to Treat P2 Codebook.xlsx"))


## Load assessment windows
ax_windows <- readRDS(clean_data_staging_intermediate_dir %+% "Phase 2 Assessment Windows.rds")


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
check_raw_data_ver(raw_metadata, list(p3m_path), list(p3m_raw), "p3m_qualtrics")



####  Clean Data  ####
## Create log
# Create lists for logging (a) items used to compute item completion rate below via
# compute_item_completion_rate() and (b) items used to compute means via mean_across()
log <- list(
  item_completion_rate = list(),
  mean_items = list()
)


## Clean columns
p3m_recoded <- p3m_raw %>%
  
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
  
  # Clean remaining columns by row
  rowwise() %>%
  mutate(
    
    ## Metadata
    # ID (manually correcting as necessary)
    lsmh_id = case_when(
      
      # Cases to be manually recoded
      lsmh_id == "LMSH00886" ~ "LSMH00886",
      
      # Cases where both match
      lsmh_id == p3m_lsmh_id ~ lsmh_id,
      
      # Cases where one is missing (keep the non-missing value)
      is.na(lsmh_id) & !is.na(p3m_lsmh_id) ~ p3m_lsmh_id,
      is.na(p3m_lsmh_id) & !is.na(lsmh_id) ~ lsmh_id,
      
      # Cases where both are missing
      is.na(lsmh_id) & is.na(p3m_lsmh_id) ~ NA_character_,
      
      # Additional cases are flagged for cleaning
      T ~ "ID Combination Unaccounted For (lsmh_id '" %+% lsmh_id %+% "', p3m_lsmh_id '" %+% p3m_lsmh_id %+% "')"
      
    ),
    
    # Survey completion
    p3m_complete = Finished == 1,
    
    # Survey datetime and duration
    p3m_datetime = EndDate,
    p3m_date = date(p3m_datetime),
    p3m_duration = EndDate - StartDate,
    
    
    ## Child treatment history
    # Current and lifetime treatment
    p3m_childtx_lifetime = p3m_childtx_1 == 1 | p3m_childtx_3 == 1,
    p3m_childtx_current = p3m_childtx_3 == 1,
    

    ## CDI-2 (Children's Depression Inventory - 2)
    # Overall mean score
    p3m_cdi_mean = mean_across("p3m", "CDI-2 P", name = "p3m_cdi_mean"),
    
    # Emotional problems subscale
    p3m_cdi_emotional_mean = mean_across("p3m", "CDI-2 P", "Emotional Problems", name = "p3m_cdi_emo_mean"),
    
    # Functional problems subscale
    p3m_cdi_functional_mean = mean_across("p3m", "CDI-2 P", "Functional Problems", name = "p3m_cdi_fun_mean"),
    
    
    ## BHS-4 (Beck Hopelessness Scale - 4-item)
    # Overall mean score
    p3m_bhs_mean = mean_across("p3m", "bhs", name = "p3m_bhs_mean"),
    
    
    ## BFAMG (Brief Family Assessment Measure - General Scale)
    # Overall mean score
    p3m_bfamg_mean = mean_across("p3m", "bfamg", name = "p3m_bfamg_mean"),
    
    
    ## 17 items from BSI-18 (Brief Symptom Inventory-18)
    # Overall mean score (without suicidal thoughts item)
    p3m_bsi_mean = mean_across("p3m", "bsi", name = "p3m_bsi_mean"),
    
    # Somatization subscale
    p3m_bsi_s_mean = mean_across("p3m", "bsi", "S", name = "p3m_bsi_s_mean"),
    
    # Depression subscale (without suicidal thoughts item)
    p3m_bsi_d_mean = mean_across("p3m", "bsi", "D", name = "p3m_bsi_d_mean"),
    
    # Anxiety subscale
    p3m_bsi_a_mean = mean_across("p3m", "bsi", "A", name = "p3m_bsi_a_mean"),
    
    
    ## BACE (Barriers to Accessing Care Evaluation)
    # Overall mean score
    p3m_bace_mean = mean_across("p3m", "bace", name = "p3m_bace_mean"),
    
    # Treatment stigma subscale
    p3m_bace_stigma_mean = mean_across("p3m", "bace", "Treatment Stigma", name = "p3m_bace_stigma_mean"),
    
    
    ## SCARED (Screen for Child Anxiety and Related Disorders)
    # Overall mean score
    p3m_scared_mean = mean_across("p3m", "scared", name = "p3m_scared_mean"),
    
    # Panic disorder/significant somatic symptoms subscale
    p3m_scared_paso_mean = mean_across("p3m", "scared", "PA/SO", name = "p3m_scared_paso_mean"),
    
    # Generalized anxiety disorder subscale
    p3m_scared_ga_mean = mean_across("p3m", "scared", "GA", name = "p3m_scared_ga_mean"),
    
    # Separation anxiety disorder subscale
    p3m_scared_sep_mean = mean_across("p3m", "scared", "SEP", name = "p3m_scared_sep_mean"),
    
    # Social phobic disorder subscale
    p3m_scared_soc_mean = mean_across("p3m", "scared", "SOC", name = "p3m_scared_soc_mean"),
    
    # Significant school avoidance symptoms
    p3m_scared_sch_mean = mean_across("p3m", "scared", "SCH", name = "p3m_scared_sch_mean"),
    
  ) %>%
  
  ungroup() %>%
  
  select(
    
    # Metadata
    lsmh_id,
    p3m_complete,
    p3m_date,
    p3m_datetime,
    p3m_duration,

    # Child treatment history
    matches("childtx_lifetime"),
    matches("childtx_current"),
    
    # Measures
    matches("_cdi_"),
    matches("_bhs_"),
    matches("_bsi_"),
    matches("_bace_"),
    matches("_bfamg_"),
    matches("_scared_")
    
  ) %>%
  
  # Compute item completion rate
  compute_item_completion_rate("p3m") # TODO: Fix which columns are used to compute this


## Check that values are in expected range
items_to_check <- p3m_recoded %>%
  select(
    matches("_cdi_"),
    matches("_bhs_"),
    matches("_bsi_"),
    matches("_bace_"),
    matches("_bfamg_"),
    matches("_scared_"),
    -ends_with("mean")
  ) %>%
  names()

walk(
  items_to_check,
  ~ check_values( # Helper function
    .data = p3m_recoded,
    .item = .x
  )
)


## Remove invalid responses
# Known valid LSMH IDs
valid_ids <- id_lookup %>%
  filter(action == "keep") %>%
  distinct(lsmh_id)

invalid_ids <- setdiff(p3m_recoded$lsmh_id, valid_ids$lsmh_id)

p3m_valid <- p3m_recoded %>%
  inner_join(
    valid_ids,
    by = "lsmh_id",
    relationship = "many-to-one"
  )

# Just FYI: This is how many IDs/rows included known LSMH IDs matched for removal
p3m_recoded %>%
  filter(lsmh_id %in% id_lookup$lsmh_id[id_lookup$action == "drop"]) %>%
  count(lsmh_id)

# Just FYI: No rows contained unknown LSMH IDs (good!)
# If these rows indicate typos or other errors in the IDs, fix them in the mutate() above
p3m_recoded %>%
  filter(!lsmh_id %in% id_lookup$lsmh_id) %>%
  count(lsmh_id)


## Filter to assessment window
# Add assessment window information
p3m_with_window <- p3m_valid %>%
  left_join(
    ax_windows,
    by = "lsmh_id",
    relationship = "many-to-one"
  ) %>%
  mutate(
    response_in_window = p3m_date >= ax_window_3m_start & p3m_date <= ax_window_3m_end,
    response_too_early = p3m_date < ax_window_3m_start,
    response_too_late = p3m_date > ax_window_3m_end
  )

# Responses by window
p3m_with_window %>%
  count(response_in_window, response_too_early, response_too_late)

# Participants by window
p3m_with_window %>%
  group_by(lsmh_id) %>%
  summarize(
    any_response_in_window = any(response_in_window),
    any_response_too_early = any(response_too_early),
    any_response_too_late = any(response_too_late)
  ) %>%
  count(any_response_in_window, any_response_too_early, any_response_too_late)

# Filter to in-window responses only
p3m_in_window <- p3m_with_window %>%
  filter(response_in_window)


## Deduplicate
# Identify duplicates
identify_duplicates(p3m_in_window, lsmh_id, p3m_complete)

# Remove duplicates
p3m_deduplicated <- remove_duplicates(p3m_in_window, lsmh_id, p3m_datetime)

# Double-check baseline deduplication
identify_duplicates(p3m_deduplicated, lsmh_id, p3m_complete)



####  Save Data  ####
# Save clean Qualtrics data
saveRDS(p3m_deduplicated, clean_data_staging_dir %+% "Phase 2 Parent Qualtrics Clean Data - 3m.rds")

# Save log
saveRDS(log, clean_data_staging_intermediate_dir %+% "Phase 2 Parent Qualtrics Clean Data Log - 3m.rds")
