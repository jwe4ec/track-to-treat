## Track-to-Treat Phase 2 Data Cleaning, Tracking Log and ID Lookup
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
source(here("Directory Helper Functions.R"))
source(here("Version Control Helper Functions.R"))
source(here("Qualtrics Data Cleaning Helper Functions.R"))


## Load tracking log (completed by RA for parents who inquired about study)
# Get directories using helper function
dirs <- get_p2_dirs(c("raw_tracking_log_data", "clean_data_staging", "clean_data_staging_intermediate"))

# Load raw Phase Sheet of tracking log (storing path)
raw_data_path <- file.path(dirs$raw_tracking_log_data, "2026.04.09 Track to Treat P2 Tracking Log 2.0 - Phase Sheet.csv")
tl <- read.csv(raw_data_path, skip = 1)  # Skip first row describing sheet


## Check raw tracking log version using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
check_raw_data_ver(raw_metadata, list(raw_data_path), list(tl), "tl")



####  Clean Tracking Log (Part 1)  ####
### Fix LSMH and LifePak IDs (manually as necessary)
tl_fixed_ids <- tl %>%
  rowwise() %>%
  mutate(
    lsmh_id = sub("\n$", "", LSMH.ID),
    
    lifepak_id = case_when(
      
      # Cases to be manually recoded (none)

      # All others (helper function for cases in which IDs are same or one/both IDs are missing)
      TRUE ~ resolve_id_pair(as.character(LifePak.ID), as.character(LifePak.ID.1))
      
    ),
    lifepak_id = str_pad(lifepak_id, width = 6, side = "left", pad = "0")
  ) %>%
  ungroup()


# Check LSMH and LifePak ID formats
warn_invalid_id_format(tl_fixed_ids$lsmh_id)
warn_invalid_id_format(tl_fixed_ids$lifepak_id, type = "lifepak_id")


### Retain all cases, except for any test cases (already absent); no duplicates
# - Note: Retain all cases, reflecting everyone who inquired about study, so that
#   all are in ID lookup created below and in participant flowchart created later


### Clean columns
tl_selected <- tl_fixed_ids %>%
  mutate(
  
    # IDs ("lsmh_id" and "lifepak_id" cleaned above)
    
    # Phase (field used by RAs to track participants' current stage in study)
    # - Possible values:
    #   - Recruitment, Not Interested, No Contact, Not Eligible, Eligible,
    #     Baseline, B - Reschedule, LifePak, Intervention, I - Scheduled,
    #     I - Reschedule, 3M, 6M, 12M, 18M, 24M, Study Complete
    # - May not be entirely accurate or consistent across participants
    #   - e.g., Per former RA Arielle Smith on 3/6/2026, it's possible that some 
    #     participants who enrolled were moved to "Not Interested" or "No Contact"
    #     if they withdrew or became unreachable (whereas other participants were
    #     moved to the next study stage, or even "Study Complete", if they withdrew
    #     or became unreachable at a given stage)
    #   - e.g., "Not Eligible" includes (a) participants deemed ineligible before
    #     enrollment (before phone screen, at phone screen, or after phone screen
    #     but before enrollment), (b) participants deemed ineligible after enrollment,
    #     and (c) some (but not all) participants deemed invalid after enrollment
    # - See ID lookup (created below) for LSMH ID and LifePak ID combinations that
    #   should be dropped from study data during data cleaning
    tl_phase = Phase
    
  ) %>%
  select(lsmh_id, lifepak_id, tl_phase)  # LifePak ID is removed in Part 2 below after creating ID lookup



####  Create ID Lookup  ####
# - Whereas the tracking log above reflects all participants who inquired about
#   study (one participant per row) and is used later to create cohort indicators
#   for participant flowchart and data analysis, the ID lookup below includes not
#   only all participants who inquired about study, but also test users. The ID
#   lookup can also have multiple rows per person (if a given LSMH ID corresponds
#   to multiple LifePak IDs). The ID lookup indicates which LSMH ID and LifePak ID
#   combinations to drop when cleaning the study data.

id_lookup <- tl_selected %>%
  
  ### Manually add rows for test IDs
  ## Initialize "test" indicator and "notes" column
  mutate(
    test = FALSE,
    notes = NA_character_
  ) %>%
  
  
  ## Add test LSMH IDs
  # - "LSMH00000" and "LSMH12345": IDs are not in tracking log or LSMH Participant
  #   Database, and their phone screening data are clearly test data
  # - "LSMH00123": Even though ID is in LSMH Participant Database, it isn't in tracking 
  #   log or any study data except phone screen, where it seems to be a test (RA name and
  #   parent/child name are same and don't match those in LSMH Participant Database)
  add_row(
    lsmh_id = c("LSMH00000", "LSMH00123", "LSMH12345"),
    test = TRUE,
    notes = "Apparent test data"
  ) %>%
  
  
  ## Add test LifePak IDs
  add_row(
    lifepak_id = "162922",
    test = TRUE,
    notes = "Test data, per README_ttt_p2_data_collection"
  ) %>%
  
  add_row(
    lifepak_id = c("200466", "428939", "093122"),
    test = TRUE,
    notes = "Test data, per Piloting Feedback TTT P2 LifePak"
  ) %>%
  
  
  ### Create indicators and "action" column and add notes
  mutate(
    
    ## Manually create indicators and add notes
    # Ineligible
    ineligible = tl_phase == "Not Eligible",
    
    notes = case_when(
      # Some (nonexhaustive) reasons manually identified when computing participant flowchart
      ineligible & lsmh_id == "LSMH00878" ~ "Not eligible per tracking log (insufficient age after enrollment)",
      ineligible & lsmh_id == "LSMH02600" ~ "Not eligible per tracking log (insufficient computer after enrollment)",
      
      # Others
      ineligible ~ "Not eligible per tracking log",
      TRUE ~ notes
    ),
    
    # Invalid
    invalid = lsmh_id %in% "LSMH01083" & lifepak_id %in% "324562" |
                lsmh_id %in% "LSMH01811" & lifepak_id %in% "764447" |
                lsmh_id %in% "LSMH01884" & lifepak_id %in% "404350" | 
                lsmh_id %in% "LSMH02405" & lifepak_id %in% "413115",
    
    notes = if_else(invalid, "Invalid (outside USA), per README_ttt_p2_data_collection", notes),
    
    # Opted out and data cannot be used
    opted_out_rm_data = lsmh_id %in% "LSMH00888" & lifepak_id %in% "366106" |
                          lsmh_id %in% "LSMH01614" & lifepak_id %in% "291599",
    
    notes = if_else(opted_out_rm_data, "Opted out, per README_ttt_p2_data_collection", notes),
    
    
    ## Create action column reflecting which IDs to drop vs. keep during data cleaning
    # - For use only with study data after enrollment (for phone screen data, use "test" indicator
    #   instead to exclude only test cases, retaining other cases for participant flowchart)
    action = if_else(test | ineligible | invalid | opted_out_rm_data, "drop", "keep"),
    
    
    ## Add notes not requiring indicators
    # Incorrect LifePak ID in Qualtrics data
    notes = if_else(lsmh_id %in% "LSMH00953" & lifepak_id %in% "268045" |
                      lsmh_id %in% "LSMH00992" & lifepak_id %in% "493955",
                    "LifePak ID in Qualtrics data is incorrect, per README_ttt_p2_data_collection", notes),
    
    # Opted out but data can be used
    notes = if_else(lsmh_id %in% "LSMH00896" & lifepak_id %in% "552344",
                    "Opted out but data can be used, per README_ttt_p2_data_collection", notes),
    
    # Opted out but data can be used, noting source of LifePak ID
    notes = if_else(lsmh_id %in% "LSMH02350" & lifepak_id %in% "007996",
                    "Opted out but data can be used, per README_ttt_p2_data_collection; " %+%
                      "LifePak ID per youth baseline Qualtrics data and Track to Treat P2 Tracking Log 2.0", notes),
    
    
    ## Add notes for cases of two LifePak IDs (additional LifePak ID added manually below)
    # Case 1. Opted out but data can be used, noting use of two LifePak IDs
    notes = if_else(lsmh_id %in% "LSMH01820" & lifepak_id %in% "917619",
                    "Used two LifePak IDs; opted out but data can be used, per README_ttt_p2_data_collection", notes),
  
    # Case 2. Has two LifePak IDs
    notes = if_else(lsmh_id %in% "LSMH01019" & lifepak_id %in% "906962" |
                      lsmh_id %in% "LSMH01155" & lifepak_id %in% "322476" |
                      lsmh_id %in% "LSMH01269" & lifepak_id %in% "707268" |
                      lsmh_id %in% "LSMH01786" & lifepak_id %in% "867499" |
                      lsmh_id %in% "LSMH01841" & lifepak_id %in% "131166" |
                      lsmh_id %in% "LSMH01914" & lifepak_id %in% "063040" |
                      lsmh_id %in% "LSMH01989" & lifepak_id %in% "576558" |
                      lsmh_id %in% "LSMH02181" & lifepak_id %in% "961421",
                    "Used two LifePak IDs, per README_ttt_p2_data_collection", notes),
    
    # Case 3. Has two LifePak IDs from redownloading app twice
    notes = if_else(lsmh_id %in% "LSMH02422" & lifepak_id %in% "095929",
                    "Used two LifePak IDs (from redownloading app twice), per README_ttt_p2_data_collection", notes),
    
  ) %>%
  
  
  ### Manually add rows for additional LifePak IDs (for cases above)
  # Case 1
  add_row(
    lsmh_id = "LSMH01820",
    lifepak_id = "217510",
    action = "keep", test = FALSE, invalid = FALSE, opted_out_rm_data = FALSE,
    notes = "Used two LifePak IDs; opted out but data can be used, per README_ttt_p2_data_collection"
  ) %>%
  
  # Case 2
  bind_rows(
    tribble(
      ~lsmh_id,    ~lifepak_id,
      "LSMH01019", "823958",
      "LSMH01155", "334418",
      "LSMH01269", "122772",
      "LSMH01786", "326117",
      "LSMH01841", "303379",
      "LSMH01914", "500856",
      "LSMH01989", "995977",
      "LSMH02181", "898891"
    ) %>%
    mutate(action = "keep", test = FALSE, invalid = FALSE, opted_out_rm_data = FALSE,
           notes  = "Used two LifePak IDs, per README_ttt_p2_data_collection")
  ) %>%
  
  # Case 3
  bind_rows(
    tribble(
      ~lsmh_id,    ~lifepak_id, ~action, ~notes,
      "LSMH02422", "632541",    "keep",  "Used two LifePak IDs (from redownloading app twice), " %+%
                                           "per README_ttt_p2_data_collection",
      "LSMH02422", "850326",    "drop",  "Incorrect LifePak ID before redownloading app twice, " %+%
                                           "per youth baseline Qualtrics data and README_ttt_p2_data_collection"
    ) %>%
    mutate(test = FALSE, invalid = FALSE, opted_out_rm_data = FALSE)
  ) %>%
  

  ### Sort and select columns
  arrange(lsmh_id, lifepak_id) %>%
  select(
    # IDs
    lsmh_id, lifepak_id,
    
    # Action used to filter study data after enrollment
    action,
    
    # Test indicator used to filter phone screen data
    test,
    
    # Two indicators used to create cohort indicators for participant flow and analysis
    invalid, opted_out_rm_data,
    
    # Notes
    notes
  )



####  Clean Tracking Log (Part 2)  ####
# Remove LifePak ID
tl_clean <- tl_selected %>% select(-lifepak_id)



####  Save Data  ####
# Save clean tracking log
saveRDS(tl_clean, file.path(dirs$clean_data_staging_intermediate, "Phase 2 Clean Tracking Log.rds"))

# Save ID lookup
saveRDS(id_lookup, file.path(dirs$clean_data_staging_intermediate, "Phase 2 ID Lookup.rds"))
