## Track-to-Treat Phase 2 Data Cleaning, Phone Screen
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


## Load Qualtrics phone screen data (completed by RA with parent on phone)
# Get directories using helper function
dirs <- get_p2_dirs(c("raw_qualtrics_data", "clean_data_staging_intermediate"))

# Load raw Qualtrics dataset (storing path) in this format: [respondent][wave]_raw
# - Note: Use "timeZone" specified for date columns (e.g., "StartDate") in third row of raw CSV
ps_raw_data_path <- file.path(dirs$raw_qualtrics_data, "DP5+Phase+2+-+Screener_February+26,+2026_14.30_n.csv")
ps_raw <- read_survey(ps_raw_data_path, time_zone = "America/Chicago")


## Load ID lookup
id_lookup <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 ID Lookup.rds"))


## Check raw Qualtrics data version using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
check_raw_data_ver(raw_metadata, list(ps_raw_data_path), list(ps_raw), "ps_qualtrics")



####  Clean Phone Screen Data  ####
### Fix LSMH IDs (manually as necessary)
ps_fixed_ids <- ps_raw %>%
  rowwise() %>%
  mutate(
    lsmh_id = case_when(
      
      ## Cases to be manually recoded
      # Incorrect second entry of LSMH ID
      ttt_lsmh_id == "LSMH01172" & ttt_lsmh_id_validate == "LSMH01178" ~ "LSMH01172",
      ttt_lsmh_id == "LSMH01491" & ttt_lsmh_id_validate == "LSMH01490" ~ "LSMH01491",
      ttt_lsmh_id == "LSMH01021" & ttt_lsmh_id_validate == "LSMH01428" ~ "LSMH01021",
      ttt_lsmh_id == "LSMH01709" & ttt_lsmh_id_validate == "LSMH01707" ~ "LSMH01709",
      ttt_lsmh_id == "LSMH02235" & ttt_lsmh_id_validate == "LSMH02236" ~ "LSMH02235",
      
      # Extra digits
      ttt_lsmh_id == "LSMH001012" ~ "LSMH01012",
      ttt_lsmh_id == "LSMH001008" ~ "LSMH01008",
      ttt_lsmh_id == "LSMH1212312312" ~ NA_character_,
      
      # Incorrect LSMH IDs
      ttt_lsmh_id == "LSMH02440" & ResponseId == "R_2sSWmES8uSqBkBo" ~ "LSMH02442",
      ttt_lsmh_id == "LSMH02467" & ResponseId == "R_YPwYS9ldg0rjPWh" ~ "LSMH02466",
      ttt_lsmh_id == "LSMH02116" & ResponseId == "R_3kB5u1jRTCqqmPR" ~ "LSMH02150",

      ## All others (helper function for cases in which IDs are same or one/both IDs are missing)
      TRUE ~ resolve_id_pair(ttt_lsmh_id, ttt_lsmh_id_validate)
      
    )
  ) %>%
  ungroup()

# Check LSMH ID format
warn_invalid_id_format(ps_fixed_ids$lsmh_id)


### Remove invalid responses
ps_valid_ids <- ps_fixed_ids %>%
  filter(
    # No LSMH ID (unclear why)
    !is.na(lsmh_id),
    
    # Test LSMH IDs per ID lookup
    # - Exclude only test IDs; do not exclude other cases (e.g., per "id_lookup$action",
    #   which is designed for use only with study data after enrollment), so that other
    #   cases can be reflected in participant flowchart
    !(lsmh_id %in% with(id_lookup, setdiff(lsmh_id[test], NA))),
    
    # Apparent test response in phone screen data (where an RA's name is used for parent
    # name, and parent/child names do not match those in LSMH Participant Database), but 
    # not in general (ID is in tracking log and LSMH Participant Database). Thus, exclude
    # this ID from phone screening data but do not mark it as test data in ID lookup.
    !(lsmh_id == "LSMH00530"),
    
    # LSMH ID duplicated for potential sibling (same parent name) who didn't enroll
    !(lsmh_id == "LSMH02077" & ResponseId == "R_3pauh6ZYcIYuq6E"),
    !(lsmh_id == "LSMH02153" & ResponseId == "R_ue02k9kiXJhqEXT"),
    !(lsmh_id == "LSMH02322" & ResponseId == "R_3ixnNhZy2l0kUZ7"),
    
    # LSMH ID duplicated for unknown youth (different parent name) who didn't enroll
    !(lsmh_id == "LSMH01234" & ResponseId == "R_3PNuGD3qipUm9FP"),
    !(lsmh_id == "LSMH01836" & ResponseId == "R_1FErSprBi8epLiI"),
    !(lsmh_id == "LSMH02554" & ResponseId == "R_tMuk4za9vzyI2o9")
  )


### Identify and remove duplicates
# Identify duplicates using helper function
identify_duplicates(ps_valid_ids, lsmh_id, phase = 2)

# Manually remove second screen for "LSMH01083" (first was used for enrollment)
ps_filtered <- ps_valid_ids %>%
  filter(!(lsmh_id == "LSMH01083" & StartDate == "2021-06-21 10:58:13"))

# Remove duplicates (keep most recent screening, retaining any ties)
ps_deduplicated <- ps_filtered %>%
  group_by(lsmh_id) %>%
  slice_max(order_by = EndDate, n = 1, with_ties = TRUE) %>%
  ungroup()

# Double-check deduplication (handle any ties if present)
identify_duplicates(ps_deduplicated, lsmh_id, phase = 2)


### Clean columns
ps_recoded <- ps_deduplicated %>%
  
  # Clean columns and create eligibility indicators
  mutate(
    
    ## Metadata
    # ID ("lsmh_id" cleaned above)
    
    # Survey completion
    ps_complete = !is.na(EndDate),
    
    # Survey datetime and duration
    ps_datetime = EndDate,
    ps_date = date(ps_datetime),
    ps_duration = EndDate - StartDate,
    
    
    ## Ineligible indicators (by reason)
    # Youth not 11-16 years of age
    # - Don't use Option 7 ("None of the above"); it was "Excluded From Analysis" in Qualtrics, yielding NA
    ps_inelig_age = !(ttt_child_age %in% 1:6),
    
    # Youth has insufficient smartphone access
    ps_inelig_phone = ttt_phone_access == 2,
    
    # Youth has insufficient computer access
    ps_inelig_computer = ttt_remote_computer == 2,
    
    # Youth has insufficient software access
    ps_inelig_software = ttt_remote_chromzoom == 3,
    
    # Youth has intellectual disability (asked only if not performing at grade level)
    ps_inelig_intell_disab = ifelse(is.na(ttt_atgradelevel), NA,
                                    ttt_atgradelevel == 2 & ttt_intdis == 1),
    
    # Youth in immediate danger of acting on SI (asked only if currently has SI)
    ps_inelig_imminent_risk = ifelse(is.na(ttt_currentideation), NA,
                                     ttt_currentideation == 5 & ttt_imminentrisk == 1),
    
    # Youth has psychosis
    ps_inelig_psychosis = ttt_psychosis == 1,
    
    # Youth has insufficient depression symptoms: (a) lacks IEP/504 plan that includes
    #   depression accommodations, (b) no depression treatment-seeking in past 2 years,
    #   and (c) CDI-P total T score is < 60 (<80th percentile) for youth age/gender
    ps_inelig_depression = ttt_iep504 != 2 & ttt_txseeking == 2 & ttt_rcadsmeet == 2,
    
    
    ## Ineligible indicator (any reason)
    ps_inelig = rowSums(across(starts_with("ps_inelig_")), na.rm = TRUE) == 1,
    
    
    ## Eligible indicator
    ps_elig = !ps_inelig
    
  ) %>%
  
  # Select variables
  select(
    
    # Metadata
    lsmh_id,
    ps_complete,
    ps_date,
    ps_datetime,
    ps_duration,
    
    # Eligibility indicators
    ps_inelig_age, ps_inelig_phone, ps_inelig_computer, ps_inelig_software, ps_inelig_intell_disab,
    ps_inelig_imminent_risk, ps_inelig_psychosis, ps_inelig_depression,
    
    ps_inelig, ps_elig
    
  )



####  Save Data  ####
# Save clean Qualtrics data
saveRDS(ps_recoded, file.path(dirs$clean_data_staging_intermediate, "Phase 2 Clean Phone Screen Data.rds"))
