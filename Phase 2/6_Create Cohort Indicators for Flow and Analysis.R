## Track-to-Treat Phase 2 Data Cleaning, Cohort Indicators for Flow and Analysis
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
dirs <- get_p2_qualtrics_dirs(c("clean_data_staging", "clean_data_staging_intermediate"))

# Load cleaned Qualtrics phone screening data
ps_clean <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Clean Phone Screen Data.rds")

# Load cleaned and merged Qualtrics data at other waves
y_merged <- readRDS(dirs$clean_data_staging %+% "Phase 2 Youth Qualtrics Clean Data - All Waves.rds")
p_merged <- readRDS(dirs$clean_data_staging %+% "Phase 2 Parent Qualtrics Clean Data - All Waves.rds")


## Load LSMH IDs meeting free-text exclusion criteria identified in "Youth Intervention.R" Qualtrics script
exclude_ids <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 LSMH IDs Meeting Free-Text Exclusion Criteria.rds")


## Load clean LifePak data without free-response items (until these are deidentified)
lifepak_clean <- readRDS(dirs$clean_data_staging %+% "Phase 2 LifePak Clean Data Without Free Responses.rds")


## Load clean tracking log
tl_clean <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Clean Tracking Log.rds")


## Load ID lookup
id_lookup <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 ID Lookup.rds")



####  Compare LSMH IDs Between Phone Screen and Other Clean Data  ####
# Capture LSMH IDs ineligible vs. eligible per phone screen
ps_inelig_ids <- with(ps_clean, lsmh_id[ps_inelig])
ps_elig_ids   <- with(ps_clean, lsmh_id[ps_elig])

# No LSMH IDs kept in LifePak data or other Qualtrics waves were deemed ineligible at phone screen
stopifnot(
  sum(lifepak_clean$lsmh_id %in% ps_inelig_ids) == 0,
  sum(y_merged$lsmh_id      %in% ps_inelig_ids) == 0,
  sum(p_merged$lsmh_id      %in% ps_inelig_ids) == 0
)

# But "LSMH02060" in other clean data lacks phone screen (unclear why)
# - Capture this LSMH ID for use in computing flow below
lsmh_id_enrolled_wout_ps <- "LSMH02060"

stopifnot(
  setdiff(lifepak_clean$lsmh_id, ps_elig_ids) == lsmh_id_enrolled_wout_ps,
  setdiff(y_merged$lsmh_id,      ps_elig_ids) == lsmh_id_enrolled_wout_ps,
  setdiff(p_merged$lsmh_id,      ps_elig_ids) == lsmh_id_enrolled_wout_ps
)



####  Compute Participant Flow  ####
# - Note: Potential participants inquired about the study by submitting a contact form 
#   (which collected parent contact info and youth age). Upon receipt of a contact form,
#   RAs created an LSMH ID in the tracking log and then tried to contact the parent. If 
#   the youth age was eligible and the parent could be reached and was interested in the
#   study, the RA conducted a phone screening with the parent and entered the parent's
#   responses into the Qualtrics phone screening survey.

## Compute flow (starting from tracking log of all LSMH IDs who inquired about study)
flow_inquired <- tl_clean %>%
  # Add condition (from merged youth Qualtrics data)
  left_join(y_merged[c("lsmh_id", "condition")], by = "lsmh_id", relationship = "one-to-one") %>%
  
  # Compute indicator columns (drawing from phone screening data, LSMH ID captured above,
  #   ID lookup, LifePak data, and merged youth Qualtrics data)
  mutate(
    # Inquired
    inquired = TRUE,
    
    # Screened (including methods)
    screened_by_phone             = lsmh_id %in% ps_clean$lsmh_id,
    screened_by_contact_form_only = !screened_by_phone & tl_phase == "Not Eligible",
    screened_by_unknown_method    = lsmh_id == lsmh_id_enrolled_wout_ps,  # Enrolled but lacks phone screen
    screened                      = screened_by_phone | screened_by_contact_form_only | screened_by_unknown_method,
    
    # Not screened (by reason)
    not_screened_unreachable  = !screened & tl_phase == "No Contact",
    not_screened_uninterested = !screened & tl_phase == "Not Interested",
    
    # Eligibility helper columns
    eligible_per_phone_screen   = lsmh_id %in% with(ps_clean, lsmh_id[ps_elig]),
    eligible_per_unknown_method = lsmh_id == lsmh_id_enrolled_wout_ps,  # Enrolled but lacks phone screen
    
    inelig_before_phone_screen_age = screened_by_contact_form_only,
    
    inelig_after_phone_screen = eligible_per_phone_screen & (
      tl_phase == "Not Eligible" | 
        lsmh_id %in% with(id_lookup, setdiff(lsmh_id,
                                             c(lsmh_id[action == "keep"], NA)))
    ),
    inelig_after_phone_screen_age           = lsmh_id == "LSMH00878",  # Per "Track to Treat P2 Tracking Log 2.0"
    inelig_after_phone_screen_computer      = lsmh_id == "LSMH02600",  # Per "Track to Treat P2 Tracking Log 2.0"
    inelig_after_phone_screen_outside_usa   = lsmh_id %in% with(id_lookup, setdiff(lsmh_id[invalid], NA)),
    inelig_after_phone_screen_withdrew_data = lsmh_id %in% with(id_lookup, setdiff(lsmh_id[opted_out_rm_data], NA)),
    inelig_after_phone_screen_unknown       = inelig_after_phone_screen &
      !inelig_after_phone_screen_age & !inelig_after_phone_screen_computer & 
      !inelig_after_phone_screen_outside_usa & !inelig_after_phone_screen_withdrew_data,
    
    # Eligible
    eligible = (eligible_per_phone_screen & !inelig_after_phone_screen) | eligible_per_unknown_method,

    # Ineligible (by reason)
    inelig_age           = inelig_before_phone_screen_age | 
                             lsmh_id %in% with(ps_clean, lsmh_id[ps_inelig_age]) |
                             inelig_after_phone_screen_age,
    
    inelig_phone         = lsmh_id %in% with(ps_clean, lsmh_id[ps_inelig_phone]),
    inelig_computer      = lsmh_id %in% with(ps_clean, lsmh_id[ps_inelig_computer]) |
                             inelig_after_phone_screen_computer,
    inelig_software      = lsmh_id %in% with(ps_clean, lsmh_id[ps_inelig_software]),
    inelig_lack_tech     = inelig_phone | inelig_computer | inelig_software,
    
    inelig_intell_disab  = lsmh_id %in% with(ps_clean, lsmh_id[ps_inelig_intell_disab]),
    inelig_imminent_risk = lsmh_id %in% with(ps_clean, lsmh_id[ps_inelig_imminent_risk]),
    inelig_psychosis     = lsmh_id %in% with(ps_clean, lsmh_id[ps_inelig_psychosis]),
    inelig_depression    = lsmh_id %in% with(ps_clean, lsmh_id[ps_inelig_depression]),
    inelig_outside_usa   = inelig_after_phone_screen_outside_usa,
    inelig_withdrew_data = inelig_after_phone_screen_withdrew_data,
    inelig_unknown       = inelig_after_phone_screen_unknown,

    # Enrolled
    enrolled = lsmh_id %in% y_merged$lsmh_id,
    
    # Not enrolled (by reason)
    not_enrolled_unreachable  = eligible & !enrolled & tl_phase == "No Contact",
    not_enrolled_uninterested = eligible & !enrolled & tl_phase == "Not Interested",
    
    # Youth with baseline, EMA, and intervention surveys
    yb_present  = lsmh_id %in% with(y_merged, lsmh_id[yb_complete]),
    ema_present = lsmh_id %in% lifepak_clean$lsmh_id,
    yi_present  = lsmh_id %in% with(y_merged, lsmh_id[yi_complete]),
    
    # Youth randomized and who finished SSI
    randomized   = !is.na(condition),
    finished_ssi = lsmh_id %in% with(y_merged, lsmh_id[yi_ssi_complete]),

    # Youth with follow-up surveys
    y3m_present  = lsmh_id %in% with(y_merged, lsmh_id[y3m_complete]),
    y6m_present  = lsmh_id %in% with(y_merged, lsmh_id[y6m_complete]),
    y12m_present = lsmh_id %in% with(y_merged, lsmh_id[y12m_complete]),
    y18m_present = lsmh_id %in% with(y_merged, lsmh_id[y18m_complete]),
    y24m_present = lsmh_id %in% with(y_merged, lsmh_id[y24m_complete]),
    
    # Excluded from ITT sample (but kept in flow above) per free-text exclusion criteria
    exclude_per_free_text = lsmh_id %in% exclude_ids$lsmh_id,
    
    # To analyze in ITT sample
    analyze_itt_sample = randomized & !exclude_per_free_text
  ) %>%
  select(
    lsmh_id, inquired,
    not_screened_unreachable, not_screened_uninterested,
    screened, screened_by_contact_form_only, screened_by_phone, screened_by_unknown_method,
    inelig_age, inelig_lack_tech, inelig_phone, inelig_computer, inelig_software,
    inelig_intell_disab, inelig_imminent_risk, inelig_psychosis, inelig_depression, 
    inelig_outside_usa, inelig_withdrew_data, inelig_unknown, eligible,
    not_enrolled_unreachable, not_enrolled_uninterested, enrolled,
    yb_present, ema_present, yi_present, randomized, condition, finished_ssi,
    y3m_present, y6m_present, y12m_present, y18m_present, y24m_present,
    exclude_per_free_text, analyze_itt_sample
  )



####  Check Flow Logic and Document Sample Sizes  ####
with(flow_inquired, stopifnot(
  # Inquired
  sum(inquired) == 2204,
  sum(inquired) == sum(screened, inquired & !screened),
  
    # Inquired but not screened (with reasons)
    sum(inquired & !screened) == 1233,
    sum(inquired & !screened) == sum(not_screened_unreachable, not_screened_uninterested),
    sum(not_screened_unreachable) == 757, sum(not_screened_uninterested) == 476,

  # Screened
  sum(screened) == 971,
  sum(screened) == sum(screened_by_phone, screened_by_contact_form_only, screened_by_unknown_method),
  sum(screened) == sum(eligible, screened & !eligible),
  
    # Screened but ineligible (with reasons)
    sum(screened & !eligible) == 354,
    sum(screened & !eligible) == sum(inelig_age, inelig_lack_tech, inelig_intell_disab, inelig_imminent_risk, 
                                     inelig_psychosis, inelig_depression, inelig_outside_usa,
                                     inelig_withdrew_data, inelig_unknown),
    sum(inelig_age) == 106, sum(inelig_lack_tech) == 60, sum(inelig_intell_disab) == 14, sum(inelig_imminent_risk) == 5, 
    sum(inelig_psychosis) == 46, sum(inelig_depression) == 111, sum(inelig_outside_usa) == 4,
    sum(inelig_withdrew_data) == 2, sum(inelig_unknown) == 6,
  
      # Breakdown of lack of technology
      sum(inelig_lack_tech) == sum(inelig_phone, inelig_computer, inelig_software),
      sum(inelig_phone) == 40, sum(inelig_computer) == 20, sum(inelig_software) == 0,
  
  # Eligible
  sum(eligible) == 617,
  sum(eligible) == sum(enrolled, eligible & !enrolled),
  
    # Eligible but not enrolled (with reasons)
    sum(eligible & !enrolled) == 363,
    sum(eligible & !enrolled) == sum(not_enrolled_unreachable, not_enrolled_uninterested),
    sum(not_enrolled_unreachable) == 207, sum(not_enrolled_uninterested) == 156,
  
  # Enrolled
  sum(enrolled) == 254,
  
  # Has youth baseline, EMA, and intervention surveys
  sum(yb_present)   == 254,
  sum(ema_present)  == 254,
  sum(yi_present)   == 219,
  
  # Randomized to and finished SSI, by condition
  sum(randomized)   == 219, table(randomized, condition)["TRUE", ]   == c(BA = 70, GMI = 75, ST = 74),
  sum(finished_ssi) == 216, table(finished_ssi, condition)["TRUE", ] == c(BA = 68, GMI = 74, ST = 74),
  
  # Has youth follow-up surveys, by condition
  sum(y3m_present)  == 177, table(y3m_present, condition)["TRUE", ]  == c(BA = 57, GMI = 60, ST = 60),
  sum(y6m_present)  == 153, table(y6m_present, condition)["TRUE", ]  == c(BA = 47, GMI = 54, ST = 52),
  sum(y12m_present) == 129, table(y12m_present, condition)["TRUE", ] == c(BA = 39, GMI = 46, ST = 44),
  sum(y18m_present) == 117, table(y18m_present, condition)["TRUE", ] == c(BA = 36, GMI = 43, ST = 38),
  sum(y24m_present) == 114, table(y24m_present, condition)["TRUE", ] == c(BA = 35, GMI = 42, ST = 37),
  
  # To analyze in ITT sample, by condition
  sum(analyze_itt_sample) == 215, table(analyze_itt_sample, condition)["TRUE", ] == c(BA = 68, GMI = 73, ST = 74),
  sum(analyze_itt_sample) == sum(randomized) - sum(exclude_per_free_text),
  
    # Excluded from ITT sample (but kept in flow above) per free-text exclusion criteria, by condition
    sum(exclude_per_free_text) == 4, table(exclude_per_free_text, condition)["TRUE", ] == c(BA = 2, GMI = 2, ST = 0)
))



####  Save Data  ####
# Save cohort indicators and condition for participant flow and data analysis
saveRDS(flow_inquired, dirs$clean_data_staging %+% "Phase 2 Cohort Indicators for Flow and Analysis.rds")
