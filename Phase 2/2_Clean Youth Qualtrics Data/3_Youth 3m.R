## Track-to-Treat Phase 2 Data Cleaning, Youth Qualtrics, 3-Month Follow-Up
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
y3m_path <- raw_data_dir %+% "DP5+Phase+2+-+Youth+-+FU+1+-+3M_May+6,+2025_09.45_n.csv"
y3m_raw <- read_survey(y3m_path, time_zone = "America/Chicago")


## Load ID lookup
id_lookup <- read_csv(here("Phase 2", "2025.05.26 Track to Treat P2 ID Lookup.csv"))


## Load item-level codebook file
codebook <- load_p2_codebook(here("Phase 2", "2025.05.28 Track to Treat P2 Codebook.xlsx"))


## Load assessment windows
ax_windows <- readRDS(clean_data_staging_intermediate_dir %+% "Phase 2 Assessment Windows.rds")


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
check_raw_data_ver(raw_metadata, list(y3m_path), list(y3m_raw), "y3m_qualtrics")



####  Clean Data  ####
## Create log
# Create lists for logging (a) items used to compute item completion rate below via
# compute_item_completion_rate() and (b) items used to compute means via mean_across()
log <- list(
  item_completion_rate = list(),
  mean_items = list()
)


## Clean columns
y3m_recoded <- y3m_raw %>%
  
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
      lsmh_id == "LMSH00886" & is.na(y3m_lsmh_id) ~ "LSMH00886",
      lsmh_id == "LMSH00886" & y3m_lsmh_id == "LSMH00886" ~ "LSMH00886",
      lsmh_id == "LSMH" & y3m_lsmh_id == "LSMH02265" ~ "LSMH02265",
      lsmh_id == "LSMH01829" & y3m_lsmh_id == "LSMH08129" ~ "LSMH01829",
      lsmh_id == "LSMH02416" & y3m_lsmh_id == "LSMH ID, LSMH02416" ~ "LSMH02416",

      # Cases where both match
      lsmh_id == y3m_lsmh_id ~ lsmh_id,
      
      # Cases where one is missing (keep the non-missing value)
      is.na(lsmh_id) & !is.na(y3m_lsmh_id) ~ y3m_lsmh_id,
      is.na(y3m_lsmh_id) & !is.na(lsmh_id) ~ lsmh_id,
      
      # Cases where both are missing
      is.na(lsmh_id) & is.na(y3m_lsmh_id) ~ NA_character_,
      
      # Additional cases are flagged for cleaning
      T ~ "ID Combination Unaccounted For (lsmh_id '" %+% lsmh_id %+% "', y3m_lsmh_id '" %+% y3m_lsmh_id %+% "')"
      
    ),
    
    # Survey completion
    y3m_complete = Finished == 1,
    
    # Survey datetime and duration
    y3m_datetime = EndDate,
    y3m_date = date(y3m_datetime),
    y3m_duration = EndDate - StartDate,
    
    
    ## CDI-2 (Children's Depression Inventory - 2)
    # Overall mean score
    y3m_cdi_mean = mean_across("y3m", "CDI-2 SR", name = "y3m_cdi_mean"), # mean_across() from helper function script
    
    # Negative mood/physical symptoms subscale
    y3m_cdi_nmps_mean = mean_across("y3m", "CDI-2 SR", "Negative Mood/Physical Symptoms", name = "y3m_cdi_nmps_mean"),
    
    # Negative self-esteem subscale
    y3m_cdi_nse_mean = mean_across("y3m", "CDI-2 SR", "Negative Self-Esteem", name = "y3m_cdi_nse_mean"),
    
    # Ineffectiveness subscale
    y3m_cdi_inef_mean = mean_across("y3m", "CDI-2 SR", "Ineffectiveness", name = "y3m_cdi_inef_mean"),
    
    # Interpersonal problems subscale
    y3m_cdi_inter_mean = mean_across("y3m", "CDI-2 SR", "Interpersonal Problems", name = "y3m_cdi_inter_mean"),
    
    # Emotional problems subscale
    y3m_cdi_emotional_mean = ((y3m_cdi_nmps_mean * 9) + (y3m_cdi_nse_mean * 6)) / 15,
    
    # Functional problems subscale
    y3m_cdi_functional_mean = ((y3m_cdi_inef_mean * 8) + (y3m_cdi_inter_mean * 5)) / 13,
    
    
    ## BHS-4 (Beck Hopelessness Scale - 4-item)
    # Overall mean score
    y3m_bhs_mean = mean_across("y3m", "bhs", name = "y3m_bhs_mean"),
    
    
    ## PCSC (Primary Control Scale for Children)
    # Overall mean score
    y3m_pcsc_mean = mean_across("y3m", "pcsc", name = "y3m_pcsc_mean"),
    
    # Academic subscale
    y3m_pcsc_academic_mean = mean_across("y3m", "pcsc", "Academic", name = "y3m_pcsc_academic_mean"),
    
    # Social subscale
    y3m_pcsc_social_mean = mean_across("y3m", "pcsc", "Social", name = "y3m_pcsc_social_mean"),
    
    # Behavioral subscale
    y3m_pcsc_behavioral_mean = mean_across("y3m", "pcsc", "Behavioral", name = "y3m_pcsc_behavioral_mean"),
    
    
    ## SCSC (Secondary Control Scale for Children)
    # Overall mean score
    y3m_scsc_mean = mean_across("y3m", "scsc", name = "y3m_scsc_mean"),
    
    
    ## BADS (Behavioral Activation for Depression Scale)
    # Activation subscale
    y3m_bads_ac_mean = mean_across("y3m", "bads", "AC", name = "y3m_bads_ac_mean"),
    
    # Avoidance/rumination subscale    
    y3m_bads_ar_mean = mean_across("y3m", "bads", "AR", name = "y3m_bads_ar_mean"),
    
    # Work/school impairment subscale
    y3m_bads_ws_mean = mean_across("y3m", "bads", "WS", name = "y3m_bads_ws_mean"),
    
    # Social impairment subscale
    y3m_bads_si_mean = mean_across("y3m", "bads", "SI", name = "y3m_bads_si_mean"),
    
    # Overall score can also be computed (for instructions, see https://doi.org/b23r6w )
    
    
    ## SHS (Self-Hate Scale)
    # Overall mean score
    y3m_self_hate_mean = mean_across("y3m", "self_hate_scale", name = "y3m_self_hate_mean"),
    
    
    ## IDAS-II (Inventory of Depression and Anxiety Symptoms - II)
    # Given that scoring likely depends on intended use, we output items but do 
    # not score them (see Table 1 of https://doi.org/f4b85p for scale info)
    
    
    ## SCARED (Screen for Child Anxiety and Related Disorders)
    # Overall mean score
    y3m_scared_mean = mean_across("y3m", "scared", name = "y3m_scared_mean", exclude = "y3m_scared_c_11"),
    
    # Panic disorder/significant somatic symptoms subscale
    y3m_scared_paso_mean = mean_across("y3m", "scared", "PA/SO", name = "y3m_scared_paso_mean"),
    
    # Generalized anxiety disorder subscale
    y3m_scared_ga_mean = mean_across("y3m", "scared", "GA", name = "y3m_scared_ga_mean"),
    
    # Separation anxiety disorder subscale
    y3m_scared_sep_mean = mean_across("y3m", "scared", "SEP", name = "y3m_scared_sep_mean"),
    
    # Social phobic disorder subscale
    y3m_scared_soc_mean = mean_across("y3m", "scared", "SOC", name = "y3m_scared_soc_mean", exclude = "y3m_scared_c_11"),
    
    # Significant school avoidance symptoms
    y3m_scared_sch_mean = mean_across("y3m", "scared", "SCH", name = "y3m_scared_sch_mean"),
    
    
    ## SHAPS (Snaith-Hamilton Pleasure Scale)
    # Overall mean score
    y3m_shaps_mean = mean_across("y3m", "shaps", name = "y3m_shaps_mean"),
    
    
    ## SRET (Self-Referential Encoding Task)
    # (Currently a low priority to code given how time-intensive this is; see
    # Dainer-Best et al., 2018)
    # Items (both baseline and 3m): "SRET", "SRET.keys", "SRET.time", "SRET.words", "tlcond"
    
    
    ## DRS (Dietary Restriction Screener)
    # Two items that do not need to be recoded or combined
    
    
    ## SITBI-SF (Self-Injurious Thoughts and Behaviors Interview - Short Form)
    # Many items but no recoding or combining (but ranges need to be checked)
    
    
    ## IPTQ (Implicit Personality Theory Questionnaire)
    # Overall mean score
    y3m_iptq_mean = mean_across("y3m", "iptq", name = "y3m_iptq_mean"),
    
    
    ## BFAMG (Brief Family Assessment Measure - General Scale)
    # Overall mean score
    y3m_bfamg_mean = mean_across("y3m", "bfamg", name = "y3m_bfamg_mean"),
    
    
    ## MPVS (Multidimensional Peer Victimization Scale)
    # Overall mean score
    y3m_mpvs_mean = mean_across("y3m", "mpvs", name = "y3m_mpvs_mean"),
    
    # Physical victimization subscale
    y3m_mpvs_physical_mean = mean_across("y3m", "mpvs", "Physical Victimization", name = "y3m_mpvs_physical_mean"),
    
    # Social manipulation subscale
    y3m_mpvs_social_mean = mean_across("y3m", "mpvs", "Social Manipulation", name = "y3m_mpvs_social_mean"),
    
    # Verbal victimization subscale
    y3m_mpvs_verbal_mean = mean_across("y3m", "mpvs", "Verbal Victimization", name = "y3m_mpvs_verbal_mean"),
    
    # Attacks on property subscale
    y3m_mpvs_property_mean = mean_across("y3m", "mpvs", "Attacks on Property", name = "y3m_mpvs_property_mean"),
    
    
    ## UCLA (UCLA Loneliness Scale, aka ULS)
    # Overall mean score
    y3m_ucla_mean = mean_across("y3m", "ucla", name = "y3m_ucla_mean")
    
  ) %>%
  ungroup() %>%
  select(
    
    # Metadata
    lsmh_id,
    y3m_complete,
    y3m_date,
    y3m_datetime,
    y3m_duration,
    
    # Measures
    matches("_cdi_"),
    matches("_bhs_"),
    matches("_pcsc_"),
    matches("_scsc_"),
    matches("_bads_"),
    matches("_shs_"),
    matches("_idas_"),
    matches("_scared_"),
    matches("_shaps_"),
    matches("_drs_"),
    matches("_sitbi_"), - matches("sitbi_.*_TEXT"),
    matches("_iptq_"),
    matches("_bfamg_"),
    matches("_mpvs_"),
    matches("_ucla_")
    
  ) %>%
  
  # Compute item completion rate
  compute_item_completion_rate("y3m") # TODO: Fix which columns are used to compute this


## Check that values are in expected range
items_to_check <- y3m_recoded %>%
  select(
    matches("_cdi_"),
    matches("_bhs_"),
    matches("_pcsc_"),
    matches("_scsc_"),
    matches("_bads_"),
    matches("_idas_"),
    matches("_scared_"),
    matches("_shaps_"),
    matches("_drs_"),
    matches("_iptq_"),
    matches("_bfamg_"),
    matches("_mpvs_"),
    matches("_ucla_"),
    matches("_self_hate_"),
    matches("_agency_"),   # TODO: Seems we need to remove "agency" and "pathways" here
    matches("_pathways_"),
    matches("_pfs_"),
    -ends_with("mean")
  ) %>%
  names()

walk(
  items_to_check,
  ~ check_values( # Helper function
    .data = y3m_recoded,
    .item = .x
  )
)


## Remove invalid responses
# Known valid LSMH IDs
valid_ids <- id_lookup %>%
  filter(action == "keep") %>%
  distinct(lsmh_id)

invalid_ids <- setdiff(y3m_recoded$lsmh_id, valid_ids$lsmh_id)

y3m_valid <- y3m_recoded %>%
  inner_join(
    valid_ids,
    by = "lsmh_id",
    relationship = "many-to-one"
  )

# Just FYI: This is how many IDs/rows included known LSMH IDs matched for removal
y3m_recoded %>%
  filter(lsmh_id %in% id_lookup$lsmh_id[id_lookup$action == "drop"]) %>%
  count(lsmh_id)

# Just FYI: No rows contained unknown LSMH IDs (good!)
# If these rows indicate typos or other errors in the IDs, fix them in the mutate() above
y3m_recoded %>%
  filter(!lsmh_id %in% id_lookup$lsmh_id) %>%
  count(lsmh_id)


## Filter to assessment window
# Add assessment window information
y3m_with_window <- y3m_valid %>%
  left_join(
    ax_windows,
    by = "lsmh_id",
    relationship = "many-to-one"
  ) %>%
  mutate(
    response_in_window = y3m_date >= ax_window_3m_start & y3m_date <= ax_window_3m_end,
    response_too_early = y3m_date < ax_window_3m_start,
    response_too_late = y3m_date > ax_window_3m_end
  )

# Responses by window
y3m_with_window %>%
  count(response_in_window, response_too_early, response_too_late)

# Participants by window
y3m_with_window %>%
  group_by(lsmh_id) %>%
  summarize(
    any_response_in_window = any(response_in_window),
    any_response_too_early = any(response_too_early),
    any_response_too_late = any(response_too_late)
  ) %>%
  count(any_response_in_window, any_response_too_early, any_response_too_late)

# Filter to in-window responses only
y3m_in_window <- y3m_with_window %>%
  filter(response_in_window)


## Deduplicate
# Identify duplicates
identify_duplicates(y3m_in_window, lsmh_id, y3m_complete)

# Remove duplicates
y3m_deduplicated <- remove_duplicates(y3m_in_window, lsmh_id, y3m_datetime)

# Double-check baseline deduplication
identify_duplicates(y3m_deduplicated, lsmh_id, y3m_complete)



####  Save Data  ####
# Save clean Qualtrics data
saveRDS(y3m_deduplicated, clean_data_staging_dir %+% "Phase 2 Youth Qualtrics Clean Data - 3m.rds")

# Save log
saveRDS(log, clean_data_staging_intermediate_dir %+% "Phase 2 Youth Qualtrics Clean Data Log - 3m.rds")
