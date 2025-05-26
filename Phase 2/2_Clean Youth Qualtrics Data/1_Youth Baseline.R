## Track-to-Treat Phase 2 Data Cleaning, Youth Qualtrics, Baseline
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
yb_path <- raw_data_dir %+% "DP5+Phase+2+-+Youth+-+Baseline_May+6,+2025_09.43_n.csv"
yb_raw <- read_survey(yb_path, time_zone = "America/Chicago")


## Load ID lookup
id_lookup <- read_csv(here("Phase 2", "2025.05.26 Track to Treat P2 ID Lookup.csv"))


## Load item-level codebook file
codebook <- load_p2_codebook(here("Phase 2", "2025.05.26 Track to Treat P2 Codebook.xlsx"))


## Check raw Qualtrics data versions using helper function
raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
check_raw_data_ver(raw_metadata, list(yb_path), list(yb_raw), "yb_qualtrics")



####  Clean Data  ####
## Create log
# Create lists for logging (a) items used to compute item completion rate below via
# compute_item_completion_rate() and (b) items used to compute means via mean_across()
log <- list(
  item_completion_rate = list(),
  mean_items = list()
)


## Clean columns
yb_recoded <- yb_raw %>%
  
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
      lsmh_id == "LMSH00886" & yb_lsmh_id == "LSMH00886" ~ "LSMH00886",
      lsmh_id == "LMSH00886" & is.na(yb_lsmh_id) ~ "LSMH00886",
      lsmh_id == "LSMH02264?Redirect=0" & is.na(yb_lsmh_id) ~ "LSMH02264",
      lsmh_id == "Baseline" & yb_lsmh_id == "LSMH02533" ~ "LSMH02533",
      lsmh_id == "Baseline" & is.na(yb_lsmh_id) ~ NA_character_,
      
      # Cases where both match
      yb_lsmh_id == lsmh_id ~ lsmh_id,
      
      # Cases where one is missing (keep the non-missing value)
      is.na(yb_lsmh_id) & !is.na(lsmh_id) ~ lsmh_id,
      is.na(lsmh_id) & !is.na(yb_lsmh_id) ~ yb_lsmh_id,
      
      # Cases where both are missing
      is.na(lsmh_id) & is.na(yb_lsmh_id) ~ NA_character_,
      
      # Additional cases are flagged for cleaning
      T ~ "ID Combination Unaccounted For (" %+% lsmh_id %+% ", " %+% yb_lsmh_id %+% ")"
      
    ),

    # Survey completion
    yb_complete = Finished == 1,
    
    # Survey datetime and duration
    yb_datetime = EndDate,
    yb_date = date(yb_datetime),
    yb_duration = EndDate - StartDate,
    
    
    ## CDI-2 (Children's Depression Inventory - 2)
    # Overall mean score
    yb_cdi_mean = mean_across("yb", "CDI-2 SR", name = "yb_cdi_mean"), # mean_across() from helper function script
    
    # Negative mood/physical symptoms subscale
    yb_cdi_nmps_mean = mean_across("yb", "CDI-2 SR", "Negative Mood/Physical Symptoms", name = "yb_cdi_nmps_mean"),

    # Negative self-esteem subscale
    yb_cdi_nse_mean = mean_across("yb", "CDI-2 SR", "Negative Self-Esteem", name = "yb_cdi_nse_mean"),

    # Ineffectiveness subscale
    yb_cdi_inef_mean = mean_across("yb", "CDI-2 SR", "Ineffectiveness", name = "yb_cdi_inef_mean"),

    # Interpersonal problems subscale
    yb_cdi_inter_mean = mean_across("yb", "CDI-2 SR", "Interpersonal Problems", name = "yb_cdi_inter_mean"),

    # Emotional problems subscale
    yb_cdi_emotional_mean = ((yb_cdi_nmps_mean * 9) + (yb_cdi_nse_mean * 6)) / 15,

    # Functional problems subscale
    yb_cdi_functional_mean = ((yb_cdi_inef_mean * 8) + (yb_cdi_inter_mean * 5)) / 13,

    
    ## BHS-4 (Beck Hopelessness Scale - 4-item)
    # Overall mean score
    yb_bhs_mean = mean_across("yb", "bhs", name = "yb_bhs_mean"),

    
    ## PCSC (Primary Control Scale for Children)
    # Overall mean score
    yb_pcsc_mean = mean_across("yb", "pcsc", name = "yb_pcsc_mean"),

    # Academic subscale
    yb_pcsc_academic_mean = mean_across("yb", "pcsc", "Academic", name = "yb_pcsc_academic_mean"),

    # Social subscale
    yb_pcsc_social_mean = mean_across("yb", "pcsc", "Social", name = "yb_pcsc_social_mean"),

    # Behavioral subscale
    yb_pcsc_behavioral_mean = mean_across("yb", "pcsc", "Behavioral", name = "yb_pcsc_behavioral_mean"),

    
    ## SCSC (Secondary Control Scale for Children)
    # Overall mean score
    yb_scsc_mean = mean_across("yb", "scsc", name = "yb_scsc_mean"),

    
    ## BADS (Behavioral Activation for Depression Scale)
    # Activation subscale
    yb_bads_ac_mean = mean_across("yb", "bads", "AC", name = "yb_bads_ac_mean"),

    # Avoidance/rumination subscale    
    yb_bads_ar_mean = mean_across("yb", "bads", "AR", name = "yb_bads_ar_mean"),

    # Work/school impairment subscale
    yb_bads_ws_mean = mean_across("yb", "bads", "WS", name = "yb_bads_ws_mean"),

    # Social impairment subscale
    yb_bads_si_mean = mean_across("yb", "bads", "SI", name = "yb_bads_si_mean"),

    # Overall score can also be computed (for instructions, see https://doi.org/b23r6w )
    
    
    ## SHS (Self-Hate Scale)
    # Overall mean score
    yb_self_hate_mean = mean_across("yb", "self_hate_scale", name = "yb_self_hate_mean"),

    
    ## IDAS-II (Inventory of Depression and Anxiety Symptoms - II)
    # Given that scoring likely depends on intended use, we output items but do 
    # not score them (see Table 1 of https://doi.org/f4b85p for scale info)
    
    
    ## SCARED (Screen for Child Anxiety and Related Disorders)
    # Overall mean score
    yb_scared_mean = mean_across("yb", "scared", name = "yb_scared_mean", exclude = "yb_scared_c_11"),

    # Panic disorder/significant somatic symptoms subscale
    yb_scared_paso_mean = mean_across("yb", "scared", "PA/SO", name = "yb_scared_paso_mean"),

    # Generalized anxiety disorder subscale
    yb_scared_ga_mean = mean_across("yb", "scared", "GA", name = "yb_scared_ga_mean"),

    # Separation anxiety disorder subscale
    yb_scared_sep_mean = mean_across("yb", "scared", "SEP", name = "yb_scared_sep_mean"),

    # Social phobic disorder subscale
    yb_scared_soc_mean = mean_across("yb", "scared", "SOC", name = "yb_scared_soc_mean", exclude = "yb_scared_c_11"),

    # Significant school avoidance symptoms
    yb_scared_sch_mean = mean_across("yb", "scared", "SCH", name = "yb_scared_sch_mean"),

    
    ## SHAPS (Snaith-Hamilton Pleasure Scale)
    # Overall mean score
    yb_shaps_mean = mean_across("yb", "shaps", name = "yb_shaps_mean"),

    
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
    yb_iptq_mean = mean_across("yb", "iptq", name = "yb_iptq_mean"),

    
    ## BFAMG (Brief Family Assessment Measure - General Scale)
    # Overall mean score
    yb_bfamg_mean = mean_across("yb", "bfamg", name = "yb_bfamg_mean"),

    
    ## MPVS (Multidimensional Peer Victimization Scale)
    # Overall mean score
    yb_mpvs_mean = mean_across("yb", "mpvs", name = "yb_mpvs_mean"),

    # Physical victimization subscale
    yb_mpvs_physical_mean = mean_across("yb", "mpvs", "Physical Victimization", name = "yb_mpvs_physical_mean"),

    # Social manipulation subscale
    yb_mpvs_social_mean = mean_across("yb", "mpvs", "Social Manipulation", name = "yb_mpvs_social_mean"),

    # Verbal victimization subscale
    yb_mpvs_verbal_mean = mean_across("yb", "mpvs", "Verbal Victimization", name = "yb_mpvs_verbal_mean"),

    # Attacks on property subscale
    yb_mpvs_property_mean = mean_across("yb", "mpvs", "Attacks on Property", name = "yb_mpvs_property_mean"),

    
    ## UCLA (UCLA Loneliness Scale, aka ULS)
    # Overall mean score
    yb_ucla_mean = mean_across("yb", "ucla", name = "yb_ucla_mean")
    
  ) %>%
  ungroup() %>%
  
  # Select variables
  select(
    
    # Metadata
    lsmh_id,
    yb_complete,
    yb_date,
    yb_datetime,
    yb_duration,
    
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
  compute_item_completion_rate("yb") # TODO: Fix which columns are used to compute this


## Check that values are in expected range
items_to_check <- yb_recoded %>%
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
    matches("_agency_"),
    matches("_pathways_"),
    matches("_pfs_"),
    -ends_with("mean")
  ) %>%
  names()

walk(
  items_to_check,
  ~ check_values( # Helper function
    .data = yb_recoded,
    .item = .x
  )
)


## Remove invalid responses
# Known valid LSMH IDs
valid_ids <- id_lookup %>%
  filter(action == "keep") %>%
  distinct(lsmh_id)

invalid_ids <- setdiff(yb_recoded$lsmh_id, valid_ids$lsmh_id)

yb_valid <- yb_recoded %>%
  inner_join(
    valid_ids,
    by = "lsmh_id",
    relationship = "many-to-one"
  )

# Just FYI: This is how many IDs/rows included known LSMH IDs matched for removal
yb_recoded %>%
  filter(lsmh_id %in% id_lookup$lsmh_id[id_lookup$action == "drop"]) %>%
  count(lsmh_id)

# Just FYI: No rows contained unknown LSMH IDs (good!)
# If these rows indicate typos or other errors in the IDs, fix them in the mutate() above
yb_recoded %>%
  filter(!lsmh_id %in% id_lookup$lsmh_id) %>%
  count(lsmh_id)


## Deduplicate
# Identify duplicates
identify_duplicates(yb_valid, lsmh_id, yb_complete)

# Remove duplicates
yb_deduplicated <- remove_duplicates(yb_valid, lsmh_id, yb_datetime)

# Double-check baseline deduplication
identify_duplicates(yb_deduplicated, lsmh_id, yb_complete)



####  Save Data  ####
# Save clean Qualtrics data
saveRDS(yb_deduplicated, clean_data_staging_dir %+% "Phase 2 Youth Qualtrics Clean Data - Baseline.rds")

# Save log
saveRDS(log, clean_data_staging_intermediate_dir %+% "Phase 2 Youth Qualtrics Clean Data Log - Baseline.rds")
