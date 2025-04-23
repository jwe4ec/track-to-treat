## Track-to-Treat Phase 1 Data Cleaning
## Clean youth Qualtrics data and add LSMH ID to LifePak Data
# R version 4.4.3

####  Startup  ####
## Load packages
library(groundhog) # 3.2.2
groundhog_date <- "2025-03-28"
meta.groundhog(groundhog_date)
groundhog.library(
  pkg = c("tidyverse", "lubridate", "qualtRics", "here", "openxlsx"),
  date = groundhog_date
)
`%+%` <- paste0


## Load helper functions
source(here("Qualtrics Data Cleaning Helper Functions.R"))


## Load data
# Save directories
raw_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\Qualtrics Data\\Raw Data\\"
clean_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\Clean Data (Isaac)\\"
clean_data_staging_dir <- clean_data_dir %+% "staging\\"
clean_data_staging_intermediate_dir <- clean_data_staging_dir %+% "intermediate\\"

# Load raw Qualtrics datasets in this format: [respondent][wave]_[administration]_raw
# - Note: Use "timeZone" specified for date columns (e.g., "StartDate") in third row of raw CSV
yb_in_person_raw <- read_survey(raw_data_dir %+% "dp5_b_child_p1_numeric.csv", time_zone = "America/Denver")
yb_remote_raw <- read_survey(raw_data_dir %+% "dp5_b_child_remote_p1_numeric.csv", time_zone = "America/Denver")
y3m_raw <- read_survey(raw_data_dir %+% "dp5_3m_child_p1_numeric.csv", time_zone = "America/Denver")

# Load intermediate LifePak data
nis_valid <- readRDS(clean_data_staging_intermediate_dir %+% "Phase 1 LifePak Clean Data Without LSMH ID.rds")

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
# Note on variable overlap: 
# - No variables appear in the in-person dataset only
# - yb_scared_c_11 appears in the remote dataset only; because mean_across() 
#   drops NAs, this item is excluded from scales in the remote dataset
# - Other variables appear in the remote dataset only, but are less important
#   ("password_child", click and time on page information, yb_interview, 
#   and yb_scared_c_11)
yb_raw <- bind_rows(
  list(
    "in-person" = yb_in_person_raw, 
    "remote" = yb_remote_raw
    ),
  .id = "administration"
)


### Confirm that all IDs match "validate" columns and then remove "validate" columns
all(yb_raw$yb_lsmh_id == yb_raw$`yb_lsmh_id_ validate`, na.rm = TRUE)
all(yb_raw$`yb_LifePak ID` == yb_raw$`yb_LifePak ID Verify`, na.rm = TRUE)
all(yb_raw$yb_phone == yb_raw$yb_phone_validate, na.rm = TRUE)
all(y3m_raw$y3m_lsmh_id == y3m_raw$`y3_lsmh_id_ validate`, na.rm = TRUE)

yb_raw[, c("yb_lsmh_id_ validate", "yb_LifePak ID Verify", "yb_phone_validate")] <- NULL
y3m_raw[, "y3_lsmh_id_ validate"] <- NULL


### Remove invalid responses
# Invalid IDs: Tests or survey previews
invalid_ids <- c("LSMH00000", "LSMH00000000111", "LSMH00001", "LSMH00062", "LSMH000", "LSMH000000")

# Remove invalid responses using helper function
yb_valid_ids <- remove_invalid_responses(yb_raw, yb_lsmh_id)
y3m_valid_ids <- remove_invalid_responses(y3m_raw, y3m_lsmh_id)


### Correct IDs
# Manually correct ID that was entered incorrectly at 3m
y3m_valid_ids$y3m_lsmh_id[y3m_valid_ids$y3m_lsmh_id == "LSMH00196"] <- "LSMH00169"

# Manually add or change LifePak IDs as needed, per readme_ttt_p1
yb_valid_ids$`yb_LifePak ID`[yb_valid_ids$yb_lsmh_id == "LSMH00097"] <- "092521"
yb_valid_ids$`yb_LifePak ID`[yb_valid_ids$yb_lsmh_id == "LSMH00457"] <- "292656"
yb_valid_ids$`yb_LifePak ID`[yb_valid_ids$yb_lsmh_id == "LSMH00483"] <- "558692"
yb_valid_ids$`yb_LifePak ID`[yb_valid_ids$yb_lsmh_id == "LSMH00617"] <- "479327"
yb_valid_ids$`yb_LifePak ID`[yb_valid_ids$yb_lsmh_id == "LSMH00306"] <- "130294"
yb_valid_ids$`yb_LifePak ID`[yb_valid_ids$yb_lsmh_id == "LSMH00416"] <- "946021"

# Fill LifePak ID across duplicates using helper function
yb_valid_ids <- fill_lifepak_id(yb_valid_ids, yb_lsmh_id)


### Add LSMH ID to LifePak Data
# If any more LSMH ID issues emerge later in this script or in cleaning parent 
# Qualtrics data, fix them before this step
lsmh_id_lookup <- yb_valid_ids %>%
  rename(
    lifepak_id = `yb_LifePak ID`,
    lsmh_id = yb_lsmh_id
  ) %>%
  distinct(lifepak_id, lsmh_id)

nis_valid_with_lsmh_id <- nis_valid %>%
  left_join(lsmh_id_lookup, by = "lifepak_id", relationship = "many-to-one")


### Remove surveys outside of assessment window
## Obtain EMA notification dates from LifePak data and compute end of EMA period
ema_notif_dates <- nis_valid_with_lsmh_id %>%
  group_by(lifepak_id) %>%
  summarise(
    lsmh_id = unique(lsmh_id),
    first_ema_notif_date = min(notification_date),
    last_ema_notif_date = max(notification_date),
    end_ema_period = first_ema_notif_date + days(21),
    .groups = "drop"
  )


## Compute potential assessment window dates for Qualtrics surveys
# - Note: Excel formulas were used to compute assessment windows by adding months 
#   (e.g., "=DATE(YEAR(A1), MONTH(A1) + 3, DAY(A1))"). When adding months yields
#   a date that is not real (due to 28-31 days in a month), Excel rolls to the next
#   real date, whereas R rolls to the last real date (when using "%m+%") or yields 
#   an error (if using "+"). Thus, consider some leeway in assessment windows.

# Compute potential assessment windows based on baseline survey completion date
# (for now, do this only for participants without duplicate surveys at baseline)
ax_windows <- yb_valid_ids %>%
  select(yb_lsmh_id, StartDate, EndDate) %>%
  rename(lsmh_id = yb_lsmh_id,
         StartDate_yb = StartDate,
         EndDate_yb = EndDate) %>%
  
  filter(!(lsmh_id %in% c("LSMH00190", "LSMH00355"))) %>% # Those with baseline duplicates from identify_duplicates() below
  left_join(ema_notif_dates, by = "lsmh_id", relationship = "many-to-one") %>%
  
  mutate(start_window_3m_v5 = as_date(EndDate_yb) %m+% months(3),
         end_window_3m_v5 = start_window_3m_v5 %m+% months(1),
         
         start_window_3m_v6 = start_window_3m_v5 - days(1),
         end_window_3m_v6 = end_window_3m_v5 + days(1))


## Compute indicators of (a) baseline survey completion before EMA start date and
## (b) follow-up survey completion in assessment window using helper function
yb_valid_ids <- mark_done_in_ax_window(yb_valid_ids, "yb_lsmh_id", "yb", ax_windows)
y3m_valid_ids <- mark_done_in_ax_window(y3m_valid_ids, "y3m_lsmh_id", "y3m", ax_windows)

# TODO: Consider which ax_window to use, focusing on participants with no duplicates





yb_dup <- identify_duplicates(yb_valid_ids, yb_lsmh_id)
yb_dup_ids <- yb_dup$yb_lsmh_id[yb_dup$total > 1]

y3m_dup <- identify_duplicates(y3m_valid_ids, y3m_lsmh_id)
y3m_dup_ids <- y3m_dup$y3m_lsmh_id[y3m_dup$total > 1]

dup_ids <- c(yb_dup_ids, y3m_dup_ids)

test_3m_one <- y3m_valid_ids[!(y3m_valid_ids$y3m_lsmh_id %in% dup_ids), ]
test_3m_one <- test_3m_one[, c("y3m_lsmh_id", "StartDate", "EndDate", 
                               "first_ema_notif_date", "last_ema_notif_date",
                               "start_window_3m_v5", "end_window_3m_v5", "in_window_3m_v5",
                               "start_window_3m_v6", "end_window_3m_v6", "in_window_3m_v6")]

nrow(test_3m_one) == 60 # 60 participants without duplicates at baseline or 3 months

sum(!test_3m_one$in_window_3m_v5) == 6 # 3 months after baseline completion
sum(!test_3m_one$in_window_3m_v6) == 6 # 3 months after baseline completion +/- 1 day on window dates    (makes sense if Excel rolls forward but R rolls back)

test_3m_one$diff_from_start_v5 <- as_date(test_3m_one$EndDate) - as_date(test_3m_one$start_window_3m_v5) # Should be positive
too_early_v5 <- test_3m_one$diff_from_start_v5[test_3m_one$diff_from_start_v5 < 0]
length(too_early_v5) == 0        # 0 finished before start_window_3m_v5
test_3m_one$diff_from_end_v5 <- as_date(test_3m_one$EndDate) - as_date(test_3m_one$end_window_3m_v5)     # Should be negative
too_late_v5 <- test_3m_one$diff_from_end_v5[test_3m_one$diff_from_end_v5 > 0]
length(too_late_v5) == 6         # 6 finished 12-54 days after end_window_3m_v5
sort(too_late_v5) == c(12, 17, 19, 31, 50, 54)

test_3m_one$diff_from_start_v6 <- as_date(test_3m_one$EndDate) - as_date(test_3m_one$start_window_3m_v6) # Should be positive
too_early_v6 <- test_3m_one$diff_from_start_v6[test_3m_one$diff_from_start_v6 < 0]
length(too_early_v6) == 0        # 0 finished before start_window_3m_v6
test_3m_one$diff_from_end_v6 <- as_date(test_3m_one$EndDate) - as_date(test_3m_one$end_window_3m_v6)     # Should be negative
too_late_v6 <- test_3m_one$diff_from_end_v6[test_3m_one$diff_from_end_v6 > 0]
length(too_late_v6) == 6         # 6 finished 11-53 days after end_window_3m_v6
sort(too_late_v6) == c(11, 16, 18, 30, 49, 53)

(test_3m_one$y3m_lsmh_id[test_3m_one$diff_from_start_v6 < 0 | test_3m_one$diff_from_end_v6 > 0]) # IDs (unsorted)
  # "LSMH00039", "LSMH00306", "LSMH00516" (also for p3m), "LSMH00492", "LSMH00661" (also for p3m), "LSMH00604"


## TODO: Remove 3-month surveys outside assessment window





### Create lists for logging (a) items used to compute item completion rates below via
### compute_item_completion_rate(), (b) items used to compute means via mean_across(),
### and (c) clean codebook (edited and added to log below)
log <- list(item_completion_rate = list(),
            mean_items = list())


### Deduplicate
# Compute item completion rate using helper function (given that Qualtrics's "Progress" 
# and "Finished" variables reflect only clicking through survey, not completing items)
yb_valid_ids <- compute_item_completion_rate(yb_valid_ids, "yb")
y3m_valid_ids <- compute_item_completion_rate(y3m_valid_ids, "y3m")

# Identify duplicates using helper function
identify_duplicates(yb_valid_ids, yb_lsmh_id)
identify_duplicates(y3m_valid_ids, y3m_lsmh_id)

# TODO (JE to revise after removing surveys outside assessment window above): Remove duplicates using helper function
yb_deduplicated <- remove_duplicates(yb_valid_ids, yb_lsmh_id)
y3m_deduplicated <- remove_duplicates(y3m_valid_ids, y3m_lsmh_id)





# Double-check work
identify_duplicates(yb_deduplicated, yb_lsmh_id)
identify_duplicates(y3m_deduplicated, y3m_lsmh_id)


### Merge data by LSMH ID
# IDs in baseline not 3m
setdiff(yb_deduplicated$yb_lsmh_id, y3m_deduplicated$y3m_lsmh_id)

# IDs in 3m not baseline
setdiff(y3m_deduplicated$y3m_lsmh_id, yb_deduplicated$yb_lsmh_id)

# Full join
y_merged <- full_join(
  yb_deduplicated,
  y3m_deduplicated,
  by = c("yb_lsmh_id" = "y3m_lsmh_id"),
  relationship = "one-to-one",
  suffix = c(".yb", ".y3m")
)


### Clean columns
## Correct misspelled item prefixes in the data and codebook
# Data: Before
prefixes_data <- str_extract(colnames(y_merged), "^.*?(?=_)")
table(prefixes_data)

# Data: Fixing
colnames(y_merged) <- gsub("^y3_", "y3m_", colnames(y_merged))
colnames(y_merged) <- gsub("^y3n_", "y3m_", colnames(y_merged))
colnames(y_merged) <- gsub("^yd_", "yb_", colnames(y_merged))

# Data: After
prefixes_data <- str_extract(colnames(y_merged), "^.*?(?=_)")
table(prefixes_data)

# Codebook: Before
prefixes_codebook <- str_extract(codebook$item, "^.*?(?=_)")
table(prefixes_codebook)

# Codebook: Fixing
codebook$item <- gsub("^y3n_", "y3m_", codebook$item)

# Codebook: After
prefixes_codebook <- str_extract(codebook$item, "^.*?(?=_)")
table(prefixes_codebook)

# Add clean codebook to log
log$codebook_clean <- codebook

# Data collected but not included here:
# - Self-Referential Encoding Task (SRET)
# - Prognostic Pessimism for Depression scale (PPD)
# - Pubertal Development Scale (PDS)
# - Desired intervention (assessed at 3 months)
y_clean <- y_merged %>%
  
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
      .cols = any_of(codebook$item[codebook$reversed]),
      .fns = ~ codebook$reverse_base[codebook$item == cur_column()] - .x
    )
  ) %>%

  # Clean columns and create composites
  rowwise() %>%
  mutate(
    
    ## Metadata
    # ID
    lsmh_id = yb_lsmh_id,
    lifepak_id = `yb_LifePak ID`,
    
    # Administration mode
    yb_administration = administration,
    
    # Survey completion
    yb_complete = !is.na(EndDate.yb),
    y3m_complete = !is.na(EndDate.y3m),
    
    # Survey datetime and duration
    yb_date = EndDate.yb,
    yb_duration = EndDate.yb - StartDate.yb,
    y3m_date = EndDate.y3m,
    y3m_duration = EndDate.y3m - StartDate.y3m,
    
    
    ## CDI-2 (Children's Depression Inventory - 2)
    # Overall mean score
    yb_cdi_mean = mean_across("yb", "CDI-2 SR", name = "yb_cdi_mean"), # mean_across() from helper function script
    y3m_cdi_mean = mean_across("y3m", "CDI-2 SR", name = "y3m_cdi_mean"),
    
    # Negative mood/physical symptoms subscale
    yb_cdi_nmps_mean = mean_across("yb", "CDI-2 SR", "Negative Mood/Physical Symptoms", name = "yb_cdi_nmps_mean"),
    y3m_cdi_nmps_mean = mean_across("y3m", "CDI-2 SR", "Negative Mood/Physical Symptoms", name = "y3m_cdi_nmps_mean"),
    
    # Negative self-esteem subscale
    yb_cdi_nse_mean = mean_across("yb", "CDI-2 SR", "Negative Self-Esteem", name = "yb_cdi_nse_mean"),
    y3m_cdi_nse_mean = mean_across("y3m", "CDI-2 SR", "Negative Self-Esteem", name = "y3m_cdi_nse_mean"),
    
    # Ineffectiveness subscale
    yb_cdi_inef_mean = mean_across("yb", "CDI-2 SR", "Ineffectiveness", name = "yb_cdi_inef_mean"),
    y3m_cdi_inef_mean = mean_across("y3m", "CDI-2 SR", "Ineffectiveness", name = "y3m_cdi_inef_mean"),
    
    # Interpersonal problems subscale
    yb_cdi_inter_mean = mean_across("yb", "CDI-2 SR", "Interpersonal Problems", name = "yb_cdi_inter_mean"),
    y3m_cdi_inter_mean = mean_across("y3m", "CDI-2 SR", "Interpersonal Problems", name = "y3m_cdi_inter_mean"),

    # Emotional problems subscale
    yb_cdi_emotional_mean = ((yb_cdi_nmps_mean * 9) + (yb_cdi_nse_mean * 6)) / 15,
    y3m_cdi_emotional_mean = ((y3m_cdi_nmps_mean * 9) + (y3m_cdi_nse_mean * 6)) / 15,

    # Functional problems subscale
    yb_cdi_functional_mean = ((yb_cdi_inef_mean * 8) + (yb_cdi_inter_mean * 5)) / 13,
    y3m_cdi_functional_mean = ((y3m_cdi_inef_mean * 8) + (y3m_cdi_inter_mean * 5)) / 13,
  
    
    ## BHS-4 (Beck Hopelessness Scale - 4-item)
    # Overall mean score
    yb_bhs_mean = mean_across("yb", "bhs", name = "yb_bhs_mean"),
    y3m_bhs_mean = mean_across("y3m", "bhs", name = "y3m_bhs_mean"),
    
    
    ## PCSC (Primary Control Scale for Children)
    # Overall mean score
    yb_pcsc_mean = mean_across("yb", "pcsc", name = "yb_pcsc_mean"),
    y3m_pcsc_mean = mean_across("y3m", "pcsc", name = "y3m_pcsc_mean"),
    
    # Academic subscale
    yb_pcsc_academic_mean = mean_across("yb", "pcsc", "Academic", name = "yb_pcsc_academic_mean"),
    y3m_pcsc_academic_mean = mean_across("y3m", "pcsc", "Academic", name = "y3m_pcsc_academic_mean"),
    
    # Social subscale
    yb_pcsc_social_mean = mean_across("yb", "pcsc", "Social", name = "yb_pcsc_social_mean"),
    y3m_pcsc_social_mean = mean_across("y3m", "pcsc", "Social", name = "y3m_pcsc_social_mean"),
    
    # Behavioral subscale
    yb_pcsc_behavioral_mean = mean_across("yb", "pcsc", "Behavioral", name = "yb_pcsc_behavioral_mean"),
    y3m_pcsc_behavioral_mean = mean_across("y3m", "pcsc", "Behavioral", name = "y3m_pcsc_behavioral_mean"),
    
    
    ## SCSC (Secondary Control Scale for Children)
    # Overall mean score
    yb_scsc_mean = mean_across("yb", "scsc", name = "yb_scsc_mean"),
    y3m_scsc_mean = mean_across("y3m", "scsc", name = "y3m_scsc_mean"),
    
    
    ## BADS (Behavioral Activation for Depression Scale)
    # Activation subscale
    yb_bads_ac_mean = mean_across("yb", "bads", "AC", name = "yb_bads_ac_mean"),
    y3m_bads_ac_mean = mean_across("y3m", "bads", "AC", name = "y3m_bads_ac_mean"),
    
    # Avoidance/rumination subscale    
    yb_bads_ar_mean = mean_across("yb", "bads", "AR", name = "yb_bads_ar_mean"),
    y3m_bads_ar_mean = mean_across("y3m", "bads", "AR", name = "y3m_bads_ar_mean"),
    
    # Work/school impairment subscale
    yb_bads_ws_mean = mean_across("yb", "bads", "WS", name = "yb_bads_ws_mean"),
    y3m_bads_ws_mean = mean_across("y3m", "bads", "WS", name = "y3m_bads_ws_mean"),
    
    # Social impairment subscale
    yb_bads_si_mean = mean_across("yb", "bads", "SI", name = "yb_bads_si_mean"),
    y3m_bads_si_mean = mean_across("y3m", "bads", "SI", name = "y3m_bads_si_mean"),
    
    # Overall score can also be computed (for instructions, see https://doi.org/b23r6w )
    
    
    ## SHS (Self-Hate Scale)
    # Overall mean score
    yb_shs_mean = mean_across("yb", "shs", name = "yb_shs_mean"),
    y3m_shs_mean = mean_across("y3m", "shs", name = "y3m_shs_mean"),
    
    
    ## IDAS-II (Inventory of Depression and Anxiety Symptoms - II)
    # Overall mean score
    yb_idas_mean = mean_across("yb", "idas", name = "yb_idas_mean"),
    y3m_idas_mean = mean_across("y3m", "idas", name = "y3m_idas_mean"),
    
    
    ## SCARED (Screen for Child Anxiety and Related Disorders)
    # Overall mean score
    yb_scared_mean = mean_across("yb", "scared", name = "yb_scared_mean", exclude = "yb_scared_c_11"),
    y3m_scared_mean = mean_across("y3m", "scared", name = "y3m_scared_mean", exclude = "y3m_scared_c_11"),
    
    # Panic disorder/significant somatic symptoms subscale
    yb_scared_paso_mean = mean_across("yb", "scared", "PA/SO", name = "yb_scared_paso_mean"),
    y3m_scared_paso_mean = mean_across("y3m", "scared", "PA/SO", name = "y3m_scared_paso_mean"),
    
    # Generalized anxiety disorder subscale
    yb_scared_ga_mean = mean_across("yb", "scared", "GA", name = "yb_scared_ga_mean"),
    y3m_scared_ga_mean = mean_across("y3m", "scared", "GA", name = "y3m_scared_ga_mean"),
    
    # Separation anxiety disorder subscale
    yb_scared_sep_mean = mean_across("yb", "scared", "SEP", name = "yb_scared_sep_mean"),
    y3m_scared_sep_mean = mean_across("y3m", "scared", "SEP", name = "y3m_scared_sep_mean"),
    
    # Social phobic disorder subscale
    yb_scared_soc_mean = mean_across("yb", "scared", "SOC", name = "yb_scared_soc_mean", exclude = "yb_scared_c_11"),
    y3m_scared_soc_mean = mean_across("y3m", "scared", "SOC", name = "y3m_scared_soc_mean", exclude = "y3m_scared_c_11"),
    
    # Significant school avoidance symptoms
    yb_scared_sch_mean = mean_across("yb", "scared", "SCH", name = "yb_scared_sch_mean"),
    y3m_scared_sch_mean = mean_across("y3m", "scared", "SCH", name = "y3m_scared_sch_mean"),
    
    
    ## SHAPS (Snaith-Hamilton Pleasure Scale)
    # Overall mean score
    yb_shaps_mean = mean_across("yb", "shaps", name = "yb_shaps_mean"),
    y3m_shaps_mean = mean_across("y3m", "shaps", name = "y3m_shaps_mean"),
    
    
    ## SRET (Self-Referential Encoding Task)
    # (Currently a low priority to code given how time-intensive this is; see
    # Dainer-Best et al., 2018)
    # Items (both baseline and 3m): "SRET", "SRET.keys", "SRET.time", "SRET.words", "tlcond"

    
    ## DRS (Dietary Restriction Screener)
    # Two items that do not need to be recoded or combined
    
    
    ## SITBI-SF (Self-Injurious Thoughts and Behaviors Interview - Short Form)
    # Many items but no recoding or combining
    
    
    ## IPTQ (Implicit Personality Theory Questionnaire)
    # Overall mean score
    yb_iptq_mean = mean_across("yb", "iptq", name = "yb_iptq_mean"),
    y3m_iptq_mean = mean_across("y3m", "iptq", name = "y3m_iptq_mean"),
    
    
    ## BFAMG (Brief Family Assessment Measure - General Scale)
    # Overall mean score
    yb_bfamg_mean = mean_across("yb", "bfamg", name = "yb_bfamg_mean"),
    y3m_bfamg_mean = mean_across("y3m", "bfamg", name = "y3m_bfamg_mean"),
    
    
    ## MPVS (Multidimensional Peer Victimization Scale)
    # Overall mean score
    yb_mpvs_mean = mean_across("yb", "mpvs", name = "yb_mpvs_mean"),
    y3m_mpvs_mean = mean_across("y3m", "mpvs", name = "y3m_mpvs_mean"),
    
    # Physical victimization subscale
    yb_mpvs_physical_mean = mean_across("yb", "mpvs", "Physical Victimization", name = "yb_mpvs_physical_mean"),
    y3m_mpvs_physical_mean = mean_across("y3m", "mpvs", "Physical Victimization", name = "y3m_mpvs_physical_mean"),
    
    # Social manipulation subscale
    yb_mpvs_social_mean = mean_across("yb", "mpvs", "Social Manipulation", name = "yb_mpvs_social_mean"),
    y3m_mpvs_social_mean = mean_across("y3m", "mpvs", "Social Manipulation", name = "y3m_mpvs_social_mean"),
    
    # Verbal victimization subscale
    yb_mpvs_verbal_mean = mean_across("yb", "mpvs", "Verbal Victimization", name = "yb_mpvs_verbal_mean"),
    y3m_mpvs_verbal_mean = mean_across("y3m", "mpvs", "Verbal Victimization", name = "y3m_mpvs_verbal_mean"),
    
    # Attacks on property subscale
    yb_mpvs_property_mean = mean_across("yb", "mpvs", "Attacks on Property", name = "yb_mpvs_property_mean"),
    y3m_mpvs_property_mean = mean_across("y3m", "mpvs", "Attacks on Property", name = "y3m_mpvs_property_mean"),
    
    ## UCLA (UCLA Loneliness Scale, aka ULS)
    # Overall mean score
    yb_ucla_mean = mean_across("yb", "ucla", name = "yb_ucla_mean"),
    y3m_ucla_mean = mean_across("y3m", "ucla", name = "y3m_ucla_mean")

  ) %>%
  
  ungroup() %>%
  
  select(
    
    # Metadata
    lsmh_id,
    lifepak_id,
    yb_administration,
    yb_complete,
    y3m_complete,
    yb_date,
    yb_duration,
    y3m_date,
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
    
  )


### Check that values are in expected range
items_to_check <- y_clean %>%
  select(
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
    # matches("_sitbi_"), # 
    matches("_iptq_"),
    matches("_bfamg_"),
    matches("_mpvs_"),
    matches("_ucla_"),
    -ends_with("mean")
  ) %>%
  names()

walk(
  items_to_check,
  ~ check_values( # Helper function
    .data = y_clean,
    .item = .x
  )
)



####  Save Clean Qualtrics Data and Log, Assessment Windows, and Clean LifePak Data  ####
saveRDS(y_clean, clean_data_staging_dir %+% "Phase 1 Youth Qualtrics Clean Data.rds")
saveRDS(log, clean_data_staging_dir %+% "Phase 1 Youth Qualtrics Clean Data Log.rds")

saveRDS(ax_windows, clean_data_staging_intermediate_dir %+% "Phase 1 Assessment Windows.rds")

saveRDS(nis_valid_with_lsmh_id, clean_data_staging_dir %+% "Phase 1 LifePak Clean Data.rds")