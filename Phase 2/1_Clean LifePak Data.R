## Track-to-Treat Phase 2 Data Cleaning, LifePak Data
# R version 4.4.3

####  Startup  ####
## Load packages
library(groundhog) # 3.2.2
groundhog_date <- "2025-03-28"
meta.groundhog(groundhog_date)
groundhog.library(
  pkg = c("tidyverse", "tidylog", "lubridate", "here"),
  date = groundhog_date
)
`%+%` <- paste0


## Load helper functions
source(here("Version Control Helper Functions.R"))


## Load data
# Save directories
raw_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT P2\\Data\\LifePak\\2025.05.21\\"
clean_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT P2\\Data\\Clean Data (Isaac)\\"
clean_data_staging_dir <- clean_data_dir %+% "staging\\"
clean_data_staging_intermediate_dir <- clean_data_staging_dir %+% "intermediate\\"

# Load NIS ("notification-initiated survey") datasets
raw_data_paths <- lst(
  nis_1 = raw_data_dir %+% "TRACK to TREAT P2\\NIS_Wide20250521_17_42_45.csv",
  nis_2 = raw_data_dir %+% "TRACK to TREAT P2 - LSMH01019\\NIS_Wide20250521_17_33_11.csv",
  nis_3 = raw_data_dir %+% "TRACK to TREAT P2 - LSMH01155\\NIS_Wide20250521_17_27_29.csv",
  nis_4 = raw_data_dir %+% "TRACK to TREAT P2 - Pilot 2\\NIS_Wide20250521_19_44_38.csv",
)

raw_data <- lapply(raw_data_paths, read.csv)
list2env(raw_data, envir = .GlobalEnv)

# Load ID lookup
id_lookup <- read_csv(here("Phase 2", "2025.05.26 Track to Treat P2 ID Lookup.csv"))


## Check raw LifePak data versions using helper function
# raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
# check_raw_data_ver(raw_metadata, raw_data_paths, raw_data, "lifepak")



####  Clean Data  ####
## Combine downloads into one dataset
# All datasets have the same column names, but types are not always consistent
nis_combined <- lst(
  nis_1,
  nis_2,
  nis_3,
  nis_4
) %>%
  map(
    .f = ~ {.} %>%
      select(-starts_with("GPS")) %>% # GPS is inconsistently formatted, but we don't need it anyway
      mutate(Responded = as.character(Responded)) # Responded should be a character vector
  ) %>%
  bind_rows(.id = "dataset")


## Clean combined data
nis_clean <- nis_combined %>%
  
  # Clean columns
  mutate(
    
    ## Metadata
    # Dataset
    dataset = dataset,
    
    # ID
    lifepak_id = gsub(".*-", "", Participant.ID),
    
    # Survey type (EMA or feedback)
    survey_type = case_match(
      Session.Name,
      "3T Project" ~ "EMA",
      "3T Project Feedback" ~ "Feedback"
    ),
    
    # Notification date and datetime (in participant devices' local times)
    notification_datetime = as_datetime(Notification.Time),
    notification_date = as_date(notification_datetime),
    
    # Response start indicator (logical)
    response_started = Responded == "1",
    
    # Response lag
    response_lag_seconds = as.difftime(Session.Instance.Response.Lapse),
    
    # Response duration
    response_duration = as.difftime(Session.Length),
    
    # Response completion indicators (logical)
    response_ended = !is.na(response_duration),
    response_ended_within_2h = if_else(
      response_ended,
      response_lag_seconds + response_duration <= 7200,
      F
    ),
    
    # Response date and datetime
    response_start_datetime = notification_datetime + response_lag_seconds,
    response_end_datetime = response_start_datetime + response_duration,
    response_start_date = as_date(response_start_datetime),
    
    # Response data
    # ema_[...].1 variables capture the same construct as ema_[...] variables, but for different rows
    # No rows have non-missing data for both columns
    # These variables need to be combined
    # This may serve the same purpose as [...]_day and [...]_night in Phase 1
    sad = case_when(
      !is.na(ema_sad) ~ ema_sad,
      !is.na(ema_sad.1) ~ ema_sad.1,
      T  ~ NA_real_
    ),
    
    bad = case_when(
      !is.na(ema_bad) ~ ema_bad,
      !is.na(ema_bad.1) ~ ema_bad.1,
      T ~ NA_real_
    ),
    
    interest = case_when(
      !is.na(ema_interest) ~ ema_interest,
      !is.na(ema_interest.1) ~ ema_interest.1,
      T ~ NA_real_
    ),

    energy = case_when(
      !is.na(ema_energy) ~ ema_energy,
      !is.na(ema_energy.1) ~ ema_energy.1,
      T ~ NA_real_
    ),
    
    focus = case_when(
      !is.na(ema_focus) ~ ema_focus,
      !is.na(ema_focus.1) ~ ema_focus.1,
      T ~ NA_real_
    ),
    
    movement = case_when(
      !is.na(ema_movement) ~ ema_movement,
      !is.na(ema_movement.1) ~ ema_movement.1,
      T ~ NA_real_
    ),
    
    control = case_when(
      !is.na(ema_control) ~ ema_control,
      !is.na(ema_control.1) ~ ema_control.1,
      T ~ NA_real_
    ),
    
    fun = case_when(
      !is.na(ema_fun) ~ ema_fun,
      !is.na(ema_fun.1) ~ ema_fun.1,
      T ~ NA_real_
    ),
    
    # Across EMA variables, if response was not completed in time, recode as NA
    across(
      all_of(c("sad", "bad", "interest", "energy", "focus", "movement", "control", "fun")),
      ~ if_else(
        response_ended_within_2h,
        .,
        NA_real_
      )
    ),
    
    fun_rev = 100 - fun # Creating a reverse-coded item so that there is a set of 8 items all representing dysfunction
    
  ) %>%
  
  # Select clean columns
  select(
    
    # Metadata
    dataset,
    lifepak_id,
    survey_type,
    notification_date,
    notification_datetime,
    response_started,
    response_start_date,
    response_start_datetime,
    response_ended,
    response_end_datetime,
    response_lag_seconds,
    response_duration,
    response_ended_within_2h,
    
    # Response data
    sad, bad, interest, energy, focus, movement, control, fun, fun_rev,
    
    # Most pleasant and most unpleasant event from the day
    most_pleasant = best,
    most_unpleasant = worst,
    
    # Another open-ended response worth keeping
    other
    
  ) %>%
  
  # Filter to only EMA data (not "feedback" surveys, which were administered after EMA surveys)
  filter(survey_type == "EMA")


## Remove invalid responses, adding lsmh_id
# Known LifePak IDs along with their lsmh_id
valid_ids <- id_lookup %>%
  filter(action == "keep") %>%
  drop_na(lifepak_id) %>%
  select(lsmh_id, lifepak_id)

nis_valid_with_lsmh_id <- nis_clean %>%
  inner_join(
    valid_ids,
    by = "lifepak_id",
    relationship = "many-to-one"
  )

# Just FYI: This is how many IDs/rows included known LifePak IDs matched for removal
nis_clean %>%
  filter(lifepak_id %in% id_lookup$lifepak_id[id_lookup$action == "drop"]) %>%
  count(lifepak_id)
  
# Just FYI: This is how many IDs/rows included unknown LifePak IDs
nis_clean %>%
  filter(!lifepak_id %in% id_lookup$lifepak_id) %>%
  count(lifepak_id)

# Just FYI: These known LifePak IDs do not appear in the data
valid_ids %>%
  filter(!lifepak_id %in% nis_valid_with_lsmh_id$lifepak_id) %>%
  pull(lifepak_id)


## Arrange by lsmh_id, then notification_datetime
nis_arranged <- nis_valid_with_lsmh_id %>%
  arrange(
    lsmh_id,
    notification_datetime
  )


## Deduplicate by row
# No duplicate rows
nis_arranged %>%
  distinct() %>%
  invisible()


## Deduplicate by response
# No duplicate responses
nis_arranged %>%
  count(lifepak_id, response_start_datetime) %>%
  drop_na() %>% 
  filter(n > 1)


## Deduplicate by notification
# A number of notifications appear more than once
duplicate_notifications <- nis_arranged %>% 
  count(lifepak_id, notification_datetime) %>% 
  filter(n > 1)

duplicate_notifications

# To deduplicate, always keep the first row where response_ended_within_2h == T
nis_deduplicated <- nis_arranged %>%
  group_by(lifepak_id, notification_datetime) %>%
  arrange(desc(response_ended_within_2h), response_start_datetime) %>%
  slice_head(n = 1) %>%
  ungroup()

nis_deduplicated %>% 
  count(lifepak_id, notification_datetime) %>%
  arrange(desc(n))



####  Save Data  ####
# Save clean LifePak data
saveRDS(nis_deduplicated, clean_data_staging_dir %+% "Phase 2 LifePak Clean Data.rds")

# Save clean LifePak data without free-response items (until these are deidentified)
nis_deduplicated %>%
  select(-c("most_pleasant", "most_unpleasant", "other")) %>%
  saveRDS(clean_data_staging_dir %+% "Phase 2 LifePak Clean Data Without Free Responses.rds")
