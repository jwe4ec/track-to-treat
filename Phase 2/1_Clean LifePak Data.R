## Track-to-Treat Phase 1 Data Cleaning
## LifePak data
# R version 4.4.3

####  Startup  ####
## Load packages
library(groundhog) # 3.2.2
groundhog_date <- "2025-03-28"
meta.groundhog(groundhog_date)
groundhog.library(
  pkg = c("tidyverse", "lubridate", "here", "digest"),
  date = groundhog_date
)
`%+%` <- paste0


## Load helper functions
source(here("Version Control Helper Functions.R"))


## Load data
# Save directories
raw_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT P2\\Data\\LifePak\\"
clean_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT P2\\Data\\Clean Data (Isaac)\\"
clean_data_staging_dir <- clean_data_dir %+% "staging\\"
clean_data_staging_intermediate_dir <- clean_data_staging_dir %+% "intermediate\\"

# Load NIS ("notification-initiated survey") datasets
raw_data_paths <- list(
  nis_1 = raw_data_dir %+% "DataReports\\TRACK_to_T_NIS_Wide20230823_19_49_36_1.csv",
  nis_2 = raw_data_dir %+% "DataReports\\TRACK_to_T_NIS_Wide20230823_19_49_36_2.csv",
  nis_01019_1 = raw_data_dir %+% "LSMH01019\\TRACK_to_T_NIS_Wide20210427_13_22_30.csv",
  nis_01019_2 = raw_data_dir %+% "LSMH01019\\TRACK_to_T_NIS_Wide20210611_15_22_53.csv",
  nis_01019_3 = raw_data_dir %+% "LSMH01019\\TRACK_to_T_NIS_Wide20210729_21_07_30.csv",
  nis_01019_4 = raw_data_dir %+% "LSMH01019\\TRACK_to_T_NIS_Wide20210730_21_14_15.csv",
  nis_01155 = raw_data_dir %+% "LSMH01155\\TRACK_to_T_NIS_Wide20210803_14_13_35.csv",
  nis_01550 = raw_data_dir %+% "LSMH01550\\LSMH01550_TRACK_to_T_NIS_Wide20220101_16_36_46.csv"
)

raw_data <- lapply(raw_data_paths, read.csv)
list2env(raw_data, envir = .GlobalEnv)


## Check raw LifePak data versions using helper function
# raw_metadata <- read.csv(here("Phase 2", "Raw P2 Metadata.csv"))
# check_raw_data_ver(raw_metadata, raw_data_paths, raw_data, "lifepak")



####  Clean Data  ####
## Combine downloads into one dataset
# All datasets have the same column names, but types are not always consistent
nis_combined <- lst(
  nis_1,
  nis_2,
  nis_01019_1,
  nis_01019_2,
  nis_01019_3,
  nis_01019_4,
  nis_01155,
  nis_01550
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


## Correct lifepak_id
# lifepak_id 322476 (LSMH01019, or LSMH01155, or something?) is also 334418
nis_clean$lifepak_id[nis_clean$lifepak_id == "334418"] <- "322476"

# lifepak_id 122772 (LSMH01269) is also 707268
nis_clean$lifepak_id[nis_clean$lifepak_id == "707268"] <- "122772"

# lifepak_id 867499 (LSMH01786) is also 326117
nis_clean$lifepak_id[nis_clean$lifepak_id == "326117"] <- "867499"

# lifepak_id 131166 (LSMH01841) is also 303379 
nis_clean$lifepak_id[nis_clean$lifepak_id == "303379"] <- "131166"

# lifepak_id 898891 (LSMH02181) is also 961421  
nis_clean$lifepak_id[nis_clean$lifepak_id == "961421"] <- "898891"

# lifepak_id 632541 (LSMH02422) is also 095929  
nis_clean$lifepak_id[nis_clean$lifepak_id == "095929"] <- "632541"

# lifepak_id 906962 (LSMH01019) is also 823958  
nis_clean$lifepak_id[nis_clean$lifepak_id == "823958"] <- "906962"


## Arrange by lifepak_id, then notification_datetime
nis_arranged <- nis_clean %>%
  arrange(
    lifepak_id,
    notification_datetime
  )


## Deduplicate by row
# Taking distinct rows addresses identical rows that were simply saved in multiple datasets
nis_distinct <- nis_arranged %>%
  select(-dataset) %>%
  distinct()


## Deduplicate by response
# One duplicate response remains; this was noted in the readme. It seems that one response value
# (energy = 0) was somehow recorded as a separate response from the others
duplicate_responses <- nis_distinct %>%
  count(lifepak_id, response_start_datetime) %>%
  drop_na() %>% 
  filter(n > 1)

duplicate_responses %>%
  left_join(nis_distinct)

# This can be addressed manually by dropping the less complete response and manually recoding energy = 0
nis_manually_filtered_587713 <- nis_distinct %>%
  filter(
    !(
      lifepak_id == "587713"
      & response_start_datetime == as_datetime("2023-03-12 11:03:41")
      & energy %in% 0
    )
  ) %>%
  mutate(
    energy = if_else(
      lifepak_id == "587713" & response_start_datetime == as_datetime("2023-03-12 11:03:41"),
      0,
      energy
    )
  )

duplicate_responses %>%
  left_join(nis_manually_filtered_587713)


## Deduplicate by notification
# A number of notifications appear more than once
duplicate_notifications <- nis_manually_filtered_587713 %>% 
  count(lifepak_id, notification_datetime) %>% 
  filter(n > 1)

duplicate_notifications

duplicate_notifications %>%
  left_join(
    nis_distinct, 
    by = c("lifepak_id", "notification_datetime")
  ) %>%
  select(lifepak_id, notification_datetime, n, response_start_datetime, response_ended)

# To deduplicate, always keep the first row where response_ended_within_2h == T
nis_deduplicated <- nis_manually_filtered_587713 %>%
  group_by(lifepak_id, notification_datetime) %>%
  arrange(desc(response_ended_within_2h), response_start_datetime) %>%
  slice_head(n = 1) %>%
  ungroup()

nis_deduplicated %>% 
  count(lifepak_id, notification_datetime) %>%
  arrange(desc(n))


## Remove additional invalid responses; according to README_ttt_p2_data_collection...
# 162922 was a test response
# 366106 opted out of participating
# 324562 appeared fraudulent
# 764447 appeared fraudulent
# 404350 was ineligible
# 413115 was ineligible
nis_valid <- nis_deduplicated %>%
  filter(
    !lifepak_id %in% c("162922", "366106", "324562", "764447", "404350", "413115")
  )



####  Save Data  ####
saveRDS(nis_valid, clean_data_staging_intermediate_dir %+% "Phase 2 LifePak Clean Data Without LSMH ID.rds")
