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
raw_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\LifePak Raw Data (Do Not Modify)\\"
clean_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\Clean Data (Isaac)\\"
clean_data_staging_dir <- clean_data_dir %+% "staging\\"
clean_data_staging_intermediate_dir <- clean_data_staging_dir %+% "intermediate\\"

# Load NIS ("notification-initiated survey") datasets (storing paths)
raw_data_paths <- list(nis_1 = raw_data_dir %+% "3T_P1_V1_NIS_2020_Mar_02.csv",
                       nis_2 = raw_data_dir %+% "3T_P1_V2_NIS_2020_Mar_13.csv",
                       nis_3 = raw_data_dir %+% "3T_P1_V2_NIS_21200_958251_Download2.csv",
                       nis_4 = raw_data_dir %+% "3T_P1_V2_NIS_21200_958251_Download3.csv",
                       nis_5 = raw_data_dir %+% "3T_P1_V4_NIS.csv")

raw_data <- lapply(raw_data_paths, read.csv)
list2env(raw_data, envir = .GlobalEnv)


## Check raw LifePak data versions using helper function
raw_metadata <- read.csv(here("Phase 1", "Raw P1 Metadata.csv"))
check_raw_data_ver(raw_metadata, raw_data_paths, raw_data, "lifepak")



####  Clean Data  ####
## Combine downloads into one dataset
# Align variable names across datasets
# (Plus some other modifications to allow for binding, including removing GPS
# information [which doesn't have the same type across datasets and changing
# variable type for other variables])
nis_1_renamed <- nis_1 %>%
  rename(
    intro_feedback_1 = instruct_feedback_1,
    intro_feedback_2 = instruct_feedback_2,
    five_times_a_day = five_times_per_day,
    instruct_helpful = instruction_helpful
  ) %>%
  select(-starts_with("GPS"))

nis_2_renamed <- nis_2 %>%
  rename_with(
    .fn = ~ gsub("_v2", "", .)
  ) %>%
  select(-starts_with("GPS"))

nis_3_renamed <- nis_3 %>%
  rename_with(
    .fn = ~ gsub("_347|_v2|347", "", .)
  ) %>%
  select(-starts_with("GPS")) %>%
  mutate(Responded = as.character(Responded))

nis_4_renamed <- nis_4 %>%
  rename_with(
    .fn = ~ gsub("_v2", "", .)
  ) %>%
  select(-starts_with("GPS"))

nis_5_renamed <- nis_5 %>%
  rename_with(
    .fn = ~ gsub("_v2", "", .)
  ) %>%
  select(-starts_with("GPS"))


## Bind datasets
nis_combined <- lst(
  nis_1_renamed,
  nis_2_renamed,
  nis_3_renamed,
  nis_4_renamed,
  nis_5_renamed
) %>%
  bind_rows(.id = "dataset")


## Clean columns
# Other variables not currently included: feedback variables (e.g,. intro_feedback_1,
# five_times_a_day, notice_change, aware_mood, aware_felt_worse, aware_felt_better)
# Note that if we aren't using feedback data, we can remove those rows!
nis_clean <- nis_combined %>%
  mutate(
    
    ## Metadata
    # Dataset
    dataset = dataset,
    
    # ID
    lifepak_id = gsub(".*-", "", Participant.ID),
    
    # Survey type (EMA or feedback)
    survey_type = case_match(
      Session.Name,
      c("3T Project Day", "3T Project Night") ~ "EMA",
      "3T Project Feedback" ~ "Feedback"
    ),
    
    # Time of day (day or night)
    time_of_day = case_match(
      Session.Name,
      "3T Project Day" ~ "Day",
      "3T Project Night" ~ "Night"
    ),
    
    # Notification date and datetime (in participant devices' local times)
    notification_datetime = as_datetime(Notification.Time),
    notification_date = as_date(notification_datetime),
    
    # Response indicator (logical)
    response_started = Responded == "1",
    
    # Response lag
    response_lag_seconds = as.difftime(Session.Instance.Response.Lapse),

    # Response duration
    response_duration = as.difftime(Session.Length),
    
    # Response completed in window
    response_ended_within_2h = if_else(
      response_started,
      response_lag_seconds + response_duration <= 7200,
      F
    ),
    
    # Response date and datetime
    response_start_datetime = notification_datetime + response_lag_seconds,
    response_end_datetime = response_start_datetime + response_duration,
    response_start_date = as_date(response_start_datetime),
    
    # Response data
    sad = case_when(
      time_of_day == "Day" ~ sad_day,
      time_of_day == "Night" ~ sad_night,
      is.na(time_of_day) ~ NA_real_
    ),
    
    bad = case_when(
      time_of_day == "Day" ~ bad_day,
      time_of_day == "Night" ~ bad_night,
      is.na(time_of_day) ~ NA_real_
    ),
    
    interest = case_when(
      time_of_day == "Day" ~ interest_day,
      time_of_day == "Night" ~ interest_night,
      is.na(time_of_day) ~ NA_real_
    ),
    interest = if_else(interest < 0, 0, interest),
    
    energy = case_when(
      time_of_day == "Day" ~ energy_day,
      time_of_day == "Night" ~ energy_night,
      is.na(time_of_day) ~ NA_real_
    ),
    
    focus = case_when(
      time_of_day == "Day" ~ focus_day,
      time_of_day == "Night" ~ focus_night,
      is.na(time_of_day) ~ NA_real_
    ),
    
    movement = case_when(
      time_of_day == "Day" ~ movement_day,
      time_of_day == "Night" ~ movement_night,
      is.na(time_of_day) ~ NA_real_
    ),
    
    control = case_when(
      time_of_day == "Day" ~ control_day,
      time_of_day == "Night" ~ control_night,
      is.na(time_of_day) ~ NA_real_
    ),
    
    fun = case_when(
      time_of_day == "Day" ~ fun_day,
      time_of_day == "Night" ~ fun_night,
      is.na(time_of_day) ~ NA_real_
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
  select(

    # Metadata
    dataset,
    lifepak_id,
    survey_type,
    time_of_day,
    notification_date,
    notification_datetime,
    response_started,
    response_start_date,
    response_start_datetime,
    response_end_datetime,
    response_duration,
    response_lag_seconds,
    response_ended_within_2h,

    # Response data
    sad, bad, interest, energy, focus, movement, control, fun, fun_rev,

    # Most pleasant and most unpleasant event from the day
    most_pleasant = best_night,
    most_unpleasant = worst_night,

    # Another open-ended response worth keeping
    other_night

  ) %>%
  
  # Filter to only EMA data (not "feedback" surveys, which were administered after EMA surveys)
  filter(survey_type == "EMA")


## Correct lifepak_id
# lifepak_id 958251 (LSMH00347) is also 034516 and 878753
nis_clean$lifepak_id[nis_clean$lifepak_id == "034516"] <- "958251"
nis_clean$lifepak_id[nis_clean$lifepak_id == "878753"] <- "958251"


## Arrange by lifepak_id, then notification_datetime
nis_clean <- nis_clean %>%
  arrange(
    lifepak_id,
    notification_datetime
  )


## Clean rows from datasets overlapping in time for lifepak_id 958251
# Use Night row from "nis_3_renamed" (vs. empty Night row from "nis_2_renamed" at "2020-03-24 21:32:22")
# Use rows from "nis_4_renamed" (vs. empty rows from "nis_2_renamed" on and after "2020-03-27 12:25:51")
nis_clean <- nis_clean[!(nis_clean$lifepak_id == "958251" & 
                           nis_clean$dataset == "nis_2_renamed" &
                           (nis_clean$notification_datetime == as_datetime("2020-03-24 21:32:22") |
                              nis_clean$notification_datetime >= as_datetime("2020-03-27 12:25:51"))), ]


## Deduplicate
# No duplicate responses
duplicate_responses <- nis_clean %>%
  count(lifepak_id, response_start_datetime) %>%
  drop_na() %>% 
  filter(n > 1)

duplicate_responses

# Two notifications appear twice; in both cases, there is no more than one valid response
duplicate_notifications <- nis_clean %>% 
  count(lifepak_id, notification_datetime) %>% 
  filter(n > 1) %>%
  left_join(
    nis_clean, 
    by = c("lifepak_id", "notification_datetime")
  )

duplicate_notifications

# Of these duplicated notifications, keep the first row where response_ended_within_2h == T
duplicate_notifications_to_keep <- duplicate_notifications %>%
  group_by(lifepak_id, notification_datetime) %>%
  arrange(desc(response_ended_within_2h), response_start_datetime) %>%
  slice_head(n = 1) %>%
  ungroup()

duplicate_notifications_to_keep

duplicate_notifications_to_drop <- duplicate_notifications %>%
  anti_join(
    duplicate_notifications_to_keep,
    by = c("lifepak_id", "notification_datetime", "response_start_datetime")
  )

duplicate_notifications_to_drop

# Remove invalid responses manually here
nis_deduplicated <- nis_clean %>%
  anti_join(
    duplicate_notifications_to_drop,
    by = c("lifepak_id", "notification_datetime", "response_start_datetime")
  )


## Remove invalid responses
# LifePak ID 297469 unenrolled from the study and asked to have data removed
# LifePak ID 234803 does not match to Qualtrics, and does not include valid data
nis_valid <- nis_deduplicated %>%
  filter(
    !lifepak_id %in% c("297469", "234803")
  )



####  Save Data  ####
saveRDS(nis_valid, clean_data_staging_intermediate_dir %+% "Phase 1 LifePak Clean Data Without LSMH ID.rds")