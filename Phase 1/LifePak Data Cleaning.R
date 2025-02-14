## Track-to-Treat Phase 1 Data Cleaning
## LifePak data
# R version 4.1.2

####  Startup  ####
## Load packages
library(tidyverse) # 2.0.0
library(lubridate) # 1.9.3
`%+%` <- paste0


## Load data
# Save directory
raw_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\LifePak Raw Data (Do Not Modify)\\"
clean_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\Clean Data (Isaac)\\"

# Load NIS ("notification-initiated survey") datasets
nis_1 <- read.csv(raw_data_dir %+% "3T_P1_V1_NIS_2020_Mar_02.csv")
nis_2 <- read.csv(raw_data_dir %+% "3T_P1_V2_NIS_2020_Mar_13.csv")
nis_3 <- read.csv(raw_data_dir %+% "3T_P1_V2_NIS_21200_958251_Download2.csv")
nis_4 <- read.csv(raw_data_dir %+% "3T_P1_V2_NIS_21200_958251_Download3.csv")
nis_5 <- read.csv(raw_data_dir %+% "3T_P1_V4_NIS.csv")



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
nis_clean <- nis_combined %>%
  mutate(
    
    ## Metadata
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
    
    # Notification date and datetime
    notification_datetime = as_datetime(Notification.Time),
    notification_date = as_date(notification_datetime),
    
    # Response indicator (logical)
    responded = Responded == "1",
    
    # Response lag
    response_lag_seconds = as.difftime(Session.Instance.Response.Lapse),
    responded_in_2h_or_less = if_else(
      responded,
      response_lag_seconds <= 7200,
      F
    ),

    # Response date and datetime
    response_datetime = notification_datetime + response_lag_seconds,
    response_date = as_date(response_datetime),

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
    
    # Across EMA variables, if response did not come in time, recode as NA
    across(
      all_of(c("sad", "bad", "interest", "energy", "focus", "movement", "control", "fun")),
      ~ if_else(
        responded_in_2h_or_less,
        .,
        NA_real_
      )
    ),
    
    fun_rev = 100 - fun # Creating a reverse-coded item so that there is a set of 8 items all representing dysfunction
    
  ) %>%
  select(
    
    # Metadata
    lifepak_id,
    survey_type,
    time_of_day,
    notification_date,
    notification_datetime,
    responded,
    response_date,
    response_datetime,
    response_lag_seconds,
    responded_in_2h_or_less,
    
    # Response data
    sad, bad, interest, energy, focus, movement, control, fun, fun_rev,
    
    # Most pleasant and most unpleasant event from the day
    most_pleasant = best_night, 
    most_unpleasant = worst_night
    
  ) %>%
  
  arrange(
    
    lifepak_id,
    notification_datetime
    
  )


## Correct lifepak_id
# lifepak_id 958251 (LSMH00347) is also 034516 and 878753
nis_clean$lifepak_id[nis_clean$lifepak_id == "034516"] <- "958251"
nis_clean$lifepak_id[nis_clean$lifepak_id == "878753"] <- "958251"


## Deduplicate
# No duplicate responses
nis_clean %>%
  count(lifepak_id, response_datetime) %>%
  drop_na() %>% 
  filter(n > 1)

# Three notifications appear twice; in all cases, there is only one valid response
nis_clean %>% 
  count(lifepak_id, notification_datetime) %>% 
  filter(n > 1) %>%
  left_join(nis_clean)

# Remove invalid responses manually here
nis_deduplicated <- nis_clean %>%
  filter(
    !(lifepak_id == "816917" & response_datetime %in% as_datetime("2020-10-11 18:24:54")),
    !(lifepak_id == "905207" & response_datetime %in% as_datetime("2020-11-28 11:20:56")),
    !(lifepak_id == "958251" & response_datetime %in% as_datetime("2020-03-31 11:21:46"))
  )


## Remove invalid responses
# LifePak ID 297469 unenrolled from the study and asked to have data removed
# LifePak ID 234803 does not match to Qualtrics, and does not include valid data
nis_valid <- nis_deduplicated %>%
  filter(
    !lifepak_id %in% c("297469", "234803")
  )



####  Save Data  ####
saveRDS(nis_valid, clean_data_dir %+% "Phase 1 LifePak Data.rds")
