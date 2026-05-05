## Compare my clean LifePak data to Michael's
# R version 4.4.3

## Load packages
library(groundhog) # 3.2.2
groundhog_date <- "2025-03-28"
meta.groundhog(groundhog_date)
groundhog.library(
  pkg = c("tidyverse", "here"),
  date = groundhog_date
)
`%+%` <- paste0


## Load helper functions
source(here("Helper Functions", "Directories.R"))


## Load data
# Get directories using helper function
dirs <- get_p1_dirs("clean_data_staging")
jslab_dir <- get_jslab_dir()

# Mine
nis_valid <- readRDS(file.path(dirs$clean_data_staging, "Phase 1 LifePak Clean Data.rds"))
lp_me <- nis_valid %>%
  mutate(lifepak_id = as.numeric(lifepak_id))

# Michael's
lp_mi <- read.csv(file.path(jslab_dir, "TRACK to TREAT", "Data",
                            "Processed Data", "2022 From Michael Mullarkey",
                            "deid_cleaned_lifepak_ttt_phase_1.csv")) %>%
  mutate(
    notification_datetime = as_datetime(notification_time),
    response_datetime = as_datetime(response_time)
  )


## Compare LifePak IDs
ids_me <- unique(lp_me$lifepak_id)
ids_mi <- unique(lp_mi$lifepak_id)

setdiff(ids_mi, ids_me) # 34516 [this should have been changed to 958251, per readme_ttt_p1]
setdiff(ids_me, ids_mi) # None


## Compare rows
test <- full_join(
  lp_me %>%
    mutate(in_dataset = T),
  lp_mi %>%
    mutate(in_dataset = T),
  by = c("lifepak_id", "notification_datetime"),
  suffix = c(".me", ".michael")
)

test %>%
  count(in_dataset.me, in_dataset.michael)


## Compare values
for(x in c("sad", "bad", "interest", "energy", "focus", "movement", "control", "fun")) {
  
  match <- test[[x %+% ".me"]] == test[[x %+% ".michael"]]
  
  print("Variable: " %+% x)
  
  print("Match rate: " %+% scales::percent(mean(match, na.rm = T)))
  
}
