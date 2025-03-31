## Compare my clean LifePak data to Michael's
# R version 4.4.3
`%+%` <- paste0
library(groundhog) # 3.2.2
groundhog.library(
  pkg = "tidyverse",
  date = "2025-03-28"
)

## Load data
# Mine
clean_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\Clean Data (Isaac)\\"
nis_valid <- readRDS(clean_data_dir %+% "Phase 1 LifePak Data.rds")
lp_me <- nis_valid %>%
  mutate(lifepak_id = as.numeric(lifepak_id))

# Michael's
lp_mi <- read.csv("R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\Processed Data\\2022 From Michael Mullarkey\\deid_cleaned_lifepak_ttt_phase_1.csv") %>%
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
