## Track-to-Treat Phase 2 Data Cleaning, Youth Qualtrics Data Merging
# R version 4.4.3

####  Startup  ####
## Load packages
library(groundhog) # 3.2.2
groundhog_date <- "2025-03-28"
meta.groundhog(groundhog_date)
groundhog.library(
  pkg = c("tidyverse", "tidylog", "here"),
  date = groundhog_date
)


## Load helper functions
source(here("Qualtrics Data Cleaning Helper Functions.R"))
source(here("Version Control Helper Functions.R"))


## Load Qualtrics data
# Get directories using helper function
dirs <- get_p2_qualtrics_dirs(c("clean_data_staging", "clean_data_staging_intermediate"))

# Load clean data by wave into list
y_clean <- list()

y_clean$yb <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data - Baseline.rds")
y_clean$yi <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data - Intervention.rds")
y_clean$y3m <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data - 3m.rds")
y_clean$y6m <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data - 6m.rds")
y_clean$y12m <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data - 12m.rds")
y_clean$y18m <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data - 18m.rds")
y_clean$y24m <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data - 24m.rds")

# Load logs by wave into list
y_log <- list()

y_log$yb <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data Log - Baseline.rds")
y_log$yi <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data Log - Intervention.rds")
y_log$y3m <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data Log - 3m.rds")
y_log$y6m <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data Log - 6m.rds")
y_log$y12m <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data Log - 12m.rds")
y_log$y18m <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data Log - 18m.rds")
y_log$y24m <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 Youth Qualtrics Clean Data Log - 24m.rds")

# Load LSMH IDs meeting exclusion criteria per youth intervention free-text responses
# - These were identified and exported in "Youth Intervention.R" (see script for details)
exclude_ids <- readRDS(dirs$clean_data_staging_intermediate %+% "Phase 2 LSMH IDs Meeting Free-Text Exclusion Criteria.rds")


## Load item-level codebook using helper function
codebook <- load_p2_codebook(here("Phase 2", "2025.07.02 Track to Treat P2 Codebook.xlsx"))



####  Merge Data  ####
# No IDs in intervention and follow-up surveys not in baseline
lsmh_ids_after_yb <- y_clean %>%
  map(~ .x$lsmh_id) %>%
  unlist() %>%
  unique()

stopifnot(length(setdiff(lsmh_ids_after_yb, y_clean$yb$lsmh_id)) == 0)

# Merge
y_merged <- reduce(
  y_clean,
  full_join,
  by = "lsmh_id",
  relationship = "one-to-one"
)



####  Filter Data  ####
# Drop LSMH IDs meeting exclusion criteria per youth intervention free-text responses
y_merged_filtered <- y_merged %>%
  left_join(
    exclude_ids[c("lsmh_id", "exclude")],
    by = "lsmh_id",
    relationship = "one-to-one"
  ) %>%
  filter(exclude != 1 | is.na(exclude)) %>%
  select(-exclude)



####  Inspect Completion Rates  ####
# Where completion means response is present but not necessarily complete
y_merged_filtered %>%
  count(
    yb_complete = !is.na(yb_complete),
    yi_complete = !is.na(yi_complete),
    y3m_complete = !is.na(y3m_complete),
    y6m_complete = !is.na(y6m_complete),
    y12m_complete = !is.na(y12m_complete),
    y18m_complete = !is.na(y18m_complete),
    y24m_complete = !is.na(y24m_complete),
  ) %>% 
  print(n = Inf)



####  Restructure Log  ####
# Items used to compute item completion rates
item_completion_rate <- lapply(names(y_log), \(prefix) y_log[[prefix]]$item_completion_rate[[prefix]])
names(item_completion_rate) <- names(y_log)

# Items used to compute means
mean_items <- lapply(y_log, \(log) log$mean_items)

# Clean codebooks (edited only at "yb" and yi" to date)
y_codebooks_clean <- list(yb = y_log$yb$yb_codebook_clean,
                          yi = y_log$yi$yi_codebook_clean,
                          y3m = codebook,
                          y6m = codebook,
                          y12m = codebook,
                          y18m = codebook,
                          y24m = codebook)

# Restructured log
y_log_restructured <- mget(c("item_completion_rate", "mean_items", "y_codebooks_clean"))



####  Save Data  ####
# Save data
saveRDS(y_merged_filtered, dirs$clean_data_staging %+% "Phase 2 Youth Qualtrics Clean Data - All Waves.rds")

# Save log
saveRDS(y_log_restructured, dirs$clean_data_staging %+% "Phase 2 Youth Qualtrics Clean Data Log - All Waves.rds")
