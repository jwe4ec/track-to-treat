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
source(here("Helper Functions", "Directories.R"))
source(here("Helper Functions", "Version Control.R"))
source(here("Helper Functions", "Qualtrics Cleaning.R"))


## Load Qualtrics data
# Get directories using helper function
dirs <- get_p2_dirs(c("clean_data_staging", "clean_data_staging_intermediate"))

# Load clean data by wave into list
y_clean <- list()

y_clean$yb   <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Youth Qualtrics Clean Data - Baseline.rds"))
y_clean$yi   <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Youth Qualtrics Clean Data - Intervention.rds"))
y_clean$y3m  <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Youth Qualtrics Clean Data - 3m.rds"))
y_clean$y6m  <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Youth Qualtrics Clean Data - 6m.rds"))
y_clean$y12m <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Youth Qualtrics Clean Data - 12m.rds"))
y_clean$y18m <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Youth Qualtrics Clean Data - 18m.rds"))
y_clean$y24m <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Youth Qualtrics Clean Data - 24m.rds"))

# Load logs by wave into list
y_log <- list()

y_log$yb   <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Youth Qualtrics Clean Data Log - Baseline.rds"))
y_log$yi   <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Youth Qualtrics Clean Data Log - Intervention.rds"))
y_log$y3m  <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Youth Qualtrics Clean Data Log - 3m.rds"))
y_log$y6m  <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Youth Qualtrics Clean Data Log - 6m.rds"))
y_log$y12m <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Youth Qualtrics Clean Data Log - 12m.rds"))
y_log$y18m <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Youth Qualtrics Clean Data Log - 18m.rds"))
y_log$y24m <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Youth Qualtrics Clean Data Log - 24m.rds"))


## Load corrected item-level codebook
codebook <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Qualtrics Corrected Codebook.rds"))



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



####  Inspect Completion Rates  ####
# Where completion means response is present but not necessarily complete
y_merged %>%
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

# Clean codebook
# - Corrected in "Correct Codebook and Raw Youth Data.R", with further youth edits
# only in "Youth Intervention.R" to date
y_codebook_clean <- y_log$yi$yi_codebook_clean

# Restructured log
y_log_restructured <- mget(c("item_completion_rate", "mean_items", "y_codebook_clean"))



####  Save Data  ####
# Save data
# - Note: To analyze intent-to-treat sample, filter LSMH IDs per "analyze_itt_sample" in
#   "Phase 2 Cohort Indicators for Flow and Analysis.rds"
saveRDS(y_merged, file.path(dirs$clean_data_staging, "Phase 2 Youth Qualtrics Clean Data - All Waves.rds"))

# Save log
saveRDS(y_log_restructured, file.path(dirs$clean_data_staging, "Phase 2 Youth Qualtrics Clean Data Log - All Waves.rds"))
