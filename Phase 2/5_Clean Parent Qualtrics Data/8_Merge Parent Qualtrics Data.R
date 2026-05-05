## Track-to-Treat Phase 2 Data Cleaning, Parent Qualtrics Data Merging
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
p_clean <- list()

p_clean$pb   <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Parent Qualtrics Clean Data - Baseline.rds"))
p_clean$p3m  <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Parent Qualtrics Clean Data - 3m.rds"))
p_clean$p6m  <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Parent Qualtrics Clean Data - 6m.rds"))
p_clean$p12m <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Parent Qualtrics Clean Data - 12m.rds"))
p_clean$p18m <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Parent Qualtrics Clean Data - 18m.rds"))
p_clean$p24m <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Parent Qualtrics Clean Data - 24m.rds"))

# Load logs by wave into list
p_log <- list()

p_log$pb   <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Parent Qualtrics Clean Data Log - Baseline.rds"))
p_log$p3m  <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Parent Qualtrics Clean Data Log - 3m.rds"))
p_log$p6m  <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Parent Qualtrics Clean Data Log - 6m.rds"))
p_log$p12m <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Parent Qualtrics Clean Data Log - 12m.rds"))
p_log$p18m <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Parent Qualtrics Clean Data Log - 18m.rds"))
p_log$p24m <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Parent Qualtrics Clean Data Log - 24m.rds"))

# Load youth intervention data, for `condition`
yi_clean <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Youth Qualtrics Clean Data - Intervention.rds"))


## Load corrected item-level codebook
codebook <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 Qualtrics Corrected Codebook.rds"))



####  Merge Data  ####
# No IDs in follow-up surveys not in baseline
lsmh_ids_after_pb <- p_clean %>%
  map(~ .x$lsmh_id) %>%
  unlist() %>%
  unique()

stopifnot(length(setdiff(lsmh_ids_after_pb, p_clean$pb$lsmh_id)) == 0)

# Get condition
yi_condition <- yi_clean %>%
  select(lsmh_id, condition)

# Merge
p_merged <- reduce(
  .init = yi_condition,
  .x = p_clean,
  full_join,
  by = "lsmh_id",
  relationship = "one-to-one"
)



####  Inspect Completion Rates  ####
# Where completion means response is present but not necessarily complete
p_merged %>%
  count(
    pb_complete = !is.na(pb_complete),
    p3m_complete = !is.na(p3m_complete),
    p6m_complete = !is.na(p6m_complete),
    p12m_complete = !is.na(p12m_complete),
    p18m_complete = !is.na(p18m_complete),
    p24m_complete = !is.na(p24m_complete),
  ) %>% 
  print(n = Inf)



####  Restructure Log  ####
# Items used to compute item completion rates
item_completion_rate <- lapply(names(p_log), \(prefix) p_log[[prefix]]$item_completion_rate[[prefix]])
names(item_completion_rate) <- names(p_log)

# Items used to compute means and counts
mean_items <- lapply(p_log, \(log) log$mean_items)
count_items <- lapply(p_log, \(log) log$count_items)

# Clean codebook
# - Corrected in "Correct Codebook and Raw Youth Data.R", with no further parent edits to date
p_codebook_clean <- codebook

# Restructured log
p_log_restructured <- mget(c("item_completion_rate", "mean_items", "count_items", "p_codebook_clean"))



####  Save Data  ####
# Save data
# - Note: To analyze intent-to-treat sample, filter LSMH IDs per "analyze_itt_sample" in
#   "Phase 2 Cohort Indicators for Flow and Analysis.rds"
saveRDS(p_merged, file.path(dirs$clean_data_staging, "Phase 2 Parent Qualtrics Clean Data - All Waves.rds"))

# Save log
saveRDS(p_log_restructured, file.path(dirs$clean_data_staging, "Phase 2 Parent Qualtrics Clean Data Log - All Waves.rds"))
