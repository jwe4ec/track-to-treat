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
`%+%` <- paste0


## Load helper functions
source(here("Qualtrics Data Cleaning Helper Functions.R"))
source(here("Version Control Helper Functions.R"))


## Load Qualtrics data
# Save directories
clean_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT P2\\Data\\Clean Data (Isaac)\\"
clean_data_staging_dir <- clean_data_dir %+% "staging\\"
clean_data_staging_intermediate_dir <- clean_data_staging_dir %+% "intermediate\\"

# Load clean data by wave
pb_clean <- readRDS(clean_data_staging_dir %+% "Phase 2 Parent Qualtrics Clean Data - Baseline.rds")
p3m_clean <- readRDS(clean_data_staging_dir %+% "Phase 2 Parent Qualtrics Clean Data - 3m.rds")

# Load logs by wave
pb_log <- readRDS(clean_data_staging_intermediate_dir %+% "Phase 2 Parent Qualtrics Clean Data Log - Baseline.rds")
p3m_log <- readRDS(clean_data_staging_intermediate_dir %+% "Phase 2 Parent Qualtrics Clean Data Log - 3m.rds")

# Load youth intervention data, for `condition`
yi_clean <- readRDS(clean_data_staging_dir %+% "Phase 2 Youth Qualtrics Clean Data - Intervention.rds")


## Load item-level codebook file using helper function
codebook <- load_p2_codebook(here("Phase 2", "2025.07.02 Track to Treat P2 Codebook.xlsx"))



####  Merge Data  ####
# Remove columns as needed
pb_selected <- pb_clean %>%
  select(-item_completion_rate)

p3m_selected <- p3m_clean %>%
  select(-c(item_completion_rate, yi_date))

yi_selected <- yi_clean %>%
  select(lsmh_id, condition)

# Merge
p_merged <- yi_selected %>%
  full_join(
    pb_selected,
    by = "lsmh_id",
    relationship = "one-to-one"
  ) %>%
  full_join(
    p3m_selected,
    by = "lsmh_id",
    relationship = "one-to-one"
  )

# Check duplicated data in primary outcome over time
check_dups_over_time(p_merged, c("pb", "p3m"), "CDI-2 P")

ids_to_drop <- check_dups_over_time(p_merged, c("pb", "p3m"), "CDI-2 P") %>%
  drop_na() %>%
  distinct(lsmh_id)

p_merged_filtered <- p_merged %>%
  anti_join(
    ids_to_drop,
    by = "lsmh_id"
  )

# Completion rates
p_merged_filtered %>%
  count(
    !is.na(pb_date),
    !is.na(p3m_date)
  )



####  Merge Logs  ####
# Include codebook (unedited to date)
p_log <- list(item_completion_rate = list(pb = pb_log$item_completion_rate$pb,
                                          p3m = p3m_log$item_completion_rate$p3m),
              mean_items = c(pb_log$mean_items,
                             p3m_log$mean_items),
              count_items = pb_log$count_items,
              p_codebook_clean = codebook)



####  Save Data  ####
# Save data
saveRDS(p_merged_filtered, clean_data_staging_dir %+% "Phase 2 Parent Qualtrics Clean Data - All Waves.rds")

# Save log
saveRDS(p_log, clean_data_staging_dir %+% "Phase 2 Parent Qualtrics Clean Data Log - All Waves.rds")
