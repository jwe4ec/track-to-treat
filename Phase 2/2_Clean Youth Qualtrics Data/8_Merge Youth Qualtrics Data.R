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
yb_clean <- readRDS(clean_data_staging_dir %+% "Phase 2 Youth Qualtrics Clean Data - Baseline.rds")
yi_clean <- readRDS(clean_data_staging_dir %+% "Phase 2 Youth Qualtrics Clean Data - Intervention.rds")
y3m_clean <- readRDS(clean_data_staging_dir %+% "Phase 2 Youth Qualtrics Clean Data - 3m.rds")

# Load logs by wave
yb_log <- readRDS(clean_data_staging_intermediate_dir %+% "Phase 2 Youth Qualtrics Clean Data Log - Baseline.rds")
yi_log <- readRDS(clean_data_staging_intermediate_dir %+% "Phase 2 Youth Qualtrics Clean Data Log - Intervention.rds")
y3m_log <- readRDS(clean_data_staging_intermediate_dir %+% "Phase 2 Youth Qualtrics Clean Data Log - 3m.rds")


## Load ID lookup
id_lookup <- read_csv(here("Phase 2", "2025.08.01 Track to Treat P2 ID Lookup.csv"))


## Load item-level codebook file using helper function
codebook <- load_p2_codebook(here("Phase 2", "2025.07.02 Track to Treat P2 Codebook.xlsx"))



####  Merge Data  ####
# Remove columns as needed
yb_selected <- yb_clean %>%
  select(-item_completion_rate)

yi_selected <- yi_clean %>%
  select(-item_completion_rate)

y3m_selected <- y3m_clean %>%
  select(-c(item_completion_rate, yi_date))

# IDs in intervention and follow-up surveys not in baseline
length(setdiff(c(yi_clean$lsmh_id, y3m_clean$lsmh_id),
               yb_clean$lsmh_id)) == 0

# Merge
y_merged <- yb_selected %>% 
  full_join(
    yi_selected,
    by = "lsmh_id",
    relationship = "one-to-one"
  ) %>%
  full_join(
    y3m_selected,
    by = "lsmh_id",
    relationship = "one-to-one"
  )

# Check duplicated data in primary outcome over time
check_dups_over_time(y_merged, c("yb", "y3m"), "CDI-2 SR")

ids_to_drop <- check_dups_over_time(y_merged, c("yb", "y3m"), "CDI-2 SR") %>%  # TODO: JE to evaluate this after all waves added
  drop_na() %>%
  distinct(lsmh_id)

y_merged_filtered <- y_merged %>%
  anti_join(
    ids_to_drop,
    by = "lsmh_id"
  )

# Completion rates (where completion means response is present but not necessarily complete)
y_merged_filtered %>%
  count(
    !is.na(yb_date),
    !is.na(yi_date),
    !is.na(y3m_date)
  )



####  Merge Logs  ####
# Include clean codebooks (edited only at "yi" to date)
y_log <- list(item_completion_rate = list(yb = yb_log$item_completion_rate$yb,
                                          yi = yi_log$item_completion_rate$yi,
                                          y3m = y3m_log$item_completion_rate$y3m),
              mean_items = c(yb_log$mean_items,
                             yi_log$mean_items,
                             y3m_log$mean_items),
              y_codebooks_clean = list(yb = codebook,
                                       yi = yi_log$yi_codebook_clean,
                                       y3m = codebook))



####  Save Data  ####
# Save data
saveRDS(y_merged_filtered, clean_data_staging_dir %+% "Phase 2 Youth Qualtrics Clean Data - All Waves.rds")

# Save log
saveRDS(y_log, clean_data_staging_dir %+% "Phase 2 Youth Qualtrics Clean Data Log - All Waves.rds")
