## Track-to-Treat Phase 1 Data Cleaning
## Add LSMH ID to LifePak Data
# R version 4.4.3

####  Startup  ####
## Load packages
library(groundhog) # 3.2.2
groundhog_date <- "2025-03-28"
meta.groundhog(groundhog_date)
groundhog.library(
  pkg = "tidyverse",
  date = groundhog_date
)
`%+%` <- paste0


## Load data
# Save directory
clean_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\Clean Data (Isaac)\\"
clean_data_staging_dir <- clean_data_dir %+% "staging\\"
clean_data_staging_intermediate_dir <- clean_data_staging_dir %+% "intermediate\\"

# Load intermediate LifePak data and clean Qualtrics data
nis_valid <- readRDS(clean_data_staging_intermediate_dir %+% "Phase 1 LifePak Clean Data Without LSMH ID.rds")
clean_qualtrics_data <- readRDS(clean_data_staging_dir %+% "Phase 1 Youth Qualtrics Clean Data.rds")



####  Add LSMH ID to LifePak Data  ####
lsmh_id_lookup <- clean_qualtrics_data %>%
  distinct(lifepak_id, lsmh_id)

nis_valid_with_lsmh_id <- nis_valid %>%
  left_join(lsmh_id_lookup, by = "lifepak_id", relationship = "many-to-one")



####  Save Clean LifePak Data  ####
saveRDS(nis_valid_with_lsmh_id, clean_data_staging_dir %+% "Phase 1 LifePak Clean Data.rds")