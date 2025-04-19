## Track-to-Treat Phase 1 Data Cleaning
## Create clean data release
# R version 4.4.3

####  Startup  ####
## Load packages
library(groundhog) # 3.2.2
groundhog_date <- "2025-03-28"
meta.groundhog(groundhog_date)
groundhog.library(
  pkg = "here",
  date = groundhog_date
)
`%+%` <- paste0


## Load helper function
source(here("Clean Data Release Helper Function.R"))


## Define directories
clean_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\Clean Data (Isaac)\\"
clean_data_staging_dir <- clean_data_dir %+% "staging\\"
clean_data_final_dir <- clean_data_dir %+% "final_read_only\\"



####  Instructions  ####
# Running the function below will prompt you to provide info via the console 
# in order to save versioned copies of the clean data into a versioned folder

# 1. You will first be asked to enter a version number and your first name (e.g., v0.1_Jeremy).
#    - Use numbers < 1 (e.g., 0.1) for development versions and numbers >= 1.0 for versions suitable
#      for analysis. When creating a new version, describe it in the README.md of this repository.

# 2. You will next be asked to enter the date the cleaning code was last modified (e.g., YYYY-MM-DD).
#    The date you run this script (which may be different) will be auto-obtained from your system.

# 3. You will then be asked to confirm that you would like to use this info to:
#    - Create a versioned folder in the "final_read_only" directory (e.g., "YYYY-MM-DD_v0.1_Jeremy"),
#      where the date is the date you ran this script
#    - Save copies of files from the "staging" directory to the versioned folder, prepending the 
#      version info to filenames (e.g., "YYYY-MM-DD_v0.1_Jeremy_Phase 1 LifePak Clean Data.rds"),
#      again where the date is the date you ran this script
#    - Save a README to the versioned folder with version info and a URL to this repository

# DO NOT modify or delete files in the "final_read_only" directory as they may be
# inputs to certain analysis pipelines!



####  Run helper function  ####
create_data_release(clean_data_staging_dir,
                    clean_data_final_dir,
                    staged_filenames = c("Phase 1 Youth Qualtrics Clean Data.rds",
                                         "Phase 1 Youth Qualtrics Clean Data Log.rds",
                                         "Phase 1 Parent Qualtrics Clean Data.rds",
                                         "Phase 1 Parent Qualtrics Clean Data Log.rds",
                                         "Phase 1 LifePak Clean Data.rds"))

# TODO: JE to (a) separate version number from person (and remove person from filename), 
# (b) add raw data versions to README, and (c) consider recording GitHub tag or commit




