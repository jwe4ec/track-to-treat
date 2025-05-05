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
source(here("Version Control Helper Functions.R"))


## Define directories
clean_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\Clean Data (Isaac)\\"
clean_data_staging_dir <- clean_data_dir %+% "staging\\"
clean_data_final_dir <- clean_data_dir %+% "final_read_only\\"



####  Instructions  ####
# Running the function below will prompt you to provide this info via the console 
# in order to save versioned copies of the clean data into a versioned folder:

# 1. Version number (e.g., v0.1)
#    - Use numbers < 1 (e.g., 0.1) for development versions and numbers >= 1.0 for 
#      versions suitable for analysis (describe versions in README.md of this repo)

# 2. Your first name (e.g., Jeremy)
#    - With no spaces

# 3. Date the cleaning code was last modified (YYYY-MM-DD)
#    - The date you run this script (may differ) will be obtained from your system

# 4. Confirmation that you would like to use this info to:
#    - Create a folder in "final_read_only" directory (e.g., "YYYY-MM-DD_v0.1_Jeremy")
#      named with date you ran this script
#    - Copy files from "staging" directory to this folder, prepending version info 
#      to filenames (e.g., "YYYY-MM-DD_v0.1_Phase 1 LifePak Clean Data.rds")
#    - Save a README.txt to the folder with version info and a URL to this repo

# DO NOT modify or delete files in the "final_read_only" directory as they may be
# inputs to certain analysis pipelines!



####  Run helper function  ####
create_data_release(clean_data_staging_dir,
                    clean_data_final_dir,
                    staged_filenames = c("Phase 1 Youth Qualtrics Clean Data.rds",
                                         "Phase 1 Youth Qualtrics Clean Data Log.rds",
                                         "Phase 1 Parent Qualtrics Clean Data.rds",
                                         "Phase 1 Parent Qualtrics Clean Data Log.rds",
                                         "Phase 1 LifePak Clean Data.rds",
                                         "Phase 1 LifePak Clean Data Without Free Responses.rds"))