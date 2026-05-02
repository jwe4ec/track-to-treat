## Check overlap across Qualtrics, LifePak datasets
# R version 4.4.3

## Load packages
library(groundhog) # 3.2.2
groundhog_date <- "2025-03-28"
meta.groundhog(groundhog_date)
groundhog.library(
  pkg = "here",
  date = groundhog_date
)
`%+%` <- paste0


## Load helper functions
source(here("Directory Helper Functions.R"))


## Load data
# Get directories using helper function
dirs <- get_p1_dirs("clean_data_staging")

# Load clean data
qualtrics_y <- readRDS(file.path(dirs$clean_data_staging, "Phase 1 Youth Qualtrics Clean Data.rds"))
qualtrics_p <- readRDS(file.path(dirs$clean_data_staging, "Phase 1 Parent Qualtrics Clean Data.rds"))
lifepak_y <- readRDS(file.path(dirs$clean_data_staging, "Phase 1 LifePak Clean Data.rds"))


## Check overlap: youth Qualtrics to LifePak
setdiff(qualtrics_y$lifepak_id, lifepak_y$lifepak_id)
# No LifePak IDs in Qualtrics that don't match to LifePak

setdiff(lifepak_y$lifepak_id, qualtrics_y$lifepak_id)
# No LifePak IDs in LifePak that don't match to Qualtrics


## Check overlap: youth Qualtrics to parent Qualtrics
setdiff(qualtrics_y$lsmh_id, qualtrics_p$lsmh_id)
# No youth who do not match to parents

setdiff(qualtrics_p$lsmh_id, qualtrics_y$lsmh_id)
# No parents who do not match to youth
