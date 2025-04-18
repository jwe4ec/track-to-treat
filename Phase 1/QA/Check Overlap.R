## Check overlap across Qualtrics, LifePak datasets
# R version 4.4.3
`%+%` <- paste0

## Load data
clean_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\Clean Data (Isaac)\\"
clean_data_staging_dir <- clean_data_dir %+% "staging\\"

qualtrics_y <- readRDS(clean_data_staging_dir %+% "Phase 1 Youth Qualtrics Clean Data.rds")
qualtrics_p <- readRDS(clean_data_staging_dir %+% "Phase 1 Parent Qualtrics Clean Data.rds")
lifepak_y <- readRDS(clean_data_staging_dir %+% "Phase 1 LifePak Clean Data.rds")


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
