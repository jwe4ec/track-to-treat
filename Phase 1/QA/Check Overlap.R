## Check overlap across Qualtrics, LifePak datasets

## Load data
clean_data_dir <- "R:\\MSS\\Schleider_Lab\\jslab\\TRACK to TREAT\\Data\\Clean Data (Isaac)\\"

qualtrics_y <- readRDS(clean_data_dir %+% "Phase 1 Youth Qualtrics Data.rds")
qualtrics_p <- readRDS(clean_data_dir %+% "Phase 1 Parent Qualtrics Data.rds")
lifepak_y <- readRDS(clean_data_dir %+% "Phase 1 LifePak Data.rds")


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
