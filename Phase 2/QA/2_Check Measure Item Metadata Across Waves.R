####  Startup  ####
# R version 4.4.3

## Load packages
library(groundhog) # 3.2.2
groundhog_date <- "2025-03-28"
meta.groundhog(groundhog_date)
groundhog.library(
  pkg = c("tidyverse", "tidylog", "lubridate", "qualtRics", "openxlsx", "here", "digest"),
  date = groundhog_date
)


## Load helper functions
source(here("Helper Functions", "Directories.R"))
source(here("Helper Functions", "Qualtrics Cleaning.R"))
source(here("Phase 2", "QA", "QA Helper Functions.R"))


## Load data
# Get directories using helper function
dirs <- get_p2_dirs("clean_data_staging_intermediate")

# List of raw youth and parent Qualtrics data
dat_ls <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 QA List of Raw Qualtrics Data.rds"))

# List of raw youth and parent Qualtrics data columns by type
dat_ls_cols <- readRDS(file.path(dirs$clean_data_staging_intermediate, "Phase 2 QA List of Raw Qualtrics Data Columns by Type.rds"))


## Load item-level codebook
codebook <- load_p2_codebook(here("Phase 2", "2026.04.18 Track to Treat P2 Codebook.xlsx"))



####  Check if stems of measure item column names are same across waves  ####
### Get prefixes and stems of measure items for youth and parent surveys 
meas_item_cols_prefixes_stems <- lapply(dat_ls_cols, function(dat_cols) {
  meas_item_cols <- dat_cols$meas_item_cols
  
  # Identify columns lacking prefixes
  cols_lacking_prefixes <- meas_item_cols[!grepl("_", meas_item_cols)]
  cols_with_prefixes <- setdiff(meas_item_cols, cols_lacking_prefixes)
  
  # Identify prefixes as everything before first underscore (if present)
  prefixes <- unique(sub("_.*", "", cols_with_prefixes))
  
  # Identify stems as everything after first underscore (or full name if no underscore)
  stems <- sub("^[^_]+_", "", meas_item_cols)

  return(list(
    cols_lacking_prefixes = cols_lacking_prefixes,
    prefixes = prefixes,
    stems = stems)
  )
})

y_meas_item_cols_prefixes_stems <- meas_item_cols_prefixes_stems[grepl("^y", names(meas_item_cols_prefixes_stems))]
p_meas_item_cols_prefixes_stems <- meas_item_cols_prefixes_stems[grepl("^p", names(meas_item_cols_prefixes_stems))]


### Check prefixes (already checked, with corrections documented, in other QA script, but keep both checks)
## Check for columns lacking prefixes
lapply(y_meas_item_cols_prefixes_stems, \(x) x$cols_lacking_prefixes)
lapply(p_meas_item_cols_prefixes_stems, \(x) x$cols_lacking_prefixes)

# In "yb", SRET items lack prefixes
sret_items <- c("SRET", "SRET.words", "SRET.keys", "SRET.time", "tlcond")

stopifnot(setequal(y_meas_item_cols_prefixes_stems$yb_raw$cols_lacking_prefixes, sret_items))


## Check for unexpected prefixes
y_prefixes_by_wave <- lapply(y_meas_item_cols_prefixes_stems, \(x) x$prefixes)
p_prefixes_by_wave <- lapply(p_meas_item_cols_prefixes_stems, \(x) x$prefixes)

y_prefixes <- unique(unlist(y_prefixes_by_wave))
p_prefixes <- unique(unlist(p_prefixes_by_wave))

expected_y_prefixes <- c("yb", "yi", paste0("y", c(3, 6, 12, 18, 24), "m"))
expected_p_prefixes <- c("pb",       paste0("p", c(3, 6, 12, 18, 24), "m"))

unexpected_y_prefixes <- setdiff(y_prefixes, expected_y_prefixes)
unexpected_p_prefixes <- setdiff(p_prefixes, expected_p_prefixes)

# Unexpected prefixes in youth data
stopifnot(setequal(unexpected_y_prefixes, c("b", "y312", "y18n")))

# Unexpected prefixes in parent data
stopifnot(setequal(unexpected_p_prefixes, c("p3m\020"))) 


### Confirm that all measure items within a given wave are unique
stopifnot(
  all(sapply(y_meas_item_cols_prefixes_stems, \(x) length(x$stems) == length(unique(x$stems)))),
  all(sapply(p_meas_item_cols_prefixes_stems, \(x) length(x$stems) == length(unique(x$stems))))
)


### Confirm that measure item stems are same across waves
## Compute number of waves in which each stem is present (using helper function)
y_stem_wave_counts_df <- compute_stem_wave_counts(y_meas_item_cols_prefixes_stems)
p_stem_wave_counts_df <- compute_stem_wave_counts(p_meas_item_cols_prefixes_stems)


## For youth, inspect number of waves in which each stem is present
# As expected, the only stems in 1 wave are those assessed only at "yb" or "yi"
yb_only_stems <- sret_items

yi_only_pattern <- paste(paste0("^",
                                c("perc_change", "pfs", "pre_bhs", "post_bhs", 
                                  "pre_iptq", "post_iptq", "pre_shs", "post_shs"),
                                "_"),
                         collapse = "|")

yi_only_stems <- grep(yi_only_pattern, y_stem_wave_counts_df$stem, value = TRUE)

stopifnot(
  setequal(yb_only_stems,
           with(y_stem_wave_counts_df, stem[n_present_waves == 1 & present_waves == "yb_raw"])),
  setequal(yi_only_stems,
           with(y_stem_wave_counts_df, stem[n_present_waves == 1 & present_waves == "yi_raw"]))
)

# As expected, all other stems are in 6 waves (not "yi") out of the 7 youth waves
# - Exception: Stems for the 9 BADS-SF items that were assessed only at "yi" (but have the same
#   stems as 9 of the 25 full BADS items assessed at all 6 other waves) are in all 7 waves
#   - Thus, exclude BADS-SF stems at "yi" when checking labels and classes in sections below
bads_sf_stems <- paste0("bads_", 1:9)

y_other_stems <- setdiff(y_stem_wave_counts_df$stem, c(yb_only_stems, yi_only_stems, bads_sf_stems))

stopifnot(
  setequal(y_other_stems,
           with(y_stem_wave_counts_df, stem[n_present_waves == 6 & absent_waves == "yi_raw"])),
  setequal(bads_sf_stems,
           with(y_stem_wave_counts_df, stem[n_present_waves == 7 & grepl("yi_raw", present_waves)]))
)


## For parents, inspect number of waves in which each stem is present
# As expected, the only stems in 1 wave are those assessed only at "pb"
# - Note: 3 of the 11 "caregiver1" stems were repeated across waves
repeated_caregiver1_stems <- c("caregiver1_1", "caregiver1_3", "caregiver1_3_10_TEXT")

pb_only_caregiver1_stems <- c("caregiver1_2", "caregiver1_4", "caregiver1_4_8_TEXT", "caregiver1_5", 
                              "caregiver1_5_5_TEXT", "caregiver1_6", "caregiver1_7", "caregiver1_8")

pb_only_pattern <- paste(paste0("^",
                                c("caregiver2", "child_aces", "covid", "parent_aces", "siblings"),
                                "_"),
                         collapse = "|")

pb_only_stems <- c(
  "birthorder", "caretaker", "caretaker_living", "childethnicity", "childethnicity_8_TEXT", "childsex",
  "dependent", "grade", "grade_13_TEXT", "income", "online_tx1", "online_tx2", "school", "school_7_TEXT", 
  "siblings_1", "siblings_2", "single_parent", "teletherapy1", "teletherapy2", pb_only_caregiver1_stems,
  grep(pb_only_pattern, p_stem_wave_counts_df$stem, value = TRUE)
)

stopifnot(
  setequal(pb_only_stems,
           with(p_stem_wave_counts_df, stem[n_present_waves == 1 & present_waves == "pb_raw"]))
)

# As expected, the only stem in 5 waves (not "pb") out of the 6 parent waves is "childtx_change"
stopifnot(
  setequal("childtx_change",
           with(p_stem_wave_counts_df, stem[n_present_waves == 5 & absent_waves == "pb_raw"]))
)

# As expected, all other stems are in all 6 parent waves
# - Exception: Inconsistent "accommodations_2" stems (see other QA script) are each in 2 waves
accommodations_2_stems <- c("accom_2", "accommodations_", "accommodations_2")

p_other_stems <- setdiff(p_stem_wave_counts_df$stem, c(pb_only_stems, "childtx_change", accommodations_2_stems))

stopifnot(
  setequal(p_other_stems,
           with(p_stem_wave_counts_df, stem[n_present_waves == 6])),
  setequal(accommodations_2_stems,
           with(p_stem_wave_counts_df, stem[n_present_waves == 2]))
)



####  Check if labels of repeated-measure items are same across waves  ####
### Get labels (and classes) of measure items and name by measure item stems
meas_item_col_lbls_clss <- lapply(names(dat_ls), function(dat_name) {
  dat                 <- dat_ls[[dat_name]]
  meas_item_cols      <- dat_ls_cols[[dat_name]]$meas_item_cols
  
  # As above, identify stems as everything after first underscore (or full name if no underscore)
  meas_item_col_stems <- sub("^[^_]+_", "", meas_item_cols)
  
  out <- list()
  
  out$lbls <- lapply(meas_item_cols, \(col) attr(dat[[col]], "label"))
  out$clss <- lapply(meas_item_cols, \(col) class(dat[[col]]))
  
  names(out$lbls) <- meas_item_col_stems
  names(out$clss) <- meas_item_col_stems
  
  return(out)
})
names(meas_item_col_lbls_clss) <- names(dat_ls)

y_meas_item_col_lbls_clss <- meas_item_col_lbls_clss[grepl("^y", names(meas_item_col_lbls_clss))]
p_meas_item_col_lbls_clss <- meas_item_col_lbls_clss[grepl("^p", names(meas_item_col_lbls_clss))]


### Confirm that all measure item labels within a given survey are unique
stopifnot(
  all(sapply(y_meas_item_col_lbls_clss, \(wave) length(wave$lbls) == length(unique(wave$lbls)))),
  all(sapply(p_meas_item_col_lbls_clss, \(wave) length(wave$lbls) == length(unique(wave$lbls))))
)


### Check labels for repeated-measure items (i.e., those present in > 1 wave)
## Create data frames of repeated-measure items with different raw labels across waves (using helper)
# - Inspection of these reveals need to clean labels to remove trivial differences (done below)
y_diff_lbl_df <- create_diff_repeated_meas_item_lbl_df(y_meas_item_col_lbls_clss, y_stem_wave_counts_df)
p_diff_lbl_df <- create_diff_repeated_meas_item_lbl_df(p_meas_item_col_lbls_clss, p_stem_wave_counts_df)


## Clean labels to remove trivial differences
y_meas_item_col_lbls_clss_clean <- lapply(y_meas_item_col_lbls_clss, function(wave) {
  wave_lbls <- wave$lbls
  wave_item_stems <- names(wave_lbls)
  
  # Remove any extra whitespace (e.g., spaces, tabs, newlines) and any leading/trailing whitespace
  wave_lbls <- lapply(wave_lbls, \(x) trimws(gsub("\\s+", " ", x)))
  
  # Remove survey-specific prefixes from labels for SCARED items and SITBI Item 3b
  prefix_stems <- grep("^(scared|sitbi_3b)_", wave_item_stems, value = TRUE)
  wave_lbls[prefix_stems] <- lapply(wave_lbls[prefix_stems], \(x) sub("^(yb|y\\d+m)_", "", x))
  
  wave$lbls <- wave_lbls
  
  return(wave)
})

p_meas_item_col_lbls_clss_clean <- lapply(p_meas_item_col_lbls_clss, function(wave) {
  wave_lbls <- wave$lbls
  wave_item_stems <- names(wave_lbls)
  
  # Remove any extra whitespace (e.g., spaces, tabs, newlines) and any leading/trailing whitespace
  wave_lbls <- lapply(wave_lbls, \(x) trimws(gsub("\\s+", " ", x)))
  
  # Remove survey-specific prefixes from labels for SCARED items
  prefix_stems <- grep("^scared_", wave_item_stems, value = TRUE)
  wave_lbls[prefix_stems] <- lapply(wave_lbls[prefix_stems], \(x) sub("^(pb|p\\d+m)_", "", x))
  
  # Remove "If YES" (only present at baseline) from labels for "childmeds_2" items
  if_yes_stems <- grep("^childmeds_2_", wave_item_stems, value = TRUE)
  wave_lbls[if_yes_stems] <- lapply(wave_lbls[if_yes_stems], \(x) sub("^If YES, p", "P", x))
  
  wave$lbls <- wave_lbls
  
  return(wave)
})


## Use clean labels to recreate data frames of repeated-measure items with different labels across waves
y_diff_clean_lbl_df <- create_diff_repeated_meas_item_lbl_df(y_meas_item_col_lbls_clss_clean, y_stem_wave_counts_df)
p_diff_clean_lbl_df <- create_diff_repeated_meas_item_lbl_df(p_meas_item_col_lbls_clss_clean, p_stem_wave_counts_df)


## Inspect labels that differ across waves (using helper)
# For youth, differences are due to (a) minor typos (some SCARED, SCSC, and SITBI items)
# or (b) referring to "grades" versus "marks" at some time points (some PCSC items)
# - Noted wording differences in README and documented details in raw codebook

y_clean_lbl_diffs <- list(
  typo_get_vs_gets      = get_diff_lbls(y_diff_clean_lbl_df, "scared_a_2"),
  typo_miss_or          = get_diff_lbls(y_diff_clean_lbl_df, "sitbi_1a"),
  typo_miss_is          = get_diff_lbls(y_diff_clean_lbl_df, c("sitbi_2d", "sitbi_3c", "sitbi_4c")),
  typo_miss_punctuation = get_diff_lbls(y_diff_clean_lbl_df, c("scared_c_9", "scsc_4",
                                                               "sitbi_3b_1", "sitbi_3b_2", "sitbi_3b_3")),
  
  grades_vs_marks       = get_diff_lbls(y_diff_clean_lbl_df, c("pcsc_1", "pcsc_7", "pcsc_13"))
)

stopifnot(
  setequal(unname(unlist(lapply(y_clean_lbl_diffs, names))),
           y_diff_clean_lbl_df$stem)
)

# For parents, no differences
stopifnot(nrow(p_diff_clean_lbl_df) == 0)


## Manually confirm that labels for various "accommodations_2" stems are same across waves
##   (given that the code above checks within each stem but not across stems)
accommodations_2_regex <- paste0("^p.*(", paste0(accommodations_2_stems, collapse = "|"), ")$")
a2_lbls <- check_item_pattern_label(dat_ls[grep("^p", names(dat_ls))], accommodations_2_regex)

stopifnot(length(unique(unname(unlist(a2_lbls)))) == 1)


####  Check if classes of repeated-measure items are same across waves  ####
## Get stems with different classes across waves (using helper)
y_stems_diff_clss <- get_stems_diff_clss(y_meas_item_col_lbls_clss, y_stem_wave_counts_df)
p_stems_diff_clss <- get_stems_diff_clss(p_meas_item_col_lbls_clss, p_stem_wave_counts_df)

stopifnot(
  setequal(y_stems_diff_clss, c("sitbi_3b_2", "sitbi_3b_3", "sitbi_3b_4", "sitbi_4b_3","sitbi_4b_4")),
  setequal(p_stems_diff_clss, "caregiver1_3_10_TEXT")
)


## Inspect classes that differ across waves (using helper)
# For youth, differences are due to character responses at some waves (SITBI 3b and 4b items above)
# - Recoded responses in "Correct Codebook and Raw Youth Data.R"
get_clss(y_meas_item_col_lbls_clss, y_stems_diff_clss)

# For parents, differences are due to all NAs at some waves for "caregiver1_3_10_TEXT"
# - Recoded to character in "Correct Raw Parent Data.R"
get_clss(p_meas_item_col_lbls_clss, p_stems_diff_clss)


## Manually confirm that classes for various "accommodations_2" stems are same across waves
##   (given that the code above checks within each stem but not across stems)
a2_clss <- get_clss(p_meas_item_col_lbls_clss, accommodations_2_stems)

stopifnot(length(unique(unname(unlist(a2_clss)))) == 1)
