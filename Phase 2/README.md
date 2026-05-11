# Phase 2 Specifics

This README supplements the [main README](../README.md), which applies
to Phases 1-2, with additional details specific to Phase 2.

## Table of Contents

- [File Organization](#file-organization)
- [Tracking Log Cleaning](#tracking-log-cleaning)
- [LifePak Data Cleaning](#lifepak-data-cleaning)
- [Qualtrics Data Cleaning](#qualtrics-data-cleaning)
- [Outputs](#outputs)

## File Organization

### Private Data

```plaintext
jslab/                                      # JSLAB_DIR_WINDOWS or JSLAB_DIR_UNIX defined in ".Renviron"
|
└── TRACK to TREAT P2/Data/                 # Phase 2 data folder
    ├── README_ttt_p2_data_collection.docx  # Data collection README
    ├── Tracking Log/                       # Raw tracking log
    ├── LifePak/2025.05.21/                 # Raw LifePak data
    ├── Qualtrics/Raw/2026.02.26_final/     # Raw Qualtrics data
    └── Clean Data (Jeremy and Isaac)/
        ├── staging/                        # Clean data staged for release to "final_read_only/
        │   └── intermediate/               # Intermediate data used in data cleaning
        └── final_read_only/                # Versioned releases of clean Phase 2 data
```

### Code

```plaintext
.
├── .Renviron  # Excluded from GitHub; defines JSLAB_DIR_WINDOWS or JSLAB_DIR_UNIX
├── .gitignore  # Used to prevent committing ".Renviron" and other files to GitHub
├── README.md
├── Helper Functions/  # Used across Phases 1-2
|   ├── Directories.R
|   ├── Version Control.R
|   └── Qualtrics Cleaning.R
|
└── Phase 2/  # Phase 2 cleaning
    ├── Raw P2 Metadata.csv
    ├── 2026.04.18 Track to Treat P2 Codebook.xlsx  # Item-level codebook used to clean Qualtrics data
    ├── 1_Clean Tracking Log and Create ID Lookup.R
    ├── 2_Clean Phone Screen Data.R
    ├── 3_Clean LifePak Data.R
    ├── 4_Clean Youth Qualtrics Data/
    │   ├── 1_Correct Codebook and Raw Youth Data.R
    │   ├── 2_Youth Baseline.R
    │   ├── 3_Youth Intervention.R
    │   ├── 4_Youth 3m.R
    │   ├── 5_Youth 6m.R
    │   ├── 6_Youth 12m.R
    │   ├── 7_Youth 18m.R
    │   ├── 8_Youth 24m.R
    │   └── 9_Merge Youth Qualtrics Data.R
    ├── 5_Clean Parent Qualtrics Data/
    │   ├── 1_Correct Raw Parent Data.R
    │   ├── 2_Parent Baseline.R
    │   ├── 3_Parent 3m.R
    │   ├── 4_Parent 6m.R
    │   ├── 5_Parent 12m.R
    │   ├── 6_Parent 18m.R
    │   ├── 7_Parent 24m.R
    │   └── 8_Merge Parent Qualtrics Data.R
    ├── 6_Create Cohort Indicators for Flow and Analysis.R
    ├── 7_Create Clean Data Release.R
    └── QA/  # Scripts checking for issues to clean
        ├── 1_Inspect Raw Qualtrics Data and Codebook.R
        ├── 2_Check Measure Item Metadata Across Waves.R
        └── QA Helper Functions.R
```

## Tracking Log Cleaning

* Phase Sheet of tracking log is cleaned and used to create the following
  * ID lookup of LSMH and LifePak IDs (used to drop vs. keep certain IDs during cleaning)
  * Cohort indicators for participant flowchart and data analysis (see above)

## LifePak Data Cleaning

* Use LSMH ID to refer to unique participants
  * Unlike in Phase 1, multiple LifePak IDs for a given participant are not merged into one LifePak ID
  * Moreover, LifePak ID in youth baseline Qualtrics data is not cleaned
* Phase 2 clean data lacks Phase 1 clean data's `time_of_day` variable
  * This is because in Phase 2 the day and night EMA surveys were both named `"3T Project"`
* Phase 2 clean data's `other` variable is equivalent to Phase 1 clean data's `other_night`
* Considerably more messiness with EMA surveys given larger sample in Phase 2
* Many participants have fewer than 105 notifications, and some have more (redownloaded app)
* Clean data is outputted with and without these free responses (to deidentify in future):
  * `most_pleasant`, `most_unpleasant`, `other`

## Qualtrics Data Cleaning

* `load_p2_codebook()` helper expands repeated-measure items with `[x]` prefix to `b` and `[3-24]m`
* Phone screen (entered by RA with parent on phone) is cleaned before youth/parent data at study waves
* To move certain rows to correct waves, the tasks below are done across waves in `Correct Codebook and Raw Youth Data.R` and `Correct Raw Parent Data.R` before cleaning each wave individually
  * Fix item prefixes in codebook and column names in data
  * Remove extraneous columns (including click, page time variables)
  * Create `_original_dataset` column labeling each row's original survey dataset
  * Recode certain items that interfere with binding rows across waves
  * Manually move certain rows to correct waves
* Self-reported `_date` columns in parent data are overwritten with date from `EndDate` timestamp
* Beck Hopelessness Scale-4 items had different scale in youth intervention survey vs. other surveys
  * Script recodes values from 1-4 to 0-3 for consistency over time (surveys did not display numbers)
  * But wording differences remain:
    * "somewhat false"/"somewhat true" at intervention vs. "sort of false"/"sort of true" elsewhere
* Some items had minor wording differences across time points (see raw codebook for details)
  * Youth item `scared_a_2`: "get" vs. "gets"
  * Youth items `pcsc_1`, `pcsc_7`, and `pcsc_13`: "grades" vs. "marks"
  * Youth item `sitbi_1a`: missing "or" at some waves
  * Youth items `sitbi_2d`, `sitbi_3c`, `sitbi_4c`: missing "is" at some waves
* Surveys retained at each wave (given need to deduplicate surveys within assessment windows):
  * `yb`: Youth who did `yb` survey in window (within 3 weeks before starting EMA) and started EMA
    * Given that `yb` window is based on first EMA notification date (see `Youth Baseline.R`)
  * `yi`: Youth who did `yi` survey in window (within 6 weeks after end of 3-week EMA period)
    * See `Youth Intervention.R`
  * `y3m`-`y24m`: Youth who did given survey in window (3-24 months after ending `yi`, +6 weeks and -1 week) and `yi` survey in window
    * Given that `y3m`-`y24m` windows are based on `yi` end date (see `Youth Intervention.R`)
  * `pb`, `p3m`-`p24m`: Parents whose surveys are in same windows used for `yb` and `y3m`-`y24m`
  * The approximate assessment windows above were reasonably extended from the original windows
* Data collected but not cleaned (see raw codebook for details)
  * Youth
    * Prognostic Pessimism for Depression scale (PPD)
    * Pubertal Development Scale (PDS)
    * Self-Referential Encoding Task (SRET)
  * Parent
    * COVID-19 items
    * Prognostic Pessimism for Depression scale (PPD)

## Outputs

* Clean LifePak and Qualtrics data (at baseline, intervention, and 3-24 months) for valid participants
  * But **further filtering is needed if intent-to-treat (ITT) sample is desired (see below)**
* Cohort indicators for all people who inquired about study
  * See `Phase 2 Cohort Indicators for Flow and Analysis.rds`. Use this to:
    * Create participant flowchart
      * See sample sizes in `Create Cohort Indicators for Flow and Analysis.R`
    * **Filter LSMH IDs to those for whom `analyze_itt_sample` is `TRUE` to get ITT sample**
      * Defined as those randomized but not meeting free-text exclusion criteria