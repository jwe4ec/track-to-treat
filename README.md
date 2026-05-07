# track-to-treat

Centralized data cleaning code for Project Track to Treat (TTT)

## Table of Contents

- [Citation](#citation)
- [Project Overview](#project-overview)
- [File Organization](#file-organization)
- [General Approach](#general-approach)
- [Tracking Log Cleaning](#tracking-log-cleaning)
- [LifePak Data Cleaning](#lifepak-data-cleaning)
- [Qualtrics Data Cleaning](#qualtrics-data-cleaning)
- [Outputs](#outputs)
- [Version Control](#version-control)
  - [Releases](#releases)

## Citation

When using a given [release][#releases] of the cleaning code or associated clean data, please cite the version number and Version DOI for the release. For the full citation including this version information, see the [Release Notes][releases].

## Project Overview

Phase 1 of TTT aims to use parameters from network models estimated from ecological momentary assessment (EMA) data to predict 3-month changes in depression symptoms and related constructs in depressed adolescents.

- Phase 1 was an observational study and included no intervention.
- After youth and their parent each completed a baseline Qualtrics survey, youth completed 21 days of EMA (5 pings per day) administered via LifePak followed by a Qualtrics survey 3 months later.

Phase 2 aims (a) to use network parameters from EMA data to predict treatment response in depressed adolescents and (b) to test the efficacy of two single-session interventions (SSIs) over a 2-year follow-up period.

- Phase 2, using a different sample from Phase 1, consisted of an observational period followed by a randomized controlled trial testing behavioral activation and growth mindset SSIs against an active control SSI.
- After youth and their parent each completed a baseline Qualtrics survey, youth completed 21 days of EMA (5 pings per day) administered via LifePak followed by a Qualtrics intervention survey (including pre-SSI measures, random assignment to and completion of one of the three SSIs, and post-SSI measures). Youth and their parent then each completed Qualtrics surveys 3, 6, 12, 18, and 24 months later.
- Phase 2 study registration: [https://clinicaltrials.gov/study/NCT04607902](https://clinicaltrials.gov/study/NCT04607902)

Old data cleaning code, including documentation (some of which informed the code in this repository), is here:

* Phase 1: https://github.com/jwe4ec/ttt-p1-cleaning-old
* Phase 2: https://github.com/jwe4ec/ttt-p2-lifepak-cleaning-old

## File Organization

### Private Data

Raw and clean data and additional READMEs are stored privately in `jslab/` on the [FSMResFiles][FSMResFiles] server at [Northwestern's Feinberg School of Medicine][feinberg] (see tree below).[^1] For versioned releases of clean data, see [Releases](#releases). 

```plaintext
jslab/                                      # JSLAB_DIR_WINDOWS or JSLAB_DIR_UNIX defined in ".Renviron"
|
├── TRACK to TREAT/Data/                    # Phase 1 data folder
|   ├── readme_ttt_p1.docx                  # Data collection README
│   ├── LifePak Raw Data (Do Not Modify)/   # Raw LifePak data
│   ├── Qualtrics Data/Raw Data/            # Raw Qualtrics data
│   └── Clean Data (Jeremy and Isaac)/
│       ├── staging/                        # Clean data staged for release to "final_read_only/"
│       │   └── intermediate/               # Intermediate data used in data cleaning
│       └── final_read_only/                # Versioned releases of clean Phase 1 data
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

After downloading this repo, create an `.Renviron` file (do not commit it to GitHub) in the project root (shown as `.` in tree below). In `.Renviron`, set the path to `jslab/` by defining one of these environment variables:

```plaintext
JSLAB_DIR_WINDOWS = "path/to/jslab"  # For Windows
JSLAB_DIR_UNIX = "path/to/jslab"     # For macOS/Linux
```

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
├── Phase 1/
│   ├── Raw P1 Metadata.csv
│   ├── 2025.05.01 Track to Treat P1 Codebook.xlsx  # Item-level codebook used to clean Qualtrics data
│   ├── 1_Clean LifePak Data.R
│   ├── 2_Clean Youth Qualtrics Data and Add LSMH ID to LifePak Data.R
│   ├── 3_Clean Parent Qualtrics Data.R
│   ├── 4_Create Clean Data Release.R
│   └── QA/
│       ├── Check Overlap.R  # Checking ID overlap across datasets
│       └── Compare Clean LifePak Datasets.R  # Checking clean LifePak data to previous versions
|
└── Phase 2/
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

## General Approach

* Scripts are numbered in the order to be run
* Scripts generally do these tasks:
  * Load data and check raw data versions
  * Remove invalid and duplicated responses
  * Merge datasets (across waves, etc.)
  * Clean selected columns
  * Manually correct IDs as necessary

## Tracking Log Cleaning

### Phase 1

* Not included in cleaning pipeline

### Phase 2

* Phase Sheet of tracking log is cleaned and used to create the following
  * ID lookup of LSMH and LifePak IDs (used to drop vs. keep certain IDs during cleaning)
  * Cohort indicators for participant flowchart and data analysis (see above)

## LifePak Data Cleaning

* LifePak IDs here are 6 digits (5-digit IDs elsewhere have leading 0 here; take care when comparing IDs)
* Clean data includes EMA surveys only (excludes "feedback surveys", which were given after EMA surveys)
* Raw `Notification.Time` is in local time zones of participants' devices (per LifeData support)
  * Clean timestamp stores these in UTC (actual time zones could be derived from [incomplete] GPS data)
* LifePak cleaning scripts output free-responses to check for identifiers to intermediate data folder
  * TODO: Jeremy Eberle to review Alyssa Gorkin's initial checks of the responses (see scripts for details)

### Phase 1 Specifics

* Negative values for `interest` are recoded as 0 in the clean data
  * `3T_P1_V1_NIS_2020_Mar_02.csv` from survey `"TRACK to TREAT P1"` had some negative values for `Session.Name` `"3T Project Day"`, whose response options for this item were set from -2 to 100
* Some participants got their first notification after 7:30 am; it's unclear how or why
* Empty rows from multiple datasets overlapping in time for LifePak ID 958251 are removed
* Most participants have 105 total notifications, but some have fewer; it's unclear why

### Phase 2 Specifics

* Use LSMH ID to refer to unique participants
  * Unlike in Phase 1, multiple LifePak IDs for a given participant are not merged into one LifePak ID
  * Moreover, LifePak ID in youth baseline Qualtrics data is not cleaned
* Phase 2 clean data lacks Phase 1 clean data's `time_of_day` variable
  * This is because in Phase 2 the day and night EMA surveys were both named `"3T Project"`
* Phase 2 clean data's `other` variable is equivalent to Phase 1 clean data's `other_night`
* Considerably more messiness with EMA surveys given larger sample in Phase 2
* Many participants have fewer than 105 notifications, and some have more (redownloaded app)

## Qualtrics Data Cleaning

* When items are reverse-coded, cleaning scripts unreverse them (while retaining the original item name)
  * By contrast, in LifePak data, when items are reversed the suffix `_rev` is appended to the item name
* Item excluded from composite variables
  * Parent BSI-18 item on suicidal thoughts, which was not administered
* Some youth and parent item names differ (compare item content before comparing responses)
    * E.g., parent item `scared_b_16` corresponds to youth item `scared_c_1`
* Log list files are created to log:
  * Items used to compute item completion rates (see `log$item_completion_rate`)
  * Items used to compute means and counts (see `log$mean_items` and `log$count_items`)
    * Confirm the items are correct before analyzing the means and counts
  * Clean youth and parent codebooks (with selected columns; see raw codebook for all columns)
* Take care when comparing timestamps between LifePak/Qualtrics datasets (different time zones)
* Ranges of youth SITBI-SF items need to be checked against those expected

### Phase 1 Specifics

* Phone screen data are not included in cleaning pipeline
* Additional items excluded from composite variables
  * Parent item `scared_c_1`, which was entered into survey incorrectly
  * Child item `scared_c_11`, which was absent from in-person baseline survey
* Surveys retained at each wave (given need to deduplicate surveys within assessment windows):
  * `yb`: Youth who did `yb` survey in window (within 1 week before starting EMA) and started EMA
    * Given that `yb` window is based on first EMA notification date (see youth Qualtrics script)
    * All youth started EMA and did `yb` the day before, but 1 parent did `pb` a week early
  * `y3m`: Youth who did `y3m` survey in window (3 months after ending `yb`, +6 weeks; none were early) and `yb` survey in window
    * Given that `y3m` window is based on `yb` end date (see youth Qualtrics script)
  * `pb`, `p3m`: Parents whose surveys are in same windows used for `yb` and `y3m`
  * The approximate assessment windows above were reasonably extended from the original windows
* Clean Columns section lists raw data available that have not yet been cleaned
* Raw timestamps are in `America/Denver` time zone

### Phase 2 Specifics

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

### Phase 1

* Clean LifePak and Qualtrics data (at baseline and 3 months) for valid participants
  * No further filtering is needed
  
### Phase 2

* Clean LifePak and Qualtrics data (at baseline, intervention, and 3-24 months) for valid participants
  * But **further filtering is needed if intent-to-treat (ITT) sample is desired (see below)**
* Cohort indicators for all people who inquired about study
  * See `Phase 2 Cohort Indicators for Flow and Analysis.rds`. Use this to:
    * Create participant flowchart
      * See sample sizes in `Create Cohort Indicators for Flow and Analysis.R`
    * **Filter LSMH IDs to those for whom `analyze_itt_sample` is `TRUE` to get ITT sample**
      * Defined as those randomized but not meeting free-text exclusion criteria

## Version Control

* Expected raw data versions are tracked in `Raw <P1/P2> Metadata.csv` files
* Cleaning scripts save data to `staging/` to avoid overwriting data in `final_read_only/`
* `Create Clean Data Release.R` creates versioned "releases" of data in `final_read_only/`
  * Versions < v1.0 are for development only; versions >= v1.0 are suitable for analysis
  * To view the code (and raw data versions) for a release, go to this repo's corresponding [tag](https://github.com/jwe4ec/track-to-treat/tags)

### Releases

* **v1.0 (2025-05-12)**
  * Phase 1: Cleans LifePak and Qualtrics data (collection over)
    * Clean LifePak data is outputted with and without these free responses (to deidentify in future): `most_pleasant`, `most_unpleasant`, `other_night`
  * Phase 2: Not cleaned for this release
* **v2.0 (TODO: planned)**
  * Phase 1: Same output as v1.0
  * Phase 2: Cleans LifePak data and Qualtrics data (collection over)
    * Clean LifePak data is outputted with and without these free responses (to deidentify in future): `most_pleasant`, `most_unpleasant`, `other`

<!-- Reference Links -->

[fsmresfiles]: https://www.feinberg.northwestern.edu/it/services/server-storage-and-data/research-data-storage.html
[feinberg]: https://www.feinberg.northwestern.edu/
[releases]: https://github.com/jwe4ec/track-to-treat/releases

<!-- Footnotes -->

[^1]: Before this repo was transferred from https://github.com/isaacahuvia/track-to-treat on 2026-04-25, clean data were outputted to `.../Clean Data (Isaac)/` (renamed `.../Clean Data (Jeremy and Isaac)/`).