# Phase 1 Specifics

This README supplements the [main README](../README.md), which applies
to Phases 1-2, with additional details specific to Phase 1.

## Table of Contents

- [File Organization](#file-organization)
- [Tracking Log Cleaning](#tracking-log-cleaning)
- [LifePak Data Cleaning](#lifepak-data-cleaning)
- [Qualtrics Data Cleaning](#qualtrics-data-cleaning)
- [Outputs](#outputs)

## File Organization

### Private Data

```plaintext
jslab/TRACK to TREAT/Data/             # Phase 1 data folder
├── readme_ttt_p1.docx                 # Data collection README
├── LifePak Raw Data (Do Not Modify)/  # Raw LifePak data
├── Qualtrics Data/Raw Data/           # Raw Qualtrics data
└── Clean Data (Jeremy and Isaac)/
    ├── staging/                       # Clean data staged for release to "final_read_only/"
    │   └── intermediate/              # Intermediate data used in data cleaning
    └── final_read_only/               # Versioned releases of clean Phase 1 data
```

### Code

```plaintext
./Phase 1/  # Phase 1 cleaning
├── README.md
├── Raw P1 Metadata.csv
├── 2025.05.01 Track to Treat P1 Codebook.xlsx  # Item-level codebook used to clean Qualtrics data
├── 1_Clean LifePak Data.R
├── 2_Clean Youth Qualtrics Data and Add LSMH ID to LifePak Data.R
├── 3_Clean Parent Qualtrics Data.R
├── 4_Create Clean Data Release.R
└── QA/
    ├── Check Overlap.R  # Checking ID overlap across datasets
    └── Compare Clean LifePak Datasets.R  # Checking clean LifePak data to previous versions
```

## Tracking Log Cleaning

* Not included in cleaning pipeline

## LifePak Data Cleaning

* Negative values for `interest` are recoded as 0 in the clean data
  * `3T_P1_V1_NIS_2020_Mar_02.csv` from survey `"TRACK to TREAT P1"` had some negative values for `Session.Name` `"3T Project Day"`, whose response options for this item were set from -2 to 100
* Some participants got their first notification after 7:30 am; it's unclear how or why
* Empty rows from multiple datasets overlapping in time for LifePak ID 958251 are removed
* Most participants have 105 total notifications, but some have fewer; it's unclear why
* Clean data is outputted with and without these free responses (to deidentify in future):
  * `most_pleasant`, `most_unpleasant`, `other_night`

## Qualtrics Data Cleaning

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

## Outputs

* Clean LifePak and Qualtrics data (at baseline and 3 months) for valid participants
  * No further filtering is needed