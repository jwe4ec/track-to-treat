# track-to-treat

Centralized data cleaning code for Track to Treat. Data are stored in the local repository on /resfiles.

Old data cleaning code, including documentation (some of which informed the code in this repository), is here:

* Phase 1: https://github.com/jwe4ec/ttt-p1-main-analysis [URL may change]
* Phase 2: https://github.com/jwe4ec/ttt-p2-cleaning [URL may change]

File organization:

* README.md
* Phase 1/
  * README.md
  * Raw P1 Metadata.csv (used to track expected raw data versions for checks against loaded files)
  * Track to Treat P1 Codebook.xlsx (an item-level codebook used to clean the Qualtrics data)
  * 1_Clean LifePak Data.R
  * 2_Clean Youth Qualtrics Data and Add LSMH ID to LifePak Data.R
  * 3_Clean Parent Qualtrics Data.R
  * 4_Create Clean Data Release.R
  * QA/
    * Check Overlap.R (checking ID overlap across datasets)
    * Compare Clean LifePak Datasets.R (checking clean LifePak data to previous versions)
* Phase 2/
  * README.md
  * [TBD]

Data cleaning notes:

* Scripts are numbered in the order to be run
* Scripts generally follow the same flow:
  * Load data and check raw data versions
  * Remove invalid and duplicated responses
  * Merge datasets (across waves, etc.)
  * Clean selected columns
  * Manually correct IDs as necessary
  
* LifePak data
  * LifePak IDs here are 6 digits (5-digit IDs elsewhere have leading 0 here; take care when comparing IDs)
  * Clean data includes EMA surveys only (excludes "feedback surveys", which were given after EMA surveys)
  * Negative values for `interest` are recoded as 0 in the clean data
    * "3T_P1_V1_NIS_2020_Mar_02.csv" from survey "TRACK to TREAT P1" had some negative values for `Session.Name` "3T Project Day", whose response options for this item were set from -2 to 100
  * Some participants got their first notification after 7:30 am; it's unclear how or why
  * Most participants have 105 total notifications, but some have fewer and one has more; it's unclear why
  * Raw `Notification.Time` is in local time zones of participants' devices (per LifeData support)
    * Clean timestamp stores these in UTC (actual time zones could be derived from [incomplete] GPS data)

* Qualtrics data
  * Clean Columns section lists raw data available that have not yet been cleaned
  * When items are reverse-coded, cleaning scripts unreverse them (while retaining the original item name)
    * By contrast, in LifePak data, when items are reversed the suffix "_rev" is appended to the item name
  * Item exclusions
    * Parent item `scared_c_1` was entered into survey incorrectly and is excluded from composite variables
    * Child item `scared_c_11` was absent from in-person baseline survey and is excluded from composites
  * Some youth and parent item names differ (compare item content before comparing responses)
    * E.g., parent item `scared_b_16` corresponds to youth item `scared_c_1`
  * Log list files are created to log:
    * Items used to compute item completion rates (see `log$item_completion_rate`)
    * Items used to compute means and counts (see `log$mean_items` and `log$count_items`)
      * Confirm the items are correct before analyzing the means and counts
    * Clean youth and parent codebooks
  * Raw timestamps are in "America/Denver" time zone for Phase I and in "America/Chicago" for Phase II
    * Take care when comparing timestamps between LifePak/Qualtrics datasets

Version control:
  * Expected raw data versions are tracked in "Raw <P1/P2> Metadata.csv" files
  * Cleaning scripts save data to `staging/` to avoid overwriting data in `final_read_only/`
  * `Create Clean Data Release.R` creates versioned "releases" of data in `final_read_only/`
    * Versions < v1.0 are for development only; versions >= v1.0 are suitable for analysis
    * To view the code (and raw data versions) for a release, go to this repo's corresponding [tag](https://git-scm.com/book/en/v2/Git-Basics-Tagging)
  * Releases:
    * v1.0 **(pending)**
      * Phase I: Cleans LifePak and Qualtrics data (collection over)
        * Clean LifePak data is outputted with and without free responses below (to deidentify in v3.0):
          * `most_pleasant`, `most_unpleasant`, `other_night`
      * Phase II: Not cleaned for this release
    * v2.0 **(planned)**
      * Phase I: Same output as v1.0
      * Phase II: Cleans LifePak data (collection over) and _interim_ Qualtrics data (collection ongoing)
        * Clean LifePak data is outputted with and without free responses above (to deidentify in v3.0)
        * Qualtrics collection through 18 months over; collection through 24 months expected over 11/2025
    * v3.0 **(planned)**
      * Phase I: Cleans LifePak (with deidentified free responses) and Qualtrics data (collection over)
      * Phase II: Cleans LifePak (with deidentified free responses) and Qualtrics data (collection over)