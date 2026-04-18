# track-to-treat

Centralized data cleaning code for Track to Treat. Data are stored in the local repository on /resfiles.

Old data cleaning code, including documentation (some of which informed the code in this repository), is here:

* Phase 1: https://github.com/jwe4ec/ttt-p1-main-analysis [URL may change]
* Phase 2: https://github.com/jwe4ec/ttt-p2-cleaning [URL may change]

File organization:

* README.md
* Phase 1/
  * Raw P1 Metadata.csv (used to track expected raw data versions for checks against loaded files)
  * 2025.05.01 Track to Treat P1 Codebook.xlsx (an item-level codebook used to clean the Qualtrics data)
  * 1_Clean LifePak Data.R
  * 2_Clean Youth Qualtrics Data and Add LSMH ID to LifePak Data.R
  * 3_Clean Parent Qualtrics Data.R
  * 4_Create Clean Data Release.R
  * QA/
    * Check Overlap.R (checking ID overlap across datasets)
    * Compare Clean LifePak Datasets.R (checking clean LifePak data to previous versions)
* Phase 2/
  * Raw P2 Metadata.csv (used to track expected raw data versions for checks against loaded files)
  * 2026.04.03 Track to Treat P2 Codebook.xlsx (an item-level codebook used to clean the Qualtrics data)
    * "load_p2_codebook()" helper expands repeated-measure items with "[x]" prefix to "b" and "[3-24]m"
  * 1_Clean Tracking Log and Create ID Lookup.R
  * 2_Clean Phone Screen Data.R
  * 3_Clean LifePak Data.R
  * 4_Clean Youth Qualtrics Data/
    * 1_Correct Codebook and Raw Youth Data.R
    * 2_Youth Baseline.R
    * 3_Youth Intervention.R
    * 4_Youth 3m.R
    * 5_Youth 6m.R
    * 6_Youth 12m.R
    * 7_Youth 18m.R
    * 8_Youth 24m.R
    * 9_Merge Youth Qualtrics Data.R
  * 5_Clean Parent Qualtrics Data/
    * 1_Correct Raw Parent Data.R
    * 2_Parent Baseline.R
    * 3_Parent 3m.R
    * 4_Parent 6m.R
    * 5_Parent 12m.R
    * 6_Parent 18m.R
    * 7_Parent 24m.R
    * 8_Merge Parent Qualtrics Data.R
  * 6_Create Cohort Indicators for Flow and Analysis.R
  * 7_Create Clean Data Release.R
  * QA/
    * Inspect Raw Qualtrics Data and Codebook.R (checking raw data and codebook for issues to clean)

Data cleaning notes:

* Scripts are numbered in the order to be run
* Scripts generally follow the same flow:
  * Load data and check raw data versions
  * Remove invalid and duplicated responses
  * Merge datasets (across waves, etc.)
  * Clean selected columns
  * Manually correct IDs as necessary
* Phase 1 outputs
  * Clean LifePak and Qualtrics data (at baseline and 3 months) for valid participants
    * No further filtering is needed
* Phase 2 outputs
  * Clean LifePak and Qualtrics data (at baseline, intervention, and 3-24 months) for valid participants
    * But further filtering is needed if intent-to-treat (ITT) sample is desired (see below)
  * Cohort indicators for all people who inquired about study
    * See "Phase 2 Cohort Indicators for Flow and Analysis.rds". Use this to:
      * Create participant flowchart
        * See sample sizes in "4_Create Cohort Indicators for Flow and Analysis.R"
      * Filter LSMH IDs to those for whom `analyze_itt_sample` is `TRUE` to get ITT sample
        * Defined as those randomized but not meeting free-text exclusion criteria
  
* Tracking Log
  * Phase 1 specifics:
    * Not included in cleaning pipeline
  * Phase 2 specifics:
    * Phase Sheet of tracking log is cleaned and used to create the following
      * ID lookup of LSMH and LifePak IDs (used to drop vs. keep certain IDs during cleaning)
      * Cohort indicators for participant flowchart and data analysis (see above)

* LifePak data
  * LifePak IDs here are 6 digits (5-digit IDs elsewhere have leading 0 here; take care when comparing IDs)
  * Clean data includes EMA surveys only (excludes "feedback surveys", which were given after EMA surveys)
  * End-of-day free-responses in clean data are deidentified
    * If future cleaning retains additional rows in clean data, those rows need to be deidentified
  * Raw `Notification.Time` is in local time zones of participants' devices (per LifeData support)
    * Clean timestamp stores these in UTC (actual time zones could be derived from [incomplete] GPS data)
  * Phase 1 specifics:
    * Negative values for `interest` are recoded as 0 in the clean data
      * "3T_P1_V1_NIS_2020_Mar_02.csv" from survey "TRACK to TREAT P1" had some negative values for `Session.Name` "3T Project Day", whose response options for this item were set from -2 to 100
    * Some participants got their first notification after 7:30 am; it's unclear how or why
    * Empty rows from multiple datasets overlapping in time for LifePak ID 958251 are removed
    * Most participants have 105 total notifications, but some have fewer; it's unclear why
  * Phase 2 specifics:
    * Use LSMH ID to refer to unique participants
      * Unlike in Phase 1, multiple LifePak IDs for a given participant are not merged into one LifePak ID
      * Moreover, LifePak ID in youth baseline Qualtrics data is not cleaned
    * Phase 2 clean data lacks Phase 1 clean data's "time_of_day" variable
      * This is because in Phase 2 the day and night EMA surveys were both named "3T Project"
    * Phase 2 clean data's "other" variable is equivalent to Phase 1 clean data's "other_night"
    * Considerably more messiness with EMA surveys given larger sample in Phase 2
    * Many participants have fewer than 105 notifications, and some have more (redownloaded app)

* Qualtrics data
  * When items are reverse-coded, cleaning scripts unreverse them (while retaining the original item name)
    * By contrast, in LifePak data, when items are reversed the suffix "_rev" is appended to the item name
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
  * Phase 1 specifics:
    * Phone screen data are not included in cleaning pipeline
    * Additional items excluded from composite variables
      * Parent item `scared_c_1`, which was entered into survey incorrectly
      * Child item `scared_c_11`, which was absent from in-person baseline survey
    * Clean Columns section lists raw data available that have not yet been cleaned
    * Raw timestamps are in "America/Denver" time zone
  * Phase 2 specifics:
    * Phone screen data (entered by RA with parent on phone) are cleaned before youth/parent data at study waves
    * To move certain rows to correct waves, the tasks below are done across waves in "Correct Codebook and Raw Youth Data.R" and "Correct Raw Parent Data.R" before cleaning each wave individually
      * Fix item prefixes in codebook and column names in data
      * Remove extraneous columns (including click, page time variables)
      * Create "_original_dataset" column labeling each row's original survey dataset
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
    * After cleaning each wave individually, LSMH IDs meeting exclusion criteria per youth intervention free-text responses are dropped in "Merge Youth Qualtrics Data.R" and "Merge Parent Qualtrics Data.R"
    * TODO: Note which participants are retained at each wave (e.g., "yb" removed if not started EMA)
    * Youth data collected but not cleaned:
      * TODO
    * Parent data collected but not cleaned:
      * TODO

Version control:
  * Expected raw data versions are tracked in "Raw <P1/P2> Metadata.csv" files
  * Cleaning scripts save data to `staging/` to avoid overwriting data in `final_read_only/`
  * `Create Clean Data Release.R` creates versioned "releases" of data in `final_read_only/`
    * Versions < v1.0 are for development only; versions >= v1.0 are suitable for analysis
    * To view the code (and raw data versions) for a release, go to this repo's corresponding [tag](https://github.com/isaacahuvia/track-to-treat/tags)
  * Releases:
    * **v1.0 (2025-05-12)**
      * Phase 1: Cleans LifePak and Qualtrics data (collection over)
        * Clean LifePak data is outputted with and without free responses below (to deidentify in future)
          * `most_pleasant`, `most_unpleasant`, `other_night`
      * Phase 2: Not cleaned for this release
    * **v2.0 (TODO: planned)**
      * Phase 1: Same output as v1.0
      * Phase 2: Cleans LifePak data and Qualtrics data (collection over)
        * Clean LifePak data is outputted with and without free responses above (to deidentify in future)