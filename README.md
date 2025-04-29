# track-to-treat

Centralized data cleaning code for Track to Treat. Data are stored in the local repository on /resfiles.

Old data cleaning code, including documentation (some of which informed the code in this repository), is available here:

* Phase 1: https://github.com/jwe4ec/ttt-p1-main-analysis [URL may change]
* Phase 2: https://github.com/jwe4ec/ttt-p2-cleaning [URL may change]

File organization:

* README.md
* Phase 1/
  * README.md
  * Track to Treat P1 Codebook.xlsx (an item-level codebook used to clean the Qualtrics data)
  * 1_Qualtrics Data Cleaning - Youth.R
  * 2_Qualtrics Data Cleaning - Parents.R
  * 3_LifePak Data Cleaning.R
  * QA/
    * Check Overlap.R (checking ID overlap across datasets)
    * Compare Clean LifePak Datasets.R (checking clean LifePak data to previous versions)
* Phase 2/
  * README.md
  * [TBD]

Data cleaning notes:

* Scripts are numbered in the order to be run
* Scripts generally follow the same flow:
  * Load data
  * Remove invalid and duplicated responses
  * Merge datasets (across waves, etc.)
  * Clean selected columns
    * Note: In the Qualtrics data, the Clean Columns section lists raw data available that have not yet been cleaned
  * Manually correct IDs as necessary
    * Note: LifePak IDs here are 6 digits (5-digit IDs elsewhere have a leading 0 here; take care when comparing/selecting IDs)
* In the Qualtrics data, a Log list file is created to:
  * Log items used to compute item completion rates via `compute_item_completion_rate()` (see `log$item_completion_rate`)
  * Log items used to compute means via `mean_across()` (see `log$mean_items` and confirm the items are correct before analyzing the means)
* In the Qualtrics data, on the parent-report SCARED, one item (`scared_c_1`) was entered into the survey incorrectly and is excluded from composite variables
* In the Qualtrics data, on the child-report SCARED, one item (`scared_c_11`) was not included on the in-person baseline survey and is excluded from composite variables
* Clean LifePak data includes EMA survey data only (i.e., excludes "feedback surveys", which were administered after EMA surveys)
* In the LifePak data, there are some negative values for `interest`; it's unclear how or why
* In the LifePak data, some participants got their first notification after 7:30; it's unclear how or why
* In the LifePak data, most participants have 105 total notifications, but some have fewer and one has more; it's unclear why
* When Qualtrics items are reverse-coded, the data cleaning script puts them back in the right direction (retaining the original item name)
  * Note: By contrast, in the LifePak data, when items are reversed the suffix "_rev" is appended to the item name.
* Take care when comparing timestamps between datasets
  * In the LifePak data, raw `Notification.Time` is in local time zones of participants' devices (per LifeData support)
    * The clean timestamp stores these local times in UTC (the actual time zones would need to be derived from LifePak GPS data, which is missing for some observations)
  * In the Qualtrics data, raw timestamps are in "America/Denver" time zone for Phase I and in "America/Chicago" for Phase II
  
TODO:

* In the clean LifePak data, these free-response columns need to be deidentified as needed:
  * `most_pleasant`, `most_unpleasant`, `other_night`