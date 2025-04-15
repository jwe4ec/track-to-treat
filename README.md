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
  * Qualtrics Data Cleaning - Youth.R
  * Qualtrics Data Cleaning - Parents.R
  * LifePak Data Cleaning.R
  * QA/
    * Check Overlap.R (checking ID overlap across datasets)
    * Compare Clean LifePak Datasets.R (checking clean LifePak data to previous versions)
* Phase 2/
  * README.md
  * [TBD]

Data cleaning notes:

* Scripts generally follow the same flow:
  * Load data
  * Remove invalid and duplicated responses
  * Merge datasets (across waves, etc.)
  * Clean columns
  * Manually correct IDs as necessary
* In the Qualtrics data, items used to compute means via mean_across() are logged in Mean Items Log list files (confirm the items are correct before analyzing the means)
* In the Qualtrics data, on the parent-report SCARED, one item (`scared_c_1`) was entered into the survey incorrectly and is excluded from composite variables
* In the Qualtrics data, on the child-report SCARED, one item (`scared_c_11`) was not included on the in-person baseline survey and is excluded from composite variables
* Clean LifePak data includes EMA survey data only (i.e., excludes "feedback surveys")
* In the LifePak data, there are some negative values for `interest`; it's unclear how or why
* In the LifePak data, some participants got their first notification after 7:30; it's unclear how or why
* In the LifePak data, most participants have 105 total notifications, but some have fewer and one has more; it's unclear why
* When items are reverse-coded, the data cleaning script puts them back in the right direction (retaining the original item name)