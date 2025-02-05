# track-to-treat

Centralized data cleaning code for Track to Treat. Data are stored in the local repository on /resfiles.

Old data cleaning code, including documentation (some of which informed the code in this repository), is available here:

* Phase 1: https://github.com/jwe4ec/ttt-p1-main-analysis [URL may change]
* Phase 2: [TBD]

File organization:

* README.md
* Phase 1/
  * README.md
  * Track to Treat P1 Codebook.xlsx (an item-level codebook used to clean the Qualtrics data)
  * Qualtrics Data Cleaning - Youth.R
  * Qualtrics Data Cleaning - Parents.R
  * LifePak Data Cleaning.R
  * Data Merging.R [TBD]
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
* In the parent-report SCARED, one item (`scared_c_1`) was entered into the survey incorrectly and is excluded from composite variables
* In the LifePak data, there are some negative values for `interest`; it's unclear how or why
  