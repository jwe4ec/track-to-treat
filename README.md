# track-to-treat

Centralized data cleaning for Project Track to Treat

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
- [Phase-Specific Releases](#phase-specific-releases)

## Citation

When using a given [release](#phase-specific-releases) of this code or associated clean data, please cite the version number and Version DOI for the release. **For the citation including this version information, see the [Release Notes][releases].**

## Project Overview

Phase 1 of Project Track to Treat (TTT) aims to use parameters from network models fit to ecological momentary assessment (EMA) data to predict 3-month changes in symptoms in depressed adolescents.

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
jslab/                       # JSLAB_DIR_WINDOWS or JSLAB_DIR_UNIX defined in ".Renviron"
|
├── TRACK to TREAT/Data/     # Phase 1 data folder
└── TRACK to TREAT P2/Data/  # Phase 2 data folder
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
├── Phase 1/  # Phase 1 cleaning scripts
└── Phase 2/  # Phase 2 cleaning scripts
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

See supplemental READMEs for Phases [1](./Phase 1/README.md) and [2](./Phase 2/README.md)

## LifePak Data Cleaning

* LifePak IDs here are 6 digits (5-digit IDs elsewhere have leading 0 here; take care when comparing IDs)
* Clean data includes EMA surveys only (excludes "feedback surveys", which were given after EMA surveys)
* Raw `Notification.Time` is in local time zones of participants' devices (per LifeData support)
  * Clean timestamp stores these in UTC (actual time zones could be derived from [incomplete] GPS data)
* LifePak cleaning scripts output free-responses to check for identifiers to intermediate data folder
  * TODO: Jeremy Eberle to review Alyssa Gorkin's initial checks of the responses (see scripts for details)

For phase-specific details, see supplemental READMEs for Phases [1](./Phase 1/README.md) and [2](./Phase 2/README.md)

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

For phase-specific details, see supplemental READMEs for Phases [1](./Phase 1/README.md) and [2](./Phase 2/README.md)

## Outputs

See supplemental READMEs for Phases [1](./Phase 1/README.md) and [2](./Phase 2/README.md)

## Version Control

* Expected raw data versions are tracked in `Raw <P1/P2> Metadata.csv` files
* Cleaning scripts save data to `staging/` to avoid overwriting data in `final_read_only/`

## Phase-Specific Releases

* Although this development repository houses code for both Phases 1-2, releases are phase-specific
  * Each phase has its own version numbers, OSF project (for clean data), and Zenodo record (for code)
* New releases of both data and code are created after key updates to a phase's clean data/code/docs

### Clean Data Releases

* `Create Clean Data Release.R` creates versioned local "releases" of data in `final_read_only/`
* A copy of the phase's data release is uploaded in a ZIP to the phase's OSF project
  * TODO: The OSF projects are currently private but eventually will be public
    * Phase 1 OSF project: https://osf.io/yjv72
    * Phase 2 OSF project: https://osf.io/8pa3z
  * LifePak free responses are excluded from the upload
* Do not delete any previous releases (they may be used in certain analyses!)

### Corresponding Code Releases

* See this repo's [Releases][releases] for:
  * The phase's scripts (uploaded in a ZIP to Assets) that were run to create a given data release
    * The notes and ZIP are also uploaded to the phase's Zenodo record, which mints a Version DOI
    * (For snapshot of whole repo at time of release, see source code ZIP in Assets or link to tag)
  * **Citation for a given release's code and associated clean data**
    * Please cite this (vs. the OSF project), as it includes both the version number and Version DOI

### Phase 1 Releases

* **TODO (create tag): phase1_v1.1 (2026-05-09)**
  * Updates code and README; clean data unchanged from `phase1_v1.0`
* **TODO (change tag): phase1_v1.0 (2025-05-12)**
  * Cleans LifePak and Qualtrics data (collection over)

### Phase 2 Releases

* **TODO (create tag): phase2_v1.0 (2026-05-09)**
  * Cleans LifePak and Qualtrics data (collection over)

<!-- Reference Links -->

[fsmresfiles]: https://www.feinberg.northwestern.edu/it/services/server-storage-and-data/research-data-storage.html
[feinberg]: https://www.feinberg.northwestern.edu/
[releases]: https://github.com/jwe4ec/track-to-treat/releases

<!-- Footnotes -->

[^1]: Before this repo was transferred from https://github.com/isaacahuvia/track-to-treat on 2026-04-25, clean data were outputted to `.../Clean Data (Isaac)/` (renamed `.../Clean Data (Jeremy and Isaac)/`).