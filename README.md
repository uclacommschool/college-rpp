# UCLA Community Schools Postsecondary Pathways Database


## Table of Contents

1.  [Overview](#overview)
2.  [Repository Structure](#repository-structure)
3.  [Privacy & Data Policy](#privacy--data-policy)
4.  [Dependencies](#dependencies)
5.  [Annual vs Recurring Tasks](#annual-vs-recurring-tasks)
6.  [Workflow](#workflow)
7.  [Data Dictionary](#data-dictionary)
8.  [NSC Date Formats](#nsc-date-formats)
9.  [Tracking Policy](#tracking-policy)
10. [References](#references)

------------------------------------------------------------------------

## Overview

This repository contains the scripts and functions to clean, merge, and
maintain the UCLA Community Schools Postsecondary Database (PSD). The
PSD tracks college enrollment and outcomes for students from graduation
through 8 years post-high school using data from the National Student
Clearinghouse (NSC).

For the full postsecondary data process including outcomes methodology,
missing list workflow, and school-facing data collection, see
`PSD_HANDBOOK.md` (coming soon).

------------------------------------------------------------------------

## Repository Structure

    college-rpp/
    ├── clean-psd/
    │   ├── 00-update-master-student-list.R   # Add incoming cohort to master student list
    │   ├── 01-merge-nsc-to-psd.R             # Merge NSC data with PSD
    │   ├── 02-create-missing-list-from-psd.R # Generate missing student list
    │   ├── 03-transform-missing-list.R       # Create school-facing missing list
    │   ├── 04-clean-missing-list.R           # Clean missing data for merging
    │   ├── 05-calculate-outcomes.R           # Calculate college-going outcomes
    │   ├── 06-merge-missing-data.R           # Merge missing data back to PSD
    │   ├── psd_rfk_function_list.R           # Helper functions
    │   └── institution_lookup.csv            # Institution classification reference
    ├── README.md                             # GitHub display README
    └── README.qmd                            # README source (Quarto)

------------------------------------------------------------------------

## Privacy & Data Policy

### Student Identifiers

The PSD uses two student identifiers:

| Identifier | Description | Contains PII | Use Case |
|----|----|----|----|
| `student_id` | Original school-assigned ID | YES — contains birthdate and gender | Internal joins with NSC data only |
| `psd_id` | PSD-assigned privacy-safe key | NO | All external reporting and sharing |

### Identifier History

- **2012–2023** — `student_id` submitted to NSC was the LAUSD-assigned
  ID which contained PII (birthdate and gender embedded in the ID
  structure)
- **2024 onward** — `student_id` moved to a generated `psd_id` to remove
  PII from the NSC submission and external reporting

`psd_id` was introduced in 2024 as a non-PII identifier. All cohorts
from 2012 onward have been backfilled with a `psd_id` for consistent
reporting across all years.

**Policy: Use `psd_id` for any data shared outside the research team.**
`student_id` should never appear in external reports, presentations, or
shared files.

### `psd_id` Format

Format: `YYYYXXXXnn`

| Component | Example | Description                 |
|-----------|---------|-----------------------------|
| `YYYY`    | `2024`  | High school graduation year |
| `XXXX`    | `AFLB`  | Random 4-letter code        |
| `nn`      | `74`    | Random 2-digit number       |

Example: `2024AFLB74` = Class of 2024 student

Note: For cohorts 2024 onward, `psd_id` is submitted to NSC as the
`student_id`. For cohorts 2012–2023, `student_id` was the LAUSD-assigned
ID — `psd_id` was backfilled for all prior cohorts in 2024 for
consistent reporting. See Privacy & Data Policy for full details.

`psd_id` is generated manually in Excel when creating the NSC graduate
file each August/September before submission to NSC.

------------------------------------------------------------------------

## Dependencies

**R Packages:** readr, readxl, lubridate, dplyr, stringr, openxlsx,
janitor

**Box Folder:** `College Data/Postsecondary Database/` -
`institution_lookup.csv` — institution classification reference table -
`UCLA Community School PSD/` — PSD files and master student list -
`National Student Clearinghouse/` — NSC StudentTracker reports

------------------------------------------------------------------------

## Annual vs Recurring Tasks

### Once per year (August/September)

Run `00-update-master-student-list.R` AFTER the Summer NSC pull: -
Requires new graduate file submitted to NSC - Delayed to Aug/Sep because
summer employment data collection is not always completed by June - Must
be completed BEFORE the Fall NSC pull (November) - New cohort enrollment
records first appear in the November pull

### Three times per year (November, April, August)

Run `01-merge-nsc-to-psd.R` after each NSC pull: - Summer (August) -
Fall (November) — new cohort appears for first time - Spring (April)

------------------------------------------------------------------------

## Workflow

The PSD is updated 3x per year following each NSC data pull (November,
April, August).

### Step 1 — Submit Graduate File to NSC

- Collect graduate roster from school counselor (June/July)
- Assign `psd_id` to each student manually in Excel
- Format file per NSC Submit Graduates File Guide
- Submit `.txt` file to NSC via FTP
- See [NSC Effective
  Dates](https://help.studentclearinghouse.org/sths/knowledge-base/about-effective-dates-and-academic-years/)

### Step 2 — Run `00-update-master-student-list.R`

Run once per year after the Summer NSC pull: - Updates master student
list with incoming cohort - Must complete before Step 4 - See Annual vs
Recurring Tasks for timing details

### Step 3 — Download NSC Data

- Log into [NSC FTP site](https://www.studentclearinghouse.org/) and
  download:
  - StudentTracker Reports folder
  - Student Detail Report CSV
- Save to Box:
  `College Data/National Student Clearinghouse/UCLACS StudentTracker Reports/[Year Month]/`

### Step 4 — Run `01-merge-nsc-to-psd.R`

- Update file paths and date filters marked `⚠️ UPDATE EACH RUN`
- Run Parts 1–6 in order
- Output saves to Box following naming convention:
  `DDmonthYYYY-schoolsitename-psd-authorfamilyname.csv`

### Step 5 — Create Missing List (November only)

- Run `02-create-missing-list-from-psd.R`
- Run `03-transform-missing-list.R`
- See `PSD_HANDBOOK.md` for full missing list workflow

### Step 6 — School-Facing Data Collection (November only)

- Run `04-clean-missing-list.R`
- Share school-facing missing list with counselors
- Collect follow-up enrollment data
- Run `06-merge-missing-data.R` to merge back to PSD
- See `PSD_HANDBOOK.md` for full data collection process

------------------------------------------------------------------------

## Data Dictionary

### Column Sources

| Column              | Source | Description                     |
|---------------------|--------|---------------------------------|
| `student_id`        | PSD    | Unique student identifier       |
| `first_name`        | NSC    | Student first name              |
| `middle_name`       | NSC    | Student middle name             |
| `last_name`         | NSC    | Student last name               |
| `name_suffix`       | NSC    | Name suffix                     |
| `record_found`      | NSC    | NSC found a record (Y/N)        |
| `req_return_field`  | NSC    | NSC requester return field      |
| `high_school_code`  | NSC    | High school code                |
| `college_code`      | NSC    | Federal school code             |
| `college_name`      | NSC    | College name                    |
| `college_state`     | NSC    | College state                   |
| `cc_4year`          | NSC    | 2-year or 4-year institution    |
| `public_private`    | NSC    | Public or private institution   |
| `enrollment_begin`  | NSC    | Enrollment start date           |
| `enrollment_end`    | NSC    | Enrollment end date             |
| `enrollment_status` | NSC    | Enrollment status               |
| `he_graduated`      | NSC    | Student graduated (Y/N)         |
| `coll_grad_date`    | NSC    | College graduation date         |
| `degree_title`      | NSC    | Degree earned                   |
| `major`             | NSC    | Field of study                  |
| `college_sequence`  | NSC    | Enrollment sequence number      |
| `program_code`      | NSC    | NSC program code                |
| `hs_grad_date`      | NSC    | High school graduation date     |
| `status_source`     | PSD    | Source of enrollment record     |
| `record_year`       | PSD    | Year of enrollment record       |
| `record_term`       | PSD    | Term of enrollment record       |
| `system_type`       | PSD    | Institution classification      |
| `hs_grad_year`      | PSD    | High school graduation year     |
| `gender`            | MASTER | Student gender                  |
| `race_ethnicity`    | MASTER | Student race/ethnicity          |
| `poverty_indicator` | MASTER | Low income indicator            |
| `hs_diploma`        | MASTER | High school diploma type        |
| `notes`             | MASTER | Staff free text notes           |
| `psd_id`            | PSD    | PSD assigned student identifier |

### `status_source`

| Value           | Description                                        |
|-----------------|----------------------------------------------------|
| `NSC`           | Record confirmed by National Student Clearinghouse |
| `STAFF`         | Record confirmed by school counselor               |
| `SELF-REPORTED` | Student self-reported enrollment                   |
| `OLD_PSD`       | Pre-2019 record from wide-format database          |
| `MISSING DATA`  | Status unknown, follow-up needed                   |

### `record_term`

| Value    | Description             |
|----------|-------------------------|
| `FALL`   | Fall semester/quarter   |
| `WINTER` | Winter quarter          |
| `SPRING` | Spring semester/quarter |
| `SUMMER` | Summer session          |

### `system_type`

| Value      | Description                      |
|------------|----------------------------------|
| `UC`       | University of California         |
| `CSU`      | California State University      |
| `CCC`      | California Community College     |
| `INP_NP`   | In-state private non-profit      |
| `INP_FP`   | In-state private for-profit      |
| `OUT_4YR`  | Out-of-state public 4-year       |
| `OUT_4YRP` | Out-of-state private 4-year      |
| `OUT_CC`   | Out-of-state community college   |
| `OUT_FP`   | Out-of-state for-profit          |
| `NON_CCC`  | Non-California community college |

### `cc_4year`

| Value           | Description                       |
|-----------------|-----------------------------------|
| `2-YEAR`        | 2-year institution                |
| `4-YEAR`        | 4-year institution                |
| `NO ENROLLMENT` | Confirmed not enrolled in college |
| `MISSING DATA`  | Enrollment status unknown         |

------------------------------------------------------------------------

## NSC Date Formats

NSC date formats have changed over time. The `parse_dates()` function
handles all three formats automatically:

| Period    | Format       | Example      |
|-----------|--------------|--------------|
| 2019+     | `YYYYMMDD`   | `20190826`   |
| Pre-2019  | `M/D/YY`     | `8/27/12`    |
| Saved PSD | `YYYY-MM-DD` | `2019-08-26` |

------------------------------------------------------------------------

## Tracking Policy

- Students are tracked for **8 years** post-graduation
- NSC pulls occur **3x per year**: November, April, August
- Missing data follow-up occurs **after November pull only**
- Students missing for **3 consecutive years** are marked inactive:
  write `"YYYY Stop Track"` in notes (e.g. `"2025 Stop Track"`)
- If an inactive student reappears in NSC data, active tracking resumes

------------------------------------------------------------------------

## References

- [College and Postsecondary Paths Database Handbook](PSD_HANDBOOK.md)
- [Google Doc
  PSD](https://docs.google.com/document/d/1M090A6vtENs6DNSHB36u9FrIz8kg9DG05sGfT4XIZJs/)
- [NSC Effective
  Dates](https://help.studentclearinghouse.org/sths/knowledge-base/about-effective-dates-and-academic-years/)
- [NSC Submit Graduates File
  Guide](https://help.studentclearinghouse.org/sths/)
