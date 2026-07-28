################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 00 - Update Master Student List >
## [ AUTH ] < Ariana Dimagiba / aridimagiba >
## [ INIT ] < 04/14/2026, updated 05/19/2026 by aridimagiba >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------
library(readr)
library(readxl)
library(dplyr)

## ---------------------------
## UPDATE EACH RUN - checklist
## ---------------------------
# 1. recent_nsc_graduate_file — update file name to most recent graduate file
#                               File naming convention: uclacs_nscgradfile_classYYYY.txt
# 2. master_stu_list          — update file name to most recent master student list
#                               File naming convention: uclacs_master_studentlist_classYY-YY.xlsx
# 3. clean_grad               — update grad_year to match incoming cohort year (Part 1.1)
# 4. Output file name         — update year range in output file name (Part 4.1)

## ---------------------------
## directory paths
## ---------------------------

code_file_dir<-file.path(".")

# Detect OS and set Box path accordingly
if (.Platform$OS.type == "windows") {
  box_file_dir <- file.path(Sys.getenv("USERPROFILE"), "Box")
} else {
  # Box Drive syncs via CloudStorage on Mac
  box_file_dir <- file.path(Sys.getenv("HOME"), "Library", "CloudStorage", "Box-Box")
}


## -----------------------------------------------------------------------------
## load all raw data sets
## -----------------------------------------------------------------------------

# NOTE: Graduate file is intentionally created in Excel, not R
# School staff provide demographics in non-standardized formats — Excel 
# accepts whatever format staff provide while reducing their burden.
# psd_id is generated manually in Excel before NSC submission.

#RFK Pathfile new graduate file
#"College and Career RPP", "1. NSC Dataset", "RFK", "Graduate Files", "Raw Data (TXT)",
# ⚠️ UPDATE: update file name to most recent graduate file "uclacs_nscgradfile_class2026.txt"

#Mann Pathfile new graduate file
#"College and Career RPP", "1. NSC Dataset", "Mann", "Graduate Files", "Raw Data - NSC Graduate Files TXT",
# ⚠️ UPDATE: update file name to most recent graduate file "mann_nscgradfile_class2025"

# Load new graduate file
recent_nsc_graduate_file <- read_delim(file.path(box_file_dir,
                                                 "College and Career RPP",
                                                 "1. NSC Dataset", 
                                                 "Mann", 
                                                 "Graduate Files", 
                                                 "Raw Data - NSC Graduate Files TXT",
                                                 # ⚠️ UPDATE: update file name to most recent graduate file 
                                                 "mann_nscgradfile_class2025.txt"
                                                 ),
                                       col_names = FALSE)

#RFK Pathfile master student list
# "College and Career RPP", "1. NSC Dataset", "RFK", "RFK PSD", "Master Student List",
# ⚠️ UPDATE: update year range to reflect new cohort added "master-student-list-rfk-2012-2025.csv"

#Mann Pathfile master student list
# "College and Career RPP", "1. NSC Dataset", "Mann", "Mann PSD", "Master Student List",
# ⚠️ UPDATE: update year range to reflect new cohort added "mann_master_student_list_21-24"

# Load most recent master student list
# NOTE: Moving forward this file is saved as a csv file — now reading with read_delim
master_stu_list <- read_delim(file.path(box_file_dir,
                                        "College and Career RPP", 
                                        "1. NSC Dataset", 
                                        "Mann", 
                                        "Mann PSD", 
                                        "Master Student List",
                                        "mann_master_student_list_21-24.csv"
                                        ))

## -----------------------------------------------------------------------------
## Part 1 Clean recent_nsc_graduate_file
## -----------------------------------------------------------------------------

# 1. Define cohort year
cohort_year <- 2025  # ⚠️ UPDATE: change to incoming cohort year — used in Parts 1, 2 and 3

# 2. Confirm graduate file loaded with expected structure
# Graduate file should have at least 18 columns — staff demographics occupy fixed positions
if (ncol(recent_nsc_graduate_file) < 18) {
  stop("⚠️ Graduate file has fewer than 18 columns — check file format before proceeding")
}

# 3. Clean graduate file and prepare for binding
clean_grad <- recent_nsc_graduate_file %>%
  # NSC graduate file has a fixed column structure per NSC Submit Graduates File Guide
  # Column positions selected:
  # 3:5   — C: first_name, D: middle_name, E: last_name
  # 10:11 — J: psd_id (student ID), K: hs_diploma (diploma type)
  # 16:18 — P: gender, Q: race_ethnicity, R: poverty_indicator
  # Row 1 is the header row — removed with slice(-1)
  # Reference: NSC StudentTracker for High Schools Submit Graduates File Guide (March 2021)
  select(3:5, 10:11, 16:18) %>%
  slice(-1) %>%
  setNames(c(
    "first_name", "middle_name", "last_name",
    "psd_id", "hs_diploma",
    "gender", "race_ethnicity", "poverty_indicator"
  )) %>%
  mutate(
    hs_grad_year = cohort_year,        # ⚠️ UPDATE each run
    student_id   = psd_id,
    notes        = NA_character_
  ) %>%
  select(
    student_id, first_name, middle_name, last_name, hs_grad_year,
    gender, race_ethnicity, poverty_indicator, hs_diploma, psd_id, notes
  )

# Check clean_grad produced rows
if (nrow(clean_grad) == 0) {
  stop("⚠️ clean_grad has 0 rows — check graduate file format and column positions")
}

## -----------------------------------------------------------------------------
## Part 2 Check Master Student List
## -----------------------------------------------------------------------------

# 1. Confirm master student list loaded with expected columns
expected_master_cols <- c("student_id", "first_name", "middle_name", 
                          "last_name", "hs_grad_year", "gender", 
                          "race_ethnicity", "poverty_indicator", 
                          "hs_diploma", "psd_id", "notes")

missing_master_cols <- expected_master_cols[!expected_master_cols %in% names(master_stu_list)]

# Check clean_grad produced rows
if (length(missing_master_cols) > 0) {
  stop(paste("⚠️ The following expected columns are missing from master_stu_list:",
             paste(missing_master_cols, collapse = ", ")))
}

if (cohort_year %in% unique(master_stu_list$hs_grad_year)) {
  stop(paste("⚠️ Class of", cohort_year, 
             "already exists in master_stu_list — avoid duplicate binding"))
}

## -----------------------------------------------------------------------------
## Part 3 Merge new cohort to master student list
## ----------------------------------------------------------------------------

# 1. Confirm column names match before binding
stopifnot(
  "Column mismatch between master_stu_list and clean_grad" =
    setequal(names(master_stu_list), names(clean_grad))
)

# 2. Bind new cohort list to new master student lists
new_master_student <- bind_rows(master_stu_list, clean_grad) %>%
  distinct()

# 3. Check new_master_list is larger than previous
if (nrow(new_master_student) <= nrow(master_stu_list)) {
  stop("⚠️ new_master_student has no new rows — check clean_grad before proceeding")
}
## -----------------------------------------------------------------------------
## Part 4 Write new csv file to Box
## -----------------------------------------------------------------------------

#RFK Pathfile master student list
# "College and Career RPP", "1. NSC Dataset", "RFK", "RFK PSD", "Master Student List",
# ⚠️ UPDATE: update year range to reflect new cohort added "master-student-list-rfk-2012-2026.csv"

#Mann Pathfile master student list
# "College and Career RPP", "1. NSC Dataset", "Mann", "Mann PSD", "Master Student List",
# ⚠️ UPDATE: update year range to reflect new cohort added "mann_master_student_list_2021-2025.csv"

# 1. Write new master csv file 
write.csv(new_master_student, file = file.path(box_file_dir,
                                               "College and Career RPP",
                                               "1. NSC Dataset", "Mann",
                                               "Mann PSD",
                                               "Master Student List",
                                               # ⚠️ UPDATE: update year range to reflect new cohort added
                                               "mann_master_student_list_2021-2025.csv"
                                               ),
          row.names = FALSE
) 

# NAMING CONVENTION:
# - Rename output file using:
#   "master-student-list-schoolsitename-yearrange.csv"
# - Example:
#   "master-student-list-rfk-2012-2025.csv"


## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
