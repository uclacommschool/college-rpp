################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 01-merge-nsc-to-psd.R >
## [ AUTH ] < Jeffrey Yo / yjeffrey77, Ariana Dimagiba / aridimagiba >
## [ INIT ] < 4/30/2022, updated 05/18/2026 by aridimagiba >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------
library(readr)
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(openxlsx)
library(janitor)

## ---------------------------
## set working directory
## ---------------------------

# sets working directory to the folder the script is saved in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## ---------------------------
## directory paths
## ---------------------------

code_file_dir<-file.path(".", "clean-psd")

# Detect OS and set Box path accordingly
if (.Platform$OS.type == "windows") {
  box_file_dir <- file.path(Sys.getenv("USERPROFILE"), "Box")
} else {
  # Box Drive syncs via CloudStorage on Mac
  box_file_dir <- file.path(Sys.getenv("HOME"), "Library", "CloudStorage", "Box-Box")
}

## -----------------------------------------------------------------------------
## load psd helper functions
## -----------------------------------------------------------------------------

#run the psd_rfk_function_list.R script, which contains all the helper 
#functions to clean and create the NSC data with the existing PSD data.

#use "source" function to run the script: 
source(file.path("psd_rfk_function_list.R"))

## -----------------------------------------------------------------------------
## UPDATE EACH RUN - checklist
## -----------------------------------------------------------------------------

# 1. nsc_detail_report  — update folder name (e.g. "2025 December") and file name
# 2. master_stu_list    — update file name to most recent master student list
# 3. previous_psd       — update file name to most recent PSD output
# 4. See Part 5         — update enrollment_begin and coll_grad_date date filters
# 5. See Part 5         — update output file name (naming convention below)
# Run in order: Parts 1 → 2 → 3 → 4 → 5


## -----------------------------------------------------------------------------
## load all raw data sets
## -----------------------------------------------------------------------------

# load institution lookup reference table
institution_lookup <- read_csv(file.path(box_file_dir,
                                         "College and Career RPP",
                                         "1. NSC Dataset",
                                         "institution_lookup.csv"))
if (nrow(institution_lookup) == 0) {
  stop("⚠️ institution_lookup.csv loaded but is empty — check file path and contents")
}

#load new nsc student detail csv file
nsc_detail_report <-read_csv(file.path(box_file_dir,
                                       "College and Career RPP",
                                       "1. NSC Dataset",
                                       #⚠️ UPDATE: change to school site
                                       "Mann",
                                       "Student Tracker Reports",
                                       #⚠️ UPDATE: change to current NSC report folder name (e.g. "2025 December")
                                       "2025-2026 Student Tracker Reports",
                                       "2026 April",
                                       # ⚠️ UPDATE: change to current NSC detail report file name
                                       "10102683hsst_10102683-216457-DETAIL-EFFDT-20260416-RUNDT-20260709.csv"
                                       ))

#load most recent master student directory file
master_stu_list<- read_csv(file.path(box_file_dir,
                                     "College and Career RPP",
                                     "1. NSC Dataset",
                                     #⚠️ UPDATE: change to school site
                                     "Mann",
                                     "Mann PSD",
                                     "Master Student List",
                                     #⚠️ UPDATE: change to most recent master student list file name
                                     "mann_master_student_list_2021-2025.csv"
                                     ))

#load most recent psd file
previous_psd <- read_csv(file.path(box_file_dir,
                                   "College and Career RPP",
                                   "1. NSC Dataset",
                                   "Mann",
                                   "Mann PSD",
                                   # ⚠️ UPDATE: change to most recent PSD file name
                                   "20250916-mann-psd-dimagiba.csv"
                                   ))

## -----------------------------------------------------------------------------
##  Part 1 - Clean NSC Dataset
## -----------------------------------------------------------------------------

# 1. Standardize NSC column names using clean_nsc_names()
nsc_data <- clean_names_nsc_data(nsc_detail_report)

# Confirm key columns were renamed correctly
expected_cols <- c("college_code", "record_found", "cc_4year",
                   "he_graduated", "coll_grad_date", "hs_grad_date",
                   "req_return_field")

missing_cols <- expected_cols[!expected_cols %in% names(nsc_data)]

if (length(missing_cols) > 0) {
  stop(paste("⚠️ The following expected columns are missing after clean_nsc_names():",
             paste(missing_cols, collapse = ", "),
             "— NSC file format may have changed"))
}
# 2. Add student ID using add_student_id()
nsc_data <- add_student_id(nsc_data)

# 3. Review college enrollment counts from NSC data
# Useful for spotting unexpected new colleges and confirming high-enrollment institutions
college_names <- nsc_data %>% 
  group_by(college_name) %>% 
  summarize(num_names = n(), .groups = "drop") %>%
  arrange(desc(num_names))
print(college_names) 

# 4. Standardize selected college names using college_code
nsc_data<- nsc_data %>%
  mutate(college_name2 = case_when(
    # Cal Poly campuses - NSC sends inconsistent truncated names
    college_code == "001149-00" ~ "CALIFORNIA STATE POLYTECHNIC UNIVERSITY, HUMBOLDT",
    college_code == "001143-00" ~ "CALIFORNIA STATE POLYTECHNIC UNIVERSITY, SAN LUIS OBISPO",
    college_code == "001144-00"~ "CALIFORNIA STATE POLYTECHNIC UNIVERSITY, POMONA",
    TRUE ~ as.character(college_name))) %>%
  select(
    student_id,  first_name, middle_name, last_name, name_suffix, 
    req_return_field, record_found,high_school_code, hs_grad_date, college_code, 
    college_name2, college_state, cc_4year, public_private,enrollment_begin, 
    enrollment_end, enrollment_status, he_graduated, coll_grad_date, 
    degree_title, major, college_sequence,program_code
    ) %>% 
  rename(college_name = 'college_name2')

# 5. Add PSD-specific variables using add_psd_variables()
# Creates system_type, record_year, record_term, status_source
# enrollment_begin arrives from NSC as YYYYMMDD string
# ymd() in add_psd_variables() converts to date; month() extracts record_term logic
nsc_data <- add_psd_variables(nsc_data, institution_lookup)

# Confirm new columns were added correctly
expected_psd_cols <- c("system_type", "record_year", "record_term", "status_source")
missing_psd_cols <- expected_psd_cols[!expected_psd_cols %in% names(nsc_data)]

if (length(missing_psd_cols) > 0) {
  stop(paste("⚠️ The following expected columns are missing after add_psd_variables():",
             paste(missing_psd_cols, collapse = ", ")))
}

# Confirm all colleges matched to institution_lookup
new_colleges <- nsc_data |>
  filter(is.na(system_type)) |>
  distinct(college_code, college_name) |>
  arrange(college_name)

if (nrow(new_colleges) > 0) {
  cat("⚠️  WARNING: The following colleges are missing from institution_lookup.csv:\n")
  print(new_colleges)
  cat("Add them to institution_lookup.csv in Box before proceeding.\n")
  stop("Unmatched colleges found.")
}
# GUIDANCE:
# - Add new colleges to institution_lookup.csv in Box
# - Refer to PSD documentation to determine correct system_type

## -----------------------------------------------------------------------------
## Part 2 - Clean Master Student List
## -----------------------------------------------------------------------------
# GUIDANCE:
# - Verify that the expected most recent class year appears in the output below.
# - REQUIRED before proceeding: confirm the most recent graduating class is present.
# - If missing: check the correct file was loaded and has been updated by the counselor.
# - Use the count below for a breakdown by graduating class.

# 1. Verify master student list loaded correctly
# Check graduation years present and student counts per cohort
unique(master_stu_list$hs_grad_year)
master_stu_list %>%
  count(hs_grad_year)

# Confirm expected cohort is present — warning only, not a stop
# cohort may not be ready yet in August — confirm with counselor before proceeding
current_year <- as.integer(format(Sys.Date(), "%Y"))
expected_cohort <- current_year - 1

if (!expected_cohort %in% unique(master_stu_list$hs_grad_year)) {
  warning(paste("⚠️ Class of", expected_cohort,
                "not found in master student list.",
                "Confirm with counselor before proceeding."))
}

# 2. Prepare master list to merge with NSC data
master_stu_df <- master_stu_list %>% mutate(
  notes = as.character(notes)) %>% 
  select(student_id, gender, race_ethnicity, poverty_indicator,
         hs_diploma,psd_id,notes)

## -----------------------------------------------------------------------------
## Part 3 - Merge clean nsc data with clean master student list
## -----------------------------------------------------------------------------

# 1. Check for NSC records that don't match master list BEFORE merging
# record_found == "N" means NSC searched but found no enrollment — expected, not an error
# record_found == "Y" with no master list match = student ID mismatch — investigate
# before proceeding. 
# after rm(), the anti-join result is discarded — it was for review only 

nsc_data_anti <- nsc_data %>%
  anti_join(master_stu_df, by = "student_id") %>%
  filter(record_found != "N")

if (nrow(nsc_data_anti) > 0) {
  cat("⚠️  WARNING: The following students have NSC records but no master list match:\n")
  print(nsc_data_anti %>% select(last_name, first_name, student_id, record_found))
  cat("Investigate student ID mismatches before proceeding.\n")
}

rm(nsc_data_anti)

# 2. Merge NSC data with master student list
nsc_data <- merge_nsc_master(nsc_data, master_stu_df)

# Confirm merge produced rows
if (nrow(nsc_data) == 0) {
  stop("⚠️ Merge produced 0 rows — check student_id format matches between NSC and master list")
}

# 3. Assign column classes
nsc_data <- assign_column_classes(nsc_data)

# 4. Parse dates
nsc_data <- parse_dates(nsc_data)

## -----------------------------------------------------------------------------
## Part 4 - Clean previous psd
## -----------------------------------------------------------------------------

# 1. Convert the previous PSD to data frame 
# read_csv returns a tibble converting ensures compatibility downstream
psd_data <- data.frame(previous_psd)

# 2. Assigns correct class types to all columns 
psd_data <- assign_column_classes(psd_data)

# 3. Parse dates columns to Date class
psd_data<- parse_dates(psd_data)

## -----------------------------------------------------------------------------
## Part 5 - Select and bind new NSC file records with most recent PSD
## -----------------------------------------------------------------------------

# 1. Create smaller dataframe with NEW college enrollment records
# GUIDANCE:
# Filters to NEW enrollment records only — avoids duplicating records already in previous PSD
# First date  = day after the latest enrollment_begin from the PREVIOUS NSC pull.
#               You can refer to the last NSC report date in the Box folder name
# To find first date: max(psd_data$enrollment_begin, na.rm = TRUE)
# Second date = latest enrollment_begin in new NSC pull
# To find second date: max(nsc_data$enrollment_begin, na.rm = TRUE)
# Example: filter(between(enrollment_begin, as.Date('2025-07-08'), as.Date('2025-10-27')))
#          First date  = 2025-07-08 (day after latest enrollment_begin in August 2025 NSC pull)
#          Second date = 2025-10-27 (latest enrollment_begin in December 2025 NSC pull)
# ⚠️ UPDATE DATES BELOW each run
nsc_enrollment_data <- nsc_data %>% 
  filter(between(enrollment_begin, as.Date('2025-06-23'), as.Date('2026-02-17')))

# 2. Create smaller dataframe with NEW college graduation records
# GUIDANCE:
# Filters to NEW graduation records only — avoids duplicating records already in previous PSD
# First date  = day after the latest coll_grad_date from the previous psd.
#               You can refer to the last NSC report date in the Box folder name
# To find first date: max(psd_data$coll_grad_date,na.rm = TRUE)
# Second date = latest coll_grad_date in new NSC pull
# To find second date: max(nsc_data$coll_grad_date, na.rm = TRUE)
# Example: filter(between(coll_grad_date, as.Date('2025-06-18'), as.Date('2025-12-02')))
#          First date  = 2025-06-18 (day after latest coll_grad_date in August 2025 NSC pull)
#          Second date = 2025-12-02 (latest coll_grad_date in December 2025 NSC pull)
# ⚠️ UPDATE DATES BELOW each run
nsc_grads_data <- nsc_data %>% 
  filter(between(coll_grad_date, as.Date('2025-05-08'), as.Date('2025-12-12')))

# 3a. Confirm all data frames have the same 34 variable columns and class types
# Check column names match across all three data frames

stopifnot(
  "Column mismatch: psd_data vs nsc_enrollment_data" =
    identical(names(psd_data), names(nsc_enrollment_data)),
  "Column mismatch: psd_data vs nsc_grads_data" =
    identical(names(psd_data), names(nsc_grads_data))
)


# 3b. Check class types match across all three data frames
check_type(list(nsc_enrollment_data, nsc_grads_data, psd_data),
           c("nsc_enrollment", "nsc_grads", "psd"))

check_type_mismatch(list(nsc_enrollment_data, nsc_grads_data, psd_data),
                    c("nsc_enrollment", "nsc_grads", "psd"))

# 4. Bind to enrollment and graduation records to most up-to-date PSD 

current_psd<-bind_rows(
  psd = psd_data,
  enrollment = nsc_enrollment_data,
  graduation = nsc_grads_data
)

# Confirm current_psd is larger than previous PSD
if (nrow(current_psd) < nrow(psd_data)) {
  stop("⚠️ current_psd has fewer rows than previous PSD — something went wrong in binding")
}

# 5. Sort by consistency and readability ----
current_psd <- current_psd %>%
  arrange(hs_grad_date,last_name, first_name, middle_name, enrollment_begin)

# 6. Format dates for export
current_psd <- current_psd %>%
  mutate(
    enrollment_begin = format(enrollment_begin, "%Y-%m-%d"),
    enrollment_end = format(enrollment_end, "%Y-%m-%d"),
    coll_grad_date = format(coll_grad_date, "%Y-%m-%d"),
    hs_grad_date = format(hs_grad_date, "%Y-%m-%d")
    ) 

## -----------------------------------------------------------------------------
## Part 6 - Export updated PSD to Box
## -----------------------------------------------------------------------------

#1. Write new psd csv file to Box
# NAMING CONVENTION: "YYYYMMDD-schoolsitename-psd-authorlastname.csv"
# Example: "20260521-rfk-psd-dimagiba.csv"

write.csv(current_psd,
          file = file.path(box_file_dir,
                           "College and Career RPP",
                           "1. NSC Dataset",
                           "Mann",
                           "Mann PSD",
                           #⚠️ UPDATE: change to current date and author name following naming convention
                           "20260721-mann-psd-sanchez.csv"
                           ),
          row.names = FALSE)

# Confirm the file was exported to Box folder
cat("✅ Export complete:", nrow(current_psd), "rows written.\n")

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
