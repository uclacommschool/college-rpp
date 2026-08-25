################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 01-merge-nsc-to-psd.R >
## [ AUTH ] < Jeffrey Yo / yjeffrey77, Ariana Dimagiba / aridimagiba >
## [ INIT ] < 4/30/2022, updated 08/25/2026 by aridimagiba >
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
## UPDATE EACH RUN - checklist
## -----------------------------------------------------------------------------

# 1. CONFIG block below — school_site, nsc_academic_year_folder,
#    nsc_effective_date_folder, nsc_detail_report_filename,
#    master_list_filename, previous_psd_filename
# 2. CONFIG block below — output_psd_filename
# Run in order: Parts 1 → 2 → 3 → 4 → 5

## ---------------------------
## ⚠️ CONFIG — everything that changes per run lives here. Update this
## block only; nothing below should need touching for a routine new pull.
## ---------------------------

# ⚠️ UPDATE: school site folder name and PSD folder name
school_site <- "RFK"
school_site_psd_folder <- "RFK PSD"

# ⚠️ UPDATE: NSC report folder structure — academic year folder, then the
# specific effective-date folder for THIS pull (e.g. "2025 November" or
# "2026 April"). Run this script once per effective date (November,
# April, August) — update both for each run.
nsc_academic_year_folder <- "2025-2026 Student Tracker Reports"
nsc_effective_date_folder <- "2026 April"

# ⚠️ UPDATE: current NSC detail report file name (matches the effective
# date folder above)
nsc_detail_report_filename <- "10042443_10042443-215216-DETAIL-EFFDT-20260416-RUNDT-20260526.csv"

# ⚠️ UPDATE: most recent master student list file name
master_list_filename <- "master-student-list-rfk-2012-2025.csv"

# ⚠️ UPDATE: most recent PSD file name — the PSD this run merges into.
# When running this script multiple times in sequence (e.g. November
# then April), point this at the PREVIOUS run's output, not the original
# pre-November snapshot.
previous_psd_filename <- "20260824-rfk-psd-dimagiba.csv"

# ⚠️ UPDATE: this run's output filename (dated per export), following the
# convention "YYYYMMDD-schoolsitename-psd-authorlastname"
output_psd_filename <- "20260825-rfk-psd-dimagiba.csv"

## -----------------------------------------------------------------------------
## load psd helper functions
## -----------------------------------------------------------------------------

#run the psd_core_function_list.R script, which contains all the helper 
#functions to clean and create the NSC data with the existing PSD data.

#use "source" function to run the script: 
source(file.path("psd_core_function_list.R"))

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
nsc_detail_report <- read_csv(file.path(box_file_dir,
                                        "College and Career RPP",
                                        "1. NSC Dataset",
                                        school_site,
                                        "Student Tracker Reports",
                                        nsc_academic_year_folder,
                                        nsc_effective_date_folder,
                                        nsc_detail_report_filename))

#load most recent master student directory file
master_stu_list <- read_csv(file.path(box_file_dir,
                                      "College and Career RPP",
                                      "1. NSC Dataset",
                                      school_site,
                                      school_site_psd_folder,
                                      "Master Student List",
                                      master_list_filename))

#load most recent psd file
previous_psd <- read_csv(file.path(box_file_dir,
                                   "College and Career RPP",
                                   "1. NSC Dataset",
                                   school_site,
                                   school_site_psd_folder,
                                   previous_psd_filename))

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

# 3. Standardize selected college names using college_code
# NOTE: previously this step also printed a manual college_names/num_names
# table for a human to visually scan for unexpected new colleges. That
# check is now automated below (see "Confirm all colleges matched to
# institution_lookup") via an explicit stop() rather than a printed table
# someone has to remember to read — removed here to avoid duplicating the
# same check in two places.
nsc_data<- nsc_data %>%
  mutate(college_name2 = case_when(
    # Cal Poly campuses - NSC sends inconsistent truncated names
    college_code == "001149-00" ~ "CALIFORNIA STATE POLYTECHNIC UNIVERSITY -  HUMBOLDT",
    college_code == "001143-00" ~ "CALIFORNIA STATE POLYTECHNIC UNIVERSITY - SAN LUIS OBISPO",
    college_code == "001144-00"~ "CALIFORNIA STATE POLYTECHNIC UNIVERSITY - POMONA",
    TRUE ~ as.character(college_name))) %>%
  select(
    student_id,  first_name, middle_name, last_name, name_suffix, 
    req_return_field, record_found,high_school_code, hs_grad_date, college_code, 
    college_name2, college_state, cc_4year, public_private,enrollment_begin, 
    enrollment_end, enrollment_status, he_graduated, coll_grad_date, 
    degree_title, major, college_sequence,program_code
  ) %>% 
  rename(college_name = 'college_name2')

# 4. Add PSD-specific variables using add_psd_variables()
# Creates system_type, record_year, record_term, status_source
# enrollment_begin arrives from NSC as YYYYMMDD string
# ymd() in add_psd_variables() converts to date; month() extracts record_term logic
# NOTE: add_psd_variables() has its own internal row-count guard around the
# institution_lookup join. This outer check is a second, independent layer —
# it still catches an inflated row count even if the function is edited
# later and that internal check is accidentally removed.
n_before_psd_vars <- nrow(nsc_data)
nsc_data <- add_psd_variables(nsc_data, institution_lookup)
nsc_data <- assert_row_count_stable(n_before_psd_vars, nsc_data, "add_psd_variables()")

# Confirm new columns were added correctly
expected_psd_cols <- c("system_type", "record_year", "record_term", "status_source")
missing_psd_cols <- expected_psd_cols[!expected_psd_cols %in% names(nsc_data)]

if (length(missing_psd_cols) > 0) {
  stop(paste("⚠️ The following expected columns are missing after add_psd_variables():",
             paste(missing_psd_cols, collapse = ", ")))
}

# Confirm all colleges matched to institution_lookup
# record_found == "N" means NSC found no enrollment at all for that
# student — college_code/college_name are legitimately NA in that case,
# since there's no institution to look up, not a missing institution_lookup
# entry. Only flag rows where NSC actually returned a college_code but it
# still failed to match institution_lookup (a genuine lookup gap).
new_colleges <- nsc_data |>
  filter(is.na(system_type), record_found != "N") |>
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
# - Both checks below used to rely on someone visually scanning printed
#   output (unique()/count()) before deciding whether to proceed. Replaced
#   with explicit stop()/warning() checks so a bad master list halts the
#   script instead of depending on a human catching it.
# - Use the count below for a breakdown by graduating class (kept for
#   reference/debugging — not itself a gate).

# 1. Verify master student list loaded correctly
# Check graduation years present and student counts per cohort
master_stu_list %>%
  count(hs_grad_year)

# Automated sanity check — replaces visual scan of unique(hs_grad_year).
# Flags NA years, or years outside the plausible range (tracking runs back
# to 2012 per program history; can't legitimately be later than this year).
current_year <- as.integer(format(Sys.Date(), "%Y"))

bad_years <- master_stu_list %>%
  filter(is.na(hs_grad_year) | hs_grad_year < 2012 | hs_grad_year > current_year)

if (nrow(bad_years) > 0) {
  stop("⚠️ master_stu_list has invalid hs_grad_year value(s): ",
       paste(sort(unique(bad_years$hs_grad_year)), collapse = ", "),
       ". Investigate before proceeding.")
}

# 2. Confirm expected cohort is present.
# Whether a missing cohort is a hard stop or just a warning depends on
# WHICH pull this is, not just today's date — per the annual schedule,
# only the August pull (right after June's Update Master Student List
# step) has a legitimate reason the newest cohort might not be loaded
# yet. November/April pulls should always have it; treat that as an
# error rather than a warning that's easy to miss.
expected_cohort <- current_year - 1
is_august_pull <- str_detect(nsc_effective_date_folder, "(?i)august")

if (!expected_cohort %in% unique(master_stu_list$hs_grad_year)) {
  msg <- paste0("Class of ", expected_cohort,
                " not found in master student list.")
  if (is_august_pull) {
    warning("⚠️ ", msg,
            " Confirm with counselor before proceeding (expected timing for the August pull).")
  } else {
    stop("⚠️ ", msg, " This cohort should already be loaded for the ",
         nsc_effective_date_folder, " pull — investigate before proceeding.")
  }
}

# 3. Prepare master list to merge with NSC data
master_stu_df <- master_stu_list %>% mutate(
  notes = as.character(notes)) %>% 
  select(student_id, gender, race_ethnicity, poverty_indicator,
         hs_diploma,psd_id,notes)

# 3b. Resolve duplicate/missing student_ids before merging — without this,
# inner_join()'s many-to-many fan-out inflates nsc_data row count (this is
# the exact bug caught by assert_row_count_not_increased() below).
#
# NA student_id: dplyr treats NA == NA as a match, so any NSC row with a
# blank student_id would match EVERY blank-id master row. These can't be
# meaningfully joined at all — pull them out and flag for review rather
# than letting them fan-out into the merge.
master_stu_na <- master_stu_df %>% filter(is.na(student_id))
master_stu_df <- master_stu_df %>% filter(!is.na(student_id))

if (nrow(master_stu_na) > 0) {
  cat("⚠️  ", nrow(master_stu_na), " master list row(s) have no student_id — ",
      "excluded from merge, review separately.\n", sep = "")
}

# True duplicate student_ids (same ID appearing more than once, e.g. a
# student re-entered or updated without removing the old row): confirm
# there's no meaningful difference before collapsing, then keep one row.
dup_check <- master_stu_df %>% count(student_id) %>% filter(n > 1)
if (nrow(dup_check) > 0) {
  cat("⚠️  Duplicate student_id(s) in master list, keeping first row for each:\n")
  print(master_stu_df %>% filter(student_id %in% dup_check$student_id))
}
master_stu_df <- master_stu_df %>% distinct(student_id, .keep_all = TRUE)

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
# NOTE: merge_nsc_master() has its own internal row-count guard (rows may
# legitimately drop via inner_join, but should never increase). This outer
# check is a second, independent layer against the same failure mode.
n_before_merge <- nrow(nsc_data)
nsc_data <- merge_nsc_master(nsc_data, master_stu_df)
nsc_data <- assert_row_count_not_increased(n_before_merge, nsc_data, "merge_nsc_master()")

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

# Compute this run's enrollment/graduation date filter boundaries
# automatically — a fixed, mechanical rule, not something a person needs
# to read off and retype into CONFIG each run:
#   start = day after the latest value already in previous_psd
#   end   = latest value found in THIS NSC pull
# max(..., na.rm = TRUE) on an all-NA column (e.g. a brand-new
# previous_psd with no dates yet) silently returns -Inf rather than
# erroring — checked explicitly below so that case stops the script with
# a clear message instead of producing a nonsensical filter range.
enrollment_filter_start <- max(psd_data$enrollment_begin, na.rm = TRUE) + 1
enrollment_filter_end <- max(nsc_data$enrollment_begin, na.rm = TRUE)
grad_filter_start <- max(psd_data$coll_grad_date, na.rm = TRUE) + 1
grad_filter_end <- max(nsc_data$coll_grad_date, na.rm = TRUE)

date_bounds <- c(enrollment_filter_start = enrollment_filter_start,
                 enrollment_filter_end = enrollment_filter_end,
                 grad_filter_start = grad_filter_start,
                 grad_filter_end = grad_filter_end)

if (any(!is.finite(date_bounds))) {
  stop("Could not compute one or more date filter boundaries — likely an ",
       "all-NA date column in psd_data or nsc_data (e.g. previous_psd has ",
       "no enrollment_begin/coll_grad_date values yet). Investigate before ",
       "continuing.")
}

cat("\nDate filter boundaries for this run:\n")
cat("  enrollment:", format(enrollment_filter_start, "%Y-%m-%d"), "to",
    format(enrollment_filter_end, "%Y-%m-%d"), "\n")
cat("  graduation:", format(grad_filter_start, "%Y-%m-%d"), "to",
    format(grad_filter_end, "%Y-%m-%d"), "\n\n")

## -----------------------------------------------------------------------------
## Part 5 - Select and bind new NSC file records with most recent PSD
## -----------------------------------------------------------------------------

# 1. Create smaller dataframe with NEW college enrollment records
# Filters to NEW enrollment records only — avoids duplicating records
# already in previous_psd. enrollment_filter_start/end computed
# automatically above.
nsc_enrollment_data <- nsc_data %>% 
  filter(between(enrollment_begin, enrollment_filter_start, enrollment_filter_end))

# 2. Create smaller dataframe with NEW college graduation records
# Filters to NEW graduation records only — avoids duplicating records
# already in previous_psd. grad_filter_start/end computed automatically
# above.
nsc_grads_data <- nsc_data %>% 
  filter(between(coll_grad_date, grad_filter_start, grad_filter_end))

# 3. Confirm all data frames have the same 34 variable columns and class types
# Check column names match across all three data frames

stopifnot(
  "Column mismatch: psd_data vs nsc_enrollment_data" =
    identical(names(psd_data), names(nsc_enrollment_data)),
  "Column mismatch: psd_data vs nsc_grads_data" =
    identical(names(psd_data), names(nsc_grads_data))
)


# 4. Check class types match across all three data frames
check_type(list(nsc_enrollment_data, nsc_grads_data, psd_data),
           c("nsc_enrollment", "nsc_grads", "psd"))

check_type_mismatch(list(nsc_enrollment_data, nsc_grads_data, psd_data),
                    c("nsc_enrollment", "nsc_grads", "psd"))

# 5. Bind to enrollment and graduation records to most up-to-date PSD 

current_psd<-bind_rows(
  psd = psd_data,
  enrollment = nsc_enrollment_data,
  graduation = nsc_grads_data
)

# Confirm current_psd is larger than previous PSD
if (nrow(current_psd) < nrow(psd_data)) {
  stop("⚠️ current_psd has fewer rows than previous PSD — something went wrong in binding")
}

# 6. Sort by consistency and readability ----
current_psd <- current_psd %>%
  arrange(hs_grad_date,last_name, first_name, middle_name, enrollment_begin)

# 7. Format dates for export
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
# ⚠️ UPDATE output_psd_filename in CONFIG each run — not here.

write.csv(current_psd,
          file = file.path(box_file_dir,
                           "College and Career RPP",
                           "1. NSC Dataset",
                           school_site,
                           school_site_psd_folder,
                           output_psd_filename),
          row.names = FALSE)

# Confirm the file was exported to Box folder
cat("✅ Export complete:", nrow(current_psd), "rows written.\n")

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------