################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < psd-first-college-grad-rates.R >
## [ AUTH ] < Fernando Sanchez >
## [ INIT ] < 10/1/2026 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------
library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(openxlsx)

## ---------------------------
## set working directory
## ---------------------------

# sets working directory to the folder the script is saved in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## ---------------------------
## directory paths
## ---------------------------

# Detect OS and set Box path accordingly
if (.Platform$OS.type == "windows") {
  box_file_dir <- file.path(Sys.getenv("USERPROFILE"), "Box")
} else {
  # Box Drive syncs via CloudStorage on Mac
  box_file_dir <- file.path(Sys.getenv("HOME"), "Library", "CloudStorage", "Box-Box")
}

## -----------------------------------------------------------------------------
## Part 0 VARIABLES TO UPDATE EACH RUN
## -----------------------------------------------------------------------------
# ⚠️  ⚠️  ⚠️  Update the values below before each run. Nothing else in this script
# needs to change. Run in order: Parts 1 → 2 → 3 → 4 → 5 → 6 → 7 → 8
#
# WHAT THIS SCRIPT DOES:
#   For every student in the PSD, finds the FIRST college they enrolled in after
#   high school, then checks if they earned a degree from THAT SAME college
#   within 150% time (4-year college: 6 years / 2-year college: 3 years).
#   Results (one Excel file, one sheet per table), each split by 4-YEAR / 2-YEAR:
#     - Colleges        : every first college, with graduation rate
#     - By cohort       : the same counts by high school class (class of 20XX)
#     - Time to degree  : how many years graduates took, by cohort
#     - Degree types    : (2-year only) which degrees counted as graduated
#
# 1. school_site         - school site subfolder in Box (e.g. "Mann", "RFK")
# 2. psd_folder          - Box folder where the PSD file is saved
#                          (confirm this matches the folder name in Box)
# 3. psd_filename        - PSD file to analyze
#                          NAMING CONVENTION: "YYYYMMDD-schoolsitename-psd-authorlastname.csv"
# 4. data_pull_date      - date the PSD was created (the YYYYMMDD in the file name)
#     GUIDANCE:
#     Students who already graduated are counted as graduated, even if their
#     150% time is not over yet. Students who have NOT graduated and still
#     have time left by this date are shown separately and left OUT of the
#     graduation rate (we cannot assume they will or will not graduate).
# 5. output_folder_name  - folder (inside psd_folder) where the results are saved
#    output_filename     - name of the Excel results file (one file, many sheets)
#                          NAMING CONVENTION: "YYYYMMDD-schoolsitename-grad-rates-authorlastname.xlsx"
# 6. count_certificates  - TRUE = certificates count as a degree
#                          FALSE = only associate's and bachelor's degrees count
# 7. recent_months       - how far back (in months from data_pull_date) an
#                          enrollment record counts as "still enrolled"
#     GUIDANCE:
#     Used only for students who are STILL WITHIN their 150% time window.
#     12 months = enrolled at some point in the last academic year.
#     NSC usually reports through the most recent spring term, so do not set
#     this lower than about 6 months.

school_site        <- "RFK"
psd_folder         <- file.path(box_file_dir,
                                "College and Career RPP",
                                "4. NSC Dataset",
                                school_site,
                                paste(school_site, "PSD"))
psd_filename       <- "20260829-rfk-psd-dimagiba.csv"
data_pull_date     <- as.Date("2026-08-29")
output_folder_name <- "grad-rate-analysis"
output_filename    <- "20261008-rfk-grad-rates-sanchez.xlsx"
count_certificates <- TRUE
recent_months      <- 12

# 150% time (in years) by college type. Only change if the definition changes.
years_4yr <- 6
years_2yr <- 3

# Placeholder "college names" in the PSD that are not a real college (ignored)
generic_names <- c("CCC", "CSU", "UC", "FOR-PROFIT")

## -----------------------------------------------------------------------------
## load psd data set
## -----------------------------------------------------------------------------

# load psd file. All columns are read as text; dates are parsed in Part 1.
# Blanks, "NA", and placeholder text are treated as missing values.
psd_data <- read_csv(file.path(psd_folder, psd_filename),
                     col_types = cols(.default = "c"),
                     na = c("", "NA", "MISSING DATA", "NO ENROLLMENT"))

if (nrow(psd_data) == 0) {
  stop("⚠️ PSD file loaded but is empty. Check file path and contents.")
}

# Confirm the columns this script needs are in the PSD
expected_cols <- c("student_id", "college_code", "college_name", "cc_4year",
                   "enrollment_begin", "he_graduated", "coll_grad_date",
                   "degree_title", "hs_grad_date", "hs_grad_year",
                   "record_year", "record_term")

missing_cols <- expected_cols[!expected_cols %in% names(psd_data)]

if (length(missing_cols) > 0) {
  stop(paste("⚠️ The following expected columns are missing from the PSD:",
             paste(missing_cols, collapse = ", "),
             ". PSD file format may have changed."))
}

cat("Rows loaded:", nrow(psd_data),
    "| Students:", n_distinct(psd_data$student_id), "\n")

## -----------------------------------------------------------------------------
## Part 1 - Helper functions and clean college names
## -----------------------------------------------------------------------------

# 1. Helper: turn date text into a real date
# Works for both PSD date styles: "8/27/12" and "2012-08-27"
to_date <- function(x) {
  as.Date(parse_date_time(x, orders = c("ymd", "mdy"), quiet = TRUE))
}

# 2. Helper: give records with no exact date an approximate one
# Some records (staff / self-reported / old PSD) only have a year + term.
# This gives them an approximate date so they can still be put in order.
term_to_date <- function(year, term) {
  month <- case_when(
    term == "WINTER" ~ 1,
    term == "SPRING" ~ 2,
    term == "SUMMER" ~ 6,
    term == "FALL"   ~ 9,
    TRUE             ~ 1     # e.g. "ENROLLED ANYTIME AFTER FALL"
  )
  as.Date(paste(year, month, 1, sep = "-"))
}

# 3. Standardize college names using college_code
# GUIDANCE:
# The same college can be spelled different ways in the PSD
# (e.g. "CSU - NORTHRIDGE" vs "CSU- NORTHRIDGE", or names cut off).
# Rows that share a college_code get ONE name: the spelling used most often.
# Generic placeholders (see generic_names in Part 0) are set to missing.
psd_data <- psd_data %>%
  mutate(college_name = if_else(college_name %in% generic_names,
                                NA_character_, college_name),
         college_id   = if_else(grepl("^[0-9]{6}-[0-9]{2}$", college_code),
                                college_code, college_name))

name_lookup <- psd_data %>%
  filter(!is.na(college_name)) %>%
  count(college_id, college_name) %>%
  group_by(college_id) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(college_id, clean_name = college_name)

psd_data <- psd_data %>%
  left_join(name_lookup, by = "college_id") %>%
  mutate(college_name = clean_name) %>%
  select(-clean_name)

## -----------------------------------------------------------------------------
## Part 2 - Find each student's FIRST college
## -----------------------------------------------------------------------------

# 1. Keep real enrollment records at a 2-year or 4-year college
# GUIDANCE:
# - Drops "PLANS" rows (intent to enroll, not an actual enrollment)
# - Drops graduation rows (those are used in Part 3)
# - Drops dual enrollment that happened while still in high school
# - enrollment_begin is used when available, otherwise year + term
# - end_date (enrollment_end) is used in Part 4 to find students still enrolled
enrollments <- psd_data %>%
  filter(!is.na(college_name),
         cc_4year %in% c("4-YEAR", "2-YEAR"),
         is.na(record_term)  | record_term != "PLANS",
         is.na(he_graduated) | he_graduated != "Y") %>%
  mutate(start_date = coalesce(to_date(enrollment_begin),
                               term_to_date(record_year, record_term)),
         end_date   = coalesce(to_date(enrollment_end), start_date),
         hs_grad    = to_date(hs_grad_date)) %>%
  filter(!is.na(start_date),
         is.na(hs_grad) | start_date >= hs_grad)

if (nrow(enrollments) == 0) {
  stop("⚠️ No enrollment records found. Check cc_4year and enrollment_begin columns.")
}

# 2. Keep the earliest enrollment for each student = first college
# 3. Set the 150% deadline and check if the window has closed
#    (window_closed = 150% time is over by the data_pull_date)
first_college <- enrollments %>%
  arrange(student_id, start_date) %>%
  group_by(student_id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(years_allowed = if_else(cc_4year == "4-YEAR", years_4yr, years_2yr),
         deadline      = start_date + round(years_allowed * 365.25),
         window_closed = deadline <= data_pull_date) %>%
  select(student_id, class_of = hs_grad_year, college_name, cc_4year,
         start_date, deadline, window_closed)

## -----------------------------------------------------------------------------
## Part 3 - Find degree records
## -----------------------------------------------------------------------------

# 1. Keep graduation records
# coll_grad_date is used when available, otherwise year + term
degrees <- psd_data %>%
  filter(he_graduated == "Y", !is.na(college_name)) %>%
  mutate(grad_date = coalesce(to_date(coll_grad_date),
                              term_to_date(record_year, record_term))) %>%
  select(student_id, college_name, grad_date, degree_title) %>%
  distinct()   # the PSD can list the same degree more than once

# 2. Group degree titles into degree types
# GUIDANCE:
# Degree titles are typed many different ways ("ASSOCIATE OF ARTS",
# "ASSOCIATE'S IN ARTS", "AA TRANSFER"...). This groups them into types.
# degree_levels is in order from highest to lowest; when a student has more
# than one degree, the highest one is reported.
degree_levels <- c("Master's degree",
                   "Bachelor's degree",
                   "Associate degree for transfer (AA-T / AS-T)",
                   "Associate degree",
                   "Certificate",
                   "Other",
                   "Degree title not reported")

degrees <- degrees %>%
  mutate(degree_type = case_when(
    is.na(degree_title)                                    ~ "Degree title not reported",
    grepl("MASTER", degree_title)                          ~ "Master's degree",
    grepl("BACH|BACEL|BACCAL|^B\\.?[AS]\\.?$", degree_title) ~ "Bachelor's degree",
    grepl("TRANSFER|-DTA|AA-T|AS-T", degree_title)         ~ "Associate degree for transfer (AA-T / AS-T)",
    grepl("ASSOC", degree_title)                           ~ "Associate degree",
    grepl("CERTIF", degree_title)                          ~ "Certificate",
    TRUE                                                   ~ "Other"
  ),
  # ordered = TRUE so min() picks the HIGHEST degree (first in degree_levels)
  degree_type = factor(degree_type, levels = degree_levels, ordered = TRUE))

# 3. Drop certificates if count_certificates is FALSE (see Part 0)
if (!count_certificates) {
  degrees <- degrees %>% filter(!grepl("CERTIF", degree_title))
}

## -----------------------------------------------------------------------------
## Part 4 - Check if students graduated from their first college in 150% time
## -----------------------------------------------------------------------------

# 1. Match degrees to the student's FIRST college (any time after starting)
# years_to_degree = years from first enrollment to degree, rounded UP
#   (e.g. 3.8 years = "4 years", 4.3 years = "5 years")
# within_150 = degree earned on or before the 150% deadline
first_college_degrees <- first_college %>%
  inner_join(degrees, by = c("student_id", "college_name")) %>%
  filter(grad_date >= start_date) %>%
  mutate(years_to_degree = pmax(1, ceiling(as.numeric(grad_date - start_date) / 365.25)),
         within_150      = grad_date <= deadline)

# 2. First degree each student earned from their first college (any time)
# Used for the time to degree tables (includes degrees AFTER 150% time)
first_degree <- first_college_degrees %>%
  group_by(student_id) %>%
  summarise(first_grad_date = min(grad_date),
            years_to_degree = min(years_to_degree))

# 3. Students who graduated within 150% time (or before) and their degree type
# If a student earned more than one degree in that time, the highest is kept
on_time <- first_college_degrees %>%
  filter(within_150) %>%
  group_by(student_id) %>%
  summarise(degree_type_150   = as.character(min(degree_type)),
            degree_titles_150 = paste(sort(unique(na.omit(degree_title))),
                                      collapse = "; ")) %>%
  mutate(grad_150 = TRUE)

# 4. Find students with a RECENT enrollment record
# GUIDANCE:
# "Recent" = enrollment that ended within recent_months of data_pull_date
# (see Part 0). Checked two ways:
#   - enrolled_first : still enrolled at their FIRST college
#   - enrolled_other : enrolled at a DIFFERENT college (e.g. transferred)
recent_cutoff <- data_pull_date %m-% months(recent_months)

recent_enrollments <- enrollments %>%
  filter(end_date >= recent_cutoff) %>%
  select(student_id, recent_college = college_name) %>%
  inner_join(select(first_college, student_id, college_name), by = "student_id")

enrolled_first <- recent_enrollments %>%
  filter(recent_college == college_name) %>%
  distinct(student_id) %>%
  mutate(enrolled_first = TRUE)

enrolled_other <- recent_enrollments %>%
  filter(recent_college != college_name) %>%
  distinct(student_id) %>%
  mutate(enrolled_other = TRUE)

# 5. Add the results back to every student (no match = FALSE)
students <- first_college %>%
  left_join(first_degree,   by = "student_id") %>%
  left_join(on_time,        by = "student_id") %>%
  left_join(enrolled_first, by = "student_id") %>%
  left_join(enrolled_other, by = "student_id") %>%
  mutate(grad_150       = coalesce(grad_150, FALSE),
         enrolled_first = coalesce(enrolled_first, FALSE),
         enrolled_other = coalesce(enrolled_other, FALSE))

# 6. Give every student ONE status
# GUIDANCE:
# - Graduated: earned a degree from their first college within 150% time
#   or before (includes students who graduated early, before their window closed)
# - Did not graduate: 150% time is over and no degree from their first college
# - Still in window: has NOT graduated yet but still has time left.
#   These students are NOT in the graduation rate.
students <- students %>%
  mutate(status = case_when(
    grad_150       ~ "Graduated within 150% time",
    window_closed  ~ "Did not graduate within 150% time",
    enrolled_first ~ "Still in window: enrolled at first college",
    enrolled_other ~ "Still in window: enrolled at a different college",
    TRUE           ~ "Still in window: no recent enrollment"
  ))

# Check: every student has exactly one status
stopifnot(nrow(students) == n_distinct(students$student_id))

## -----------------------------------------------------------------------------
## Part 5 - Build college tables
## -----------------------------------------------------------------------------
# GUIDANCE: column definitions for every table below
#   college                 = name of the college
#   first_enrolled          = all students whose FIRST college was this one
#
#   GRADUATION RATE (students whose outcome is known)
#   graduated_150           = graduated from this college within 150% time
#                             or before (includes early graduates)
#   did_not_graduate        = 150% time is over, no degree from this college
#   outcome_known           = graduated_150 + did_not_graduate
#   grad_rate_pct           = graduated_150 / outcome_known x 100
#
#   STILL IN WINDOW (not graduated yet, still have time, NOT in the grad rate)
#   still_in_window         = total students still within their 150% time
#     in_window_enrolled    = ...still enrolled at this college
#     in_window_other       = ...enrolled at a different college
#     in_window_none        = ...no recent enrollment anywhere
#
#   CHECK: first_enrolled = outcome_known + still_in_window

# 1. Helper: count students in each status for any grouping
#    (used for the college tables here and the cohort tables in Part 6)
count_status <- function(grouped_students) {
  grouped_students %>%
    summarise(first_enrolled     = n(),
              graduated_150      = sum(status == "Graduated within 150% time"),
              did_not_graduate   = sum(status == "Did not graduate within 150% time"),
              still_in_window    = sum(grepl("^Still in window", status)),
              in_window_enrolled = sum(status == "Still in window: enrolled at first college"),
              in_window_other    = sum(status == "Still in window: enrolled at a different college"),
              in_window_none     = sum(status == "Still in window: no recent enrollment"),
              .groups = "drop") %>%
    add_grad_rate()
}

# 2. Helper: add outcome_known and grad_rate_pct
# grad_rate_pct is left BLANK when outcome_known = 0 (nothing to divide by:
# every student in that row is still in their window)
add_grad_rate <- function(table) {
  table %>%
    mutate(outcome_known = graduated_150 + did_not_graduate,
           grad_rate_pct = if_else(outcome_known > 0,
                                   round(100 * graduated_150 / outcome_known, 1),
                                   NA_real_),
           .after = did_not_graduate)
}

# 3. Helper: add a TOTAL row at the bottom of a table
# label_col = the first column (e.g. college or class_of) where "TOTAL" goes
add_total_row <- function(table, label_col, label) {
  total_row <- table %>%
    summarise(across(where(is.numeric), sum)) %>%
    mutate(!!label_col := label, .before = 1)
  
  # The grad rate of the TOTAL row is recalculated from the totals
  # (not added up from each row)
  if ("grad_rate_pct" %in% names(table)) {
    total_row <- total_row %>%
      select(-outcome_known, -grad_rate_pct) %>%
      add_grad_rate()
  }
  
  bind_rows(table, total_row)
}

# 4. One row per college
college_table <- students %>%
  group_by(college = college_name, type = cc_4year) %>%
  count_status()

# Check: every student is counted exactly once
stopifnot(all(college_table$first_enrolled ==
                college_table$outcome_known + college_table$still_in_window))

# 5. Split into one table for 4-YEAR and one for 2-YEAR colleges
# Each table lists every college and ends with a TOTAL row for all colleges
# of that type combined.
# GUIDANCE: sort order
# - Highest grad_rate_pct at the top
# - Ties (same rate) are broken by outcome_known, larger colleges first
# - Colleges with a blank rate (no known outcomes yet) go at the bottom
make_type_table <- function(college_type) {
  college_table %>%
    filter(type == college_type) %>%
    select(-type) %>%
    arrange(desc(grad_rate_pct), desc(outcome_known)) %>%
    add_total_row("college", paste("TOTAL", college_type))
}

table_4yr <- make_type_table("4-YEAR")
table_2yr <- make_type_table("2-YEAR")

## -----------------------------------------------------------------------------
## Part 6 - Build cohort, time to degree, and degree type tables
## -----------------------------------------------------------------------------

# 1. BY COHORT: same counts as the college tables, one row per HS class
# class_of = high school graduation year (hs_grad_year)
make_cohort_table <- function(college_type) {
  students %>%
    filter(cc_4year == college_type) %>%
    group_by(class_of) %>%
    count_status() %>%
    arrange(class_of) %>%
    add_total_row("class_of", "TOTAL")
}

cohort_4yr <- make_cohort_table("4-YEAR")
cohort_2yr <- make_cohort_table("2-YEAR")

# 2. TIME TO DEGREE: how many years graduates took, one row per HS class
# GUIDANCE:
# - Counts every student who earned a degree from their FIRST college, at
#   any time, by how many years it took (rounded up, see Part 4)
# - Columns past 150% time are marked "(after 150%)". Those students are
#   counted as did_not_graduate in the other tables, since they finished
#   after the 150% deadline.
# - Recent classes have not had time for the longer columns yet
make_time_table <- function(college_type, years_allowed) {
  grads <- students %>%
    filter(cc_4year == college_type, !is.na(years_to_degree))
  
  max_years   <- max(grads$years_to_degree, years_allowed)
  year_labels <- paste0("in_", 1:max_years, if_else(1:max_years == 1, "_yr", "_yrs"),
                        if_else(1:max_years > years_allowed, " (after 150%)", ""))
  
  grads %>%
    mutate(years_label = factor(year_labels[years_to_degree], levels = year_labels)) %>%
    count(class_of, years_label) %>%
    pivot_wider(names_from   = years_label,
                values_from  = n,
                values_fill  = 0,
                names_expand = TRUE) %>%
    mutate(total_graduated = rowSums(across(starts_with("in_")))) %>%
    arrange(class_of) %>%
    add_total_row("class_of", "TOTAL")
}

time_4yr <- make_time_table("4-YEAR", years_4yr)
time_2yr <- make_time_table("2-YEAR", years_2yr)

# 3. DEGREE TYPES (2-year): which degree counted for students who
#    graduated within 150% time
# GUIDANCE:
# - One row per degree type (see degree_levels in Part 3)
# - Each student is counted ONCE, under their highest degree within 150% time
# - titles_included lists the degree titles (as typed in the PSD) in that type
titles_2yr <- first_college_degrees %>%
  filter(cc_4year == "2-YEAR", within_150) %>%
  group_by(degree_type = as.character(degree_type)) %>%
  summarise(titles_included = paste(sort(unique(na.omit(degree_title))),
                                    collapse = "; "))

degree_types_2yr <- students %>%
  filter(cc_4year == "2-YEAR", grad_150) %>%
  count(degree_type = factor(degree_type_150, levels = degree_levels),
        name = "students") %>%
  arrange(degree_type) %>%
  mutate(degree_type      = as.character(degree_type),
         pct_of_graduates = round(100 * students / sum(students), 1)) %>%
  left_join(titles_2yr, by = "degree_type") %>%
  add_total_row("degree_type", "TOTAL")

## -----------------------------------------------------------------------------
## Part 7 - Print results
## -----------------------------------------------------------------------------

show <- function(title, table) {
  cat("\n##########################################\n")
  cat("  ", title, "\n")
  cat("##########################################\n")
  print(as.data.frame(table), row.names = FALSE)
}

options(width = 250)   # print wide tables on one line

show("4-YEAR COLLEGES", table_4yr)
show("2-YEAR (COMMUNITY) COLLEGES", table_2yr)
show("4-YEAR COLLEGES BY COHORT (class of)", cohort_4yr)
show("2-YEAR COLLEGES BY COHORT (class of)", cohort_2yr)
show("4-YEAR TIME TO DEGREE BY COHORT (class of)", time_4yr)
show("2-YEAR TIME TO DEGREE BY COHORT (class of)", time_2yr)
show("2-YEAR DEGREE TYPES (graduated within 150% time)", degree_types_2yr)

## -----------------------------------------------------------------------------
## Part 8 - Export results to Box (one Excel file, one sheet per table)
## -----------------------------------------------------------------------------

# 1. Create output folder inside the PSD folder (if it does not exist yet)
output_dir <- file.path(psd_folder, output_folder_name)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 2. List the tables to export
# GUIDANCE:
# Name on the left = sheet name in Excel (31 characters max)
# Name on the right = table from Parts 4, 5, and 6
results_sheets <- list(
  "4-Year Colleges"       = table_4yr,
  "2-Year Colleges"       = table_2yr,
  "4-Year by Cohort"      = cohort_4yr,
  "2-Year by Cohort"      = cohort_2yr,
  "4-Year Time to Degree" = time_4yr,
  "2-Year Time to Degree" = time_2yr,
  "2-Year Degree Types"   = degree_types_2yr,
  "Student level"         = students
)

# 3. Write all tables into ONE Excel file, each on its own sheet
# Header row is bold, frozen at the top, and columns are sized to fit
header_style <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2",
                            border = "bottom")

# Dates are shown as YYYY-MM-DD
options(openxlsx.dateFormat = "yyyy-mm-dd")

wb <- createWorkbook()

for (sheet_name in names(results_sheets)) {
  table <- results_sheets[[sheet_name]]
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, table, headerStyle = header_style)
  freezePane(wb, sheet_name, firstRow = TRUE)
  setColWidths(wb, sheet_name, cols = seq_along(table), widths = "auto")
  
  # "auto" makes date columns too narrow (they show as ###), so widen them
  date_cols <- which(sapply(table, inherits, "Date"))
  if (length(date_cols) > 0) {
    setColWidths(wb, sheet_name, cols = date_cols, widths = 12)
  }
}

saveWorkbook(wb, file.path(output_dir, output_filename), overwrite = TRUE)

# Confirm the file was exported to Box folder
cat("✅ Export complete:", length(results_sheets), "sheets written to",
    file.path(output_dir, output_filename), "\n")

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------