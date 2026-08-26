################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < 04-clean-missing-list.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 9/3/25, updated 08/24/2026 Ariana Dimagiba >
##
################################################################################

#Goal: Transforms completed school-facing follow-up responses into
#PSD-compatible records. Produces a cleaned, PSD-column-shaped export
#(clean_data) for a separate merge script to bind into the existing PSD —
#this script does NOT perform that bind itself.

#To do this: read the completed WORKING follow-up workbook, validate and
#clean it into an internal missing list, backfill identity/demographic
#fields from the master student list and prior PSD, then apply the
#template-driven transformation (template_code + final_follow_up_note ->
#PSD fields) before exporting.

################################################################################
## ⚠️ PENDING — tracked here for continuity across sessions (not yet done):
##
## 1. Update 02-create-missing-list-from-psd.R for the new folder structure:
##    - route its regular missing-list output to "Missing List - Internal"
##    - route its new stop-tracking export to "Stop Tracking" (new folder)
##
## 2. Build the new merge script (does not exist yet). Per the
##    tracking_status/stop-tracking redesign, it should:
##    - read the existing PSD
##    - read 04's cleaned output from "Missing List - Clean"
##      (followup_clean_filename, this script's Part 7 export)
##    - read 02's stop-tracking export from "Stop Tracking"
##    - standardize schemas/column classes, bind_rows(), validate, export
##      a new dated PSD snapshot
##    - this is also where tracking_status ("active"/"stopped") records
##      get constructed and appended, per the stop-tracking redesign doc
################################################################################

## ---------------------------
## libraries
## ---------------------------

library(tidyverse)     # dplyr, stringr, purrr, etc. — core data wrangling
library(data.table)    # fread() — reads the missing-list CSV from script 02
library(readxl)        # excel_sheets(), read_excel() — reads the manually-exported
# .xlsx snapshot of the school-facing missing list (see Part 1)
library(janitor)       # clean_names() — standardizes column name formatting

## ---------------------------
## ⚠️ CONFIG — everything that changes per run lives here. Update this
## block only; nothing below should need touching for a routine new pull.
## ---------------------------

# ⚠️ UPDATE: school site folder name and PSD folder name
school_site <- "RFK"
school_site_psd_folder <- "RFK PSD"

# ⚠️ UPDATE: high_school_code for respective school site 
high_school_code <- "051662"

# The Fall term year for the CURRENT cycle (not the graduate's HS grad
# year). Anchors row generation in template_lookup in Part 2.
# ⚠️ UPDATE: change to the current cycle's Fall year each pull (i.e. 2026L
# for Fall 2026). Don't forget the trailing L, so R stores this as a true
# integer rather than a double.
current_cycle_fall_year <- 2025L

# ⚠️ UPDATE: current WORKING file name (Section 7.7 step 1)
working_list_filename <- "WORKING_2025-2026 Post-Paths Final's Follow up.xlsx"

# ⚠️ UPDATE: most recent master student list file name (Section 7.2)
master_list_filename <- "master-student-list-rfk-2012-2025.csv"

# ⚠️ UPDATE: most recent PSD file name (dated per export, e.g. from 01-merge)
previous_psd_filename <- "20260818-rfk-psd-dimagiba.csv"

# ⚠️ UPDATE: this run's cleaned output filename (dated per export). This
# is script 04's final PSD-ready output — lands in "Missing Data Follow
# Up" (repurposed per the tracking_status/stop-tracking redesign) and is
# read by the separate merge script alongside the existing PSD and script
# 02's stop-tracking export. This script does NOT bind to the PSD itself.
followup_clean_filename <- "20260818-rfk-followup-clean-dimagiba.csv"

# ⚠️ UPDATE: this run's excluded-records audit file (dated per export).
# Preserves the S/T (Duplicate/Superseded) rows dropped from clean_data
# in Part 5 Step 2, so the reason each was excluded isn't lost once the R
# session ends — previously computed but never written to disk.
excluded_records_filename <- "20260818-rfk-excluded-records-dimagiba.csv"

# ⚠️ UPDATE: manually-entered HS graduation dates for any cohort not yet
# reflected in previous_psd (hasn't been through a PSD merge yet). Add a
# row for each affected year — not limited to a single "newest" cohort,
# since more than one recent year can be missing at once. 
# Dates are plain strings, matching hs_grad_date_lookup's existing
# character type (previous_psd's dates never parse as true Date, due to
# NSC's mixed historical date formats) — don't wrap in as.Date().
manual_hs_grad_dates <- tibble::tribble(
  ~hs_grad_year, ~hs_grad_date,
  2025,          as.Date("2025-06-09"),
)

## ---------------------------
## directory paths
## ---------------------------

#see current directory
getwd()

#set current directory
code_file_dir<-file.path(".")

data_file_dir<-file.path("..","..")

# Detect OS and set Box path accordingly
if (.Platform$OS.type == "windows") {
  box_file_dir <- file.path(Sys.getenv("USERPROFILE"), "Box")
} else {
  # Box Drive syncs via CloudStorage on Mac
  box_file_dir <- file.path(Sys.getenv("HOME"), "Library", "CloudStorage", "Box-Box")
}

# set snapshot file path to the WORKING copy of the Postsecondary Paths Follow Up List.
# ⚠️ Folder reorganization (per stop-tracking/tracking_status redesign):
# "Missing List - Clean" now holds THIS script's cleaned, PSD-ready
# OUTPUT (Part 7) — the school-facing WORKING file this script READS
# moved to "Missing List - External" instead. No academic-year subfolder
# — flat structure, per team decision.
missing_list_snapshot <- file.path(box_file_dir,
                                   "College and Career RPP",
                                   "1. NSC Dataset",
                                   school_site,
                                   school_site_psd_folder,
                                   "Missing List - External",
                                   working_list_filename)

## ---------------------------
## load helper functions and lookup tables 
## ---------------------------
# load helper functions 
source(file.path("psd_core_function_list.R"))
source(file.path("clean_missing_list_function_list.R"))

# load institution lookup reference table - one row per college/institution
institution_lookup <- read_csv(file.path(box_file_dir,
                                         "College and Career RPP",
                                         "1. NSC Dataset",
                                         "institution_lookup.csv"))

# load most recent master student list — one row per student, standing
# roster maintained via Section 7.2 (00-update-master-student-list.R).
# Used for identity backfill (student_id, first_name, last_name,
# hs_grad_year) in Part 4, replacing the old previous_term/psd_merge_list
# approach, which depended on psd_merge_list already existing in the R
# session from a prior script — an undocumented cross-script dependency
# that broke if this script ran on its own.
master_stu_list <- read_csv(file.path(box_file_dir,
                                      "College and Career RPP",
                                      "1. NSC Dataset",
                                      school_site,
                                      school_site_psd_folder,
                                      "Master Student List",
                                      master_list_filename)) %>%
  clean_names()

# load current PSD (most recent merge, e.g. from 01-merge) — used only to
# backfill hs_grad_date in Part 4, since ceremony dates shift year to year
# and can't be computed from hs_grad_year alone. 
previous_psd <- read_csv(file.path(box_file_dir,
                                   "College and Career RPP",
                                   "1. NSC Dataset",
                                   school_site,
                                   school_site_psd_folder,
                                   previous_psd_filename))

#parse dates from previous_psd to ensure dates across files are normalized
previous_psd <- previous_psd %>% parse_dates()

# Validation Check: capture a before-count of non-missing hs_grad_date values to 
# confirm none of the values silently became NA after parse_dates() runs below.
n_hs_grad_date_before <- previous_psd %>% filter(!is.na(hs_grad_date)) %>% nrow()

previous_psd <- previous_psd %>% parse_dates()

n_hs_grad_date_after <- previous_psd %>% filter(!is.na(hs_grad_date)) %>% nrow()

if (n_hs_grad_date_after < n_hs_grad_date_before) {
  warning(n_hs_grad_date_before - n_hs_grad_date_after, " hs_grad_date ",
          "value(s) failed to parse with any known format (YYYYMMDD, ",
          "M/D/YY, YYYY-MM-DD, M/D/YY H:MM) and became NA — a new, ",
          "unhandled date format may exist. Investigate before trusting ",
          "hs_grad_date_lookup below.")
}

# Derive one hs_grad_date per cohort year via mode — a cohort-level fact, not a per-student one
hs_grad_date_lookup <- previous_psd %>%
  filter(!is.na(hs_grad_date), !is.na(hs_grad_year)) %>%
  count(hs_grad_year, hs_grad_date, sort = TRUE) %>%
  group_by(hs_grad_year) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(hs_grad_year, hs_grad_date)

# Fill in manually-entered dates for any cohort not yet reflected in previous_psd 
hs_grad_date_lookup <- hs_grad_date_lookup %>%
  bind_rows(
    manual_hs_grad_dates %>% filter(!hs_grad_year %in% hs_grad_date_lookup$hs_grad_year)
  )

# Validation check for duplicates dates
dup_grad_date <- hs_grad_date_lookup %>% count(hs_grad_year) %>% filter(n > 1)
if (nrow(dup_grad_date) > 0) {
  warning(nrow(dup_grad_date), " hs_grad_year(s) still have ambiguous ",
          "hs_grad_date after taking the mode — verify before continuing.")
}

## -----------------------------------------------------------------------------
## Part 1 - Read in WORKING_Postsecondary Path Follow Up List
## -----------------------------------------------------------------------------
# 1. Read in the Postsecondary Path Follow Up List into R from the manually-exported .xlsx
# snapshot (see Section 7.6 step 6a). Double check missing_list_snapshot
# above points to the correct, current dated file before running.

# pull the list of tab names (each tab = one cohort year, e.g. "2017", "2018", ...)
all_sheets <- excel_sheets(missing_list_snapshot)

# keep only tabs that are actual cohort years (4-digit year) — drops any
# non-cohort tabs (e.g. "Instructions", "Summary") that may exist in the
# WORKING file but aren't graduate data
sheets <- all_sheets[str_detect(str_trim(all_sheets), "^\\d{4}$")]
dropped_sheets <- setdiff(all_sheets, sheets)
if (length(dropped_sheets) > 0) {
  message("Skipping non-cohort tab(s): ", str_c(dropped_sheets, collapse = ", "))
}

# 2. Read every cohort tab into a list of dataframes, one element per tab
follow_up_list<- map(sheets, function(x) {
  tryCatch(
    read_excel(missing_list_snapshot, sheet = x),  # attempt to read the current tab
    error = function(e) {
      # on failure, report which tab and why, then return NULL for that tab
      # rather than halting the whole map() loop
      message("Failed on sheet: ", x, " — ", conditionMessage(e))
      NULL
    }
  )
})

# 3. Name each list element after its source tab (cohort year), so each
# dataframe is traceable back to the tab it came from
names(follow_up_list)<-sheets

## -----------------------------------------------------------------------------
## Part 2 - Clean Follow Up List
## -----------------------------------------------------------------------------
# 1. Clean follow_up_list by standardizing column names (lowercase, 
# underscores, no special characters) across every tab in the list
follow_up_list<-map(follow_up_list, clean_names)


# 2. make sure columns are the same length and the same by printing column names 
# per tab — used to manually inspect which tabs have extra/missing/misnamed columns 
# before the fixes below
map(sheets, function(x){follow_up_list[[x]] %>% colnames()})

# 3. Apply the function to every tab at once — no manual per-tab indexing needed
follow_up_list <- map(follow_up_list, standardize_tab)

# 5. Check column names in console to confirm all 9 tabs now match col_names 
# (should print 7 identical column name sets, one per cohort tab)
map(follow_up_list, colnames)

# 6. Merge list into one group stack all cleaned tabs into one dataframe; 
# .id = "cohort" adds a column recording which list index/tab each row came from
follow_up_responses<-bind_rows(follow_up_list, .id = "cohort")

## -----------------------------------------------------------------------------
## Part 3 - Validate combined data before finalizing
## -----------------------------------------------------------------------------
# GUIDANCE: bind_rows() will silently succeed even if a tab was dropped,
# duplicated, or came out with unexpected structure. These checks stop the
# script immediately (via stop()) with a clear error message if anything
# looks wrong, rather than letting a bad merge silently flow into later
# steps (joins, exports, etc.) where it would be much harder to trace back.

# 1. Row count check: total rows after binding should equal the sum of
#    rows across all individual tabs — if not, something was dropped or
#    duplicated during the bind
expected_rows <- sum(map_dbl(follow_up_list, nrow))
actual_rows <- nrow(follow_up_responses)

if (expected_rows != actual_rows) {
  stop("VALIDATION FAILED: row count mismatch after bind_rows(). Expected ",
       expected_rows, " rows (sum across all tabs) but got ", actual_rows,
       ". Check standardize_tab() output per tab before continuing.")
}

# 2. Cohort check: confirm every expected tab/cohort year actually appears
#    in the combined data (catches a tab silently failing to bind)
missing_cohorts <- setdiff(sheets, unique(follow_up_responses$cohort))

if (length(missing_cohorts) > 0) {
  stop("VALIDATION FAILED: the following cohort tab(s) are missing from ",
       "the combined data: ", str_c(missing_cohorts, collapse = ", "),
       ". Check whether these tabs failed to read or bind.")
}

# 3. Column check: confirm the expected 6 columns (5 target + cohort) are present
expected_cols <- c("cohort", "psd_id", "first_name", "last_name", "template_code",
                   "final_follow_up_note")

if (!setequal(colnames(follow_up_responses), expected_cols)) {
  stop("VALIDATION FAILED: unexpected columns in combined data.\n",
       "Expected: ", str_c(expected_cols, collapse = ", "), "\n",
       "Got: ", str_c(colnames(follow_up_responses), collapse = ", "))
}

# 4. Duplicate check: Flag genuinely duplicate entries — same psd_id AND
#    same template_code appearing more than once within a cohort (the same
#    status documented twice, likely a data entry error). A psd_id appearing 
#    multiple times with DIFFERENT template_codes is expected and valid
dup_check <- follow_up_responses %>% 
  filter(!is.na(psd_id)) %>%
  count(cohort, psd_id, template_code) %>% 
  filter(n > 1)

if (nrow(dup_check) > 0) {
  stop("VALIDATION FAILED: duplicate psd_id + template_code found within ",
       "the same cohort (same status documented more than once).\n",
       "Review these cases before continuing:\n",
       paste(capture.output(print(dup_check)), collapse = "\n"))
}

message("All validation checks passed: ", actual_rows, " rows across ",
        length(unique(follow_up_responses$cohort)), " cohorts.")

# 5. Filter away any drop any rows without a psd_id — these are blank/placeholder 
# rows that don't correspond to an actual student record
follow_up_responses<-follow_up_responses %>% filter(!is.na(psd_id))

## -----------------------------------------------------------------------------
## Part 4 - Transform Internal Missing List to PSD format
## -----------------------------------------------------------------------------

# 1. Backfill identity fields from master_stu_list and hs_grad_date from hs_grad_date_lookup 
# (derived from previous_psd above)

# sanity check: confirm master_stu_list really is one row per psd_id,
# rather than silently deduplicating away a real problem if it isn't
dup_master <- master_stu_list %>% count(psd_id) %>% filter(n > 1)
if (nrow(dup_master) > 0) {
  warning(nrow(dup_master), " psd_id(s) appear more than once in ",
          "master_stu_list — verify this is expected before continuing.")
}

# 2. Select missing columns in the master student list. Do not include notes, first_name,
# and last_name.
master_stu_bf <- master_stu_list %>%
  select(student_id, middle_name, hs_grad_year, gender, race_ethnicity,
         poverty_indicator, hs_diploma, psd_id)

# 3. Merge master_stu_bf with the follow_up_responses
psd_missing_list <- follow_up_responses %>%
  # add demographic information
  left_join(master_stu_bf, by = "psd_id") %>%
  # add hs_grad_date
  left_join(hs_grad_date_lookup, by = "hs_grad_year") %>%
  mutate(
    # add variables only generated from the NSC StudentTracker report and not 
    # applicable 
    record_found = NA,
    req_return_field = NA,
    high_school_code = high_school_code,
    enrollment_begin = NA,
    enrollment_end = NA,
    enrollment_status = NA,
    college_sequence = NA,
    program_code = NA,
    # tracking_status is always "active" for every record this script
    # produces — if a graduate had enough information collected to
    # generate ANY follow-up record this cycle (Template R missing-data
    # included), they're by definition still being actively tracked, not
    # stopped. "stopped" records are constructed separately by
    # 02-create-missing-list-from-psd.R, per the tracking_status/
    # stop-tracking redesign.
    tracking_status = "active",
    
  )

#check
colnames(psd_missing_list)

psd_missing_list %>% count(hs_grad_year)

# 4. Flag graduates who don't appear in master_stu_list at all — a failed
# left_join here means student_id/hs_grad_year/demographics are all
# silently NA, which is very different from a graduate legitimately
# having a blank field. Sets review_flag so this flows into the same
# needs_review check Part 5 already uses for institution-matching issues,
# rather than a separate, disconnected mechanism.
psd_missing_list <- psd_missing_list %>%
  mutate(review_flag = if_else(is.na(student_id), "MISSING_FROM_MASTER_LIST", NA_character_))

# 5. Flag graduates with an unresolved hs_grad_date — matters for
# downstream 6-year completion calculations, so treated as a real review
# item rather than just an informational count. Guarded by is.na(review_flag)
# so it doesn't clobber a MISSING_FROM_MASTER_LIST flag already set above
# for the same row.
psd_missing_list <- psd_missing_list %>%
  mutate(review_flag = case_when(
    is.na(hs_grad_date) & is.na(review_flag) ~ "MISSING_HS_GRAD_DATE",
    TRUE ~ review_flag
  ))

## -----------------------------------------------------------------------------
## Part 5 - Apply Template-Driven Transformation
## -----------------------------------------------------------------------------

# 1. Reset the fields this section rebuilds from the Final Follow Up Note,
# rather than trusting any prior-cycle values. Only fields
# expand_graduate_row() actually sets are reset here — college_code,
# college_state, cc_4year, public_private, and system_type are NOT reset,
# since they're exclusively filled by Step 4's institution_lookup join,
# never by expand_graduate_row() itself. Resetting them here would leave
# a stale all-NA copy that collides with the join's incoming values,
# producing duplicate .x/.y columns instead of one clean column.
psd_missing_list <- psd_missing_list %>%
  mutate(
    college_name = NA, he_graduated = NA,
    coll_grad_date = NA, degree_title = NA, major = NA,
    status_source = NA
  )

# 2. Capture S/T rows separately for the audit trail before they're dropped
# from the merge — the template text itself documents why each was
# excluded, so this file is the record of that decision, not the PSD.
excluded_records <- psd_missing_list %>%
  left_join(template_lookup %>% select(template_code, row_generation),
            by = "template_code") %>%
  filter(row_generation == "excluded")

# 3. Apply expand_graduate_row() to every graduate on the list
clean_data <- psd_missing_list %>%
  mutate(.row_id = row_number()) %>%
  group_split(.row_id) %>%
  purrr::map_dfr(expand_graduate_row, fall_year = current_cycle_fall_year) %>%
  select(-.row_id)

# 4. Re-attach college metadata (state, 2yr/4yr, public/private, system
# type) directly from institution_lookup, now that college_name is a
# properly matched canonical value via match_institution().
clean_data <- clean_data %>%
  left_join(
    institution_lookup %>%
      select(college_name, college_code, college_state, cc_4year,
             public_private, system_type),
    by = "college_name"
  )

# 5. Validate institution matching behaved as expected for A and H —
# these always name a real institution (Verified-tier full enrollment
# detail and graduation detail, respectively). A missing college_code
# here likely means match_institution() fell through to Tier 4
# (UNMATCHED_REVIEW_NEEDED) rather than a parsing issue, since parsing
# failures are already caught separately by review_flag. Sets review_flag
# on these rows (rather than just warning) so they flow into the same
# needs_review check in Step 6, instead of being a disconnected,
# unfiltered console message.

# guard: review_flag only exists as a column if at least one row hit
# PARSE_FAILED/UNRECOGNIZED_TEMPLATE_CODE inside expand_graduate_row().
# If every row parsed successfully this run, the column is entirely
# absent — and case_when() below references it on the right-hand side
# (is.na(review_flag), TRUE ~ review_flag), which requires the column to
# already exist even though it's being filled in. Ensures it's always
# present, all-NA if nothing has flagged anything yet.
if (!"review_flag" %in% names(clean_data)) {
  clean_data$review_flag <- NA_character_
}

clean_data <- clean_data %>%
  mutate(review_flag = case_when(
    template_code %in% c("A", "H") & is.na(college_code) & is.na(review_flag) ~
      "MISSING_INSTITUTION_MATCH",
    TRUE ~ review_flag
  ))

missing_institution_check <- clean_data %>%
  filter(review_flag == "MISSING_INSTITUTION_MATCH")
if (nrow(missing_institution_check) > 0) {
  message(nrow(missing_institution_check), " row(s) with template A/H are ",
          "missing institution_lookup fields (college_code is NA) — likely ",
          "an unmatched college name (see match_institution()'s ",
          "UNMATCHED_REVIEW_NEEDED tier). Flagged as review_flag = ",
          "'MISSING_INSTITUTION_MATCH' — see needs_review in Step 6.")
}

# 6. Flag rows needing manual review — unrecognized template codes,
# notes that didn't match the expected pattern for their template, or
# missing institution matches from Step 5. NA college_name is NOT a
# review flag on its own — it's legitimately NA for anyone in
# Working/Military/Other Pathway/Missing Data/Stopped Out/Intent to
# Transfer categories.
# STOPS the script rather than just warning — flagged rows represent
# genuinely unresolved parsing/matching problems, and letting the script
# continue to Part 6's export with them still unresolved risks writing
# bad/incomplete data into the PSD. Matches the same stop()-on-genuine-
# problem philosophy already used for the row count/cohort/column/
# duplicate checks in Part 3.
needs_review <- clean_data %>% filter(!is.na(review_flag))
if (nrow(needs_review) > 0) {
  stop(nrow(needs_review), " row(s) flagged for manual review — halting ",
       "before export. Review and resolve each row's review_flag (see ",
       "needs_review) before re-running this script:\n",
       paste(capture.output(print(needs_review %>% count(template_code, review_flag))),
             collapse = "\n"))
}

# 7. Row-count reconciliation: confirm every graduate from psd_missing_list
# ended up EITHER excluded (S/T, captured in excluded_records) OR present
# in clean_data with at least one row — never silently dropped for some
# other reason. This doesn't predict exact row counts (multi-row templates
# like R/hedge and continued-enrollment A/I make that combinatorial), but
# it does guarantee no graduate vanishes without a trace — exactly the
# kind of thing a dropped line (like the matched_name/college_name bugs
# we already hit twice) could otherwise cause silently.
all_input_ids <- unique(psd_missing_list$psd_id)
accounted_ids <- unique(c(clean_data$psd_id, excluded_records$psd_id))
unaccounted_ids <- setdiff(all_input_ids, accounted_ids)

if (length(unaccounted_ids) > 0) {
  stop(length(unaccounted_ids), " psd_id(s) went into Part 5 but never ",
       "appeared in clean_data OR excluded_records — investigate before ",
       "continuing:\n", paste(unaccounted_ids, collapse = ", "))
}

# 8. Final duplicate check on clean_data's OUTPUT — confirm no psd_id ends
# up with two rows sharing the same record_term + record_year, which
# would indicate an unexpected collision (e.g., two different templates
# both landing on the same term for the same graduate) rather than a
# legitimate multi-row expansion.
output_dup_check <- clean_data %>%
  filter(!is.na(record_term), !is.na(record_year)) %>%
  count(psd_id, record_term, record_year) %>%
  filter(n > 1)

if (nrow(output_dup_check) > 0) {
  stop(nrow(output_dup_check), " psd_id(s) have duplicate record_term + ",
       "record_year combinations in clean_data — review before ",
       "continuing:\n",
       paste(capture.output(print(output_dup_check)), collapse = "\n"))
}

check <- clean_data %>%
  left_join(template_lookup %>% select(template_code, category), by = "template_code") %>%
  count(he_graduated, category, template_code)

## -----------------------------------------------------------------------------
## Part 6 - Finalize Clean Data for the Merge Script
## -----------------------------------------------------------------------------

# 1. Finalize clean_data for export: standardize casing to match NSC's own
# ALL-CAPS convention on free-text fields, and reorder/select down to
# exactly the real PSD's 34 columns.

# name_suffix is an NSC-only field, never populated by follow-up templates
# (same category as enrollment_status/record_found/req_return_field) — not
# present in master_stu_list, so it needs to be added as NA here, same as
# the other NSC-only fields Part 4 already sets to NA.
if (!"name_suffix" %in% names(clean_data)) {
  clean_data$name_suffix <- NA_character_
}

# enrollment_status is NSC-only — always NA for follow-up-derived records,
# never populated by any template (matches the guide's existing convention
# for coll_grad_date/enrollment_begin/enrollment_end).
clean_data$enrollment_status <- NA

# Uppercase free-text fields to match NSC's own ALL-CAPS convention.
# Excludes: record_term/status_source (lowercase conventions the pipeline's
# own logic depends on), psd_id/student_id/college_code (identifiers, not
# free text), and system_type/public_private/cc_4year/college_state (left
# exactly as institution_lookup.csv has them, since that's the
# authoritative reference, not something this script should reformat).
uppercase_cols <- c("degree_title", "major", "program_code", "gender",
                    "race_ethnicity", "poverty_indicator", "hs_diploma",
                    "first_name", "middle_name", "last_name")

clean_data <- clean_data %>%
  mutate(across(all_of(uppercase_cols), str_to_upper))

# Reorder/select down to the real PSD's columns plus the new
# tracking_status field, dropping working-only columns (cohort,
# template_code, final_follow_up_note, review_flag) that aren't part of
# the PSD schema at all. tracking_status is new (per the stop-tracking
# redesign) — not yet part of previous_psd's existing 34 columns, since
# the underlying CSV hasn't been migrated to include it yet. Placed near
# status_source/record_term/record_year, the other tracking-related
# metadata fields.
psd_column_order <- c(
  "student_id", "first_name", "middle_name", "last_name", "name_suffix",
  "record_found", "req_return_field", "high_school_code", "hs_grad_date",
  "college_code", "college_name", "college_state", "cc_4year", "public_private",
  "enrollment_begin", "enrollment_end", "enrollment_status", "he_graduated",
  "coll_grad_date", "degree_title", "major", "college_sequence", "program_code",
  "status_source", "tracking_status", "record_year", "record_term", "system_type",
  "hs_grad_year", "gender", "race_ethnicity", "poverty_indicator", "hs_diploma",
  "notes", "psd_id"
)

clean_data <- clean_data %>%
  select(all_of(psd_column_order))

## -----------------------------------------------------------------------------
## Part 7 - Export Files
## -----------------------------------------------------------------------------

# 1. Export clean_data — script 04's final PSD-ready output. This is NOT
# bound to the existing PSD here; that's the separate merge script's job
# (per the tracking_status/stop-tracking redesign), which reads this file
# alongside the existing PSD and script 02's stop-tracking export.
write.csv(clean_data,
          file.path(box_file_dir,
                    "College and Career RPP",
                    "1. NSC Dataset",
                    school_site,
                    school_site_psd_folder,
                    "Missing List - Clean",
                    followup_clean_filename),
          row.names = FALSE)

# 2. Export excluded_records — the audit trail for S/T (Duplicate/
# Superseded) rows dropped from clean_data in Part 5 Step 2. Previously
# computed but never written to disk, meaning the record of WHY each row
# was excluded vanished the moment the R session ended. Not read by the
# merge script (these rows are intentionally excluded from the PSD) —
# this is purely a human-readable audit companion to clean_data.
write.csv(excluded_records,
          file.path(box_file_dir,
                    "College and Career RPP",
                    "1. NSC Dataset",
                    school_site,
                    school_site_psd_folder,
                    "Missing List - Clean",
                    excluded_records_filename),
          row.names = FALSE)

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
