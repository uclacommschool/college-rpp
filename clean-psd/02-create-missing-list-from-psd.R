################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 02-create-missing-list-from-psd.R >
## [ AUTH ] < Ariana Dimagiba / aridimagiba >
## [ INIT ] < 03/25/2026, updated 08/17/2026 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------
library(readr)
library(readxl)
library(lubridate)
library(haven)
library(labelled)
library(dplyr)
library(stringr)
library(openxlsx)
library(janitor)
library(data.table)

## ---------------------------
## set working directory
## ---------------------------

# sets working directory to the folder the script is saved in.
#Protects the source() call below (bare relative path) from failing or silently loading the wrong file
# if the session's working directory wasn't already set correctly.
# NOTE: this depends on THIS script's tab being the active/focused one
# in RStudio when the line runs — if a different tab has focus, this
# will set the working directory to that tab's folder instead. Usually
# true when running via Source, but worth knowing if source() ever
# fails to find a file that's genuinely present.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## ---------------------------
## directory paths
## ---------------------------

code_file_dir<-file.path(".", "clean-psd")

data_file_dir<-file.path("..","..")

# Detect OS and set Box path accordingly
if (.Platform$OS.type == "windows") {
  box_file_dir <- file.path(Sys.getenv("USERPROFILE"), "Box")
} else {
  # Box Drive syncs via CloudStorage on Mac
  box_file_dir <- file.path(Sys.getenv("HOME"), "Library", "CloudStorage", "Box-Box")
}

## ---------------------------
## ⚠️ CONFIG — everything that changes per run lives here. Update this
## block only; nothing below should need touching for a routine new pull.
## ---------------------------

# ⚠️ UPDATE: school site folder name and PSD folder name
school_site <- "RFK"
school_site_psd_folder <- "RFK PSD"

# ⚠️ UPDATE: most recent PSD file name (dated per export, e.g. from 01-merge)
current_psd_filename <- "20260825-rfk-psd-dimagiba.csv"

# ⚠️ UPDATE: most recent master student list file name (Section 7.2)
master_list_filename <- "master-student-list-rfk-2012-2025.csv"

# The target year/term this cycle is checking against. This script only
# ever runs following the November NSC pull (per Section 5's schedule),
# so target_record_term is always "fall" — no Winter/Spring/Summer
# target-term scenario exists for this script.
# ⚠️ UPDATE: change target_record_year to the current cycle's Fall year
# each pull.
target_record_year <- 2025
target_record_term <- "fall"

# Minimum consecutive "all missing" years before a student qualifies for
# stop-tracking (see flag_new_stop_track()). Section 7.6 specifies 3.
consecutive_years_threshold <- 3

# How many years back from target_record_year to keep actively tracking
# (Section 8.4: tracking runs 8 years). Derived, not a second hardcoded
# cutoff year — keeping this as its own separate hardcoded year (e.g.
# "2016") alongside target_record_year would require updating both in
# sync every cycle, exactly the kind of drift risk that's broken other
# parts of this pipeline before.
tracking_years_back <- 8
min_hs_grad_year_tracked <- target_record_year - tracking_years_back

# ⚠️ UPDATE: this run's output filenames (dated per export)
stop_track_filename <- "20260826-rfk-stopTrack-dimagiba.csv"
missing_list_internal_filename <- "20260826-rfk-missingListInternal-dimagiba.csv"

## ---------------------------
## load helper functions
## ---------------------------

# provides assign_column_classes() and parse_dates() — the pipeline's
# authoritative schema-enforcement functions, same ones
# 01-merge-nsc-to-psd.R uses. Used below (Part 2) to type-match
# missing_current_year_prepped against the real PSD schema, rather than
# a bespoke dynamic type-match against missing_df's current types.
source(file.path("psd_core_function_list.R"))

## ---------------------------
## local helper functions
## ---------------------------

# FUNCTION: generate_missing_list
# PURPOSE:  Identifies students absent from a target year/term and returns
#           a dataframe of their most recent prior records, updated to the
#           target year/term.
# INPUT:    df — the full dataframe containing all student records
#           record_year — the target year to check against (required; no
#                default — always pass target_record_year from CONFIG)
#           record_term — the target term to check against (required; no
#                default — always pass target_record_term from CONFIG)
# OUTPUT:   A dataframe of missing students with record_year and record_term
#           set to the target values, based on each student's most recent
#           prior record.
# CALLED IN: 02-create-missing-list-from-psd.R, Part 2 step 1

generate_missing_list <- function(df, record_year, record_term) {
  
  # Students present in the target year/term
  target_ids <- df |>
    dplyr::filter(record_year == !!record_year,
                  toupper(trimws(record_term)) == record_term_target) |>
    dplyr::pull(psd_id) |>
    unique()
  
  # Students who completed a 4-YEAR degree (he_graduated == "Y" AND
  # cc_4year == "4-year") — exempt from being re-flagged as missing
  # regardless of whether they have an exact target term/year match.
  # Prevents redundant follow-up work for a graduate whose status is
  # already fully resolved elsewhere in the PSD (the S/T gap from the
  # Technical Guide's pending notes). Deliberately NOT applied to 2-year/
  # community college completions — a CC completion isn't necessarily the
  # end of someone's postsecondary journey (transfer intent still needs
  # tracking), so those students should continue to be checked. This
  # matches the scope of the flag_4yr_grad check that used to run later
  # in Part 2 — moved upstream so it prevents the redundant work in the
  # first place, rather than filtering it out after the fact.
  # ⚠️ NOTE: this script only ever runs following the November NSC pull (per
  # Section 5's schedule), so record_term here is always "fall" — no
  # Winter/Spring/Summer target-term scenario exists for this check yet.
  already_graduated_ids <- df |>
    dplyr::filter(he_graduated == "Y", cc_4year == "4-year") |>
    dplyr::pull(psd_id) |>
    unique()
  
  # All students NOT in the target year/term AND not already graduated
  # from a 4-year institution
  missing_ids <- df |>
    dplyr::filter(!psd_id %in% target_ids, !psd_id %in% already_graduated_ids) |>
    dplyr::pull(psd_id) |>
    unique()
  
  if (length(missing_ids) == 0) {
    message("No missing students found for ", record_year, " ", record_term, ".")
    # Returns df[0, ] (same columns as df, zero rows) rather than a bare
    # dplyr::tibble() (zero columns AND zero rows). Downstream code
    # (missing_current_year_prepped's schema-matching logic) relies on
    # colnames(missing_df) to know what columns to align to — a
    # zero-column tibble would silently break that in a confusing way
    # rather than a clear error, even though "no missing students" is a
    # genuinely valid outcome that shouldn't crash anything.
    return(df[0, ])
  }
  
  # Term ordering for resolving "most recent" across terms within the same year
  # FIX: the raw data contains two spellings of the same term
  # ("ENROLLED ANYTIME AFTER FALL" and "ENROLLED AFTER FALL AT ANYTIME") --
  # both are mapped to the same rank so neither silently falls back to 0
  # and gets treated as older than it actually is.
  term_order <- c("plans"=1,
                  "enrolled anytime after fall" = 2,
                  "enrolled after fall at anytime" = 2,
                  "winter" = 3, "spring" = 4, "summer" = 5, "fall" = 6)
  
  # For each missing student, grab their most recent record
  missing_df <- df |>
    dplyr::filter(psd_id %in% missing_ids) |>
    dplyr::mutate(
      .term_rank = dplyr::coalesce(term_order[tolower(trimws(record_term))], 0L)
    ) |>
    # FIX: some students have multiple PSD rows for the exact same
    # year/term (multiple NSC match records). When those tie on year and
    # term rank, prefer a row that shows he_graduated == "Y" so a
    # graduation flag recorded in a sibling row for that same term isn't
    # discarded in favor of an arbitrary non-graduated duplicate.
    dplyr::arrange(psd_id, dplyr::desc(record_year), dplyr::desc(.term_rank),
                   dplyr::desc(he_graduated == "Y")) |>
    dplyr::slice_head(n = 1, by = psd_id) |>
    dplyr::select(-.term_rank) |>
    # Stamp with target year/term (uppercased to match the PSD's existing
    # record_term convention, so this doesn't reintroduce the case-mismatch
    # bug when this output later gets merged back into next year's PSD)
    dplyr::mutate(
      record_year = !!record_year,
      record_term = record_term_target
    )
  
  message(nrow(missing_df), " missing student(s) found for ",
          record_year, " ", record_term, ".")
  
  return(missing_df)
}

# FUNCTION: flag_stopped_tracking
# PURPOSE:  Adds a binary indicator column to a dataframe flagging students
#           whose 'notes' field contains "stop track" or "stopped tracking"
#           (case-insensitive).
# INPUT:    df — a dataframe containing a 'notes' column
#           indicator_name — name of the new indicator column
#                (default: "stopped_tracking")
# OUTPUT:   The same dataframe with one new integer column
#           (1 = flagged, 0 = not flagged).
# CALLED IN: 02-create-missing-list-from-psd.R, Part 2 step 9

flag_stopped_tracking <- function(df, indicator_name = "stopped_tracking") {
  
  # Regex covers:
  #   "stop track"       (and anything after, e.g. "stop tracking")
  #   "stopped tracking"
  stop_pattern <- "stop\\s+track|stopped\\s+tracking|stopped\\s+track"
  
  df |>
    dplyr::mutate(
      "{indicator_name}" := dplyr::if_else(
        stringr::str_detect(tolower(notes), stop_pattern),
        1L, 0L,
        missing = 0L   # treat NA notes as not flagged
      )
    )
}

# FUNCTION: flag_new_stop_track
# PURPOSE:  Identifies students in the missing list who have had
#           system_type == "MISSING DATA" on EVERY row across ALL terms
#           for 3 or more consecutive full calendar years, with no
#           non-"MISSING DATA" value breaking the streak. Marks qualifying
#           students as stop-track in notes and sets stopped_tracking = 1.
# INPUT:    missing_df — output of generate_missing_list(), already run
#                through flag_stopped_tracking() so stopped_tracking
#                column exists
#           master_df — full master dataset containing all historical
#                records
#           record_year — target year (required; no default — always
#                pass target_record_year from CONFIG). Streak is
#                evaluated on all years strictly before this value.
#           consecutive_years — minimum consecutive "all_missing" years
#                to qualify (required; no default — always pass
#                consecutive_years_threshold from CONFIG)
# OUTPUT:   missing_df with notes and stopped_tracking updated for newly
#           flagged students.
# CALLED IN: 02-create-missing-list-from-psd.R, Part 2 step 5a
#
# NOTE on logic:
#   1. For each student in the master data, classify each year as:
#        - "all_missing" : every row that year has system_type == "MISSING DATA"
#        - "not_missing" : at least one row that year has a different value
#   2. Working backwards from (record_year - 1), count how many consecutive
#      "all_missing" years precede the target year. Any "not_missing" year
#      resets the streak to 0.
#   3. Students whose streak >= consecutive_years qualify.
#   4. For qualifying students in missing_df (not already flagged):
#        - Append "stop track <record_year>" to notes.
#        - Set stopped_tracking = 1.

flag_new_stop_track <- function(missing_df,
                                master_df,
                                record_year,
                                consecutive_years) {
  
  
  # ── Step 1: Classify each (psd_id, year) as all_missing or not ───────────
  # A year is "all_missing" only when every single row for that student
  # in that year has system_type == "MISSING DATA". One non-missing row
  # anywhere in the year disqualifies it.
  year_status <- master_df |>
    dplyr::filter(!is.na(psd_id), record_year < !!record_year) |>
    dplyr::group_by(psd_id, record_year) |>
    dplyr::summarise(
      all_missing = all(trimws(toupper(dplyr::coalesce(system_type, "MISSING DATA"))) == "MISSING DATA"),
      .groups = "drop"
    )
  
  # ── Step 2: Compute trailing consecutive "all_missing" streak per student ─
  # Sort years descending within each student so we can walk back from the
  # most recent year before the target. The streak breaks the moment a year
  # is NOT all_missing.
  compute_streak <- function(student_data) {
    # Rows are already for one psd_id; sort descending by year
    student_data <- student_data[order(-student_data$record_year), ]
    
    streak <- 0L
    for (i in seq_len(nrow(student_data))) {
      if (isTRUE(student_data$all_missing[i])) {
        streak <- streak + 1L
      } else {
        break   # non-missing year resets the streak — stop counting
      }
    }
    streak
  }
  
  streak_df <- year_status |>
    dplyr::group_by(psd_id) |>
    dplyr::group_modify(~ dplyr::tibble(streak = compute_streak(.x))) |>
    dplyr::ungroup()
  
  # ── Step 3: Identify newly qualifying students ────────────────────────────
  # Exclude students already flagged to avoid double-appending notes
  qualifying_ids <- streak_df |>
    dplyr::filter(streak >= !!consecutive_years) |>
    dplyr::pull(psd_id)
  
  new_stop_track_ids <- missing_df |>
    dplyr::filter(psd_id %in% qualifying_ids, stopped_tracking == 0) |>
    dplyr::pull(psd_id)
  
  n_new <- length(new_stop_track_ids)
  
  if (n_new == 0) {
    message("No new stop-track students identified for ", record_year, ".")
    return(missing_df)
  }
  
  # ── Step 4: Update notes and stopped_tracking for qualifying students ─────
  stop_label <- paste("stop track", record_year)
  
  missing_df <- missing_df |>
    dplyr::mutate(
      notes = dplyr::case_when(
        psd_id %in% new_stop_track_ids & is.na(notes)  ~ stop_label,
        psd_id %in% new_stop_track_ids & !is.na(notes) ~ paste(notes, stop_label, sep = "; "),
        TRUE                                            ~ notes
      ),
      stopped_tracking = dplyr::if_else(
        psd_id %in% new_stop_track_ids,
        1L, stopped_tracking
      )
    )
  
  message(n_new, " new stop-track student(s) flagged for ", record_year, ".")
  
  return(missing_df)
}

# FUNCTION: construct_stop_track_record
# PURPOSE:  Transforms a raw stop-track candidate (their most recent prior
#           PSD row, from generate_missing_list()) into a proper
#           PSD-compatible stop-tracking record, per the
#           tracking_status/stop-tracking redesign:
#             - Resets all unconfirmed postsecondary fields to NA —
#               preserves only identifiers and demographic information.
#             - Sets status_source = "inferred", tracking_status = "stopped".
#             - Anchors record_term/record_year to the current cycle.
#             - Writes the standardized Template W note (Postsecondary
#               Status Note Standards) into the notes field, filled in
#               with the actual academic year monitoring was discontinued.
# INPUT:    df — raw stop-track candidates (e.g. stop_track_df, filtered
#                from missing_df by stopped_tracking == 1)
#           record_year — the Fall year monitoring stopped
#                (target_record_year)
#           record_term — the term monitoring stopped (target_record_term
#                — always "fall" for this script, per Section 5's schedule)
# OUTPUT:   df with postsecondary fields reset and tracking metadata applied.
# CALLED IN: 02-create-missing-list-from-psd.R, Part 2 step 5b
#
# NOTE: this does NOT modify or replace any of the student's PRIOR PSD
# records — it produces one NEW administrative record, to be appended by
# the merge script (05-merge-missing-data.R) alongside the existing PSD
# and script 04's cleaned follow-up output. If a previously stopped
# student later reappears in an NSC report or gets new verified info,
# that's just a normal new record with tracking_status = "active" —
# nothing here needs to "undo" this record; historical records are never
# changed retrospectively.

construct_stop_track_record <- function(df, record_year, record_term) {
  
  academic_year_label <- paste0(record_year, "-", record_year + 1)
  
  stop_track_note <- paste0(
    "Graduate has had no confirmed postsecondary enrollment or ",
    "alternative-pathway information for three consecutive academic ",
    "years. Active monitoring discontinued as of ", academic_year_label,
    "; tracking will resume if the graduate appears in a subsequent NSC ",
    "report or new verified information is received."
  )
  
  df %>%
    mutate(
      # Reset all unconfirmed postsecondary fields — preserve only
      # identifiers/demographics (student_id, first_name, middle_name,
      # last_name, name_suffix, psd_id, hs_grad_year, hs_grad_date,
      # high_school_code, gender, race_ethnicity, poverty_indicator,
      # hs_diploma all pass through untouched)
      college_code = NA, college_name = NA, college_state = NA,
      cc_4year = NA, public_private = NA, enrollment_begin = NA,
      enrollment_end = NA, enrollment_status = NA, he_graduated = NA,
      coll_grad_date = NA, degree_title = NA, major = NA,
      college_sequence = NA, program_code = NA, system_type = NA,
      # Tracking metadata
      status_source = "inferred",
      tracking_status = "stopped",
      record_term = record_term,
      record_year = record_year,
      notes = stop_track_note
    )
}

## -----------------------------------------------------------------------------
## load all raw data sets
## -----------------------------------------------------------------------------

#load recently updated psd file from 01-merge script
current_psd <- read_csv(file.path(box_file_dir,
                                  "College and Career RPP",
                                  "1. NSC Dataset",
                                  school_site,
                                  school_site_psd_folder,
                                  current_psd_filename))

#remove first column
#current_psd<-current_psd %>% select(-1)

#load master student directory file
master_stu_list <- read_csv(file.path(box_file_dir,
                                      "College and Career RPP",
                                      "1. NSC Dataset",
                                      school_site,
                                      school_site_psd_folder,
                                      "Master Student List",
                                      master_list_filename))

## -----------------------------------------------------------------------------
## Part 1 - Check Data
## -----------------------------------------------------------------------------
# 1. Use the clean "nsc_data" df from "01-merge" script to confirm range of cohorts 
current_psd %>%
  filter(!is.na(hs_grad_year)) %>%
  summarise(
    min_year = min(hs_grad_year),
    max_year = max(hs_grad_year),
    n_hs_grad_years = n_distinct(hs_grad_year)
  )

# 2. Check for NA psd_id in current_psd — STOPS the script. A row with no
# psd_id can't be classified as "present" or "missing" by any of the
# filter() logic below (NA %in% anything is NA, and filter() silently
# drops NA conditions rather than treating them as FALSE) — meaning these
# rows would vanish from consideration entirely, with no trace, rather
# than being correctly counted as missing or present. Needs fixing in the
# PSD itself before this script can produce a trustworthy list.
na_psd_id_check <- current_psd %>% filter(is.na(psd_id))
if (nrow(na_psd_id_check) > 0) {
  stop(nrow(na_psd_id_check), " row(s) in current_psd have psd_id == NA — ",
       "these would silently vanish from missing/present classification ",
       "entirely. Fix in the PSD before re-running this script.")
}

# 3. Check for duplicate psd_id in current_psd — warns rather than stops,
# since a handful of legacy duplicates may be a known, pre-existing PSD
# issue rather than something this script alone should block on.
dup_psd_check <- current_psd %>% count(psd_id) %>% filter(n > 1)
if (nrow(dup_psd_check) > 0) {
  warning(nrow(dup_psd_check), " psd_id(s) appear more than once in ",
          "current_psd — verify this is expected before continuing.")
}

# 4. Check that the current cycle's graduating class actually exists in
# master_stu_list — if not, missing_current_year (Part 2) will silently
# come back empty, and the "catch this year's newest cohort" step will
# quietly do nothing. Matches the equivalent check already in
# 01-merge-nsc-to-psd.R.
if (!target_record_year %in% unique(master_stu_list$hs_grad_year)) {
  warning("⚠️ Class of ", target_record_year, " not found in master_stu_list. ",
          "Confirm the master list has been updated for this cohort before ",
          "proceeding — otherwise this cycle's newest graduates won't be ",
          "checked for missing data.")
}

# 5. Check for duplicate psd_id in master_stu_list
dup_master_check <- master_stu_list %>% count(psd_id) %>% filter(n > 1)
if (nrow(dup_master_check) > 0) {
  warning(nrow(dup_master_check), " psd_id(s) appear more than once in ",
          "master_stu_list — verify this is expected before continuing.")
}

## -----------------------------------------------------------------------------
## Part 2 - Filter students who Graduated and are not active tracking
## -----------------------------------------------------------------------------
# GUIDANCE 
# - Remove students who have completed a 4-year degree
# - Remove students who we already identified as Inactive or "stop-track"
# - Remove students who are newly inactive with missing data for three consecutive years

# 1. Create Missing List
missing_df <- generate_missing_list(current_psd,
                                    record_year = target_record_year,
                                    record_term = target_record_term)

# 2. Normalize to the pipeline's authoritative PSD schema immediately —
# current_psd came in via read_csv(), and a CSV round-trip doesn't
# guarantee types match the schema (e.g., high_school_code silently
# read in as double instead of character). Doing this now, before
# missing_current_year_prepped is built and bound in below, means both
# sides of that later bind_rows() already conform to the same schema —
# bind_rows() doesn't reliably coerce mismatched types (double vs.
# character errors outright), so both sides need to already match before
# it runs, not fixed up afterward.
missing_df <- missing_df %>%
  assign_column_classes() %>%
  parse_dates()

# 3. Add this cycle's newest graduating class — they won't have any prior
# PSD record at all yet (never went through generate_missing_list()'s
# "present vs. missing" logic, since they have no history to check
# against), so they're identified separately here: anyone in
# master_stu_list for target_record_year who hasn't shown up in
# current_psd at all.
grad_class_current <- master_stu_list %>% filter(hs_grad_year == target_record_year)
current_year_nsc <- current_psd %>% filter(hs_grad_year == target_record_year)
missing_current_year <- grad_class_current %>%
  filter(!c(psd_id %in% current_year_nsc$psd_id)) %>%
  filter(!is.na(psd_id))

# 4. Add all missing columns as NA, then reorder to match missing_df's column
# set — still needed regardless of typing, since missing_current_year
# comes from master_stu_list, which doesn't have every PSD field.
missing_current_year_prepped <- missing_current_year %>%
  mutate(!!!setNames(
    lapply(setdiff(colnames(missing_df), colnames(missing_current_year)), function(x) NA_character_),
    setdiff(colnames(missing_df), colnames(missing_current_year))
  )) %>%
  select(all_of(colnames(missing_df))) %>%
  # Cast to the pipeline's authoritative PSD schema BEFORE the bind below
  # — the NA-fill step above fills every missing column with
  # NA_character_ regardless of what type it should actually be (e.g.
  # high_school_code becomes character here, while missing_df's own
  # high_school_code was just normalized to character too, in the step
  # above — matching types on both sides is what lets bind_rows()
  # succeed; it doesn't reliably coerce double vs. character on its own).
  assign_column_classes() %>%
  parse_dates()

# 5. Add this cycle's newest graduating class' missing students together
missing_df <- bind_rows(missing_df, missing_current_year_prepped)

# 6.Capture the full candidate count now, before any further filtering —
# used by the row-count reconciliation check near the end of this script
# (Part 2, step 14) to confirm every candidate identified here is
# accounted for somewhere in the final outputs, not silently dropped.
n_total_candidates <- dplyr::n_distinct(missing_df$psd_id)

# 7.Flag students who have graduated from a 4-year college
# ⚠️ Intentionally redundant with generate_missing_list()'s
# already_graduated_ids check (same he_graduated=="Y" & cc_4year=="4-year"
# condition, applied upstream) — kept here on purpose as a defense-in-depth
# safety net, e.g. in case missing_current_year_prepped (bound in above, which
# bypasses generate_missing_list() entirely) or some other future code
# path introduces a 4-year grad into missing_df without going through the
# upstream check. Do not remove as "dead code" without understanding why
# it's still here.
missing_df <- missing_df %>%
  mutate(
    flag_4yr_grad = case_when(
      (he_graduated == "Y" & cc_4year == "4-year") ~ 1,
      TRUE ~ 0
    ))


# 8. Filter  students who have graduated from a 4-year college
missing_df<-missing_df %>% filter(flag_4yr_grad != 1)


# 9. Flag and filter students were previously assigned to stop tracking using
# notes variable in recently updated psd from 01-merge

#Flag stop tracking users
missing_df<-flag_stopped_tracking(missing_df)

# Capture how many were already flagged stop-track in a PRIOR cycle,
# before they get filtered out below. These students are intentionally
# dropped from missing_df here — they were already documented as
# stop-tracked before this run, so there's no need to re-export them —
# but the count is needed for the reconciliation check, since without it
# this would otherwise look like a silent, unaccounted drop.
n_already_stopped <- missing_df %>%
  filter(stopped_tracking == 1) %>%
  dplyr::pull(psd_id) %>%
  dplyr::n_distinct()

#filter out stop tracking users
missing_df<-missing_df %>% filter(stopped_tracking == 0)

# 10. Flag new students who have been missing for 3 consecutive years as stop-track

missing_df<-flag_new_stop_track(missing_df = missing_df,
                                master_df = current_psd,
                                record_year = target_record_year,
                                consecutive_years = consecutive_years_threshold)

# 11. Filter and create a new df for new stop-track students
# - will need to merge them in a later script when you merge missing data to psd
stop_track_df<-missing_df %>% filter(stopped_tracking == 1)

# Capture the newly-stop-tracked count before construct_stop_track_record()
# — that function only transforms field values, it doesn't change row
# count, but capturing here (right after the filter that defines this
# bucket) keeps the accounting clear regardless.
n_new_stopped <- dplyr::n_distinct(stop_track_df$psd_id)

# Transform raw candidates into proper PSD-compatible stop-tracking
# records (field resets, status_source/tracking_status, standardized
# Template W note) — see construct_stop_track_record() above.
stop_track_df <- construct_stop_track_record(stop_track_df,
                                             record_year = target_record_year,
                                             record_term = target_record_term)

# 12. Create missing list that we need to get clarified by the school
missing_df_clean<-missing_df %>% filter(stopped_tracking==0)

# 13. Keep only the last N years (see tracking_years_back in CONFIG)
n_outside_window <- missing_df_clean %>%
  filter(hs_grad_year <= min_hs_grad_year_tracked) %>%
  dplyr::pull(psd_id) %>%
  dplyr::n_distinct()

missing_df_clean<-missing_df_clean %>% filter(hs_grad_year > min_hs_grad_year_tracked)

n_active <- dplyr::n_distinct(missing_df_clean$psd_id)

# 14. Row-count reconciliation — confirm every candidate identified back
# in n_total_candidates ends up accounted for in exactly one of: already
# stop-tracked in a prior cycle, newly stop-tracked this cycle, outside
# the tracking window, or still active. STOPS the script if anyone is
# unaccounted for, rather than letting a silent drop flow through to
# export undetected.
n_accounted <- n_already_stopped + n_new_stopped + n_outside_window + n_active
if (n_accounted != n_total_candidates) {
  stop("Row-count reconciliation failed: ", n_total_candidates,
       " total candidate(s) identified, but only ", n_accounted,
       " accounted for (already stopped: ", n_already_stopped,
       "; newly stopped: ", n_new_stopped,
       "; outside tracking window: ", n_outside_window,
       "; active: ", n_active, "). Investigate before continuing.")
}

# 15. Duplicate check on final outputs — confirm no psd_id appears more
# than once in either file about to be exported.
dup_stop_track_check <- stop_track_df %>% count(psd_id) %>% filter(n > 1)
if (nrow(dup_stop_track_check) > 0) {
  stop(nrow(dup_stop_track_check), " psd_id(s) appear more than once in ",
       "stop_track_df — review before continuing.")
}

dup_missing_clean_check <- missing_df_clean %>% count(psd_id) %>% filter(n > 1)
if (nrow(dup_missing_clean_check) > 0) {
  stop(nrow(dup_missing_clean_check), " psd_id(s) appear more than once in ",
       "missing_df_clean — review before continuing.")
}

message("All validation checks passed: ", n_total_candidates,
        " total candidate(s) — ", n_already_stopped, " already stopped, ",
        n_new_stopped, " newly stopped, ", n_outside_window,
        " outside tracking window, ", n_active, " active.")

## -----------------------------------------------------------------------------
## Part 3 - Export Data
## -----------------------------------------------------------------------------

# 1. Export stop-track dataframe
if (nrow(stop_track_df) == 0) {
  message("No new stop-track students this cycle — exporting an empty ",
          "file for consistency (rather than skipping the export).")
}

# Drop working-only columns not part of the real PSD schema —
# flag_4yr_grad (Part 2's defense-in-depth safety net) and
# stopped_tracking (flag_stopped_tracking()'s indicator column) both
# exist purely to support this script's internal logic and were never
# meant to flow into the exported file. Without this, stop_track_df
# exports with 37 columns instead of the expected 35, which 05 then
# needs to reconcile against previous_psd/followup_clean's schema.
stop_track_df <- stop_track_df %>% select(-flag_4yr_grad, -stopped_tracking)

# NAMING CONVENTION: "YYYYMMDD-schoolsitename-stopTrack-authorlastname.csv"
write.csv(stop_track_df,
          file = file.path(box_file_dir,
                           "College and Career RPP",
                           "1. NSC Dataset",
                           school_site,
                           school_site_psd_folder,
                           "Stop Tracking",
                           stop_track_filename),
          row.names = FALSE)

# Confirm the file was exported to Box folder
cat("✅ Export complete:", nrow(stop_track_df), "rows written.\n")

# 2. Export missing list (internal)
if (nrow(missing_df_clean) == 0) {
  message("No active missing-data cases this cycle — exporting an empty ",
          "file for consistency (rather than skipping the export).")
}
# NAMING CONVENTION: "YYYYMMDD-schoolsitename-missingListInternal-authorlastname.csv"
write.csv(missing_df_clean,
          file = file.path(box_file_dir,
                           "College and Career RPP",
                           "1. NSC Dataset",
                           school_site,
                           school_site_psd_folder,
                           "Missing List - Internal",
                           missing_list_internal_filename),
          row.names = FALSE)

# Confirm the file was exported to Box folder
cat("✅ Export complete:", nrow(missing_df_clean), "rows written.\n")

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------