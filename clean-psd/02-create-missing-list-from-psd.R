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
## Part 0 VARIABLES TO UPDATE EACH RUN
## ---------------------------
#  ⚠️  ⚠️  ⚠️  Update the values below before each run — nothing else in this script should need to change.
#
# 1. school_site                    — school site subfolder in Box (e.g. "Mann", "RFK")
# 2. current_psd_filename           — current PSD csv file (output of 01-merge script)
# 3. master_stu_list_filename       — most recent master student list file name
# 4. target_record_year             — target year to check missing students against
# 5. target_record_term             — target term to check missing students against
# 6. output_stop_track_filename     — stop-track output file name (naming convention in Part 4)
# 7. output_missing_list_filename   — missing-list output file name (naming convention in Part 4)

school_site <- "Mann"
current_psd_filename <- "20260721-mann-psd-sanchez.csv"
master_stu_list_filename <- "mann_master_student_list_2021-2026.csv"
target_record_year <- 2025
target_record_term <- "fall"
output_stop_track_filename <- "20260817-mann-stopTrack-sanchez.csv"
output_missing_list_filename <- "20260817-mann-missingListInternal-sanchez-thirdtest.csv"

## -----------------------------------------------------------------------------
## load all raw data sets
## -----------------------------------------------------------------------------

#load recently updated psd file from 01-merge script

## For RFK
## "College and Career RPP", "1. NSC Dataset", "RFK", "RFK PSD", "DATE-rfk-psd-author.csv"

## For Mann
## "College and Career RPP", "1. NSC Dataset", "Mann", "Mann PSD", "DATE-rfk-psd-author.csv"

current_psd <-read_csv(file.path(box_file_dir,
                                 "College and Career RPP",
                                 "1. NSC Dataset",
                                 school_site,
                                 "Mann PSD",
                                 current_psd_filename
))

#remove first column
#current_psd<-current_psd %>% select(-1)

#load master student directory file
## For RFK
## "College and Career RPP", "1. NSC Dataset", "RFK", "RFK PSD", "Master Student List", "rfk_master_student_list_2021-2025.csv"

## For Mann
## "College and Career RPP", "1. NSC Dataset", "Mann", "Mann PSD", "Master Student List", "mann_master_student_list_2021-2026.csv"

master_stu_list<- read_csv(file.path(box_file_dir,
                                     "College and Career RPP",
                                     "1. NSC Dataset",
                                     school_site,
                                     "Mann PSD",
                                     "Master Student List",
                                     master_stu_list_filename
))

## -----------------------------------------------------------------------------
## Part 1 - Check Data
## -----------------------------------------------------------------------------
#1 Use the clean "nsc_data" df from "01-merge" script to confirm range of cohorts 
current_psd %>%
  filter(!is.na(hs_grad_year)) %>%
  summarise(
    min_year = min(hs_grad_year),
    max_year = max(hs_grad_year),
    n_hs_grad_years = n_distinct(hs_grad_year)
  )

#check for duplicates in NSC
current_psd %>%
  count(psd_id) %>%
  filter(n > 1)

#check for missing IDs
current_psd %>%
  count(psd_id) %>%
  filter(n > 1)

## -----------------------------------------------------------------------------
## Part 2 - Generate Missing list Function
## -----------------------------------------------------------------------------

# ─────────────────────────────────────────────────────────────────────────────
# generate_missing_list()
#
# Identifies students absent from a target year/term and returns a dataframe
# of their most recent prior records, updated to the target year/term.
#
# Args:
#   df          : The full dataframe containing all student records
#   record_year : The target year to check against  (default: 2025)
#   record_term : The target term to check against  (default: "fall")
#
# Returns:
#   A dataframe of missing students with record_year and record_term
#   set to the target values, based on each student's most recent prior record.
# ─────────────────────────────────────────────────────────────────────────────

generate_missing_list <- function(df, record_year = target_record_year, record_term = target_record_term) {
  
  # FIX: the PSD's record_term values are stored in ALL CAPS (e.g. "FALL"),
  # but this function is called with lowercase terms (e.g. "fall"). A bare
  # == comparison is case-sensitive, so target_ids was always empty and
  # EVERY student -- including ones with an actual record for the target
  # term -- was being treated as "missing". Normalize case/whitespace on
  # both sides before comparing.
  record_term_target <- toupper(trimws(record_term))
  
  # Students present in the target year/term
  target_ids <- df |>
    dplyr::filter(record_year == !!record_year,
                  toupper(trimws(record_term)) == record_term_target) |>
    dplyr::pull(psd_id) |>
    unique()
  
  # All students NOT in the target year/term
  missing_ids <- df |>
    dplyr::filter(!psd_id %in% target_ids) |>
    dplyr::pull(psd_id) |>
    unique()
  
  if (length(missing_ids) == 0) {
    message("No missing students found for ", record_year, " ", record_term, ".")
    return(dplyr::tibble())
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

## -----------------------------------------------------------------------------
## Part 3 - Filter students who Graduated and are not active tracking
## -----------------------------------------------------------------------------
# GUIDANCE 
# - Remove students who have completed a 4-year degree
# - Remove students who we already identified as Inactive or "stop-track"
# - Remove students who are newly inactive with missing data for three consecutive years

#Create Missing List
missing_df<-generate_missing_list(current_psd,
                                  record_year = target_record_year,
                                  record_term = target_record_term)

# add the target year students (Note this may change based on year)
grad_class25<-master_stu_list %>% filter(hs_grad_year == target_record_year)
current_25_nsc<-current_psd %>% filter(hs_grad_year == target_record_year)
missing_25<-grad_class25 %>% filter(!c(psd_id %in% current_25_nsc$psd_id)) %>% 
  filter(!is.na(psd_id))

# Add all missing columns as NA, then reorder to match missing_df
missing_25_prepped <- missing_25 %>%
  # Add missing columns as NA first
  mutate(!!!setNames(
    lapply(setdiff(colnames(missing_df), colnames(missing_25)), function(x) NA_character_),
    setdiff(colnames(missing_df), colnames(missing_25))
  )) %>%
  select(all_of(colnames(missing_df))) %>%
  # Cast each column to match missing_df's class
  mutate(across(
    everything(),
    ~ class(missing_df[[cur_column()]])[1] %>%
      switch(
        "numeric"   = as.numeric(.),
        "integer"   = as.integer(.),
        "logical"   = as.logical(.),
        "Date"      = as.Date(.),
        "POSIXct"   = as.POSIXct(.),
        as.character(.)  # default: character
      )
  ))

# FIX: students newly added from the master list (2025 grads with no PSD
# record at all) had no record_year/record_term stamped, so they'd show up
# blank on those columns in the export. Stamp them the same way
# generate_missing_list() stamps everyone else.
missing_25_prepped <- missing_25_prepped %>%
  mutate(
    record_year = target_record_year,
    record_term = toupper(target_record_term)
  )

#add 2025 missing students together
missing_df <- bind_rows(missing_df, missing_25_prepped)

#Flag students who have graduated from a 4-year college
# FIX: cc_4year is stored as "4-YEAR" (all caps) in the PSD, so the old
# lowercase "4-year" comparison never matched and no one was ever filtered
# out as a 4-year grad. Compare case-insensitively.
#
# FIX (full-history check): generate_missing_list() already collapsed each
# student down to a single "most recent" row before this point. If a
# student graduated 4-year in an earlier term and their *latest* PSD row is
# a later "MISSING DATA"/no-update row (common once NSC stops returning
# updates after a student is done), that single collapsed row no longer
# carries he_graduated == "Y", so checking only that row misses the
# graduation. A student who graduated from a CC or technical program
# (cc_4year == "2-YEAR" / "LESS THAN 2 YEARS") should still be tracked, so
# this only ever flags true 4-year completions -- and it flags them
# permanently, based on whether that ever happened anywhere in their PSD
# history, not just on their most recent snapshot.
ever_4yr_grad_ids <- current_psd %>%
  filter(toupper(trimws(he_graduated)) == "Y", toupper(trimws(cc_4year)) == "4-YEAR") %>%
  pull(psd_id) %>%
  unique()

missing_df <- missing_df %>%
  mutate(
    flag_4yr_grad = case_when(
      psd_id %in% ever_4yr_grad_ids ~ 1,
      (toupper(trimws(he_graduated)) == "Y" & toupper(trimws(cc_4year)) == "4-YEAR") ~ 1,
      TRUE ~ 0
    ))

#check
test<-missing_df %>% filter(hs_grad_year>2016)

#3 Filter  students who have graduated from a 4-year college
missing_df<-missing_df %>% filter(flag_4yr_grad != 1)


#4. Flag and filter students were previously assigned to stop tracking using
# _ notes variable in recently updated psd from 01-merge

# ─────────────────────────────────────────────────────────────────────────────
# flag_stopped_tracking()
#
# Adds a binary indicator column to a dataframe flagging students whose
# 'notes' field contains "stop track" or "stopped tracking" (case-insensitive).
#
# Args:
#   df             : A dataframe containing a 'notes' column
#   indicator_name : Name of the new indicator column (default: "stopped_tracking")
#
# Returns:
#   The same dataframe with one new integer column (1 = flagged, 0 = not flagged).
# ─────────────────────────────────────────────────────────────────────────────

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

#Flag stop tracking users
missing_df<-flag_stopped_tracking(missing_df)

#filter out stop tracking users
missing_df<-missing_df %>% filter(stopped_tracking == 0)

#5a.Flag new students who have been missing for 3 consecutive years as stop-track

# ─────────────────────────────────────────────────────────────────────────────
# flag_new_stop_track()
#
# Identifies students in the missing list who have had system_type == "MISSING DATA"
# on EVERY row across ALL terms for 3 or more consecutive full calendar years,
# with no non-"MISSING DATA" value breaking the streak. Marks qualifying students
# as stop-track in notes and sets stopped_tracking = 1.
#
# Logic:
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
#
# Args:
#   missing_df        : Output of generate_missing_list(), already run through
#                       flag_stopped_tracking() so stopped_tracking column exists.
#   master_df         : Full master dataset containing all historical records.
#   record_year       : Target year (default: 2025). Streak is evaluated on all
#                       years strictly before this value.
#   consecutive_years : Minimum consecutive "all_missing" years to qualify (default: 3).
#
# Returns:
#   missing_df with notes and stopped_tracking updated for newly flagged students.
# ─────────────────────────────────────────────────────────────────────────────

flag_new_stop_track <- function(missing_df,
                                master_df,
                                record_year       = 2025,
                                consecutive_years = 3) {
  
  
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


missing_df<-flag_new_stop_track(missing_df = missing_df,
                                master_df = current_psd,
                                record_year = target_record_year,
                                consecutive_years = 3)

#5b. Filter and create a new df for new stop-track students
# - will need to merge them in a later script when you merge missing data to psd
stop_track_df<-missing_df %>% filter(stopped_tracking == 1)

#5c. Create missing list that we need to get clarified by the school
missing_df_clean<-missing_df %>% filter(stopped_tracking==0)

#6. Keep only the last 8 years
missing_df_clean<-missing_df_clean %>% filter(hs_grad_year > 2016)

## -----------------------------------------------------------------------------
## Part 4 - Export Data
## -----------------------------------------------------------------------------

#stop track dataframe

# NAMING CONVENTION: "YYYYMMDD-schoolsitename-stopTrack-authorlastname.csv"
# Example: "20260521-rfk-stopTrack-sanchez.csv"

##Path for RFK
## "College and Career RPP", "1. NSC Dataset", "RFK", "RFK PSD", "Stop Tracking", "YYYYMMDD-rfk-stopTrack-authorlastname.csv"

##Path For Mann
## "College and Career RPP", "1. NSC Dataset", "Mann", "Mann PSD", "Stop Tracking", "YYYYMMDD-mann-stopTrack-authorlastname.csv"

write.csv(stop_track_df,
          file = file.path(box_file_dir,
                           "College and Career RPP",
                           "1. NSC Dataset",
                           school_site,
                           "Mann PSD",
                           "Stop Tracking",
                           output_stop_track_filename
          ),
          row.names = FALSE)

# Confirm the file was exported to Box folder
cat("✅ Export complete:", nrow(stop_track_df), "rows written.\n")


#missing list

# NAMING CONVENTION: "YYYYMMDD-schoolsitename-missingListInternal-authorlastname.csv"
# Example: "20260521-rfk-missingListInternal-sanchez.csv"

##Path for RFK
## "College and Career RPP", "1. NSC Dataset", "RFK", "RFK PSD", "Missing List - Internal", "YYYYMMDD-rfk-missingListInternal-authorlastname.csv"

##Path For Mann
## "College and Career RPP", "1. NSC Dataset", "Mann", "Mann PSD", "Missing List - Internal", "YYYYMMDD-mann-missingListInternal-authorlastname.csv"

write.csv(missing_df_clean,
          file = file.path(box_file_dir,
                           "College and Career RPP",
                           "1. NSC Dataset",
                           school_site,
                           "Mann PSD",
                           "Missing List - Internal",
                           output_missing_list_filename
          ),
          row.names = FALSE)

# Confirm the file was exported to Box folder
cat("✅ Export complete:", nrow(missing_df_clean), "rows written.\n")

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------