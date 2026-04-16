################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 02-create-missing-list-from-psd.R >
## [ AUTH ] < Ariana Dimagiba / aridimagiba >
## [ INIT ] < 03/25/2026, updated 04/15/2026 >
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

box_file_dir<-file.path("C:/Users/jyo/Box","College Data")

## -----------------------------------------------------------------------------
## load all raw data sets
## -----------------------------------------------------------------------------

#load recently updated psd file from 01-merge script
#current_psd <- read_csv(file.path("..", "updated_psd.csv"))

current_psd <- read_csv(file.path(code_file_dir,
                                  "16april2026-rfk-psd-yo-test.csv"))

#remove first column
current_psd<-current_psd %>% select(-c("...1"))

#load master student directory file
master_stu_list<- read_csv(file.path(code_file_dir,
                                     "master-student-list-rfk-2012-2025.csv")) 

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

generate_missing_list <- function(df, record_year = 2025, record_term = "fall") {
  
  # Students present in the target year/term
  target_ids <- df |>
    dplyr::filter(record_year == !!record_year, record_term == !!record_term) |>
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
  term_order <- c("plans"=1,"enrolled anytime after fall" =2,
                  "winter" = 3, "spring" = 4, "summer" = 5, "fall" = 6)
  
  # For each missing student, grab their most recent record
  missing_df <- df |>
    dplyr::filter(psd_id %in% missing_ids) |>
    dplyr::mutate(
      .term_rank = dplyr::coalesce(term_order[tolower(record_term)], 0L)
    ) |>
    dplyr::arrange(psd_id, dplyr::desc(record_year), dplyr::desc(.term_rank)) |>
    dplyr::slice_head(n = 1, by = psd_id) |>
    dplyr::select(-.term_rank) |>
    # Stamp with target year/term
    dplyr::mutate(
      record_year = !!record_year,
      record_term = !!record_term
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
                            record_year = 2025,
                            record_term = "fall")

# add the 2025 students (Note this may change based on year)
grad_class25<-master_stu_list %>% filter(hs_grad_year == 2025)
current_25_nsc<-current_psd %>% filter(hs_grad_year == 2025)
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

#add 2025 missing students together
missing_df <- bind_rows(missing_df, missing_25_prepped)

#Flag students who have graduated from a 4-year college 
missing_df <- missing_df %>%
  mutate(
    flag_4yr_grad = case_when(
      (he_graduated == "Y" & cc_4year == "4-year") ~ 1,
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
               record_year = 2025,
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
fwrite(stop_track_df, file.path(code_file_dir,
                                "16april2026-rfk-stop-track-yo-test.csv"))


#missing list
fwrite(missing_df_clean, file.path(code_file_dir,
                                "16april2026-rfk-missing-list-test.csv"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------