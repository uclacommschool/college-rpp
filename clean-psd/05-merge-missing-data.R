################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < 05-merge-missing-data.R >
## [ AUTH ] < Ariana Dimagiba >
## [ INIT ] < 08/20/2026 >
##
################################################################################

# Goal: Merge the existing PSD, script 04's cleaned follow-up output, and script 
# 02's stop-tracking output into a single new PSD snapshot. This script reads all PSD-shaped
# inputs, standardizes schemes, validates (pre-bind and post-bind), binds, and
# exports a new dated PSD snapshot.

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(readr)
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

# ⚠️ UPDATE: most recent clean follow up list file name
followup_clean_filename <- "20260818-rfk-followup-clean-dimagiba.csv"

# ⚠️ UPDATE: most recent stop tracking list file namme
stop_track_filename <- "20260826-rfk-stopTrack-dimagiba.csv"

# ⚠️ UPDATE: this run's output filenames (dated per export)
output_psd_filename <- "20260826-rfk-psd-dimagiba.csv"

# ⚠️ UPDATE: the target record year 02-create-missing-list-from-psd.R
# used for this cycle — must match target_record_year in 02's CONFIG.
# Used only by Part 3's stop-track validation (step 4), to check the
# same 3-year window flag_new_stop_track() itself evaluated when
# deciding who to stop-track.
target_record_year <- 2025

## ---------------------------
## load shared helper functions
## ---------------------------
# Provides assign_columns_classes() and parse_dates() the clean-psd pipeline
# authoritative schema-enforcement functions applied in Part 2. 
source(file.path("psd_core_function_list.R"))

## ---------------------------
## Part 1 - Load all three data sources
## ---------------------------

# 1. Load existing PSD - the PSD with NSC records from the November NSC report, 
# that does not have follow-up records from this current cycle,
previous_psd<-read_csv(file.path(box_file_dir,
                                 "College and Career RPP",
                                 "1. NSC Dataset",
                                 school_site,
                                 school_site_psd_folder,
                                 current_psd_filename))

# tracking_status is a new field (per stop-track redesign) the existing psd file 
# may not have this column yet. 
if (!"tracking_status" %in% names(previous_psd)) {
  previous_psd$tracking_status <- NA_character_
}


# 2. Load cleaned follow up output 
followup_clean <-read_csv(file.path(box_file_dir,
                                 "College and Career RPP",
                                 "1. NSC Dataset",
                                 school_site,
                                 school_site_psd_folder,
                                 "Missing List - Clean",
                                 followup_clean_filename))

# 3. Load stop-tracking output
stop_track<-read_csv(file.path(box_file_dir,
                                "College and Career RPP",
                                 "1. NSC Dataset",
                                 school_site,
                                 school_site_psd_folder,
                                 "Stop Tracking",
                                 stop_track_filename))

## -----------------------------------------------------------------------------
## Part 2 - Standardize Schema
## -----------------------------------------------------------------------------

# Apply the pipeline's authoritative schema functions to ALL THREE
# sources — not just previous_psd. Before Part 3's validation and Part 4's bind, means every
# downstream check and the bind itself can trust a consistent schema
# across all three, rather than discovering a mismatch mid-bind.

#' Standardize schema and check for silent date-parsing failures
#'
#' Runs assign_column_classes() and parse_dates() on a PSD source, then
#' warns if any of the four date columns picked up new NAs it didn't have
#' before parsing. Generalizes the hs_grad_date-specific NA-drift check
#' from 04-clean-missing-list.R to all four date columns, across all
#' three sources in this script — a new, unhandled date format slipping
#' through would otherwise fail silently.
#'
#' @param df A PSD-shaped data frame (previous_psd, followup_clean, or
#'   stop_track)
#' @param df_name The name of df, used only to identify which source a
#'   warning came from (passed automatically by imap() below via each
#'   list element's name)
#' @return df with correct column classes and parsed date columns
check_and_parse_dates <- function(df, df_name) {
  date_cols <- c("enrollment_begin", "enrollment_end", "coll_grad_date", "hs_grad_date")
  
  df <- df %>% assign_column_classes()
  before_counts <- map_int(date_cols, ~ sum(!is.na(df[[.x]])))
  df <- df %>% parse_dates()
  after_counts <- map_int(date_cols, ~ sum(!is.na(df[[.x]])))
  dropped <- before_counts - after_counts
  
  for (i in seq_along(date_cols)) {
    if (dropped[i] > 0) {
      warning(dropped[i], " ", date_cols[i], " value(s) in ", df_name,
              " failed to parse with any known format and became NA — ",
              "a new, unhandled date format may exist. Investigate ",
              "before trusting downstream date logic.")
    }
  }
  
  df
}

psd_sources <- list(previous_psd = previous_psd,
                    followup_clean = followup_clean,
                    stop_track = stop_track)

# imap() applies check_and_parse_dates() to each element AND passes along
# each element's own NAME (previous_psd/followup_clean/stop_track) as the
# df_name argument — that's how the warning above knows which source had
# the problem, not just that "some date" failed somewhere.
psd_sources <- imap(psd_sources, check_and_parse_dates)

# imap() returns a NEW list — it doesn't reach back out and update the
# original standalone previous_psd/followup_clean/stop_track variables
# that existed before they were bundled into psd_sources. Part 3's checks
# and Part 4's bind_rows() below refer to those three variables directly
# by name, not via psd_sources$..., so each corrected version needs to be
# pulled back out of the list and reassigned to the plain variable name
# the rest of the script actually expects.
previous_psd <- psd_sources$previous_psd
followup_clean <- psd_sources$followup_clean
stop_track <- psd_sources$stop_track

## -----------------------------------------------------------------------------
## Part 3 - Validate BEFORE Binding
## -----------------------------------------------------------------------------

# ⚠️ TODO (write these): per the stop-tracking redesign doc's
# "Validation requirements", confirm the following BEFORE binding —
# catching a problem here is much easier to trace than after everything
# is merged into one dataset:
#
# 1. tracking_status contains only "active", "stopped", or NA across
#      previous_psd, followup_clean, and stop_track combined — a closed
#      list, per the redesign doc. Any other value should stop() the
#      script.

check_track<- bind_rows(previous_psd, followup_clean, stop_track) %>% 
  filter(!tracking_status %in% c("active", "stopped", NA))
if (nrow(check_track) > 0) {
  stop(nrow(check_track),"VALIDATION FAILED: tracking_status contains rows with unresolved
       values. Review before continuing.")
}

# 2. No psd_id appears in BOTH follow up_clean AND stop_track for this
#      cycle — a student can't simultaneously be "actively followed up
#      on" and "just stopped" in the same run. Any overlap should stop()
#      the script.
dup_check_psd_id <- intersect(followup_clean$psd_id, stop_track$psd_id)
if (length(dup_check_psd_id) > 0) {
  stop(nrow(dup_check_psd_id), "psd_id(s) appear more than once in followup_clean AND stop_track. 
       Review whether student should be active or inactive tracking.")
}

# 3. Column names/classes must be compatible across all three sources
# before attempting to bind — check_type_mismatch() (from
# psd_core_function_list.R, already sourced above) is the same function
# 01-merge-nsc-to-psd.R uses for this exact purpose. Returns 0 rows if
# everything matches.
schema_mismatch_check <- check_type_mismatch(
  list(previous_psd, followup_clean, stop_track),
  c("previous_psd", "followup_clean", "stop_track")
)

if (nrow(schema_mismatch_check) > 0) {
  stop("VALIDATION FAILED: column class mismatch across sources — review ",
       "before continuing:\n",
       paste(capture.output(print(schema_mismatch_check)), collapse = "\n"))
}

# 4. Confirm no psd_id in stop_track actually has a real, non-missing
# record in previous_psd within the SAME 3-year window
# flag_new_stop_track() (02) itself evaluated (target_record_year - 3
# through target_record_year - 1). If someone shows up here, it's a
# genuine contradiction: they were stop-tracked in 02 on the basis of 3
# consecutive missing years, but previous_psd shows a real record inside
# that exact window — worth investigating as a bug in the stop-tracking
# decision, not just a data-quality note.
#
# Deliberately scoped to this specific recent window, not "any record,
# ever" — a student can have real history from years ago and still
# correctly qualify for stop-tracking today if the 3 years immediately
# preceding this cycle were genuinely all missing. An unscoped check
# flags nearly everyone who's ever had any real record at all (confirmed
# empirically: an unscoped version flagged 127 of 157 candidates in one
# run — almost all noise from old history, unrelated to the actual
# stop-tracking decision).
stop_track_false_positive_check <- stop_track %>%
  select(psd_id) %>%
  inner_join(
    previous_psd %>%
      filter(record_year %in% (target_record_year - 3):(target_record_year - 1)) %>%
      filter(system_type != "MISSING DATA", !is.na(system_type)),
    by = "psd_id"
  )

n_stop_track_false_positives <- dplyr::n_distinct(stop_track_false_positive_check$psd_id)

if (n_stop_track_false_positives > 0) {
  stop(n_stop_track_false_positives, " psd_id(s) in stop_track have a ",
       "real (non-MISSING DATA) record in previous_psd within the exact ",
       "3-year window used to determine stop-tracking — contradicts the ",
       "stop-tracking decision. Investigate before continuing:\n",
       paste(capture.output(print(stop_track_false_positive_check %>%
                                    distinct(psd_id))), collapse = "\n"))
}

## -----------------------------------------------------------------------------
## Part 4 - Bind and Validate After Binding
## -----------------------------------------------------------------------------

# 1. Capture pre-bind counts for the row-count reconciliation check below.
n_previous_psd <- nrow(previous_psd)
n_followup_clean <- nrow(followup_clean)
n_stop_track <- nrow(stop_track)

# 2. Bind all three sources into the new PSD.
new_psd <- bind_rows(previous_psd, followup_clean, stop_track)

# 3. Row-count reconciliation — confirm the combined dataset's row count
# exactly equals the sum of the three inputs. bind_rows() silently
# succeeding is not the same as binding correctly; this catches a dropped
# or duplicated row during the bind itself.
n_expected <- n_previous_psd + n_followup_clean + n_stop_track
n_actual <- nrow(new_psd)

if (n_actual != n_expected) {
  stop("VALIDATION FAILED: row count mismatch after bind_rows(). Expected ",
       n_expected, " rows (", n_previous_psd, " previous_psd + ",
       n_followup_clean, " followup_clean + ", n_stop_track,
       " stop_track) but got ", n_actual, ". Investigate before continuing.")
}

# 4. Duplicate check on the COMBINED new_psd — confirm no psd_id has more than 
# one row sharing the same record_term + record_year + he_graduated. 
# This key (not just psd_id + record_term + record_year) is deliberate: 
# a student can legitimately have both an enrollment record AND a graduation record 
# in the same term/year (e.g. Fall 2025 enrollment confirmed, Fall 2025 also when 
# their graduation was recorded) — he_graduated distinguishes which kind of fact each row
# represents, so those two rows are NOT a duplicate. Two rows sharing all
# four values WOULD be a genuine duplicate — the same fact recorded twice.
dup_check_bind <- new_psd %>% count(psd_id, record_term, record_year, he_graduated) %>%
  filter(n >1)
if (nrow(dup_check_bind) > 0) {
  stop(nrow(dup_check_bind), " row(s) have duplicate psd_id + record_term ",
       "+ record_year + he_graduated combinations. Review before continuing.")
}

message("All validation checks passed: ", n_actual,
        " total row(s) in new_psd — ", n_previous_psd, " from previous_psd, ",
        n_followup_clean, " from followup_clean, ", n_stop_track,
        " from stop_track.")

## -----------------------------------------------------------------------------
## Part 5 - Export New PSD Snapshot
## -----------------------------------------------------------------------------

write.csv(new_psd,
          file = file.path(box_file_dir,
                           "College and Career RPP",
                           "1. NSC Dataset",
                           school_site,
                           school_site_psd_folder,
                           output_psd_filename),
          row.names = FALSE)

cat("✅ Export complete:", nrow(new_psd), "rows written.\n")

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
