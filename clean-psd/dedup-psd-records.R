################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < dedup-psd-records.R >
## [ AUTH ] < Ariana Dimagiba >
## [ INIT ] < 08/25/2026 >
##
################################################################################

#Goal: Investigate and correct duplicate-record contamination in an
#already-produced PSD snapshot. NOT part of the regular pipeline
#(01-05) — this is a post-hoc correction script, run against a PSD
#file that's already been through 01/05, not against raw NSC/master
#list inputs.
#
#Originally motivated by discovering psd_id 2013PNCQ15 had 448 rows —
#confirmed to be 14 genuinely distinct facts (one coherent 2014-2021
#Pasadena City College enrollment history), each re-inserted repeatedly
#by years of un-deduped merges before this session's dedup fixes to
#01-merge-nsc-to-psd.R. A full-file scan then found this pattern is
#NOT isolated to one student — 196 psd_ids affected, 1179 excess rows
#total, ranging from trivial 1-row overlaps to severe compounding cases.
#
#Also connects to a known identity collision: student_id 120493M043 was
#historically shared by two real students (psd_id 2013PNCQ15 and
#2013PSXY40) from the pre-2019 manual Excel era, before psd_id existed
#as the primary key. 2013PSXY40's student_id was corrected to NA this
#session (see PENDING-tasks.md), preventing the collision from
#recurring going forward — but historical contamination already in
#previous_psd from years of this collision needs separate, manual
#resolution, since NA student_id doesn't retroactively fix past merges.
#
#⚠️ PREREQUISITE: run independently (environment cleared between
#scripts) — loads previous_psd fresh, does not depend on 01/02/04/05
#having run in this session.

################################################################################

## ---------------------------
## libraries
## ---------------------------

library(tidyverse)
library(readr)

## ---------------------------
## directory paths
## ---------------------------

if (.Platform$OS.type == "windows") {
  box_file_dir <- file.path(Sys.getenv("USERPROFILE"), "Box")
} else {
  box_file_dir <- file.path(Sys.getenv("HOME"), "Library", "CloudStorage", "Box-Box")
}

## ---------------------------
## ⚠️ CONFIG
## ---------------------------

school_site <- "RFK"
school_site_psd_folder <- "RFK PSD"

# ⚠️ UPDATE: the PSD file being checked/corrected — typically 05's most
# recent output, or previous_psd_filename from 05's own CONFIG.
psd_filename <- "20260820-rfk-psd-dimagiba.csv"

# ⚠️ UPDATE: severity threshold (in excess rows) below which a psd_id's
# duplication is treated as safe to auto-collapse without individual
# review, and at/above which it requires the manual verification
# process in Part 4. This is a judgment call, not a derived value —
# median excess rows across the first full scan was 1, mean was ~6,
# driven up by a small number of severe outliers (max 434). Adjust
# based on how much manual review capacity you have vs. how much risk
# tolerance for an unreviewed auto-collapse.
severe_threshold <- 5

# ⚠️ UPDATE: output filenames for this run.
corrected_psd_filename <- "20260825-rfk-psd-dimagiba-dedup.csv"
severity_tracking_filename <- "20260825-rfk-dedup-severity-tracking-dimagiba.csv"

## -----------------------------------------------------------------------------
## Part 1 - Load the PSD to check
## -----------------------------------------------------------------------------

previous_psd <- read_csv(file.path(box_file_dir,
                                   "College and Career RPP",
                                   "1. NSC Dataset",
                                   school_site,
                                   school_site_psd_folder,
                                   psd_filename))

## -----------------------------------------------------------------------------
## Part 2 - Full-scope scan: how many students, how severe
## -----------------------------------------------------------------------------

# What "duplicate" means here: same psd_id + record_year + record_term +
# college_code + degree_title + he_graduated appearing more than once.
# This is a slightly looser key than 04/05's dup checks (which also
# distinguish on exact dates) — deliberately, since the goal here is
# finding REPEATED facts (the same real event re-inserted by a prior
# un-deduped merge), not distinguishing legitimately close-but-different
# events the way 01's near-duplicate check does.

n_before <- nrow(previous_psd)

n_after_dedup <- previous_psd %>%
  distinct(psd_id, record_year, record_term, college_code, degree_title,
          he_graduated, .keep_all = TRUE) %>%
  nrow()

n_before
n_after_dedup
n_before - n_after_dedup  # total excess/redundant rows across the whole file

dedup_scope <- previous_psd %>%
  count(psd_id, record_year, record_term, college_code, degree_title,
       he_graduated, name = "n_copies") %>%
  filter(n_copies > 1) %>%
  group_by(psd_id) %>%
  summarize(
    n_duplicated_facts = n(),
    n_excess_rows = sum(n_copies - 1),
    max_copies = max(n_copies),
    .groups = "drop"
  ) %>%
  arrange(desc(n_excess_rows))

nrow(dedup_scope)  # how many distinct students are affected at all

dedup_scope %>% print(n = 30, width = Inf)

summary(dedup_scope$n_excess_rows)

## -----------------------------------------------------------------------------
## Part 3 - Split into safe (auto-collapse) vs. severe (manual review)
## -----------------------------------------------------------------------------

dedup_scope <- dedup_scope %>%
  mutate(severity = if_else(n_excess_rows >= severe_threshold, "severe", "safe"))

dedup_scope %>% count(severity)

safe_ids <- dedup_scope %>% filter(severity == "safe") %>% pull(psd_id)
severe_ids <- dedup_scope %>% filter(severity == "severe") %>% pull(psd_id)

## -----------------------------------------------------------------------------
## Part 4 - Manual verification helper for severe cases
## -----------------------------------------------------------------------------

#' Show one student's distinct facts for manual coherence review
#'
#' Reusable version of the process used to verify psd_id 2013PNCQ15:
#' pull all distinct facts for one student, ordered chronologically, and
#' visually check for a single coherent history (consistent institution
#' or a sensible transfer pattern, no contradictory/overlapping terms) —
#' the signature of pure duplication, safe to collapse — versus signs of
#' TWO different people's records mixed together (unexplained
#' inconsistencies, contradictions), which would need separate handling,
#' not a blanket distinct().
#'
#' @param df The full PSD data frame (previous_psd)
#' @param target_psd_id The psd_id to review
#' @return A tibble of that student's distinct facts, for visual review —
#'   not auto-classified; a human still needs to look at the output
review_student_duplication <- function(df, target_psd_id) {
  df %>%
    filter(psd_id == target_psd_id) %>%
    distinct(record_year, record_term, college_code, college_name,
            degree_title, he_graduated, .keep_all = TRUE) %>%
    select(record_year, record_term, college_name, degree_title, he_graduated) %>%
    arrange(record_year, record_term)
}

# Example usage — walk through each severe_id one at a time:
# review_student_duplication(previous_psd, severe_ids[1]) %>% print(n = Inf, width = Inf)

# ⚠️ Priority case: 2013PSXY40 — the OTHER student in the known
# student_id collision (120493M043) with 2013PNCQ15. Given the shared
# root cause, this one should be reviewed FIRST, not assumed clean.
if ("2013PSXY40" %in% severe_ids) {
  cat("⚠️ 2013PSXY40 flagged as severe — review this one first, given its\n",
      "connection to the known 120493M043 student_id collision.\n", sep = "")
  review_student_duplication(previous_psd, "2013PSXY40") %>% print(n = Inf, width = Inf)
}

# Tracking columns for manual review outcomes — fill in as each severe
# case gets reviewed via review_student_duplication() above.
severe_review_tracking <- dedup_scope %>%
  filter(severity == "severe") %>%
  mutate(
    reviewed = FALSE,      # set TRUE once review_student_duplication() has been checked
    outcome = NA_character_ # "coherent - safe to collapse" / "contamination suspected - needs separate handling"
  )

## -----------------------------------------------------------------------------
## Part 5 - Apply the fix
## -----------------------------------------------------------------------------

# Auto-collapse SAFE cases now. SEVERE cases are only collapsed here
# once their outcome in severe_review_tracking has been manually set to
# "coherent - safe to collapse" — anything still NA or flagged as
# contamination-suspected stays UNTOUCHED in the corrected output,
# preserved exactly as in the original file, pending further
# investigation.
cleared_severe_ids <- severe_review_tracking %>%
  filter(outcome == "coherent - safe to collapse") %>%
  pull(psd_id)

ids_to_collapse <- c(safe_ids, cleared_severe_ids)

previous_psd_corrected <- bind_rows(
  # collapse duplication for cleared psd_ids
  previous_psd %>%
    filter(psd_id %in% ids_to_collapse) %>%
    distinct(psd_id, record_year, record_term, college_code, degree_title,
            he_graduated, .keep_all = TRUE),
  # everyone else (unaffected students, plus severe cases not yet
  # cleared) passes through completely untouched
  previous_psd %>%
    filter(!psd_id %in% ids_to_collapse)
)

# Confirm nothing was lost beyond the intended collapse — row count
# should equal original minus exactly the excess rows removed for
# ids_to_collapse.
n_expected_removed <- dedup_scope %>%
  filter(psd_id %in% ids_to_collapse) %>%
  pull(n_excess_rows) %>%
  sum()

if (nrow(previous_psd) - nrow(previous_psd_corrected) != n_expected_removed) {
  stop("Row count after correction doesn't match expected removal — ",
       "investigate before exporting.")
}

cat("Corrected PSD: ", nrow(previous_psd), " -> ", nrow(previous_psd_corrected),
    " rows (", nrow(previous_psd) - nrow(previous_psd_corrected),
    " removed, ", length(severe_ids) - length(cleared_severe_ids),
    " severe psd_id(s) still pending manual review, left untouched)\n", sep = "")

## -----------------------------------------------------------------------------
## Part 6 - Export corrected PSD and severity tracking
## -----------------------------------------------------------------------------

# ⚠️ Exports as its OWN dated snapshot — does NOT overwrite the original
# psd_filename. Keeps a clear record of when this correction was applied
# and what it changed, and leaves the original untouched in case any
# severe case's "safe to collapse" determination needs revisiting.
# Stays alongside regular PSD snapshots (same folder) — it IS a PSD
# file, just corrected; the "-dedup" suffix already distinguishes it.
write.csv(previous_psd_corrected,
          file = file.path(box_file_dir,
                           "College and Career RPP",
                           "1. NSC Dataset",
                           school_site,
                           school_site_psd_folder,
                           corrected_psd_filename),
          row.names = FALSE)

# severity_tracking is an audit/review artifact, not a PSD snapshot —
# goes in its own "Data Quality" subfolder (matching the term your
# colleague's 01-calculate-outcomes.R Part 7 already uses), giving this
# and future audit files (e.g. 04's excluded_records, the enrollment-lag
# tracking file) a consistent, discoverable home going forward.
#
# write.csv() does NOT auto-create missing folders — if "Data Quality"
# doesn't exist yet in Box, this would otherwise fail with a
# file-not-found error at export time. Create it first if needed.
data_quality_dir <- file.path(box_file_dir,
                              "College and Career RPP",
                              "1. NSC Dataset",
                              school_site,
                              school_site_psd_folder,
                              "Data Quality")
if (!dir.exists(data_quality_dir)) {
  dir.create(data_quality_dir, recursive = TRUE)
  cat("Created new folder: ", data_quality_dir, "\n", sep = "")
}

write.csv(severe_review_tracking,
          file = file.path(box_file_dir,
                           "College and Career RPP",
                           "1. NSC Dataset",
                           school_site,
                           school_site_psd_folder,
                           "Data Quality",
                           severity_tracking_filename),
          row.names = FALSE)

cat("✅ Export complete: ", nrow(previous_psd_corrected), " rows written to ",
    corrected_psd_filename, "\n", sep = "")

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
