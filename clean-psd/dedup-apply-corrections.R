################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < dedup-apply-corrections.R >
## [ AUTH ] < Ariana Dimagiba >
## [ INIT ] < 08/29/2026 (split from dedup-psd-records.R) >
##
################################################################################

#Goal: Apply already-confirmed corrections to a PSD snapshot — lag
#annotation, targeted removal, identity-field fixes, and duplicate
#collapse — then export a corrected file. This is the EXECUTION half of
#what was previously one script (dedup-psd-records.R). It does not do
#any exploration or manual review itself; every decision it applies was
#already made and recorded in the companion file, dedup-investigation.R.
#
#⚠️ PREREQUISITE — run dedup-investigation.R FIRST, and complete (or
#deliberately stop) its review, before running this script. This script
#reads two CSV exports produced there (identity_review_tracking,
#r_review_tracking) as its source of truth for which corrections are
#confirmed — since the two scripts run as separate sessions, nothing
#from an in-memory review in dedup-investigation.R is available here
#except through those saved files.
#
#Originally motivated by discovering psd_id 2013PNCQ15 had 448 rows —
#see dedup-investigation-log.md for the full chronological
#investigation that led to everything this script applies.
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

# ⚠️ UPDATE: the PSD file being corrected — MUST match
# dedup-investigation.R's psd_filename exactly, or this script will be
# applying review outcomes to different data than what was reviewed.
psd_filename <- "20260829-rfk-psd-dimagiba.csv"

# ⚠️ UPDATE: most recent master student list file name
master_list_filename <- "master-student-list-rfk-2012-2025.csv"

# ⚠️ UPDATE: must match dedup-investigation.R's value — determines which
# psd_ids fall in safe_ids vs. severe_ids below.
severe_threshold <- 5

# ⚠️ UPDATE: the review-outcome CSVs exported by dedup-investigation.R —
# must point at the actual files produced by that script's completed
# review, not stale filenames from an earlier run.
identity_review_csv <- "20260902-rfk-identity-review-tracking-dimagiba.csv"
r_review_csv <- "20260902-rfk-r-review-tracking-dimagiba.csv"

# ⚠️ UPDATE: the contamination-correction tracking CSV — logs how each
# psd_id flagged "contamination suspected" in identity_review_tracking is
# actually resolved (full_rebuild / reassign_psd_id / split_by_identity).
contamination_corrections_csv <- "contamination_corrections_tracking.csv"

# ⚠️ UPDATE: output filenames for this run.
corrected_psd_filename <- "20261005-rfk-psd-dimagiba-dedup.csv"
severity_tracking_filename <- "20261005-rfk-dedup-severity-tracking-dimagiba.csv"

# ⚠️ UPDATE: filename per run.
write.csv(removed_inferred_rows,
          file = file.path(data_quality_dir,
                           "20261005-rfk-removed-inferred-rows-dimagiba.csv"),
          row.names = FALSE)
## -----------------------------------------------------------------------------
## Data Quality folder
## -----------------------------------------------------------------------------

data_quality_dir <- file.path(box_file_dir,
                              "College and Career RPP",
                              "4. NSC Dataset",
                              school_site,
                              school_site_psd_folder,
                              "Data Quality")
if (!dir.exists(data_quality_dir)) {
  dir.create(data_quality_dir, recursive = TRUE)
  cat("Created new folder: ", data_quality_dir, "\n", sep = "")
}

## -----------------------------------------------------------------------------
## Part 1 - Load the PSD and Master Student List to check
## -----------------------------------------------------------------------------

previous_psd <- read_csv(file.path(box_file_dir,
                                   "College and Career RPP",
                                   "4. NSC Dataset",
                                   school_site,
                                   school_site_psd_folder,
                                   psd_filename),
                         col_types = cols(high_school_code = col_character(),
                                          race_ethnicity = col_character())) %>%
                mutate(high_school_code = str_pad(high_school_code, 6, pad ="0"))

master_stu_list <- read_csv(file.path(box_file_dir,
                                      "College and Career RPP",
                                      "4. NSC Dataset",
                                      school_site,
                                      school_site_psd_folder,
                                      "Master Student List",
                                      master_list_filename),
                            col_types = cols(race_ethnicity = col_character()))

# load institution lookup reference table
institution_lookup <- read_csv(file.path(box_file_dir,
                                         "College and Career RPP",
                                         "4. NSC Dataset",
                                         "institution_lookup.csv"))


## -----------------------------------------------------------------------------
## Part 2 - severe_review_tracking (rebuilt identically to
## dedup-investigation.R's Part 6 — fully self-contained/hardcoded, no
## file read needed for this piece specifically)
## -----------------------------------------------------------------------------

severe_review_tracking <- dedup_scope %>%
  filter(severity == "severe") %>%
  mutate(
    reviewed = case_when(
      psd_id %in% c("2013PNCQ15", "2013PSXY40", "2014NDJU56",
                    "2021ZMDI63", "2021KBQP24") ~ TRUE,
      TRUE ~ FALSE
    ),
    outcome = case_when(
      psd_id == "2013PNCQ15" ~ "coherent - safe to collapse",
      psd_id == "2013PSXY40" ~ "contamination suspected - needs separate handling",
      psd_id == "2014NDJU56" ~ "coherent - safe to collapse",
      psd_id == "2021ZMDI63" ~ "coherent - safe to collapse (confirmed via raw-row check post major-fix)",
      psd_id == "2021KBQP24" ~ "coherent - safe to collapse (confirmed via raw-row check post major-fix)",
      TRUE ~ NA_character_
    )
  )

## -----------------------------------------------------------------------------
## Part 3 - Read confirmed review outcomes from dedup-investigation.R's
## CSV exports
## -----------------------------------------------------------------------------

# identity_review_tracking — read directly. The CSV itself has already
# been re-reviewed and updated at the source (2026-09-03): the 3 cases
# previously held back as "fix not yet applied" are now confirmed, so no
# in-script rewrite/normalization is needed here — a later parts filter below
# matches on the outcome prefix directly.
identity_review_tracking <- read_csv(file.path(data_quality_dir, identity_review_csv))

cat("Loaded identity_review_tracking: ", nrow(identity_review_tracking), " psd_id(s), ",
    sum(identity_review_tracking$reviewed), " reviewed.\n", sep = "")

# r_review_tracking — the comprehensive safe+severe review from
# dedup-investigation.R. This is the actual source of truth for which
# safe-tier psd_ids are confirmed collapsible, and for targeted removal 
# list below.
r_review_tracking <- read_csv(file.path(data_quality_dir, r_review_csv))

cat("Loaded r_review_tracking: ", nrow(r_review_tracking), " psd_id(s), ",
    sum(r_review_tracking$reviewed), " reviewed.\n", sep = "")

## -----------------------------------------------------------------------------
## Part 4 - Apply confirmed identity-field corrections (master-list merge)
## -----------------------------------------------------------------------------
# For every psd_id confirmed in identity_review_tracking with an outcome
# starting "single data entry error - not a collision" (reviewed = TRUE,
# str_detect on the prefix — catches the bare string plus any
# parenthetical-qualifier variants, since identity_review_tracking has
# already been re-reviewed at the source and no longer carries any
# "fix not yet applied" caveats), pull the correct identity fields
# directly from master_stu_list (standing roster, one row per psd_id)
# and overwrite ALL of that psd_id's rows in previous_psd. Identity
# fields are constant per student, so a genuine single-entry error
# should be fixed everywhere for that student, not just on the one row
# that happened to get flagged during review.
#
# This replaces the old hand-written case_when() block (one line per
# confirmed case) — new confirmed cases need no new code here, only a
# reviewed = TRUE row with the bare outcome string in
# identity_review_tracking.
#
# Runs BEFORE dedup collapse, since corrections to identity
# fields should be in place before any collapsing happens, even though
# identity fields aren't currently part of the dedup key themselves.
#
# Cases marked "contamination suspected - needs separate handling" (e.g.
# 2013PSXY40) are NOT touched here — those need separate manual
# resolution, not a merge-based fix.

identity_fix_ids <- identity_review_tracking %>%
  filter(reviewed, str_detect(outcome, "^single data entry error - not a collision")) %>%
  pull(psd_id)

cat(length(identity_fix_ids), " psd_id(s) confirmed as single data entry ",
    "error(s) — correcting from master_stu_list.\n", sep = "")

identity_cols <- c("first_name", "middle_name", "last_name", "gender",
                   "race_ethnicity", "poverty_indicator", "hs_diploma")

# Fail loudly if master_stu_list doesn't have every expected column,
# rather than silently skipping one.
missing_identity_cols <- setdiff(identity_cols, names(master_stu_list))
if (length(missing_identity_cols) > 0) {
  stop("master_stu_list is missing expected identity column(s): ",
       paste(missing_identity_cols, collapse = ", "))
}

# Flag (don't silently drop) any confirmed psd_id with no master_stu_list
# row — this can't be fixed via merge and needs separate handling.
unmatched_identity_ids <- setdiff(identity_fix_ids, master_stu_list$psd_id)
if (length(unmatched_identity_ids) > 0) {
  warning(length(unmatched_identity_ids), " confirmed psd_id(s) not found ",
          "in master_stu_list — NOT corrected: ",
          paste(unmatched_identity_ids, collapse = ", "))
}

master_identity <- master_stu_list %>%
  filter(psd_id %in% identity_fix_ids) %>%
  distinct(psd_id, .keep_all = TRUE) %>%
  select(psd_id, all_of(identity_cols)) %>%
  rename_with(~ paste0(., "_master"), all_of(identity_cols))

previous_psd <- previous_psd %>%
  left_join(master_identity, by = "psd_id")

for (col in identity_cols) {
  master_col <- paste0(col, "_master")
  previous_psd[[col]] <- dplyr::if_else(
    previous_psd$psd_id %in% identity_fix_ids,
    dplyr::coalesce(previous_psd[[master_col]], previous_psd[[col]]),
    previous_psd[[col]]
  )
}

previous_psd <- previous_psd %>%
  select(-ends_with("_master"))

# Confirm every flagged psd_id now has exactly one distinct value per
# identity column.
still_inconsistent <- previous_psd %>%
  filter(psd_id %in% identity_fix_ids) %>%
  group_by(psd_id) %>%
  summarize(across(all_of(identity_cols), ~ n_distinct(., na.rm = TRUE)),
            .groups = "drop") %>%
  filter(if_any(all_of(identity_cols), ~ . > 1))

if (nrow(still_inconsistent) > 0) {
  stop("The following confirmed psd_id(s) still have >1 distinct value ",
       "in an identity column after correction — investigate before ",
       "proceeding to the next part:\n",
       paste(capture.output(print(still_inconsistent)), collapse = "\n"))
}

# ─────────────────────────────────────────────────────────────────────
# PATTERN-BASED CORRECTION: non-breaking space (U+00A0) standing in for
# a hyphen in compound names. Confirmed via 34 psd_ids/field-instances,
# all sitting directly between two letters (e.g. "SMITH[NBSP]JONES") —
# a single bulk replacement rule, not case-by-case correction. See
# dedup-investigation-log.md [2026-08-27] for the full investigation.
previous_psd <- previous_psd %>%
  mutate(
    first_name = str_replace_all(first_name, "\u00A0", "-"),
    middle_name = str_replace_all(middle_name, "\u00A0", "-"),
    last_name = str_replace_all(last_name, "\u00A0", "-")
  )

nbsp_remaining <- previous_psd %>%
  filter(if_any(c(first_name, middle_name, last_name), ~ str_detect(., "\u00A0"))) %>%
  nrow()
nbsp_remaining
# Expect 0. If not, some instance didn't match the letter-NBSP-letter
# pattern assumed above and needs individual review before proceeding.

## -----------------------------------------------------------------------------
## Part 5 - Re-derive scan objects needed for collapse (deterministic —
## re-computed here rather than read from a file, since none of this
## depends on human judgment; it's the exact same logic as
## dedup-investigation.R's Part 2/5, just re-run against this script's
## own loaded previous_psd)
## -----------------------------------------------------------------------------

is_missing_data_row <- function(df) {
  coalesce(df$status_source == "MISSING DATA", FALSE) |
    coalesce(df$college_name == "MISSING DATA", FALSE)
}

previous_psd_dedup_candidates <- previous_psd %>% filter(!is_missing_data_row(previous_psd))
previous_psd_protected <- previous_psd %>% filter(is_missing_data_row(previous_psd))

if (nrow(previous_psd_dedup_candidates) + nrow(previous_psd_protected) != nrow(previous_psd)) {
  stop("previous_psd_dedup_candidates + previous_psd_protected does not ",
       "reconstruct previous_psd exactly — investigate before continuing.")
}

nsc_fact_fields <- c("record_found", "req_return_field", "college_code",
                     "college_name", "college_state", "cc_4year",
                     "public_private", "enrollment_begin", "enrollment_end",
                     "enrollment_status", "he_graduated", "coll_grad_date",
                     "degree_title", "major", "college_sequence", "program_code")

previous_psd_nsc <- previous_psd_dedup_candidates %>%
  filter(coalesce(status_source == "NSC", FALSE))
previous_psd_other <- previous_psd_dedup_candidates %>%
  filter(coalesce(status_source != "NSC", TRUE))

if (nrow(previous_psd_nsc) + nrow(previous_psd_other) != nrow(previous_psd_dedup_candidates)) {
  stop("previous_psd_nsc + previous_psd_other does not reconstruct ",
       "previous_psd_dedup_candidates exactly — investigate before continuing.")
}

dedup_scope_nsc <- previous_psd_nsc %>%
  count(across(all_of(c("psd_id", nsc_fact_fields))), name = "n_copies") %>%
  filter(n_copies > 1) %>%
  group_by(psd_id) %>%
  summarize(n_duplicated_facts = n(), n_excess_rows = sum(n_copies - 1),
            max_copies = max(n_copies), .groups = "drop")

dedup_scope_other <- previous_psd_other %>%
  count(psd_id, record_term, record_year, name = "n_copies") %>%
  filter(n_copies > 1) %>%
  group_by(psd_id) %>%
  summarize(n_duplicated_facts = n(), n_excess_rows = sum(n_copies - 1),
            max_copies = max(n_copies), .groups = "drop")

dedup_scope <- bind_rows(nsc = dedup_scope_nsc, other = dedup_scope_other, .id = "source_group") %>%
  group_by(psd_id) %>%
  summarize(
    n_duplicated_facts = sum(n_duplicated_facts),
    n_excess_rows = sum(n_excess_rows),
    max_copies = max(max_copies),
    .groups = "drop"
  ) %>%
  arrange(desc(n_excess_rows)) %>%
  mutate(severity = if_else(n_excess_rows >= severe_threshold, "severe", "safe"))

nrow(dedup_scope)  # should match dedup-investigation.R's count exactly —
# if not, the two scripts are looking at different data

safe_ids <- dedup_scope %>% filter(severity == "safe") %>% pull(psd_id)
severe_ids <- dedup_scope %>% filter(severity == "severe") %>% pull(psd_id)


## -----------------------------------------------------------------------------
## Part 6 - Apply the fix
## -----------------------------------------------------------------------------
#
# ids_to_collapse combines:
#   - severe-tier: cleared_severe_ids, from severe_review_tracking (Part 5)
#   - safe-tier: confirmed "coherent - safe to collapse" outcomes from
#     r_review_tracking (Part 6), filtered to psd_id %in% safe_ids —
#     r_review_tracking also covers the 5 severe psd_ids, but those are
#     authoritatively handled via severe_review_tracking instead, so
#     this filter avoids any redundancy/conflict between the two.

cleared_severe_ids <- severe_review_tracking %>%
  filter(str_detect(outcome, "^coherent - safe to collapse")) %>%
  pull(psd_id)

cleared_safe_ids <- r_review_tracking %>%
  filter(psd_id %in% safe_ids, str_detect(outcome, "^coherent - safe to collapse")) %>%
  pull(psd_id)

ids_to_collapse <- c(cleared_safe_ids, cleared_severe_ids)

cat(length(ids_to_collapse), " psd_id(s) cleared for collapse (",
    length(cleared_safe_ids), " safe-tier, ", length(cleared_severe_ids),
    " severe-tier).\n", sep = "")

# Source priority for tie-breaking WITHIN the non-NSC branch below —
# multiple different non-NSC sources (e.g. OLD_PSD vs STAFF_VERIFIED)
# can genuinely collide on record_term + record_year, and the
# highest-confidence source should be the one kept. Full priority order
# (low to high confidence), established via 4 confirmed real conflicts
# (2020BXIG42, 2020YVUU23, 2024LICE67, 2024XVYC37 — see
# dedup-investigation-log.md [2026-08-26]):
source_priority <- c(
  "MISSING DATA" = 1, "OLD_PSD" = 2, "INFERRED" = 3, "SELF-REPORTED" = 4,
  "STAFF" = 5, "STAFF_VERIFIED" = 6, "NSC" = 7
)

previous_psd_corrected <- bind_rows(
  # NSC rows for cleared psd_ids — collapse using the source-conditional
  # NSC key (ALL nsc_fact_fields must match, not a curated subset).
  previous_psd_dedup_candidates %>%
    filter(psd_id %in% ids_to_collapse, coalesce(status_source == "NSC", FALSE)) %>%
    mutate(.source_rank = coalesce(source_priority[status_source], 0)) %>%
    arrange(desc(.source_rank)) %>%
    distinct(across(all_of(c("psd_id", nsc_fact_fields))), .keep_all = TRUE) %>%
    select(-.source_rank),
  # Non-NSC rows for cleared psd_ids — collapse using psd_id +
  # record_term + record_year alone. Source-priority tie-break applies
  # here.
  previous_psd_dedup_candidates %>%
    filter(psd_id %in% ids_to_collapse, coalesce(status_source != "NSC", TRUE)) %>%
    mutate(.source_rank = coalesce(source_priority[status_source], 0)) %>%
    arrange(desc(.source_rank)) %>%
    distinct(psd_id, record_term, record_year, .keep_all = TRUE) %>%
    select(-.source_rank),
  # non-placeholder rows for psd_ids NOT cleared for collapse (unaffected
  # students, plus severe cases not yet cleared) — pass through untouched
  previous_psd_dedup_candidates %>%
    filter(!psd_id %in% ids_to_collapse),
  # ALL missing-data placeholder rows, for EVERY psd_id, unconditionally
  # untouched — these can represent genuinely independent follow-up
  # attempts, not necessarily duplicates.
  previous_psd_protected
)

# Confirm the tie-break behaved as expected: none of the 4 known
# source-conflict psd_ids should still show BOTH an NSC and a STAFF row
# for the same key after collapse.
previous_psd_corrected %>%
  filter(psd_id %in% c("2020BXIG42", "2020YVUU23", "2024LICE67", "2024XVYC37")) %>%
  count(psd_id, record_year, record_term, status_source) %>%
  filter(psd_id %in% (
    previous_psd_corrected %>%
      filter(psd_id %in% c("2020BXIG42", "2020YVUU23", "2024LICE67", "2024XVYC37")) %>%
      count(psd_id, record_year, record_term) %>%
      filter(n > 1) %>%
      pull(psd_id)
  ))
# Expect 0 rows returned above.

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
## Part 7 - Correct data contamination records
## -----------------------------------------------------------------------------

contaminated_ids <- identity_review_tracking %>%
  filter(str_detect(outcome, "contamination")) %>%
  pull(psd_id)

cat(length(contaminated_ids), " contaminated psd_id(s) identified: ",
    paste(contaminated_ids, collapse = ", "), "\n", sep = "")

contamination_corrections_tracking <- read_csv(file.path(data_quality_dir,
                                                         contamination_corrections_csv))

# ── PRE-FLIGHT: every contaminated id must have a defined correction ───
# Checked FIRST, before any row is touched — confirms coverage against
# contamination_corrections_tracking.csv directly, rather than waiting
# until after the fact to discover a gap in previous_psd_corrected. Any
# id with no reviewed row here would otherwise pass through
# completely untouched, still carrying whatever wrong data flagged it as
# contaminated in the first place — silently, since it would still have
# rows (just uncorrected ones), which a pure row-count check downstream
# would not catch.
ids_missing_correction <- setdiff(
  contaminated_ids,
  contamination_corrections_tracking %>% filter(reviewed) %>% pull(psd_id) %>% unique()
)

if (length(ids_missing_correction) > 0) {
  stop(length(ids_missing_correction), " contaminated psd_id(s) have NO reviewed ",
       "row in contamination_corrections_tracking.csv — add one before proceeding:\n",
       paste(ids_missing_correction, collapse = ", "))
}

# ── Rebuild ids: the ONLY case where blanket row removal is correct ────
# full_rebuild is the one action type where NONE of a psd_id's existing
# rows are trustworthy, so removing everything up front and rebuilding
# from scratch is the right approach. reassign_psd_id and
# split_by_identity are DELIBERATELY excluded from any blanket removal —
# both already perform their own precise, row-level removal internally
# (reassign_psd_id via anti_join on the exact matched row; split_by_identity
# via filter() scoped to its own ids right before re-adding the
# redistributed rows). Blanket-removing those ids' rows here was
# confirmed 2026-09-03 to be the direct cause of a real data-loss bug —
# e.g. 2018FOSG59 losing all 16 of its own correct rows, not just the 1
# stray one — which required an entire separate "restore untouched rows"
# workaround to undo. Scoping removal to full_rebuild ids only removes
# the bug at its source instead of patching around it.
full_rebuild_ids <- contamination_corrections_tracking %>%
  filter(reviewed, action == "full_rebuild") %>%
  pull(psd_id)

n_removed_contaminated <- previous_psd_corrected %>%
  filter(psd_id %in% full_rebuild_ids) %>%
  nrow()

previous_psd_corrected <- previous_psd_corrected %>%
  filter(!psd_id %in% full_rebuild_ids)

cat(n_removed_contaminated, " row(s) removed for ", length(full_rebuild_ids),
    " full_rebuild psd_id(s).\n", sep = "")

# 2013PSXY40 — confirmed 2026-09-03 against the original source Excel and
# old NSC reports: this psd_id shared a student_id with 2013PNCQ15
# upstream, and NONE of its merged PSD rows are correct. This is not a
# duplicate-collapse case (see Part 4/11) — every existing row for this
# psd_id is removed and replaced with rows manually rebuilt from the
# verified source data.

# ── Rebuild 2013PSXY40 from the verified source Excel ──────────────────
# Source: original source Excel (annual-snapshot format — one row per
# student, one column-pair per year), cross-checked against old NSC
# reports. hs_grad_year = 2013. Two rows per tracked year (Fall +
# "enrolled anytime after fall" of the following year), through Year 6:
#   Years 1-2 (Fall 2013 - Fall 2014 window): confirmed CSUN enrollment
#   Years 3-6: Missing data in the source sheet for all four years
# status_source = "OLD_PSD" for every row — this is a manual rebuild
# from the archival Excel, not a fresh NSC or staff-verified pull.

psd_2013psxy40_terms <- tibble::tribble(
  ~record_term,                   ~record_year, ~college_name,
  "fall",                         2013,         "CALIFORNIA STATE UNIVERSITY- NORTHRIDGE",
  "enrolled anytime after fall",  2014,         "CALIFORNIA STATE UNIVERSITY- NORTHRIDGE",
  "fall",                         2014,         "CALIFORNIA STATE UNIVERSITY- NORTHRIDGE",
  "enrolled anytime after fall",  2015,         "CALIFORNIA STATE UNIVERSITY- NORTHRIDGE",
  "fall",                         2015,         "MISSING DATA",
  "enrolled anytime after fall",  2016,         "MISSING DATA",
  "fall",                         2016,         "MISSING DATA",
  "enrolled anytime after fall",  2017,         "MISSING DATA",
  "fall",                         2017,         "MISSING DATA",
  "enrolled anytime after fall",  2018,         "MISSING DATA",
  "fall",                         2018,         "MISSING DATA",
  "enrolled anytime after fall",  2019,         "MISSING DATA",
) %>%
  mutate(psd_id = "2013PSXY40", .before = 1)

# Confirm the source row count before anything downstream depends on it.
if (nrow(psd_2013psxy40_terms) != 12) {
  stop("Expected 12 term rows for 2013PSXY40 — got ", nrow(psd_2013psxy40_terms),
       ". Check psd_2013psxy40_terms before proceeding.")
}

# Pull confirmed identity fields from master_stu_list — same approach as
# Part 7's merge-based correction, rather than hand-typing values that
# could drift from the standing roster.
identity_2013psxy40 <- master_stu_list %>%
  filter(psd_id == "2013PSXY40") %>%
  distinct(psd_id, .keep_all = TRUE) %>%
  select(psd_id, student_id, first_name, middle_name, last_name,
         hs_grad_year, gender, race_ethnicity, poverty_indicator, hs_diploma)

if (nrow(identity_2013psxy40) != 1) {
  stop("Expected exactly 1 master_stu_list row for 2013PSXY40 — got ",
       nrow(identity_2013psxy40), ". Investigate before proceeding.")
}

# Attach college metadata from institution_lookup (college_code,
# college_state, cc_4year, public_private, system_type) for the
# confirmed CSUN rows. For MISSING DATA rows, skip the join and set all
# five fields to the literal "MISSING DATA" string, matching the
# convention already used for missing-data placeholders elsewhere (see
# 06-merge-missing-data.R's psd_temp block).
psd_2013psxy40_corrected <- psd_2013psxy40_terms %>%
  left_join(
    institution_lookup %>%
      select(college_name, college_code, college_state, cc_4year,
             public_private, system_type),
    by = "college_name"
  ) %>%
  mutate(
    across(c(college_code, college_state, cc_4year, public_private, system_type),
           ~ if_else(college_name == "MISSING DATA", "MISSING DATA", .)),
    he_graduated = if_else(college_name == "MISSING DATA", NA_character_, "N")
  ) %>%
  left_join(identity_2013psxy40, by = "psd_id") %>%
  mutate(
    hs_grad_date     = as.Date("2013-06-06"),
    high_school_code = "051662",
    status_source    = "OLD_PSD",
    name_suffix       = NA_character_,
    req_return_field  = NA_character_,
    record_found      = NA_character_,
    enrollment_begin  = as.Date(NA),
    enrollment_end    = as.Date(NA),
    enrollment_status = NA_character_,
    coll_grad_date    = as.Date(NA),
    degree_title      = NA_character_,
    major             = NA_character_,
    college_sequence  = NA_character_,
    program_code      = NA_character_,
    notes = paste("Manually rebuilt 2026-09-03: prior merged records for",
                  "this psd_id confirmed incorrect (shared student_id",
                  "with 2013PNCQ15 upstream). Rebuilt from source Excel",
                  "(annual-snapshot format) + old NSC reports.")
  )

# Guard: confirm the join actually matched CSUN metadata (didn't
# silently produce NAs) and that identity fields came through.
if (any(is.na(psd_2013psxy40_corrected$college_code[
  psd_2013psxy40_corrected$college_name == "CALIFORNIA STATE UNIVERSITY- NORTHRIDGE"]))) {
  stop("institution_lookup join failed to match CSUN metadata for one or ",
       "more rows — check college_name spelling against institution_lookup.")
}

if (any(is.na(psd_2013psxy40_corrected$first_name))) {
  stop("Identity fields did not populate from master_stu_list for ",
       "2013PSXY40 — check the join before proceeding.")
}

# Align column set AND types to previous_psd_corrected before binding.
# Several fields above were hardcoded as NA_character_, but some of the
# corresponding columns in previous_psd_corrected are actually a
# different type (e.g. name_suffix inferred as double by read_csv, since
# it's blank/NA across most of the real data) — bind_rows() does not
# auto-coerce double <-> character the way it does for compatible
# numeric types, so a hardcoded-wrong type throws rather than silently
# widening. Cast every shared column to match previous_psd_corrected's
# actual type first, THEN align columns and bind, so this can't recur
# for any other hardcoded field (req_return_field, record_found,
# enrollment_status, degree_title, major, college_sequence,
# program_code all carry the same risk).
cast_to_match <- function(df, reference) {
  shared_cols <- intersect(names(df), names(reference))
  for (col in shared_cols) {
    target_class <- class(reference[[col]])[1]
    df[[col]] <- switch(target_class,
                        "character" = as.character(df[[col]]),
                        "numeric"   = as.numeric(df[[col]]),
                        "double"    = as.double(df[[col]]),
                        "integer"   = as.integer(df[[col]]),
                        "logical"   = as.logical(df[[col]]),
                        "Date"      = as.Date(df[[col]]),
                        df[[col]]  # unrecognized class — leave as-is rather than guess
    )
  }
  df
}

psd_2013psxy40_corrected <- psd_2013psxy40_corrected %>%
  cast_to_match(previous_psd_corrected)

# Any column present in previous_psd_corrected but not explicitly set
# above comes through as NA rather than causing a bind_rows() column
# mismatch.
psd_2013psxy40_corrected <- previous_psd_corrected %>%
  dplyr::slice(0) %>%
  bind_rows(psd_2013psxy40_corrected) %>%
  select(all_of(names(previous_psd_corrected)))

previous_psd_corrected <- bind_rows(previous_psd_corrected, psd_2013psxy40_corrected)

cat(nrow(psd_2013psxy40_corrected), " rebuilt row(s) added for 2013PSXY40 (",
    sum(psd_2013psxy40_corrected$college_name == "CALIFORNIA STATE UNIVERSITY- NORTHRIDGE"),
    " confirmed CSUN, ",
    sum(psd_2013psxy40_corrected$college_name == "MISSING DATA"),
    " missing data).\n", sep = "")

# ── Explicit row reassignments ──────────────────────────────────────────
# For individual rows where the row's OWN name fields don't reliably
# indicate its correct owner — the general split_by_identity join below
# would get these wrong, since it trusts each row's name fields to route
# it. Pulled out and moved explicitly first, using psd_id + record_term +
# record_year to locate the exact row rather than identity matching.
#
# Confirmed 2026-09-03: within the 2014UZIS99 / 2014RPEE11 split, the
# PLANS/2014 row under each psd_id was individually cross-attributed —
# the CSULA-PLANS row sitting under 2014UZIS99 actually belongs to
# 2014RPEE11, and the UCI-PLANS row sitting under 2014RPEE11 actually
# belongs to 2014UZIS99.

reassign_corrections <- contamination_corrections_tracking %>%
  filter(reviewed, action == "reassign_psd_id")

# Guard: every reassign_psd_id row MUST have a target_psd_id — a blank
# here would set psd_id to NA on whatever row matches, silently
# corrupting the PSD. Caught a real instance of this (four leftover
# rows from an earlier tracking-schema draft, before target_psd_id
# existed) on 2026-09-03 — this stop() exists so a similar CSV mistake
# can never again pass through silently.
missing_target <- reassign_corrections %>% filter(is.na(target_psd_id))
if (nrow(missing_target) > 0) {
  stop(nrow(missing_target), " reassign_psd_id row(s) in ",
       "contamination_corrections_tracking.csv have no target_psd_id — ",
       "cannot proceed:\n",
       paste(capture.output(print(missing_target %>%
                                    select(psd_id, record_term, record_year, notes))), collapse = "\n"))
}

reassigned_rows <- tibble()

if (nrow(reassign_corrections) > 0) {
  
  original_matched_rows <- tibble()
  
  for (i in seq_len(nrow(reassign_corrections))) {
    src_id   <- reassign_corrections$psd_id[i]
    tgt_id   <- reassign_corrections$target_psd_id[i]
    term_val <- reassign_corrections$record_term[i]
    year_val <- reassign_corrections$record_year[i]
    
    candidate_rows <- previous_psd %>%
      filter(psd_id == src_id, toupper(record_term) == toupper(term_val), record_year == year_val)
    
    if (nrow(candidate_rows) == 1) {
      # Unambiguous — single row at this psd_id/term/year, e.g. the
      # 2014UZIS99/2014RPEE11 PLANS swap.
      matched_row <- candidate_rows
      
    } else if (nrow(candidate_rows) > 1) {
      # Ambiguous — more than one row shares this psd_id/term/year with
      # no distinguishing field (e.g. two MISSING DATA rows, both
      # 2016YPAE66/Fall 2023). Disambiguate using each candidate row's
      # OWN name + hs_grad_year fields against master_stu_list's record
      # for the TARGET student — same identity-match logic used in
      # split_by_identity below, just scoped to these specific
      # candidates instead of a whole psd_id's row set.
      #
      # Match case-insensitively on name fields — confirmed 2026-09-03
      # that master_stu_list stores names in title case ("Jack Smith")
      # while previous_psd stores names in uppercase ("JACK SMITH") for
      # at least some rows, and R's exact-match join is case-sensitive.
      # ALL FOUR fields (including middle_name) remain required — this
      # only tolerates a casing difference, not a missing/differing
      # field, since two students sharing first+last name is a real
      # possibility middle_name protects against.
      #
      # hs_grad_year is ALSO coerced to character on both sides — same
      # reasoning as split_by_identity's join below: a numeric/character
      # type mismatch here silently fails every candidate match, not
      # just some.
      #
      # str_squish() (whitespace trim) added 2026-09-03 alongside
      # str_to_upper() — confirmed a manually-entered master_stu_list
      # value with stray leading/trailing whitespace can fail an exact
      # match even after case normalization, silently sending rows to
      # the wrong side of a split. The diagnostic used to investigate
      # this already included str_squish(); it just hadn't been carried
      # into this production code until now.
      target_identity <- master_stu_list %>%
        filter(psd_id == tgt_id) %>%
        distinct(first_name, middle_name, last_name, hs_grad_year) %>%
        mutate(across(c(first_name, middle_name, last_name), ~str_to_upper(str_squish(.))),
               hs_grad_year = as.character(hs_grad_year))
      
      if (nrow(target_identity) != 1) {
        stop("Expected exactly 1 master_stu_list identity for target_psd_id = ",
             tgt_id, " — found ", nrow(target_identity), ". Cannot disambiguate ",
             "row ", i, " in contamination_corrections_tracking.csv.")
      }
      
      matched_row <- candidate_rows %>%
        mutate(.first_name_upper = str_to_upper(str_squish(first_name)),
               .middle_name_upper = str_to_upper(str_squish(middle_name)),
               .last_name_upper = str_to_upper(str_squish(last_name)),
               .hs_grad_year_chr = as.character(hs_grad_year)) %>%
        semi_join(
          target_identity %>%
            rename(.first_name_upper = first_name, .middle_name_upper = middle_name,
                   .last_name_upper = last_name, .hs_grad_year_chr = hs_grad_year),
          by = c(".first_name_upper", ".middle_name_upper", ".last_name_upper", ".hs_grad_year_chr")
        ) %>%
        select(-.first_name_upper, -.middle_name_upper, -.last_name_upper, -.hs_grad_year_chr)
      
      if (nrow(matched_row) != 1) {
        stop("Ambiguous reassignment for psd_id = ", src_id, ", record_term = ",
             term_val, ", record_year = ", year_val, ": found ", nrow(candidate_rows),
             " candidate row(s), and ", nrow(matched_row), " matched target_psd_id = ",
             tgt_id, "'s identity via name + hs_grad_year (expected exactly 1). ",
             "Investigate row ", i, " in contamination_corrections_tracking.csv ",
             "before proceeding.")
      }
      
    } else {
      stop("Expected at least 1 row for psd_id = ", src_id, ", record_term = ",
           term_val, ", record_year = ", year_val, " — found 0. Check ",
           "contamination_corrections_tracking.csv row ", i, ".")
    }
    
    # Keep the ORIGINAL row (pre-reassignment psd_id) so the removal
    # step below can target this exact row — critical for the ambiguous
    # branch, where psd_id/term/year alone would also match the OTHER
    # candidate that should stay untouched (e.g. 2016YPAE66's own
    # correct Fall 2023 record).
    original_matched_rows <- bind_rows(original_matched_rows, matched_row)
    
    # Overwrite identity fields from master_stu_list for the TARGET
    # psd_id — not just psd_id itself. The ambiguous branch above
    # already guarantees name fields match the target (that's how the
    # row was found), but the unambiguous branch locates a row purely by
    # psd_id/term/year with no identity check at all — if that row was
    # originally created with a wrong psd_id AND wrong name/demographic
    # fields together (plausible for manually-entered rows), leaving
    # those fields untouched would silently carry the error forward
    # under a now-correct psd_id. Same source-of-truth approach as
    # Part 7's identity correction.
    target_master_identity <- master_stu_list %>%
      filter(psd_id == tgt_id) %>%
      distinct(psd_id, .keep_all = TRUE) %>%
      select(psd_id, student_id, first_name, middle_name, last_name,
             hs_grad_year, gender, race_ethnicity, poverty_indicator, hs_diploma)
    
    if (nrow(target_master_identity) != 1) {
      stop("Expected exactly 1 master_stu_list identity for target_psd_id = ",
           tgt_id, " — found ", nrow(target_master_identity), ". Cannot apply ",
           "identity fields for row ", i, " in ",
           "contamination_corrections_tracking.csv.")
    }
    
    reassigned_row <- matched_row %>%
      select(-any_of(c("student_id", "first_name", "middle_name", "last_name",
                       "hs_grad_year", "gender", "race_ethnicity",
                       "poverty_indicator", "hs_diploma", "psd_id"))) %>%
      bind_cols(target_master_identity)
    
    reassigned_rows <- bind_rows(reassigned_rows, reassigned_row)
  }
  
  # Remove the ORIGINAL occurrence of each reassigned row from
  # previous_psd_corrected — matched on the FULL row content (every
  # shared column), not just psd_id/record_term/record_year, so a row
  # that was disambiguated by name fields above removes only itself and
  # leaves any other candidate at the same psd_id/term/year untouched.
  # This is the ONLY removal reassign_psd_id ids ever undergo, so every other 
  # row for these ids was never touched and needs no separate restoration step.
  removal_key_cols <- intersect(names(previous_psd_corrected), names(original_matched_rows))
  
  previous_psd_corrected <- previous_psd_corrected %>%
    anti_join(original_matched_rows, by = removal_key_cols)
  
  # Same type-mismatch risk as the 2013PSXY40 rebuild: reassigned_rows'
  # identity fields (student_id, hs_grad_year, gender, race_ethnicity,
  # poverty_indicator, hs_diploma) came from master_stu_list via
  # bind_cols() above, not from previous_psd — if master_stu_list's
  # column types differ at all from previous_psd_corrected's (e.g.
  # hs_grad_year as integer vs. character), this bind_rows() would throw
  # the same "Can't combine <type> and <type>" error. cast_to_match()
  # (defined above, in the 2013PSXY40 rebuild section) is reused here.
  reassigned_rows <- reassigned_rows %>%
    cast_to_match(previous_psd_corrected)
  
  previous_psd_corrected <- bind_rows(previous_psd_corrected, reassigned_rows)
  
  cat(nrow(reassigned_rows), " row(s) explicitly reassigned via ",
      nrow(reassign_corrections), " confirmed exception(s).\n", sep = "")
}

# ── 2014UZIS99 / 2014RPEE11: hardcoded UCI-block reassignment ──────────
# Manually verified 2026-09-03 after name-matching (split_by_identity)
# proved unreliable for this specific pair across 3 separate attempts
# (case, whitespace, and a genuine content difference in middle_name all
# ruled out or fixed without resolving it — the underlying identity
# assignment itself was backwards from the original assumption). Triple-
# confirmed final resolution:
#   - The 20-row UNIVERSITY OF CALIFORNIA - IRVINE block (excluding the
#     PLANS row) currently sitting under psd_id 2014UZIS99 actually
#     belongs to 2014RPEE11.
#   - Both PLANS rows (UCI under 2014RPEE11, CSULA under 2014UZIS99) are
#     ALREADY correctly placed — confirmed NOT to need reassignment,
#     reversing an earlier (incorrect) PLANS-swap correction that has
#     since been removed from contamination_corrections_tracking.csv.
#   - The 8-row LACC block (2014RPEE11 -> 2014UZIS99) is unaffected by
#     this change and remains handled via the standard reassign_psd_id
#     mechanism above.
# Identified by college_name rather than by name-matching, since names
# are exactly what proved unreliable for this pair.

uci_block <- previous_psd %>%
  filter(psd_id == "2014UZIS99",
         college_name == "UNIVERSITY OF CALIFORNIA - IRVINE",
         toupper(record_term) != "PLANS")

if (nrow(uci_block) != 20) {
  stop("Expected exactly 20 UCI rows under 2014UZIS99 (excl. PLANS) — found ",
       nrow(uci_block), ". Data may have changed since this correction was ",
       "manually verified on 2026-09-03 — re-confirm before proceeding.")
}

uci_target_identity <- master_stu_list %>%
  filter(psd_id == "2014RPEE11") %>%
  distinct(psd_id, .keep_all = TRUE) %>%
  select(psd_id, student_id, first_name, middle_name, last_name,
         hs_grad_year, gender, race_ethnicity, poverty_indicator, hs_diploma)

if (nrow(uci_target_identity) != 1) {
  stop("Expected exactly 1 master_stu_list identity for 2014RPEE11 — found ",
       nrow(uci_target_identity), ". Cannot proceed with UCI-block reassignment.")
}

uci_block_reassigned <- uci_block %>%
  select(-any_of(c("student_id", "first_name", "middle_name", "last_name",
                   "hs_grad_year", "gender", "race_ethnicity",
                   "poverty_indicator", "hs_diploma", "psd_id"))) %>%
  bind_cols(uci_target_identity) %>%
  cast_to_match(previous_psd_corrected)

uci_removal_cols <- intersect(names(previous_psd_corrected), names(uci_block))

previous_psd_corrected <- previous_psd_corrected %>%
  anti_join(uci_block, by = uci_removal_cols) %>%
  bind_rows(uci_block_reassigned)

cat(nrow(uci_block_reassigned), " UCI row(s) reassigned from 2014UZIS99 to ",
    "2014RPEE11 (hardcoded correction).\n", sep = "")

# ── Split-by-identity corrections ───────────────────────────────────────
# For psd_ids confirmed in contamination_corrections_tracking.csv as
# "split_by_identity" — genuinely two (or more) different students'
# coherent record histories merged under one contaminated psd_id (same
# shared-upstream-student_id root cause as 2013PSXY40, but UNLIKE that
# case, both blocks of records are individually trustworthy, so this is
# a reassignment, not a rebuild-from-scratch). Every raw row for the
# flagged psd_id is re-matched to its correct owner via
# first_name/middle_name/last_name/hs_grad_year against master_stu_list
# — the standing roster is treated as the source of truth here, since
# previous_psd's own psd_id assignment is exactly what's contaminated.
#
# EXCLUDES any row already handled above via explicit reassign_psd_id —
# those rows' name fields are known NOT to reliably indicate ownership,
# so they must not also flow through this join.
#
# First confirmed case: 2014UZIS99 -> splits into 2014UZIS99 (Person_1,
# 21-row coherent UCI history) and 2014RPEE11 (Person_2, 13-row
# multi-source history) — see contamination_corrections_tracking.csv.

split_by_identity_ids <- contamination_corrections_tracking %>%
  filter(reviewed, action == "split_by_identity") %>%
  pull(psd_id)

cat(length(split_by_identity_ids), " psd_id(s) confirmed as split_by_identity: ",
    paste(split_by_identity_ids, collapse = ", "), "\n", sep = "")

if (length(split_by_identity_ids) > 0) {
  
  # Guard 1: confirm master_stu_list has no duplicate name + hs_grad_year
  # combination ACROSS THE FULL ROSTER — a duplicate here would cause a
  # raw row to fan out into multiple matches instead of resolving to one
  # correct psd_id, silently producing extra rows.
  dup_master_identity <- master_stu_list %>%
    count(first_name, middle_name, last_name, hs_grad_year) %>%
    filter(n > 1)
  
  if (nrow(dup_master_identity) > 0) {
    stop("master_stu_list has ", nrow(dup_master_identity), " name + ",
         "hs_grad_year combination(s) matching more than one roster row — ",
         "split-by-identity matching is not safe until this is resolved:\n",
         paste(capture.output(print(dup_master_identity)), collapse = "\n"))
  }
  
  split_candidates <- previous_psd %>%
    filter(psd_id %in% split_by_identity_ids) %>%
    mutate(.record_term_upper = toupper(record_term)) %>%
    # exclude rows already handled by explicit reassign_psd_id above
    # (record_term compared case-insensitively — stored values are
    # inconsistently cased across the PSD, e.g. FALL vs fall)
    anti_join(
      reassign_corrections %>%
        filter(psd_id %in% split_by_identity_ids) %>%
        mutate(.record_term_upper = toupper(record_term)) %>%
        select(psd_id, .record_term_upper, record_year),
      by = c("psd_id", ".record_term_upper", "record_year")
    ) %>%
    select(-.record_term_upper)
  
  # Match every raw row to its correct owner via identity fields.
  # Matched case-insensitively on name fields, same reasoning and
  # approach as the ambiguous branch in reassign_psd_id above —
  # master_stu_list uses title case, previous_psd uses uppercase for at
  # least some rows, and this join is otherwise case-sensitive. All four
  # fields (including middle_name) remain required.
  #
  # hs_grad_year is ALSO coerced to character on both sides for the join
  # key — confirmed 2026-09-03 that a numeric/character type mismatch
  # here causes every candidate row to silently fail to match (Guard 2
  # below catches this as "0 matched," not a partial-match issue).
  #
  # str_squish() (whitespace trim) added 2026-09-03 alongside
  # str_to_upper() — same fix as the ambiguous branch above; a
  # manually-entered master_stu_list value with stray whitespace can
  # fail an exact match even after case normalization, silently sending
  # every row in a split to the wrong side instead of just some.
  split_matched <- split_candidates %>%
    mutate(.first_name_upper = str_to_upper(str_squish(first_name)),
           .middle_name_upper = str_to_upper(str_squish(middle_name)),
           .last_name_upper = str_to_upper(str_squish(last_name)),
           .hs_grad_year_chr = as.character(hs_grad_year)) %>%
    left_join(
      master_stu_list %>%
        select(first_name, middle_name, last_name, hs_grad_year, psd_id) %>%
        mutate(.first_name_upper = str_to_upper(str_squish(first_name)),
               .middle_name_upper = str_to_upper(str_squish(middle_name)),
               .last_name_upper = str_to_upper(str_squish(last_name)),
               .hs_grad_year_chr = as.character(hs_grad_year)) %>%
        select(-first_name, -middle_name, -last_name, -hs_grad_year) %>%
        rename(matched_psd_id = psd_id),
      by = c(".first_name_upper", ".middle_name_upper", ".last_name_upper", ".hs_grad_year_chr")
    ) %>%
    select(-.first_name_upper, -.middle_name_upper, -.last_name_upper, -.hs_grad_year_chr)
  
  # Guard 2: every row must resolve to exactly one matched identity —
  # don't silently drop or misassign a row that doesn't match.
  unmatched_split_rows <- split_matched %>% filter(is.na(matched_psd_id))
  if (nrow(unmatched_split_rows) > 0) {
    stop(nrow(unmatched_split_rows), " row(s) in split_by_identity psd_id(s) ",
         "did not match any master_stu_list identity — investigate before ",
         "proceeding:\n",
         paste(capture.output(print(unmatched_split_rows %>%
                                      select(psd_id, hs_grad_year, record_term, record_year))),
               collapse = "\n"))
  }
  
  split_matched <- split_matched %>%
    mutate(psd_id = matched_psd_id) %>%
    select(-matched_psd_id) %>%
    cast_to_match(previous_psd_corrected)
  
  # Remove ONLY the original rows that went INTO split_candidates (full
  # row match, not a blanket psd_id filter) — critical fix, confirmed
  # 2026-09-03: the old `filter(!psd_id %in% split_by_identity_ids)`
  # removed EVERY row currently tagged with a split_by_identity id,
  # including rows reassign_psd_id had JUST correctly placed there
  # moments earlier in the same run (e.g. the UCI-PLANS row moved onto
  # 2014UZIS99) — those rows were deliberately excluded from
  # split_candidates precisely so they wouldn't be double-processed, but
  # the blanket filter then deleted them anyway with nothing to restore
  # them, since they were never part of split_matched. Confirmed via
  # row-count arithmetic: combined 2014UZIS99 + 2014RPEE11 total dropped
  # from an expected 43 to an observed 42 — exactly the 1 row this bug
  # silently discarded.
  split_removal_cols <- intersect(names(previous_psd_corrected), names(split_candidates))
  
  previous_psd_corrected <- previous_psd_corrected %>%
    anti_join(split_candidates, by = split_removal_cols) %>%
    bind_rows(split_matched)
  
  cat(nrow(split_matched), " row(s) reassigned across ",
      length(split_by_identity_ids), " split_by_identity psd_id(s) — now ",
      "distributed to ", n_distinct(split_matched$psd_id), " correct psd_id(s).\n", sep = "")
  
  # Guard 3 (warning, not stop): The duplicate collapse already ran before
  # this point, so any NEW duplicate created by reassigning rows onto an
  # existing psd_id (e.g. 2014RPEE11 already had its own rows elsewhere
  # in the PSD) won't get automatically resolved. Flag for manual review
  # rather than silently leaving duplicates in the exported file.
  post_split_dupes <- previous_psd_corrected %>%
    filter(psd_id %in% unique(c(split_matched$psd_id, reassigned_rows$psd_id))) %>%
    count(psd_id, record_term, record_year, status_source) %>%
    filter(n > 1)
  
  if (nrow(post_split_dupes) > 0) {
    warning(nrow(post_split_dupes), " psd_id/term/year/status_source ",
            "combination(s) now have more than one row after the split — ",
            "the duplicates collapse already ran and won't catch these ",
            "automatically. Review before export:\n",
            paste(capture.output(print(post_split_dupes)), collapse = "\n"))
  }
}

# ── SECONDARY SAFETY NET: catch a zero-row outcome as a last resort ────
# The PRE-FLIGHT check at the top  is now the primary
# safeguard — it verifies every contaminated id has a reviewed row in
# contamination_corrections_tracking.csv BEFORE any row is touched. This
# check is a second, weaker layer: under the current architecture (only
# full_rebuild ids get blanket-removed; reassign_psd_id and
# split_by_identity ids are never bulk-deleted), most ids will trivially
# have nonzero rows here regardless of whether their correction actually
# ran correctly. Kept as defense-in-depth for a genuine edge case — e.g.
# a full_rebuild or split_by_identity correction that, due to a bug,
# produces zero output rows for an id — rather than as the main
# guarantee against silent data loss.
ids_with_zero_rows <- setdiff(
  contaminated_ids,
  unique(previous_psd_corrected$psd_id)
)

if (length(ids_with_zero_rows) > 0) {
  stop(length(ids_with_zero_rows), " contaminated psd_id(s) have ZERO rows ",
       "in previous_psd_corrected despite having a reviewed correction — ",
       "investigate the correction logic for these ids before proceeding:\n",
       paste(ids_with_zero_rows, collapse = ", "))
}

## -----------------------------------------------------------------------------
## Part 8 - Targeted removal: confirmed post-graduation stale placeholders
## -----------------------------------------------------------------------------

# NOT a general automated rule, unlike Part 1.5's Rules 1/2 — a TARGETED
# correction, individually confirmed via r_review_tracking's manual
# review, for psd_ids with a genuine NSC graduation followed by a stale
# INFERRED placeholder that serves no purpose now that the student has
# actually graduated.
#
# WHY Part 1.5's Rules 1/2 don't catch these automatically: both rules
# only supersede a placeholder if an authoritative record exists for the
# SAME specific term/year. A graduation isn't a "this term" fact — it's
# terminal — but neither rule checks "has this student already
# graduated at all," only "does THIS exact term have a matching
# authoritative record."
#
# ⚠️ FIXED 08/29/2026: previously a hardcoded vector with no connection
# to any actual review record — meant a fresh run would delete the same
# 3 students' rows unconditionally, with no verification a review ever
# happened. Now reads confirmed cases directly from r_review_tracking —
# traceable back to an actual saved review, not a static assumption.
 targeted_removal_ids <- r_review_tracking %>%
  filter(str_detect(outcome, "missin")) %>%   # catches "missingdata"/"missindata" typos
  pull(psd_id)

length(targeted_removal_ids)

removed_inferred_rows <- previous_psd_corrected %>%
  filter(psd_id %in% targeted_removal_ids, status_source == "INFERRED")

# Guard: expect exactly 2 rows per confirmed psd_id (the Fall +
# Enrolled Anytime After Fall hedge pair) — not a fixed "6", since
# targeted_removal_ids is now dynamic. Do NOT proceed on an unexpected
# count.
n_expected_inferred_removal <- length(targeted_removal_ids) * 2

if (nrow(removed_inferred_rows) != n_expected_inferred_removal) {
  stop("Expected exactly ", n_expected_inferred_removal, " rows to remove (",
       length(targeted_removal_ids), " psd_id(s) x 2-row hedge pair each) — got ",
       nrow(removed_inferred_rows), ". Investigate before removing anything.")
}

previous_psd_corrected <- previous_psd_corrected %>%
  anti_join(removed_inferred_rows,
            by = c("psd_id", "record_year", "record_term", "status_source"))

cat(nrow(removed_inferred_rows), " stale INFERRED row(s) removed for ",
    length(targeted_removal_ids), " confirmed post-graduation psd_id(s).\n", sep = "")


## -----------------------------------------------------------------------------
## Part 9 - Reporting-lag annotation: flag superseded placeholder records
## -----------------------------------------------------------------------------

# NOT a change to what gets collapsed or kept — this is a pure
# ANNOTATION, added specifically to support reporting accuracy (e.g.
# college completion rate calculations) — a placeholder row that's since
# been resolved by a later authoritative record shouldn't silently
# continue counting as "still missing" in a report, even though it
# correctly stays in the data as a historical record of what was known
# at the time.
#
# MOVED HERE 2026-09-03 — originally Part 2, run immediately after
# loading previous_psd, before ANY correction was applied. Confirmed
# this was a real bug, not just an ordering preference: this logic
# matches on psd_id + record_term + record_year to decide whether a
# placeholder is "superseded" — but before the contamination
# corrections run, a psd_id's row set can contain records belonging to
# an ENTIRELY DIFFERENT student (e.g. 2018FOSG59 originally held 16 rows
# that were its own plus 1 stray row actually belonging to 2018CAEA56).
# Running this match against a contaminated row set could:
#   - incorrectly flag a placeholder as "superseded" by an authoritative
#     record that actually belongs to a different (wrongly-merged)
#     student, or
#   - fail to flag a genuinely superseded placeholder, because the
#     authoritative record that should supersede it was sitting under a
#     different psd_id due to not-yet-corrected contamination.
# Running this on previous_psd_corrected — after the duplicate collapse,
# and contamination fixes, AND then the targeted removal have ALL
# completed — guarantees the psd_id groupings and row set being compared
# are the final, correct ones, not a to-be-corrected snapshot.
#
# Confirmed via search 2026-09-03: no downstream code in this script
# reads superseded_by_later_record — it exists purely to flow into the
# exported file for reporting use, so moving it to run last has no
# effect on any earlier step's logic (collapse and corrections do not 
# depend on this flag existing beforehand).

# Design, confirmed 08/27/2026 session:
#   - ELIGIBLE TO BE FLAGGED (i.e. "old/placeholder" rows): any row
#     where status_source != "NSC" — covers STAFF, STAFF_VERIFIED,
#     SELF-REPORTED, INFERRED, OLD_PSD, and MISSING DATA rows alike.
#   - ELIGIBLE TO SUPERSEDE (i.e. "authoritative later record"): ONLY
#     status_source %in% c("NSC", "STAFF_VERIFIED"). Historical plain
#     "STAFF" rows do NOT count as authoritative — that label predates
#     the current tiered status_source system and isn't trusted the same
#     way current STAFF_VERIFIED is.
#   - Rule 1 (specific term, e.g. "Fall"): superseded if an authoritative
#     record exists for the SAME psd_id + record_term + record_year.
#   - Rule 2 (hedge term "enrolled anytime after fall"): superseded if
#     an authoritative record exists for the SAME psd_id + record_year,
#     in ANY of winter/spring/summer — the hedge term itself never
#     appears in real NSC data (NSC always resolves to a specific term
#     via add_psd_variables()'s month()-based logic), so an exact-term
#     match against the hedge string would never fire; the hedge is
#     resolved by ANY more specific term landing in that same year.

hedge_term <- "enrolled anytime after fall"
specific_terms_after_fall <- c("winter", "spring", "summer")
superseding_sources <- c("NSC", "STAFF_VERIFIED")

authoritative_records <- previous_psd_corrected %>%
  filter(status_source %in% superseding_sources) %>%
  mutate(record_term_lower = tolower(record_term)) %>%
  distinct(psd_id, record_term_lower, record_year)

rule1_superseded <- previous_psd_corrected %>%
  filter(!status_source %in% superseding_sources,
         tolower(record_term) != hedge_term) %>%
  mutate(record_term_lower = tolower(record_term)) %>%
  semi_join(authoritative_records, by = c("psd_id", "record_term_lower", "record_year")) %>%
  distinct(psd_id, record_term, record_year)

authoritative_specific_terms <- authoritative_records %>%
  filter(record_term_lower %in% specific_terms_after_fall) %>%
  distinct(psd_id, record_year)

rule2_superseded <- previous_psd_corrected %>%
  filter(!status_source %in% superseding_sources,
         tolower(record_term) == hedge_term) %>%
  semi_join(authoritative_specific_terms, by = c("psd_id", "record_year")) %>%
  distinct(psd_id, record_term, record_year)

superseded_lookup <- bind_rows(rule1_superseded, rule2_superseded) %>%
  mutate(superseded_by_later_record = TRUE)

# ⚠️ The join key alone (psd_id + record_term + record_year) isn't
# enough to safely apply the flag — the SAME key that correctly matches
# a superseded placeholder row ALSO matches the authoritative row that's
# superseding it. The extra !status_source %in% superseding_sources
# condition is required to prevent the authoritative row itself from
# incorrectly receiving the flag too.
previous_psd_corrected <- previous_psd_corrected %>%
  left_join(superseded_lookup, by = c("psd_id", "record_term", "record_year")) %>%
  mutate(superseded_by_later_record = coalesce(superseded_by_later_record, FALSE) &
           !status_source %in% superseding_sources)

n_authoritative_flagged <- previous_psd_corrected %>%
  filter(status_source %in% superseding_sources, superseded_by_later_record) %>%
  nrow()

if (n_authoritative_flagged > 0) {
  stop(n_authoritative_flagged, " authoritative-source row(s) were ",
       "incorrectly flagged as superseded — investigate the join logic ",
       "above before continuing.")
}

n_superseded <- sum(previous_psd_corrected$superseded_by_later_record)
cat(n_superseded, " row(s) flagged as superseded by a later authoritative record.\n", sep = "")

## -----------------------------------------------------------------------------
## Part 10 - Export corrected PSD and severity tracking
## -----------------------------------------------------------------------------

# ⚠️ Exports as its OWN dated snapshot — does NOT overwrite the original
# psd_filename. Keeps a clear record of when this correction was applied
# and what it changed, and leaves the original untouched in case any
# severe case's "safe to collapse" determination needs revisiting.
previous_psd_corrected %>% 
  select(-superseded_by_later_record) %>%
  write.csv(file = file.path(box_file_dir,
                           "College and Career RPP",
                           "4. NSC Dataset",
                           school_site,
                           school_site_psd_folder,
                           corrected_psd_filename),
          row.names = FALSE)

write.csv(severe_review_tracking,
          file = file.path(data_quality_dir, severity_tracking_filename),
          row.names = FALSE)

cat("✅ Export complete: ", nrow(previous_psd_corrected), " rows written to ",
    corrected_psd_filename, "\n", sep = "")

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------