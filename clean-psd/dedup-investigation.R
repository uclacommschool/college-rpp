################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < dedup-investigation.R >
## [ AUTH ] < Ariana Dimagiba >
## [ INIT ] < 08/29/2026 (split from dedup-psd-records.R) >
##
################################################################################

#Goal: Investigate duplicate-record contamination in a PSD snapshot —
#scan, tier by severity, detect identity collisions, and support manual
#review (in R, via review_student_duplication()/review_raw_records()/
#review_identity()). This is the EXPLORATORY half of what was
#previously one script (dedup-psd-records.R) — everything here depends
#on a human looking at output and deciding something. The companion
#file, dedup-apply-corrections.R, is the EXECUTION half — it only
#applies decisions already made and recorded here.
#
#Originally motivated by discovering psd_id 2013PNCQ15 had 448 rows —
#confirmed to be 14 genuinely distinct facts (one coherent 2014-2021
#Pasadena City College enrollment history), each re-inserted repeatedly
#by years of un-deduped merges before 01-merge-nsc-to-psd.R's own dedup
#fixes. A full-file scan found this pattern is not isolated — see
#dedup-investigation-log.md for the full chronological investigation.
#
#⚠️ HAND-OFF TO dedup-apply-corrections.R: this script produces two CSV
#exports (identity_review_tracking, r_review_tracking) that the other
#script reads back as its source of truth for confirmed review outcomes
#— since they run as separate sessions, nothing here is available there
#except through these saved files. Run this script's review to
#completion (or as far as you want it before applying anything) BEFORE
#running dedup-apply-corrections.R.
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

# ⚠️ UPDATE: the PSD file being investigated — must match
# dedup-apply-corrections.R's psd_filename exactly, or the two scripts
# will be reasoning about different data.
psd_filename <- "20260829-rfk-psd-dimagiba.csv"

# ⚠️ UPDATE: severity threshold (in excess rows) below which a psd_id's
# duplication is treated as safe to auto-collapse without individual
# review, and at/above which it requires the manual verification
# process in Part 6. This is a judgment call, not a derived value —
# median excess rows across the first full scan was 1, mean was ~6,
# driven up by a small number of severe outliers (max 434). Adjust
# based on how much manual review capacity you have vs. how much risk
# tolerance for an unreviewed auto-collapse. Must match
# dedup-apply-corrections.R's value, or the safe/severe tiers won't
# agree between the two scripts.
severe_threshold <- 5

## -----------------------------------------------------------------------------
## Data Quality folder — where every review-outcome export in this
## script lands (identity_review_tracking, r_review_tracking).
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
## Part 1 - Load the PSD to check
## -----------------------------------------------------------------------------

previous_psd <- read_csv(file.path(box_file_dir,
                                   "College and Career RPP",
                                   "4. NSC Dataset",
                                   school_site,
                                   school_site_psd_folder,
                                   psd_filename))

## -----------------------------------------------------------------------------
## Part 2 - Full-scope scan: how many students, how severe
## -----------------------------------------------------------------------------

# What "duplicate" means here: same psd_id + record_year + record_term +
# college_code + degree_title + major + he_graduated appearing more than
# once — for non-NSC sources. NSC sources use a much stricter,
# source-conditional rule (see below).
#
# ⚠️ REVISION LOG (08/26/2026 session):
# - `major` ADDED to the key after manual review of severe-tier cases
#   2021ZMDI63 / 2021KBQP24 showed the original key (without major)
#   silently collapsed genuinely distinct concurrent credentials (e.g.
#   two different Certificates earned the same term/college, same
#   degree_title, but different majors) as if they were duplicate facts.
#   Full-file impact of adding `major`: 1179 -> 1159 excess rows (20 rows
#   were false-positive "duplicates"); 196 -> 189 affected psd_ids (7
#   psd_ids — 2019IJAO01, 2013JFXR84, 2015HKBN03, 2018WJNA06, 2019CILR14,
#   2019LARF35, 2019OATH23 — had ZERO real duplication once major was
#   correctly accounted for; they dropped out of dedup_scope entirely).
#
# This is a slightly looser key than 04/05's dup checks (which also
# distinguish on exact dates) — deliberately, since the goal here is
# finding REPEATED facts (the same real event re-inserted by a prior
# un-deduped merge), not distinguishing legitimately close-but-different
# events the way 01's near-duplicate check does.

# ⚠️ REVISION LOG (08/27/2026 session): MISSING DATA rows excluded from
# dedup candidacy entirely (policy exclusion, not a key expansion —
# triggered by 2016CJWA71: two identical Fall + Enrolled-Anytime-After-
# Fall hedge pairs that can't be reliably told apart as "one event
# duplicated" vs. "two genuinely independent follow-up attempts that
# both came back unresolved"). Trigger: EITHER status_source ==
# "MISSING DATA" OR college_name == "MISSING DATA" marks a row as a
# protected placeholder.
#
# ⚠️ coalesce() is required here, not optional — status_source/
# college_name can be genuinely NA for some rows, and `NA == "MISSING
# DATA"` evaluates to NA, not FALSE. Without coalescing to FALSE first,
# `!is_missing_data_row(...)` and `is_missing_data_row(...)` BOTH
# evaluate to NA for those rows (filter() only keeps exact TRUE), so an
# NA-status row would silently vanish from BOTH
# previous_psd_dedup_candidates AND previous_psd_protected.
is_missing_data_row <- function(df) {
  coalesce(df$status_source == "MISSING DATA", FALSE) |
    coalesce(df$college_name == "MISSING DATA", FALSE)
}

previous_psd_dedup_candidates <- previous_psd %>% filter(!is_missing_data_row(previous_psd))
previous_psd_protected <- previous_psd %>% filter(is_missing_data_row(previous_psd))

# Guard: confirm the split is complete before anything downstream uses it.
if (nrow(previous_psd_dedup_candidates) + nrow(previous_psd_protected) != nrow(previous_psd)) {
  stop("previous_psd_dedup_candidates + previous_psd_protected does not ",
       "reconstruct previous_psd exactly — some row(s) fell through a ",
       "gap in is_missing_data_row(). Investigate before continuing.")
}

n_before <- nrow(previous_psd)

# ⚠️ REVISION LOG (08/27/2026 session): dedup key is now
# SOURCE-CONDITIONAL, not universal — the biggest structural change to
# this scan since the file's creation. Full reasoning (see
# dedup-investigation-log.md, 2026-08-27 entry, for the full
# investigation):
#
# NSC records naturally arrive as multiple, legitimately different
# reports of the same real event (progressive graduation detail —
# conferral, then degree title once conferred; and the same pattern
# confirmed on enrollment too, e.g. 2012DSCQ22's two UC Riverside rows,
# enrollment_begin 3 days apart, same NSC report reissued with a revised
# date). No curated field subset reliably distinguishes "genuinely new
# NSC fact" from "NSC re-reported the same fact" — so for NSC rows, a
# duplicate is defined as: EVERY NSC-populated field matches exactly
# (nsc_fact_fields below). Anything less than a full match is treated
# as a distinct fact, not a duplicate.
#
# Every OTHER source (STAFF, STAFF_VERIFIED, SELF-REPORTED, INFERRED,
# OLD_PSD) behaves differently in kind: one staff follow-up entry is
# meant to be one authoritative statement per term, not multiple
# legitimate versions the way NSC data is. For these, psd_id +
# record_term + record_year alone is sufficient to call it a duplicate.
#
# Result: dedup_scope shrank from 169 -> 106 psd_ids. 2012DSCQ22
# (the case that originally motivated this redesign) confirmed correctly
# dropped out of scope entirely under the new rule.
nsc_fact_fields <- c("record_found", "req_return_field", "college_code",
                     "college_name", "college_state", "cc_4year",
                     "public_private", "enrollment_begin", "enrollment_end",
                     "enrollment_status", "he_graduated", "coll_grad_date",
                     "degree_title", "major", "college_sequence", "program_code")

previous_psd_nsc <- previous_psd_dedup_candidates %>%
  filter(coalesce(status_source == "NSC", FALSE))
previous_psd_other <- previous_psd_dedup_candidates %>%
  filter(coalesce(status_source != "NSC", TRUE))

# Guard: confirm the NSC/other split is complete too — status_source can
# genuinely be NA for some candidate rows (not the string "MISSING
# DATA", which is_missing_data_row() already excludes — a real NA/blank
# value). An NA-source row defaults to FALSE for "is this NSC" and TRUE
# for "is this not-NSC", landing it in the more conservative
# previous_psd_other bucket rather than silently vanishing.
if (nrow(previous_psd_nsc) + nrow(previous_psd_other) != nrow(previous_psd_dedup_candidates)) {
  stop("previous_psd_nsc + previous_psd_other does not reconstruct ",
       "previous_psd_dedup_candidates exactly — investigate before continuing.")
}

n_after_dedup <- previous_psd_nsc %>%
  distinct(across(all_of(nsc_fact_fields)), .keep_all = TRUE) %>%
  nrow() +
  previous_psd_other %>%
  distinct(psd_id, record_term, record_year, .keep_all = TRUE) %>%
  nrow() +
  nrow(previous_psd_protected)

n_before
n_after_dedup
n_before - n_after_dedup  # total excess/redundant rows across the whole file

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
  arrange(desc(n_excess_rows))

nrow(dedup_scope)  # 106 as of 08/27/2026, after the source-conditional key fix (down from 169)

dedup_scope %>% print(n = 30, width = Inf)

summary(dedup_scope$n_excess_rows)

## -----------------------------------------------------------------------------
## Part 3 - Field-impact scan: systematically check ALL remaining fields
## -----------------------------------------------------------------------------
# Run once (08/26/2026) after discovering major, enrollment_begin, and
# enrollment_end all masked real distinctions under the original key —
# rather than keep finding gaps one field at a time, this checks every
# remaining column against the current best key to see which still
# reveal additional distinct rows. Purely investigative — its findings
# are already baked into the current key design above; nothing
# downstream depends on re-running this every time.
#
# RESULT: gender/first_name/middle_name/last_name/race_ethnicity/
# poverty_indicator all add rows, but are IDENTITY fields — a single
# real student cannot legitimately have two different values for these
# (unlike major/enrollment dates, which genuinely vary across a real
# history). Variation within one psd_id is evidence of an identity
# collision, not evidence the dedup key needs these fields — this
# triggered Part 4's identity-collision scan.

current_key <- c("psd_id", "record_year", "record_term", "college_code",
                 "degree_title", "major", "he_graduated")

current_n <- previous_psd %>% distinct(across(all_of(current_key))) %>% nrow()

candidate_fields <- setdiff(names(previous_psd), current_key)

field_impact <- purrr::map_dfr(candidate_fields, function(f) {
  n <- previous_psd %>%
    distinct(across(all_of(c(current_key, f)))) %>%
    nrow()
  tibble(field = f, adds_rows = n - current_n)
})

field_impact %>% arrange(desc(adds_rows)) %>% print(n = Inf)

## -----------------------------------------------------------------------------
## Part 4 - Identity-collision scan (triggered by Part 3 finding)
## -----------------------------------------------------------------------------
# Finds every psd_id where an identity field (gender, first_name,
# middle_name, last_name, race_ethnicity, poverty_indicator, hs_diploma)
# takes more than one distinct value — a signature of two different
# people's records merged under one psd_id, same root-cause pattern as
# the known 2013PNCQ15/2013PSXY40 student_id collision (120493M043).
#
# RESULT (08/26/2026): 102 psd_ids flagged against that day's
# previous_psd. ⚠️ This count is NOT a fixed expected value — it's
# recomputed fresh from previous_psd every run, and WILL legitimately
# change whenever previous_psd itself changes (e.g. after re-running
# 04/05 to apply a real data fix). Confirmed 08/30/2026: after
# resolving a 2021-cohort follow-up-list issue and re-running 04/05,
# this count correctly dropped to 79 — not a bug or lost data, just
# genuine inconsistencies that no longer exist in the corrected data.
# Don't treat whatever number a past comment cites as the "right"
# count to expect going forward — always trust a fresh run over a
# dated comment.

identity_fields <- c("gender", "first_name", "middle_name", "last_name",
                     "race_ethnicity", "poverty_indicator", "hs_diploma")

identity_inconsistency <- previous_psd %>%
  group_by(psd_id) %>%
  summarize(
    n_genders = n_distinct(gender, na.rm = TRUE),
    n_first_names = n_distinct(first_name, na.rm = TRUE),
    n_last_names = n_distinct(last_name, na.rm = TRUE),
    n_race = n_distinct(race_ethnicity, na.rm = TRUE),
    n_poverty = n_distinct(poverty_indicator, na.rm = TRUE),
    n_diploma = n_distinct(hs_diploma, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_genders > 1 | n_first_names > 1 | n_last_names > 1 |
           n_race > 1 | n_poverty > 1 | n_diploma > 1)

nrow(identity_inconsistency)  # recompute fresh — do not assume this matches any past run
identity_inconsistency %>% print(n = Inf, width = Inf)

#' Show one student's per-row identity fields alongside enrollment
#' content, for manual review of whether an identity-field
#' inconsistency reflects (a) a single mis-keyed/typo'd value on one
#' row, or (b) two different people's records merged under one psd_id.
#'
#' @param df The full PSD data frame (previous_psd)
#' @param target_psd_id The psd_id to review
review_identity <- function(df, target_psd_id) {
  df %>%
    filter(psd_id == target_psd_id) %>%
    select(record_year, record_term, first_name, last_name, gender,
           race_ethnicity, college_name, degree_title, he_graduated) %>%
    arrange(record_year, record_term)
}

# Tracking dataframe for identity-collision review outcomes. Same
# pattern as severe_review_tracking (Part 6): reviewed=FALSE / outcome=NA
# until manually checked; nothing gets acted on until outcome is
# explicitly set.
#
# TIERING (assigned 08/26/2026, before individual review):
#   "identity_high"   — gender or race_ethnicity varies (near-immutable
#                       fields; closest analog to the known PNCQ15/
#                       PSXY40 collision pattern) — review FIRST
#   "identity_medium" — BOTH first_name AND last_name vary together
#                       (a single legitimate name change usually touches
#                       only one of these, not both at once — cluster is
#                       mostly 2021 cohort, ~25 psd_ids)
#   "identity_low"    — only ONE of first_name/last_name/hs_diploma
#                       varies — plausibly NSC formatting quirks
#                       (maiden name, hyphenation) or legitimate status
#                       updates, not necessarily a collision
identity_review_tracking <- identity_inconsistency %>%
  mutate(
    tier = case_when(
      n_genders > 1 | n_race > 1 ~ "identity_high",
      n_first_names > 1 & n_last_names > 1 ~ "identity_medium",
      TRUE ~ "identity_low"
    ),
    reviewed = FALSE,
    outcome = NA_character_  # "single data entry error - not a collision" /
    # "contamination suspected - needs separate handling" /
    # "coherent - confirmed not a collision"
  )

identity_review_tracking %>% count(tier)

# --- Reviews completed so far (08/26/2026 session) ---

# 2013PNCQ15 / 2013PSXY40 — already known collision, predates this
# session's identity scan (surfaced via the student_id 120493M043
# investigation). PSXY40 confirmed via review_student_duplication():
# name and demographics are distinct between the two students; the
# CONTENT fields (enrollment records) are what bled together. PNCQ15's
# own enrollment history is internally coherent on its own.

# 2017WPRG16 (identity_high, race_ethnicity: n=2) — REVIEWED.
# review_identity(previous_psd, "2017WPRG16"): 11 of 12 rows = "PI",
# exactly 1 row = "HI". Enrollment content is a single coherent
# trajectory throughout — no contradictory institution, no overlapping
# terms, no sign of two interleaved histories. Tentative call: single
# mis-keyed value on one row, NOT a collision. Needs a source correction
# (HI -> PI) but should not block dedup collapse.

# 2018HPBN60 (identity_high, gender: n=2) — REVIEWED (08/26/2026).
# review_identity(previous_psd, "2018HPBN60"): 23 of 24 rows = F, exactly
# 1 row (Spring 2022) = M. Critically, Spring 2022 sits in the middle of
# the UC Merced era (2018-2023) — NOT at or near the UC Merced ->
# Colorado Technical institution transition (2023). No correlation
# between the demographic split and the institution split. Single
# mis-keyed value, NOT a collision.

# 2014ZIII79 (identity_high, gender: n=2) — REVIEWED (08/26/2026).
# review_identity(previous_psd, "2014ZIII79"): 2 rows = M (Fall 2021 and
# "enrolled anytime after fall" 2022), surrounding rows = F. Fall [year]
# + "enrolled anytime after fall" [year+1] is the EXACT paired-row
# pattern this pipeline generates mechanically (02's
# generate_missing_list() and 04's Template R "hedge" row_generation
# both clone one source row into a fall row + a fall_year+1 row at the
# same time). All other fields identical between the two M rows —
# consistent with both rows sharing one origin, i.e. one mis-keyed value
# propagated by the pipeline's own cloning logic, not two independent
# errors. Single mis-keyed value, NOT a collision.

identity_review_tracking <- identity_review_tracking %>%
  mutate(
    reviewed = case_when(
      psd_id %in% c("2017WPRG16", "2018HPBN60", "2014ZIII79") ~ TRUE,
      TRUE ~ reviewed
    ),
    outcome = case_when(
      psd_id == "2017WPRG16" ~ "single data entry error - not a collision (tentative, fix not yet applied)",
      psd_id == "2018HPBN60" ~ "single data entry error - not a collision (no correlation with institution transition; fix not yet applied)",
      psd_id == "2014ZIII79" ~ "single data entry error - not a collision (propagated via pipeline's fall/hedge-pair cloning; fix not yet applied)",
      TRUE ~ outcome
    )
  )

## -----------------------------------------------------------------------------
## Part 5 - Split into safe (auto-collapse) vs. severe (manual review)
## -----------------------------------------------------------------------------

dedup_scope <- dedup_scope %>%
  mutate(severity = if_else(n_excess_rows >= severe_threshold, "severe", "safe"))

dedup_scope %>% count(severity)

safe_ids <- dedup_scope %>% filter(severity == "safe") %>% pull(psd_id)
severe_ids <- dedup_scope %>% filter(severity == "severe") %>% pull(psd_id)

## -----------------------------------------------------------------------------
## Part 6 - Manual verification helper for severe cases
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
#' SOURCE-CONDITIONAL, matching Part 2's dedup_scope logic exactly — NOT
#' one universal distinct() key. NSC rows are only collapsed if EVERY
#' NSC-populated field matches (nsc_fact_fields, Part 2); every other
#' source collapses on just record_term + record_year. A single student
#' can have both NSC and non-NSC rows mixed together, so each source
#' group is deduplicated separately here, then recombined for display —
#' status_source is included in the output specifically so the reviewer
#' can see which rule applied to which row.
#'
#' NOTE: this shows the POST-collapse picture ("if this student were
#' cleaned up, here's what would remain") — useful for judging overall
#' coherence, but the WRONG tool for checking whether a SPECIFIC flagged
#' duplication is real, since its own distinct() calls hide the exact
#' rows a reviewer needs to see for that question. Use
#' review_raw_records() instead when reviewing dedup_scope entries
#' directly.
#'
#' @param df The full PSD data frame (previous_psd)
#' @param target_psd_id The psd_id to review
#' @return A tibble of that student's distinct facts, for visual review —
#'   not auto-classified; a human still needs to look at the output
review_student_duplication <- function(df, target_psd_id) {
  student_rows <- df %>% filter(psd_id == target_psd_id)
  
  nsc_distinct <- student_rows %>%
    filter(coalesce(status_source == "NSC", FALSE)) %>%
    distinct(across(all_of(nsc_fact_fields)), .keep_all = TRUE)
  
  other_distinct <- student_rows %>%
    filter(coalesce(status_source != "NSC", TRUE)) %>%
    distinct(record_term, record_year, .keep_all = TRUE)
  
  bind_rows(nsc_distinct, other_distinct) %>%
    select(first_name, last_name, record_year, record_term, college_name,
           degree_title, major, enrollment_begin, enrollment_end,
           he_graduated, status_source) %>%
    arrange(record_year, record_term, enrollment_begin)
}

#' Show one student's RAW, undeduplicated rows — every row, no collapsing
#'
#' Companion to review_student_duplication(), for a DIFFERENT purpose.
#' review_student_duplication() shows the POST-collapse picture; this
#' shows EVERY row exactly as it exists in previous_psd, with no
#' internal collapsing at all. Use THIS function when reviewing
#' dedup_scope entries directly — confirmed necessary via 2014NDJU56
#' (08/27/2026 session), where review_student_duplication() would have
#' hidden the 8 genuine OLD_PSD duplicate pairs that actually drove that
#' psd_id's n_excess_rows count.
#'
#' @param df The full PSD data frame (previous_psd)
#' @param target_psd_id The psd_id to review
#' @return EVERY row for that student, completely raw — a human needs to
#'   spot the actual duplicate pairs themselves, nothing pre-collapsed
review_raw_records <- function(df, target_psd_id) {
  df %>%
    filter(psd_id == target_psd_id) %>%
    select(first_name, last_name, record_year, record_term, college_name,
           degree_title, major, enrollment_begin, enrollment_end,
           he_graduated, status_source) %>%
    arrange(record_year, record_term, enrollment_begin)
}

# Example usage:
# review_raw_records(previous_psd, "2014NDJU56") %>% print(n = Inf, width = Inf)

# ⚠️ Priority case: 2013PSXY40 — the OTHER student in the known
# student_id collision (120493M043) with 2013PNCQ15. Given the shared
# root cause, this one should be reviewed FIRST, not assumed clean.
if ("2013PSXY40" %in% severe_ids) {
  cat("⚠️ 2013PSXY40 flagged as severe — review this one first, given its\n",
      "connection to the known 120493M043 student_id collision.\n", sep = "")
  review_student_duplication(previous_psd, "2013PSXY40") %>% print(n = Inf, width = Inf)
}

# Tracking columns for manual review outcomes — fill in as each severe
# case gets reviewed via review_student_duplication() above. Fully
# self-contained (unlike r_review_tracking below) — the 5 outcomes are
# hardcoded directly here, so dedup-apply-corrections.R can rebuild this
# object identically without needing to read any external file.
severe_review_tracking <- dedup_scope %>%
  filter(severity == "severe") %>%
  mutate(
    reviewed = FALSE,      # set TRUE once review_student_duplication() has been checked
    outcome = NA_character_ # "coherent - safe to collapse" / "contamination suspected - needs separate handling"
  )

## --- All 5 severe cases reviewed and recorded (08/26/2026 session) ---
#
# 2013PNCQ15 (434 excess) — review_student_duplication() shows one
# coherent 2014-2021 Pasadena City College enrollment history, no
# contradictions. Coherent, safe to collapse.
#
# 2013PSXY40 (434 excess) — structurally IDENTICAL pattern to PNCQ15 in
# review_student_duplication() output (same 14 facts, same institution,
# same excess-row count) despite being a DIFFERENT real student — name
# and demographics confirmed distinct from PNCQ15 via legacy file cross-
# check. This match is not reassuring; it's the signature of the known
# student_id 120493M043 collision: PNCQ15's enrollment CONTENT appears
# to have bled into PSXY40's psd_id. Contamination suspected — held OUT
# of collapse entirely, needs separate manual resolution (see
# PENDING-tasks.md), NOT fixable by distinct().
#
# 2014NDJU56 (10 excess) — reviewed, coherent, safe to collapse.
#
# 2021ZMDI63 (6 excess under the major-corrected key) — raw-row check
# confirmed remaining excess rows are genuine repeated facts. Coherent,
# safe to collapse.
#
# 2021KBQP24 (5 excess under the major-corrected key) — same pattern and
# resolution as ZMDI63. Coherent, safe to collapse.

severe_review_tracking <- severe_review_tracking %>%
  mutate(
    reviewed = case_when(
      psd_id %in% c("2013PNCQ15", "2013PSXY40", "2014NDJU56",
                    "2021ZMDI63", "2021KBQP24") ~ TRUE,
      TRUE ~ reviewed
    ),
    outcome = case_when(
      psd_id == "2013PNCQ15" ~ "coherent - safe to collapse",
      psd_id == "2013PSXY40" ~ "contamination suspected - needs separate handling",
      psd_id == "2014NDJU56" ~ "coherent - safe to collapse",
      psd_id == "2021ZMDI63" ~ "coherent - safe to collapse (confirmed via raw-row check post major-fix)",
      psd_id == "2021KBQP24" ~ "coherent - safe to collapse (confirmed via raw-row check post major-fix)",
      TRUE ~ outcome
    )
  )

severe_review_tracking %>% print(n = Inf, width = Inf)

## -----------------------------------------------------------------------------
## Part 7 - R-native identity review (replaces Excel sync, 08/29/2026)
## -----------------------------------------------------------------------------

# RETIRED 08/29/2026: the original top-10 safe-tier spot-check and the
# Excel-based review sync. r_review_tracking (R-native review via
# review_raw_records(), same pattern as this Part) proved more thorough
# for general duplicate review than either — full coverage of all
# dedup_scope, not a sample. Identity review moves to the same pattern
# here. identity_review_tracking (Part 4) already exists as a proper
# R-native tracking tibble — only the REVIEW MECHANISM changes (Excel ->
# review_raw_records() loop), not the underlying tracking structure or
# the 3 identity_high resolutions already recorded there.
#
# Same loop pattern as r_review_tracking — work through remaining
# unreviewed rows one at a time:
#   1. ids_to_review_identity <- identity_review_tracking %>% filter(!reviewed) %>% pull(psd_id)
#   2. i <- 1; review_raw_records(previous_psd, ids_to_review_identity[i]) %>% View()
#   3. Record: identity_review_tracking$outcome[identity_review_tracking$psd_id == ids_to_review_identity[i]] <- "..."
#      identity_review_tracking$reviewed[identity_review_tracking$psd_id == ids_to_review_identity[i]] <- TRUE
#   4. i <- i + 1, repeat
#   5. Save progress as you go: saveRDS(identity_review_tracking, "identity_review_tracking_progress.rds")
#      Resume with: identity_review_tracking <- readRDS("identity_review_tracking_progress.rds")
#
# Once complete, export to Box as CSV (not .xlsx) — this is the file
# dedup-apply-corrections.R's Part 10 reads to apply confirmed identity
# corrections. Same Level 3 PII handling as everywhere else in this
# pipeline — contains names, stays in Box's Data Quality subfolder.

ids_to_review_identity <- identity_review_tracking %>%
  filter(!reviewed) %>%
  pull(psd_id)

length(ids_to_review_identity)

# ⚠️ UPDATE: filename per run.
write.csv(identity_review_tracking_progress,
          file = file.path(data_quality_dir,
                           "20260902-rfk-identity-review-tracking-dimagiba.csv"),
          row.names = FALSE)

cat(sum(identity_review_tracking$reviewed), " of ", nrow(identity_review_tracking),
    " identity-flagged psd_id(s) reviewed.\n", sep = "")

## -----------------------------------------------------------------------------
## Part 7.5 - R-native general duplicate review (r_review_tracking)
## -----------------------------------------------------------------------------

# Covers all dedup_scope psd_ids (both safe and severe tiers, whatever
# that count is on the current previous_psd — see Part 2's note on not
# trusting a dated comment's count) — more
# thorough than any sampling approach, since it's a full pass, not a
# spot-check. Deliberately kept as a loop you run interactively in the
# CONSOLE, not baked into this persistent script — the review itself is
# a one-off, session-specific activity, and this file should stay
# runnable top-to-bottom without needing to replay every historical
# review click. Only the FINAL export step below belongs in the
# persistent script.
#
# Loop pattern (run interactively, not by sourcing this file):
#   1. ids_to_review <- dedup_scope %>% pull(psd_id)
#      r_review_tracking <- tibble(psd_id = ids_to_review, reviewed = FALSE, outcome = NA_character_)
#   2. i <- 1; review_raw_records(previous_psd, ids_to_review[i]) %>% View()
#   3. Record: r_review_tracking$outcome[i] <- "..."; r_review_tracking$reviewed[i] <- TRUE
#   4. i <- i + 1, repeat
#   5. Save progress as you go: saveRDS(r_review_tracking, "r_review_tracking_progress.rds")
#      Resume with: r_review_tracking <- readRDS("r_review_tracking_progress.rds")
#
# Once your review is complete (or as far as you want it before running
# dedup-apply-corrections.R), export it here:
#
# write.csv(r_review_tracking,
#           file = file.path(data_quality_dir,
#                            "20260902-rfk-r-review-tracking-dimagiba.csv"),
#           row.names = FALSE)
#
# dedup-apply-corrections.R reads this file back as the source of truth
# for which safe-tier psd_ids are confirmed "coherent - safe to
# collapse", and which severe-tier findings (like the 3
# post-graduation-stale-placeholder cases, Part 11.5 there) are ready
# for targeted action.

## -----------------------------------------------------------------------------
## Part 8 - Contaimination Review
## -----------------------------------------------------------------------------

#' Show one student's raw records PLUS a student_id cross-check, for
#' reviewing "contamination suspected" cases specifically.
#'
#' Wraps review_raw_records() (same raw-row display, no collapsing) and
#' adds the check that surfaced the 2013PSXY40 / 2013PNCQ15 collision
#' (shared student_id 120493M043): whether THIS psd_id's student_id is
#' shared with any OTHER psd_id in the PSD. Contamination often traces
#' back to exactly this — two students merged under one NSC-submitted
#' student_id upstream — so it's worth checking every time a
#' contamination case is reviewed, not just when a collision happens to
#' already be suspected.
#'
#' @param df The full PSD data frame (previous_psd)
#' @param target_psd_id The psd_id to review
#' @return A list: $raw_records (identical output to review_raw_records())
#'   and $shared_student_id_with (any OTHER psd_id sharing this
#'   student's student_id — empty tibble if none found)
review_contamination_case <- function(df, target_psd_id) {
  raw <- review_raw_records(df, target_psd_id)
  
  target_student_ids <- df %>%
    filter(psd_id == target_psd_id) %>%
    pull(student_id) %>%
    unique()
  
  shared_with <- df %>%
    filter(student_id %in% target_student_ids, psd_id != target_psd_id) %>%
    distinct(psd_id, student_id)
  
  list(raw_records = raw, shared_student_id_with = shared_with)
}

# Example usage:
# result <- review_contamination_case(previous_psd, "2014UZIS99")
# result$raw_records %>% print(n = Inf, width = Inf)
# result$shared_student_id_with   # non-empty = same root cause as 2013PSXY40/2013PNCQ15

# ── Contamination review loop (contamination_corrections_tracking.csv) ──
# Same interactive-console pattern as Part 7 / Part 7.5 below — run this
# loop by hand, not by sourcing the file. Covers all psd_ids currently
# flagged "contamination suspected - needs separate handling" across
# identity_review_tracking / r_review_tracking / severe_review_tracking.
#
#   1. contamination_corrections_tracking <- read_csv(file.path(data_quality_dir,
#        "contamination_corrections_tracking.csv"))
#      ids_to_review_contamination <- contamination_corrections_tracking %>%
#        filter(!reviewed) %>% pull(psd_id) %>% unique()
#   2. i <- 1; review_contamination_case(previous_psd, ids_to_review_contamination[i])
#      - inspect $raw_records for which row(s)/field(s) are actually wrong
#      - inspect $shared_student_id_with — non-empty suggests the same
#        root cause as 2013PSXY40 (full_rebuild), not a targeted fix
#   3. Record one or more rows in contamination_corrections_tracking for
#      this psd_id (action = "full_rebuild" / "remove_row" /
#      "correct_field", plus record_term/record_year/field/corrected_value
#      as needed), set reviewed = TRUE
#   4. i <- i + 1, repeat
#   5. Save progress as you go: write_csv(contamination_corrections_tracking,
#        file.path(data_quality_dir, "contamination_corrections_tracking.csv"))

# 1. Load the tracking file and get the list of unreviewed ids
contamination_corrections_tracking <- read_csv(file.path(data_quality_dir,
                                                         "contamination_corrections_tracking.csv"))
## -----------------------------------------------------------------------------
## Contamination review loop — run interactively, one id at a time
## -----------------------------------------------------------------------------
ids_to_review_contamination <- contamination_corrections_tracking %>%
  filter(!reviewed) %>%
  pull(psd_id) %>%
  unique()

length(ids_to_review_contamination)

# 2. Set i, then review one id
i <- 12

result <- review_contamination_case(previous_psd, ids_to_review_contamination[i])

anonymize_for_sharing(previous_psd, "2021RTBJ20") %>% print(n = Inf, width = Inf)

match_target_anonymized <- function(psd_id_to_check, year_range) {
  previous_psd %>%
    filter(psd_id == psd_id_to_check, record_year %in% year_range) %>%
    select(record_term, record_year, first_name, middle_name, last_name, hs_grad_year) %>%
    left_join(
      master_stu_list %>% select(first_name, middle_name, last_name, hs_grad_year, psd_id),
      by = c("first_name", "middle_name", "last_name", "hs_grad_year")
    ) %>%
    select(record_term, record_year, matched_psd_id = psd_id)
}

match_target_anonymized("2021RTBJ20", c(2023, 2024))

result$raw_records %>% print(n = Inf, width = Inf)
result$raw_records %>% view()
result$shared_student_id_with

previous_psd %>%
  filter(psd_id == "2021RTBJ20", toupper(record_term) == "FALL", record_year == 2023) %>%
  select(first_name, middle_name, last_name, hs_grad_year) %>%
  left_join(
    master_stu_list %>% select(first_name, middle_name, last_name, hs_grad_year, psd_id),
    by = c("first_name", "middle_name", "last_name", "hs_grad_year")
  ) %>%
  pull(psd_id)

# 3. Record what you found — edit the matching row(s) in
#    contamination_corrections_tracking for this psd_id.
#    Example for a single field correction:

contamination_corrections_tracking <- contamination_corrections_tracking %>%
  bind_rows(
    tibble(
      psd_id          = "2024TUSF81",
      action          = "reassign_psd_id",
      record_term     = "PLANS",
      record_year     = 2024,
      field           = NA_character_,
      corrected_value = NA_character_,
      target_psd_id   = "2024TUSF81",
      reviewed        = TRUE,
      notes = paste("Confirmed 2026-09-03: incorrect identity",
                    "resolved via name match against master_stu_list", 
                    "same pattern as 2016YPAE66, 2018FOSG59,",
                    "and 2018TYLN48.")
    )
  )

# Save progress after each id (or every few), so you don't lose review
# work if the session ends
write_csv(contamination_corrections_tracking,
          file.path(data_quality_dir, "contamination_corrections_tracking.csv"))

contamination_corrections_tracking <- read_csv(file.path(data_quality_dir,
                                                         "contamination_corrections_tracking.csv"))

print(contamination_corrections_tracking, n = Inf, width = Inf)
nrow(contamination_corrections_tracking)
# 4. Move to the next id and repeat steps 2-3
i <- i + 1

result <- review_contamination_case(previous_psd, ids_to_review_contamination[i])
result$raw_records %>% print(n = Inf, width = Inf)
result$raw_records %>% view()
result$shared_student_id_with

# dedup-apply-corrections.R's Part 12 reads this file back as the source
# of truth for how to handle every contaminated psd_id — any id left
# unreviewed here will cause Part 12's guard to stop() rather than
# silently deleting that student's records with no replacement.

## -----------------------------------------------------------------------------
## Part 9 - Investigation Log — see dedup-investigation-log.md
## ----------------------------------------------------------------------------
# The full chronological investigation log lives in its own file:
# dedup-investigation-log.md. Same Q/Check/Finding/Decision structure,
# same append-at-the-bottom convention. This is an audit trail, not
# executable code — refer to that file directly when reviewing
# investigation history or adding a new entry.

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
