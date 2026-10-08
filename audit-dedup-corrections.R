################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < audit-dedup-corrections.R >
## [ INIT ] < 09/24/2026 >
##
################################################################################

# Goal: Independently double-check what dedup-apply-corrections.R actually
# did, by comparing its INPUT PSD, its OUTPUT PSD, and the review CSVs it
# read. Read-only - nothing here modifies the PSD. Every flagged row goes
# to one audit workbook in the Data Quality folder.
#
# Everything is read as text, so the comparison can't be thrown off by
# read_csv() guessing different column types for the two files.
#
# Checks:
#   1. Row counts per psd_id    - each change explained by a correction?
#   2. Untouched students       - rows for students with NO correction
#                                  should be byte-for-byte the same
#   3. Facts dropped            - collapse should only drop repeats
#   4. Duplicates remaining     - including "reviewed safe" ids that
#                                  never got collapsed
#   5. Identity + NBSP fixes    - did Part 6's corrections reach the export?
#   6. Contamination fixes      - 2013PSXY40 rebuild, 2014UZIS99/RPEE11
#                                  split, each reassign_psd_id row
#   7. Placeholder rows         - MISSING DATA rows preserved
#   8. Targeted INFERRED removal
#   9. Superseded flag + column changes

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(openxlsx)

## ---------------------------
## ⚠️ CONFIG - must match what dedup-apply-corrections.R used
## ---------------------------

if (.Platform$OS.type == "windows") {
  box_file_dir <- file.path(Sys.getenv("USERPROFILE"), "Box")
} else {
  box_file_dir <- file.path(Sys.getenv("HOME"), "Library", "CloudStorage", "Box-Box")
}

school_site <- "RFK"
school_site_psd_folder <- "RFK PSD"

# ⚠️ The dedup scripts point at "4. NSC Dataset"; every other pipeline
# script uses "1. NSC Dataset". Set this to wherever the files really are.
nsc_root <- file.path(box_file_dir, "College and Career RPP", "4. NSC Dataset")

site_dir <- file.path(nsc_root, school_site, school_site_psd_folder)
dq_dir   <- file.path(site_dir, "Data Quality")

before_file   <- file.path(site_dir, "20260829-rfk-psd-dimagiba.csv")        # input
after_file    <- file.path(site_dir, "20260904-rfk-psd-dimagiba-dedup.csv")  # output
master_file   <- file.path(site_dir, "Master Student List",
                           "master-student-list-rfk-2012-2025.csv")
identity_csv  <- file.path(dq_dir, "20260902-rfk-identity-review-tracking-dimagiba.csv")
r_review_csv  <- file.path(dq_dir, "20260902-rfk-r-review-tracking-dimagiba.csv")
contam_csv    <- file.path(dq_dir, "contamination_corrections_tracking.csv")

severe_threshold <- 5   # must match both dedup scripts

audit_out <- file.path(dq_dir, "20260924-rfk-dedup-audit.xlsx")

## ---------------------------
## load (all as text)
## ---------------------------

read_chr <- function(p) read_csv(p, col_types = cols(.default = "c"))

before    <- read_chr(before_file)
after     <- read_chr(after_file)
master    <- read_chr(master_file)
identity_rt <- read_chr(identity_csv) %>% mutate(reviewed = as.logical(reviewed))
r_rt        <- read_chr(r_review_csv) %>% mutate(reviewed = as.logical(reviewed))
contam      <- read_chr(contam_csv)   %>% mutate(reviewed = as.logical(reviewed))

flags <- list()

## ---------------------------
## rebuild the id sets the apply script acted on (same logic, same order)
## ---------------------------

nsc_fact_fields <- c("record_found", "req_return_field", "college_code",
                     "college_name", "college_state", "cc_4year",
                     "public_private", "enrollment_begin", "enrollment_end",
                     "enrollment_status", "he_graduated", "coll_grad_date",
                     "degree_title", "major", "college_sequence", "program_code")

identity_cols <- c("first_name", "middle_name", "last_name", "gender",
                   "race_ethnicity", "poverty_indicator", "hs_diploma")

is_placeholder <- function(df) {
  coalesce(df$status_source == "MISSING DATA", FALSE) |
    coalesce(df$college_name == "MISSING DATA", FALSE)
}

scope_of <- function(df) {
  cand <- df %>% filter(!is_placeholder(df))
  nsc  <- cand %>% filter(coalesce(status_source == "NSC", FALSE))
  oth  <- cand %>% filter(coalesce(status_source != "NSC", TRUE))
  bind_rows(
    nsc %>% count(across(all_of(c("psd_id", nsc_fact_fields))), name = "n"),
    oth %>% count(psd_id, record_term, record_year, name = "n")
  ) %>%
    filter(n > 1) %>%
    group_by(psd_id) %>%
    summarise(excess = sum(n - 1), .groups = "drop")
}

scope_before <- scope_of(before) %>%
  mutate(severity = if_else(excess >= severe_threshold, "severe", "safe"))
safe_ids   <- scope_before %>% filter(severity == "safe")   %>% pull(psd_id)
severe_ids <- scope_before %>% filter(severity == "severe") %>% pull(psd_id)

# apply script: severe ids cleared ONLY on an exact outcome match
cleared_severe <- c("2013PNCQ15", "2014NDJU56")
cleared_safe <- r_rt %>%
  filter(psd_id %in% safe_ids, str_detect(outcome, "^coherent - safe to collapse")) %>%
  pull(psd_id)
collapse_ids <- unique(c(cleared_safe, cleared_severe))

identity_fix_ids <- identity_rt %>%
  filter(reviewed, str_detect(outcome, "^single data entry error - not a collision")) %>%
  pull(psd_id)

contam_rev     <- contam %>% filter(reviewed)
full_rebuild   <- contam_rev %>% filter(action == "full_rebuild")      %>% pull(psd_id)
reassign_rows  <- contam_rev %>% filter(action == "reassign_psd_id")
split_ids      <- contam_rev %>% filter(action == "split_by_identity") %>% pull(psd_id)
targeted_ids   <- r_rt %>% filter(str_detect(outcome, "missin")) %>% pull(psd_id)

# every psd_id a contamination step could have added rows to or taken from
contam_touched <- unique(c(full_rebuild, "2013PSXY40", contam$psd_id,
                           contam$target_psd_id, split_ids,
                           "2014UZIS99", "2014RPEE11")) %>% na.omit()

# split_by_identity can send rows to ids not named in the CSV - add any
# master_stu_list id whose name matches a row under a split id
if (length(split_ids) > 0) {
  norm <- function(x) str_to_upper(str_squish(x))
  split_targets <- before %>%
    filter(psd_id %in% split_ids) %>%
    transmute(k = str_c(norm(first_name), norm(middle_name), norm(last_name),
                        hs_grad_year, sep = "|")) %>%
    distinct() %>%
    inner_join(master %>% transmute(k = str_c(norm(first_name), norm(middle_name),
                                              norm(last_name), hs_grad_year, sep = "|"),
                                    psd_id), by = "k") %>%
    pull(psd_id)
  contam_touched <- unique(c(contam_touched, split_targets))
}

id_sets <- tibble(
  set = c("collapse (safe)", "collapse (severe)", "identity fix",
          "full_rebuild", "reassign_psd_id rows", "split_by_identity",
          "targeted INFERRED removal", "contamination-touched (any)"),
  n = c(length(cleared_safe), length(cleared_severe), length(identity_fix_ids),
        length(full_rebuild), nrow(reassign_rows), length(split_ids),
        length(targeted_ids), length(contam_touched))
)
print(id_sets)

## -----------------------------------------------------------------------------
## 1. Row counts per psd_id
## -----------------------------------------------------------------------------

per_id <- full_join(
  before %>% count(psd_id, name = "n_before"),
  after  %>% count(psd_id, name = "n_after"),
  by = "psd_id"
) %>%
  mutate(across(c(n_before, n_after), ~ replace_na(.x, 0L))) %>%
  left_join(scope_before %>% select(psd_id, excess, severity), by = "psd_id") %>%
  mutate(
    collapsed = psd_id %in% collapse_ids,
    targeted  = psd_id %in% targeted_ids,
    contam    = psd_id %in% contam_touched,
    expected_after = n_before -
      if_else(collapsed, coalesce(excess, 0), 0) -
      if_else(targeted, 2, 0),
    diff = n_after - expected_after
  )

# Not touched by any contamination step, but count isn't what the
# collapse + targeted removal alone predict -> unexplained change
flags$rowcount_unexplained <- per_id %>% filter(!contam, diff != 0)

# Contamination ids: shown for manual review (expected to differ)
flags$rowcount_contam_ids <- per_id %>% filter(contam) %>% arrange(psd_id)

flags$ids_lost <- per_id %>% filter(n_before > 0, n_after == 0)
flags$ids_new  <- per_id %>% filter(n_before == 0, n_after > 0)

totals <- tibble(
  item = c("rows before", "rows after", "net change",
           "expected from collapse", "expected from targeted removal",
           "net change on contamination ids"),
  rows = c(nrow(before), nrow(after), nrow(after) - nrow(before),
           -sum(per_id$excess[per_id$collapsed], na.rm = TRUE),
           -2 * length(targeted_ids),
           sum(per_id$n_after[per_id$contam]) - sum(per_id$n_before[per_id$contam]))
)
print(totals)

## -----------------------------------------------------------------------------
## 2. Untouched students should be unchanged
## -----------------------------------------------------------------------------
# Anyone not in ANY correction set. Only difference allowed: the NBSP ->
# hyphen name fix (applied to everyone) and the new superseded column.

untouched_ids <- setdiff(unique(before$psd_id),
                         c(collapse_ids, identity_fix_ids, contam_touched, targeted_ids))

fix_nbsp <- function(df) {
  df %>% mutate(across(any_of(c("first_name", "middle_name", "last_name")),
                       ~ str_replace_all(.x, "\u00A0", "-")))
}

b_u <- before %>% filter(psd_id %in% untouched_ids) %>% fix_nbsp()
a_u <- after  %>% filter(psd_id %in% untouched_ids) %>% select(all_of(names(before)))

# rows in before that don't exist in after, and vice versa
flags$untouched_missing_in_after <- anti_join(b_u, a_u, by = names(before))
flags$untouched_new_in_after     <- anti_join(a_u, b_u, by = names(before))

## -----------------------------------------------------------------------------
## 3. Facts dropped by the collapse
## -----------------------------------------------------------------------------
# A distinct fact present before but gone after. For NSC rows this should
# never happen (collapse needs ALL NSC fields to match). For non-NSC rows
# it's expected ONLY where a higher-priority source won the tie-break.

fact_key <- c("psd_id", "record_term", "record_year", "status_source", nsc_fact_fields)

flags$facts_dropped <- before %>%
  filter(!psd_id %in% contam_touched,
         !(psd_id %in% targeted_ids & status_source == "INFERRED")) %>%
  distinct(across(all_of(fact_key))) %>%
  anti_join(after %>% distinct(across(all_of(fact_key))), by = fact_key) %>%
  mutate(expected = coalesce(status_source != "NSC", TRUE) & psd_id %in% collapse_ids,
         .after = psd_id) %>%
  arrange(expected, psd_id)

## -----------------------------------------------------------------------------
## 4. Duplicates remaining
## -----------------------------------------------------------------------------

scope_after <- scope_of(after)

# ids cleared for collapse that still have excess rows
flags$collapsed_but_still_dup <- scope_after %>% filter(psd_id %in% collapse_ids)

# reviewed as safe to collapse, but never cleared by the apply script
# (e.g. severe ids whose outcome text has a qualifier, so the exact
# match in Part 7 missed them)
reviewed_safe <- unique(c(
  r_rt %>% filter(reviewed, str_detect(outcome, "^coherent - safe to collapse")) %>% pull(psd_id),
  "2013PNCQ15", "2014NDJU56", "2021ZMDI63", "2021KBQP24"
))
flags$reviewed_safe_not_collapsed <- scope_after %>%
  filter(psd_id %in% setdiff(reviewed_safe, collapse_ids))

# in scope but never reviewed at all
flags$in_scope_never_reviewed <- scope_before %>%
  filter(!psd_id %in% (r_rt %>% filter(reviewed) %>% pull(psd_id)))

# fully identical rows anywhere in the output
flags$exact_dup_rows <- after %>%
  count(across(everything()), name = "n_copies") %>%
  filter(n_copies > 1)

# new duplicates on ids that received moved rows
flags$contam_new_dups <- after %>%
  filter(psd_id %in% contam_touched) %>%
  count(psd_id, record_term, record_year, status_source, college_name, name = "n") %>%
  filter(n > 1)

## -----------------------------------------------------------------------------
## 5. Identity + NBSP fixes reached the export?
## -----------------------------------------------------------------------------

master_long <- master %>%
  filter(psd_id %in% identity_fix_ids) %>%
  distinct(psd_id, .keep_all = TRUE) %>%
  select(psd_id, all_of(identity_cols)) %>%
  pivot_longer(-psd_id, names_to = "field", values_to = "master_value")

flags$identity_fix_not_in_export <- after %>%
  filter(psd_id %in% identity_fix_ids) %>%
  select(psd_id, all_of(identity_cols)) %>%
  pivot_longer(-psd_id, names_to = "field", values_to = "psd_value") %>%
  distinct() %>%
  left_join(master_long, by = c("psd_id", "field")) %>%
  filter(!is.na(master_value), is.na(psd_value) | psd_value != master_value)

nbsp_rows <- function(df) {
  df %>% filter(if_any(c(first_name, middle_name, last_name),
                       ~ str_detect(coalesce(.x, ""), "\u00A0")))
}
flags$nbsp_still_in_export <- nbsp_rows(after) %>%
  select(psd_id, record_year, record_term)

cat("NBSP name rows - before: ", nrow(nbsp_rows(before)),
    " | after: ", nrow(nbsp_rows(after)), " (expect 0 after)\n", sep = "")

## -----------------------------------------------------------------------------
## 6. Contamination fixes
## -----------------------------------------------------------------------------

# 2013PSXY40: exactly the 12 rebuilt OLD_PSD rows, nothing else
flags$psxy40_rows <- after %>%
  filter(psd_id == "2013PSXY40") %>%
  count(status_source, college_name, name = "n") %>%
  mutate(total = sum(n), ok = total == 12 & all(status_source == "OLD_PSD"))

# 2014UZIS99 / 2014RPEE11: combined total should match the 43 confirmed
# on 2026-09-03; the 20 non-PLANS UCI rows should all sit under RPEE11
pair <- c("2014UZIS99", "2014RPEE11")
flags$uzis_rpee_totals <- tibble(
  psd_id = pair,
  n_before = map_int(pair, ~ sum(before$psd_id == .x)),
  n_after  = map_int(pair, ~ sum(after$psd_id == .x))
) %>%
  add_row(psd_id = "COMBINED (expect 43 after)",
          n_before = sum(before$psd_id %in% pair),
          n_after  = sum(after$psd_id %in% pair))

flags$uci_block_placement <- after %>%
  filter(psd_id %in% pair,
         college_name == "UNIVERSITY OF CALIFORNIA - IRVINE") %>%
  mutate(is_plans = toupper(record_term) == "PLANS") %>%
  count(psd_id, is_plans, name = "n")
# expect: RPEE11 non-PLANS = 20, UZIS99 non-PLANS = 0,
# each PLANS row still where it started

# Each reassign_psd_id row: how many matching rows sit under the source
# and target ids, before vs after
flags$reassign_check <- reassign_rows %>%
  select(psd_id, target_psd_id, record_term, record_year) %>%
  mutate(
    src_before = pmap_int(list(psd_id, record_term, record_year), function(p, t, y)
      sum(before$psd_id == p & toupper(before$record_term) == toupper(t) &
            before$record_year == y, na.rm = TRUE)),
    src_after  = pmap_int(list(psd_id, record_term, record_year), function(p, t, y)
      sum(after$psd_id == p & toupper(after$record_term) == toupper(t) &
            after$record_year == y, na.rm = TRUE)),
    tgt_before = pmap_int(list(target_psd_id, record_term, record_year), function(p, t, y)
      sum(before$psd_id == p & toupper(before$record_term) == toupper(t) &
            before$record_year == y, na.rm = TRUE)),
    tgt_after  = pmap_int(list(target_psd_id, record_term, record_year), function(p, t, y)
      sum(after$psd_id == p & toupper(after$record_term) == toupper(t) &
            after$record_year == y, na.rm = TRUE)),
    # for a real move: source should drop by 1 and target gain 1
    looks_right = (psd_id == target_psd_id & src_after == src_before) |
      (psd_id != target_psd_id & src_after == src_before - 1 & tgt_after == tgt_before + 1)
  )

# Rows under split / reassigned ids whose name doesn't match the roster
# entry for the psd_id they now sit under
norm <- function(x) str_to_upper(str_squish(x))
flags$contam_name_mismatch <- after %>%
  filter(psd_id %in% contam_touched) %>%
  select(psd_id, record_term, record_year, college_name,
         first_name, middle_name, last_name) %>%
  left_join(master %>% distinct(psd_id, .keep_all = TRUE) %>%
              select(psd_id, m_first = first_name, m_middle = middle_name,
                     m_last = last_name), by = "psd_id") %>%
  filter(norm(first_name) != norm(m_first) | norm(last_name) != norm(m_last) |
           coalesce(norm(middle_name), "") != coalesce(norm(m_middle), ""))

## -----------------------------------------------------------------------------
## 7. Placeholder (MISSING DATA) rows preserved
## -----------------------------------------------------------------------------
# Never collapsed by design. Compared without identity fields, since
# those may legitimately have been corrected.

ph_key <- setdiff(names(before), identity_cols)
flags$placeholders_lost <- before %>%
  filter(is_placeholder(before), !psd_id %in% contam_touched) %>%
  anti_join(after %>% select(all_of(names(before))), by = ph_key)

## -----------------------------------------------------------------------------
## 8. Targeted INFERRED removal
## -----------------------------------------------------------------------------
# Shows the outcome text each id was matched on - the "missin" pattern is
# broad, so confirm every id here really is a post-graduation stale case.

flags$targeted_removal <- r_rt %>%
  filter(psd_id %in% targeted_ids) %>%
  select(psd_id, outcome) %>%
  mutate(
    inferred_before = map_int(psd_id, ~ sum(before$psd_id == .x & before$status_source == "INFERRED", na.rm = TRUE)),
    inferred_after  = map_int(psd_id, ~ sum(after$psd_id  == .x & after$status_source  == "INFERRED", na.rm = TRUE)),
    has_nsc_grad    = map_lgl(psd_id, ~ any(after$psd_id == .x & after$status_source == "NSC" &
                                              after$he_graduated == "Y", na.rm = TRUE))
  )

## -----------------------------------------------------------------------------
## 9. Superseded flag + column changes
## -----------------------------------------------------------------------------

flags$column_changes <- tibble(
  column = union(names(before), names(after)),
  in_before = column %in% names(before),
  in_after  = column %in% names(after)
) %>% filter(in_before != in_after)
# superseded_by_later_record is new. 01-merge-nsc-to-psd.R's stopifnot()
# requires identical column names between the PSD and new NSC data, so
# the next NSC merge will stop unless this column is handled.

if ("superseded_by_later_record" %in% names(after)) {
  flags$superseded_by_source <- after %>%
    count(status_source, superseded_by_later_record, name = "n")
  flags$superseded_on_authoritative <- after %>%
    filter(superseded_by_later_record == "TRUE",
           status_source %in% c("NSC", "STAFF_VERIFIED"))
}

## -----------------------------------------------------------------------------
## Summary + export
## -----------------------------------------------------------------------------

summary_tbl <- tibble(check = names(flags), n_rows = map_int(flags, nrow))
print(summary_tbl, n = Inf)

sheets_out <- c(list(summary = summary_tbl, totals = totals, id_sets = id_sets), flags)
names(sheets_out) <- str_trunc(names(sheets_out), 31, ellipsis = "")
write.xlsx(sheets_out, audit_out, overwrite = TRUE)
message("✓ Audit written to: ", audit_out)

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
