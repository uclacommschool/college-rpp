################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < clean_missing_list_function_list.R >
## [ AUTH ] < Ariana Dimagiba >
## [ INIT ] < 08/19/2026 >
##
################################################################################

#Goal: Supporting function library for 04-clean-missing-list.R, matching
#the pattern already established by psd_rfk_function_list.R for
#01-merge-nsc-to-psd.R. Sourced by 04-clean-missing-list.R via:
#   source(file.path("clean_missing_list_function_list.R"))
#
#Contains: the institution name-matching system (institution_aliases,
#match_institution()), the note-template system (template_lookup,
#parse_final_note()), the per-tab cleaning function (standardize_tab(),
#target_columns), and the core per-graduate transformation
#(split_term_year(), expand_graduate_row()).
#
#Depends on institution_lookup already being loaded (from
#04-clean-missing-list.R's CONFIG/load section) before this script is
#sourced, since match_institution()'s default argument references it.

################################################################################

# institution_lookup creates a lookup table that keys on exact-string college_name 
# (confirmed against real data — "LOS ANGELES CITY COLLEGE" matches exactly), but common
# abbreviations/nicknames written in notes ("Cal State LA," "CSULA") won't
# match the canonical name at all — string-distance fuzzy matching can't
# bridge that gap either, since an abbreviation doesn't closely resemble
# the full name character-for-character. Three-tier matching:
#   1. Exact match (case-normalized) — cheap, catches full-name entries
#   2. Alias lookup — small curated table of known abbreviations/nicknames
#   3. Fuzzy match (edit distance) — catches typos/minor variants of a
#      full name not caught by 1 or 2 (NOT abbreviations — see above)
# Anything that clears none of these gets flagged for manual review rather
# than silently left blank or matched to the wrong institution.

# ⚠️ UPDATE: this starter alias set only covers a few obvious cases — add
# more as they come up in review.
# NOTE: "CALIFORNIA STATE UNIVERSITY - LOS ANGELE" (missing the trailing
# "S") is spelled that way in institution_lookup.csv intentionally — it
# matches how NSC itself formats/truncates this institution name, since
# college_name has to join against NSC data. Not a typo; don't "fix" it.
institution_aliases <- tibble::tribble(
  ~alias,          ~college_name,
  "CAL STATE LA",  "CALIFORNIA STATE UNIVERSITY - LOS ANGELE",
  "CSULA",         "CALIFORNIA STATE UNIVERSITY - LOS ANGELE",
  "UCLA",          "UNIVERSITY OF CALIFORNIA-LOS ANGELES",
  "LACC",          "LOS ANGELES CITY COLLEGE",
  "LATTC",         "LOS ANGELES TRADE TECHNICAL",
  "CSUCI",         "CALIFORNIA STATE UNIV CHANNEL ISLANDS"
)

match_institution <- function(raw_name, lookup = institution_lookup,
                              aliases = institution_aliases,
                              max_edit_dist = 4) {
  raw_norm <- str_squish(str_to_upper(raw_name))
  
  # Tier 1: exact match
  exact <- lookup %>% filter(str_to_upper(college_name) == raw_norm)
  if (nrow(exact) == 1) {
    return(exact %>% mutate(match_tier = "exact"))
  }
  
  # Tier 2: alias lookup
  alias_hit <- aliases %>% filter(str_to_upper(alias) == raw_norm)
  if (nrow(alias_hit) == 1) {
    matched <- lookup %>% filter(college_name == alias_hit$college_name[1])
    if (nrow(matched) == 1) {
      return(matched %>% mutate(match_tier = "alias"))
    }
  }
  
  # Tier 3: fuzzy match — edit distance, catches typos/minor variants of
  # a full name only (not abbreviations, per note above)
  dists <- utils::adist(raw_norm, str_to_upper(lookup$college_name))[1, ]
  best_idx <- which.min(dists)
  if (length(best_idx) == 1 && dists[best_idx] <= max_edit_dist) {
    return(lookup[best_idx, ] %>% mutate(match_tier = "fuzzy"))
  }
  
  # Tier 4: no match — flag for manual review rather than guessing
  tibble(college_name = NA_character_, match_tier = "UNMATCHED_REVIEW_NEEDED")
}

# template_lookup: Creates a lookup table with one row per template letter (A-T), grouped
# Verified-tier first (A-H), then Reported-tier (I-Q), then tier-agnostic
# special cases (R-T: Missing Data, Duplicate, Superseded). Most fields the
# script needs (status_source tier, whether a record counts toward
# Persistence/other outcome measures, and category) are fully determined by
# template_code alone — no text parsing required for these. Only templates
# with a variable college/term/pathway/program (A, F, H, I, N, Q, B, C, K)
# need the extraction step in parse_final_note() below.
#
# NOTE on status_source for Verified-tier templates (A, B, C, D, E, F):
# every Verified template's text is phrased as "[Counselor name]
# verified...", so staff is always the one documenting it — this table
# defaults those to staff_verified. alumni_direct (graduate self-reporting
# directly to the researcher, no staff intermediary) is real but rarer;
# if that distinction matters for a specific row, it needs a manual
# override, since the template text alone can't reliably distinguish it.
#
# NOTE: status_source historically was a coarse "NSC" vs. "staff" value.
# Older PSD rows keep that coarse value as-is — this script only writes
# the finer tiers (staff_verified, staff_reported, alumni_direct, linkedin,
# google, inferred) for NEW rows going forward. status_source is expected
# to be a mixed-vocabulary column across the dataset's history; anyone
# doing trend analysis on it needs to know older rows can't be broken
# down by tier.

# NOTE on Templates H and Q: these document graduation *detail*
# (college/term/degree), not ongoing enrollment, so counts_toward_outcome
# is NA for both — they feed he_graduated/degree fields. 

# row_generation encodes the Appendix A rule — how many PSD rows this
# template produces and how record_term/record_year get set:
#   "parsed"      — A, B, I, J: 1-2 rows, term(s) parsed/derived by
#                   parse_final_note() (see that function's comments)
#   "hedge"       — R: always 2 rows, fall + enrolled anytime after fall,
#                   anchored to current_cycle_fall_year — genuinely
#                   unresolved, could still appear in a later NSC pull
#                   within the same cycle
#   "single_term" — H, Q: 1 row, term/year parsed from the note itself
#                   (a specific, already-confirmed graduation date)
#   "fall_anchor" — C, D, E, F, G, K, L, M, N, O, P: 1 row,
#                   record_term="fall", record_year=current_cycle_fall_year
#                   — confirmed present-tense status, not pending resolution
#   "excluded"    — S, T: 0 rows, never merged into the PSD
template_lookup <- tibble::tribble(
  ~template_code, ~status_source,     ~counts_toward_outcome, ~category,               ~row_generation,
  "A",            "staff_verified",   TRUE,                   "Enrollment",            "parsed",
  "B",            "staff_verified",   TRUE,                   "Enrollment",            "parsed",
  "C",            "staff_verified",   NA,                     "Trade/Technical",       "fall_anchor",
  "D",            "staff_verified",   NA,                     "Working",               "fall_anchor",
  "E",            "staff_verified",   NA,                     "Military",              "fall_anchor",
  "F",            "staff_verified",   NA,                     "Other Pathway",         "fall_anchor",
  "G",            "staff_verified",   FALSE,                  "Stopped Out",           "fall_anchor",
  "H",            "staff_verified",   NA,                     "Graduation Detail",     "single_term",
  "I",            "staff_reported",   TRUE,                   "Enrollment",            "parsed",
  "J",            "staff_reported",   FALSE,                  "Enrollment",            "parsed",
  "K",            "staff_reported",   NA,                     "Trade/Technical",       "fall_anchor",
  "L",            "staff_reported",   NA,                     "Working",               "fall_anchor",
  "M",            "staff_reported",   NA,                     "Military",              "fall_anchor",
  "N",            "staff_reported",   NA,                     "Other Pathway",         "fall_anchor",
  "O",            "staff_reported",   NA,                     "Unspecified Pathway",   "fall_anchor",
  "P",            "staff_reported",   FALSE,                  "Stopped Out",           "fall_anchor",
  "Q",            NA,                 NA,                     "Graduation Detail",     "single_term",
  "R",            "inferred",         NA,                     "Missing Data",          "hedge",
  "S",            NA,                 NA,                     "Duplicate — Excluded",  "excluded",
  "T",            NA,                 NA,                     "Superseded — Excluded", "excluded",
  "U",            "staff_verified",   FALSE,                  "Intent to Transfer",    "fall_anchor",
  "V",            "staff_reported",   FALSE,                  "Intent to Transfer",    "fall_anchor",
)

# target_columns: the single source of truth for the 5-column target
# schema every cohort tab should end up with. Referenced by
# standardize_tab()'s final select(), and by expected_cols in the Part 3
# validation check.
target_columns <- c("psd_id", "first_name", "last_name",
                    "template_code", "final_follow_up_note")

# standardize_tab() Standardizes columns names for each cohort tab in the 
# school-facing Google spreadsheet:
#   1. Standardizes column names to snake_case via clean_names()
#   2. Coalesces any duplicate psd_id-style columns into a single psd_id
#      column (only one duplicate actually has a value per row)
#   3. Coalesces any duplicate first_name-style columns the same way
#   4. Selects down to the 5 columns in target_columns, in order
#
# GUIDANCE: any column not in the 5 target columns (e.g., a per-staff raw
# note column like "[name]'s Notes", or the old pre-consolidation
# college_enrollment_or_career_vocation) is dropped here, not retained —
# final_follow_up_note is the authoritative source now, so raw
# pre-consolidation inputs aren't kept downstream. A message() flags any
# unexpected column found, so a genuine typo/misnamed header in the sheet
# doesn't silently disappear without anyone noticing.
standardize_tab <- function(df) {
  
  df <- df %>% clean_names()  # standardize to snake_case
  
  # --- coalesce duplicate id columns (e.g. psd_id, psd_id_8, psd_id_9) ---
  # these happen when a tab has a blank/merged header that duplicates an
  # existing column name with a numeric suffix
  id_cols <- names(df)[str_detect(names(df), "^psd_id")]
  if (length(id_cols) > 1) {
    df$psd_id <- coalesce(!!!df[id_cols])
    df <- df %>% select(-any_of(setdiff(id_cols, "psd_id")))
  }
  
  # --- coalesce duplicate first_name columns (e.g. first_name, first_name_3) ---
  fn_cols <- names(df)[str_detect(names(df), "^first_name")]
  if (length(fn_cols) > 1) {
    df$first_name <- coalesce(!!!df[fn_cols])
    df <- df %>% select(-any_of(setdiff(fn_cols, "first_name")))
  }
  
  # --- flag (but don't retain) any unexpected column ---
  stray_cols <- setdiff(names(df), target_columns)
  if (length(stray_cols) > 0) {
    message("standardize_tab(): dropping unexpected column(s) not in ",
            "target_columns: ", str_c(stray_cols, collapse = ", "),
            ". Verify this is expected (e.g., a leftover raw note column) ",
            "rather than a typo'd header.")
  }
  
  # --- select and order the final target columns ---
  df %>%
    select(all_of(target_columns))
}

# parse_final_note(): This function extracts structured fields from a Final Follow Up
# Note. For templates that carry a variable value the rest of the script
# needs (college_name/term, pathway, or named_source). Templates with no
# variable content at all (B, C, D, E, H, I, J, L, N, O) are fully covered
# by template_lookup alone and return NULL here — no text parsing needed.
#
# GUIDANCE:
# - Case/phrasing drift is expected (e.g., "ENROLLED AFTER FALL 2025" vs.
#   the canonical "enrolled anytime after fall") — the note text is
#   normalized to lowercase/squished whitespace before matching, and the
#   "after fall" match is loose (just requires "after fall" to appear)
#   rather than requiring the exact canonical phrase.
# - For A/I: the Fall term/year IS parsed from the note text, since
#   that's the confirmed starting point. The "enrolled anytime after
#   fall" year is NEVER parsed from the note, even if one is written
#   there — it's always derived as fall_year + 1, matching the convention
#   already used to auto-generate that placeholder row elsewhere in this
#   script. This avoids the ambiguity of a written year like "after fall
#   2025," which could mean either "through 2025" or "into 2026."
# - Returns one row per term detected for A/I (one row if only Fall is
#   confirmed, two rows if both Fall and continued enrollment are
#   confirmed). Returns a single row for F/N (pathway), C/K (trade/
#   technical program name), and B/J (named source only, no
#   institution/term by definition of that template).
parse_final_note <- function(note, template_code) {
  note_norm <- str_squish(str_to_lower(note))
  named_source <- str_match(note_norm, "per ([^;.]+)")[, 2] %>% str_squish()
  
  if (template_code %in% c("A", "I")) {
    # Anchored on the term/year pattern that reliably follows the college
    # name (Fall/Winter/Spring/Summer + 4 digits), not on the first comma —
    # a comma can legitimately appear INSIDE an institution name (e.g.,
    # "CALIFORNIA STATE POLYTECHNIC UNIVERSITY, POMONA"), which would
    # truncate extraction early and silently drop the campus name if we
    # stopped at the first comma instead.
    college_name   <- str_match(note_norm, "enrolled at (.+?),\\s*(?:fall|winter|spring|summer)\\s+\\d{4}")[, 2]
    fall_year      <- str_match(note_norm, "fall (\\d{4})")[, 2] %>% as.integer()

    # Detect ANY signal of continued enrollment beyond the confirmed Fall
    # term — whether phrased as the canonical hedge ("enrolled anytime
    # after fall") or as a specific named term (e.g., "and Spring 2026").
    # Both are treated identically as the hedge row: a named term in a
    # note isn't necessarily more CONFIRMED than the vague phrase — it may
    # just reflect what a counselor expects/plans, not verified
    # attendance. The hedge exists precisely because exact re-enrollment
    # timing can't be confirmed from follow-up data; naming a season isn't
    # the same as confirming it. So the specific season/year named in the
    # note is intentionally discarded — only used to detect that *some*
    # continued-enrollment signal is present, never written to the output.
    has_continued_enrollment <- str_detect(note_norm, "after fall") ||
      str_detect(note_norm, "and (winter|spring|summer) \\d{4}")

    rows <- tibble(
      college_name = str_to_upper(college_name),
      named_source = named_source,
      record_term  = "fall",
      record_year  = fall_year
    )

    if (has_continued_enrollment && !is.na(fall_year)) {
      rows <- bind_rows(rows, tibble(
        college_name = str_to_upper(college_name),
        named_source = named_source,
        record_term  = "enrolled anytime after fall",
        record_year  = fall_year + 1L
      ))
    }
    return(rows)
    
  } else if (template_code %in% c("F", "N")) {
    # pathway is written as "...status as [pathway], per [source]..." —
    # pathway text sits between "status as" and the following comma.
    pathway <- str_match(note_norm, "status as ([^,]+),")[, 2]
    return(tibble(
      pathway      = str_squish(pathway),
      named_source = named_source
    ))
    
  } else if (template_code %in% c("C", "K")) {
    # trade/technical program name is written as "...trade/technical
    # program — [program name], per [source]..." — program name sits
    # between the em dash and the following comma.
    program_name <- str_match(note_norm, "trade/technical program\\s*—\\s*([^,]+),")[, 2]
    return(tibble(
      program_name = str_squish(program_name),
      named_source = named_source
    ))
    
  } else if (template_code %in% c("B", "J")) {
    # institution/term intentionally unspecified for this template —
    # only the named source is extractable. Applies to both tiers
    # (B=Verified, J=Reported) since both share this shape.
    return(tibble(named_source = named_source))
    
  } else if (template_code == "H") {
    # H template: "[Counselor name(s)] verified graduate completed
    # [degree_type] in [major — if known] at [college_name],
    # [record_term/record_year][, corroborated via researcher
    # (LinkedIn/Google) — if applicable]."
    # college_name is the only truly required piece — degree_type, major,
    # record_term_year, and corroboration source are all OPTIONAL and come
    # back NA when absent, rather than failing parsing entirely. Term/year
    # isn't always confirmable (only the college may be known); when NA,
    # expand_graduate_row() substitutes the current cycle's collection
    # term/year, matching the convention already documented for
    # coll_grad_date=NA cases: use the record year/term the information
    # was RECEIVED in, not a guessed completion date. Same logic applies
    # to degree_type/major — not always knowable, mirrors NSC's own gaps.
    #
    # degree_type and major are split on the word " in " — the natural
    # linguistic connector for how degrees are actually written ("Bachelor
    # of Science in Health Administration"), rather than a comma, since a
    # comma can appear inside either the degree name or an institution
    # name (see the Cal Poly Pomona case) and isn't a reliable delimiter.
    after_completed <- str_match(note_norm, "completed\\s*(.*?)\\s*at\\s+(.+)")
    degree_major_raw <- str_squish(after_completed[, 2])
    rest <- after_completed[, 3]

    if (is.na(rest)) {
      return(tibble(college_name = NA_character_, record_term_year = NA_character_,
                    degree_type = NA_character_, major = NA_character_,
                    status_source = NA_character_))
    }

    if (str_detect(degree_major_raw, " in ")) {
      dm_parts <- str_split(degree_major_raw, " in ", n = 2)[[1]] %>% str_squish()
      degree_raw <- dm_parts[1]
      major_raw  <- dm_parts[2]
    } else {
      degree_raw <- degree_major_raw
      major_raw  <- ""
    }

    parts <- str_split(rest, ",")[[1]] %>% str_squish()
    college_name_val <- if (length(parts) >= 1) {
      str_remove(parts[1], "\\.$")  # strip trailing period if college is the last piece
    } else {
      NA_character_
    }
    term_year_val <- if (length(parts) >= 2) {
      str_squish(str_remove(parts[2], "[.].*$"))
    } else {
      NA_character_
    }
    # source detected by keyword anywhere in the note, not anchored to
    # "via" — the actual phrase ("corroborated via researcher LinkedIn")
    # doesn't put "via" immediately before the platform name
    source_used <- case_when(
      str_detect(note_norm, "linkedin") ~ "linkedin",
      str_detect(note_norm, "google")   ~ "google",
      TRUE ~ NA_character_
    )

    return(tibble(
      college_name       = str_to_upper(college_name_val),
      record_term_year   = term_year_val,
      degree_type        = if (degree_raw == "") NA_character_ else degree_raw,
      major              = if (major_raw == "") NA_character_ else major_raw,
      status_source      = source_used
    ))

  } else if (template_code == "Q") {
    # Q template (unchanged shape): "graduation detail — [college],
    # [term/year], [degree] — via [linkedin/google]." Same optional-field
    # tolerance as H — college_name required, everything else optional.
    body <- str_match(note_norm, "graduation detail[^a-z0-9]*(.+)")[, 2]

    if (is.na(body)) {
      return(tibble(college_name = NA_character_, record_term_year = NA_character_,
                    degree_type = NA_character_, status_source = NA_character_))
    }

    parts <- str_split(body, ",")[[1]] %>% str_squish()
    college_name_val <- if (length(parts) >= 1) {
      str_remove(parts[1], "\\.$")
    } else {
      NA_character_
    }
    term_year_val <- if (length(parts) >= 2) {
      str_squish(str_remove(parts[2], "[.—].*$"))
    } else {
      NA_character_
    }
    degree_raw <- if (length(parts) >= 3) {
      str_squish(str_remove(parts[3], "—.*$"))
    } else {
      ""
    }
    source_used <- case_when(
      str_detect(note_norm, "linkedin") ~ "linkedin",
      str_detect(note_norm, "google")   ~ "google",
      TRUE ~ NA_character_
    )

    return(tibble(
      college_name       = str_to_upper(college_name_val),
      record_term_year   = term_year_val,
      degree_type        = if (degree_raw == "") NA_character_ else degree_raw,
      status_source      = source_used
    ))
    
  } else if (template_code %in% c("U", "V")) {
    # institution is optional ("if known") — try to extract it, but don't
    # fail if the note omits it (e.g., "expressed intent to transfer, per
    # graduate" instead of "...transfer to [institution], per graduate")
    institution <- str_match(note_norm, "transfer to ([^,]+), per")[, 2]
    return(tibble(
      institution  = str_to_upper(str_squish(institution)),
      named_source = named_source
    ))
  }
  
  return(NULL)  # D, E, G, L, M, O, P, R — fully covered by template_lookup
}

# split_term_year(): This function takes  H/Q's parse_final_note() output returns a combined
# "record_term_year" string (e.g., "spring 2023") since that's how those
# templates are phrased. Splits it into separate record_term/record_year fields
split_term_year <- function(term_year_str) {
  year <- str_extract(term_year_str, "\\d{4}") %>% as.integer()
  term <- str_squish(str_remove(term_year_str, "\\d{4}"))
  tibble(record_term = term, record_year = year)
}

# --- expand_graduate_row(): the core per-graduate transformation -----------
# Takes one row of psd_missing_list (already carrying template_code and
# final_follow_up_note) and returns 0, 1, or 2 output rows, per the
# row_generation rule in template_lookup:
#   "excluded"    (S, T)                       — 0 rows, not merged
#   "hedge"       (R)                          — 2 rows, fall + after-fall
#   "fall_anchor" (C/D/E/F/G/K/L/M/N/O/P/U/V)  — 1 row, fall-anchored, no term
#   "single_term" (H, Q)                       — 1 row, term parsed from note
#   "parsed"      (A, B, I, J)                 — 1-2 rows, from parse_final_note()
expand_graduate_row <- function(row, fall_year) {

  code <- row$template_code[1]
  note <- row$final_follow_up_note[1]

  lookup_row <- template_lookup %>% filter(template_code == code)
  if (nrow(lookup_row) != 1) {
    # unrecognized/blank template_code — flag for manual review rather
    # than silently dropping the graduate or guessing at a category
    return(row %>% mutate(review_flag = "UNRECOGNIZED_TEMPLATE_CODE", notes = note))
  }

  gen_type <- lookup_row$row_generation

  # S/T — excluded entirely; captured separately as excluded_records below,
  # not merged into the PSD at all
  if (gen_type == "excluded") {
    return(NULL)
  }

  # R — 2-row hedge, genuinely unresolved, no college/term info
  if (gen_type == "hedge") {
    base <- row %>% mutate(
      he_graduated = NA,
      status_source = lookup_row$status_source,
      notes = note
    )
    row1 <- base %>% mutate(record_term = "fall", record_year = fall_year)
    row2 <- base %>% mutate(record_term = "enrolled anytime after fall",
                             record_year = fall_year + 1L)
    return(bind_rows(row1, row2))
  }

  # C/D/E/F/G/K/L/M/N/O/P/U/V — single Fall-anchored status row, confirmed
  # present-tense fact, not pending resolution
  if (gen_type == "fall_anchor") {
    pathway_val <- NA_character_
    program_name_val <- NA_character_
    institution_val <- NA_character_
    if (code %in% c("F", "N")) {
      parsed <- parse_final_note(note, code)
      if (!is.null(parsed)) pathway_val <- parsed$pathway[1]
    }
    if (code %in% c("C", "K")) {
      parsed <- parse_final_note(note, code)
      if (!is.null(parsed)) program_name_val <- parsed$program_name[1]
    }
    if (code %in% c("U", "V")) {
      parsed <- parse_final_note(note, code)
      if (!is.null(parsed)) institution_val <- parsed$institution[1]
    }
    return(row %>% mutate(
      he_graduated = "N",
      status_source = lookup_row$status_source,
      notes = note,
      record_term = "fall",
      record_year = fall_year,
      # ⚠️ UPDATE: Other Pathway value, Trade/Technical program name, and
      # Intent to Transfer target institution (if named) all stored in
      # program_code as a shared placeholder — replace with dedicated PSD
      # field(s) if they exist separately from program_code. Not stored in
      # college_name, since these graduates aren't confirmed enrolled there.
      program_code = coalesce(pathway_val, program_name_val, institution_val)
    ))
  }

  # H/Q — single row, confirmed graduation detail. Term/year is parsed
  # directly from the note when confirmed; when not confirmable (only the
  # college is known), falls back to the current cycle's collection
  # term/year — matching the convention already documented for
  # coll_grad_date=NA cases: the record reflects when the information was
  # RECEIVED, not a guessed completion date.
  if (gen_type == "single_term") {
    parsed <- parse_final_note(note, code)
    if (is.null(parsed) || is.na(parsed$college_name[1])) {
      return(row %>% mutate(review_flag = "PARSE_FAILED", notes = note))
    }
    ty <- if (is.na(parsed$record_term_year[1])) {
      tibble(record_term = "fall", record_year = fall_year)
    } else {
      split_term_year(parsed$record_term_year[1])
    }
    # institution_match_tier from match_institution() is used here only to
    # pick the matched college_name — not retained on the output row (no
    # PSD column for QA match-confidence; internal use only)
    matched <- match_institution(parsed$college_name[1])
    return(row %>% mutate(
      he_graduated = "Y",
      # The linkedin/google override only applies to Q — Q has no staff
      # verification at all, so whichever platform corroborated it IS the
      # actual source. H is different: staff already verified the
      # graduation detail, and any linkedin/google mention is supplementary
      # corroboration layered on top, not a replacement for staff_verified.
      # Applying the override to H too (the old behavior) incorrectly
      # downgraded staff-verified records to "linkedin"/"google" whenever
      # corroboration happened to be mentioned in the note.
      status_source = if (code == "Q" && !is.na(parsed$status_source[1])) {
        parsed$status_source[1]
      } else {
        lookup_row$status_source
      },
      notes = note,
      college_name = matched$college_name[1],
      degree_title = parsed$degree_type[1],  # NA if unknown — mirrors NSC's own gaps here
      # major only exists in H's parse_final_note() output (Q has no
      # degree/major distinction) — guarded so Q rows don't error trying
      # to access a column that isn't there
      major = if ("major" %in% names(parsed)) parsed$major[1] else NA_character_,
      record_term = ty$record_term,
      record_year = ty$record_year
    ))
  }

  # A/B/I/J — parsed enrollment; 1-2 rows depending on how many terms the
  # note confirms (see parse_final_note() for the fall/after-fall logic)
  if (gen_type == "parsed") {
    parsed <- parse_final_note(note, code)
    if (is.null(parsed) || nrow(parsed) == 0) {
      return(row %>% mutate(review_flag = "PARSE_FAILED", notes = note))
    }
    matched_name <- if ("college_name" %in% names(parsed) && !is.na(parsed$college_name[1])) {
      match_institution(parsed$college_name[1])$college_name[1]
    } else {
      NA_character_  # Templates B/J — institution intentionally unspecified
    }
    return(purrr::map_dfr(seq_len(nrow(parsed)), function(i) {
      row %>% mutate(
        he_graduated = "N",
        status_source = lookup_row$status_source,
        notes = note,
        college_name = matched_name,
        record_term = if ("record_term" %in% names(parsed)) parsed$record_term[i] else NA_character_,
        record_year = if ("record_year" %in% names(parsed)) parsed$record_year[i] else NA_integer_
      )
    }))
  }

  row  # fallback — shouldn't be reached given row_generation is a closed set
}
