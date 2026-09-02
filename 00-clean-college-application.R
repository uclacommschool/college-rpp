################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 00 - clean-college-application >
## [ AUTH ] < Ariana Dimagiba / aridimagiba >
## [ INIT ] < 04/14/2026, updated 05/19/2026 by aridimagiba >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(stringdist)
library(writexl)

## ---------------------------
## ⚠️ UPDATE EACH RUN — checklist
## ---------------------------
# 1. college_applications_file  — update to current year's college application file
#                                 File naming convention: UCLA CS CLASS OF YYYY_College Applications.xlsx
# 2. cohort_year                — update to match current cohort graduation year (used in output filename)
# 3. Output file name           — auto-generated from cohort_year; confirm path is correct

cohort_year <- 2025   # ⚠️ UPDATE each run

## ---------------------------
## Directory paths
## ---------------------------
code_file_dir <- file.path(".")

# Detect OS and set Box path accordingly
if (.Platform$OS.type == "windows") {
  box_file_dir <- file.path(Sys.getenv("USERPROFILE"), "Box")
} else {
  box_file_dir <- file.path(Sys.getenv("HOME"), "Library", "CloudStorage", "Box-Box")
}

## ---------------------------
## Part 1 - Read in data
## ---------------------------

# Read in college applications tab from the source file
# ⚠️ UPDATE: change filename to current year's college application file
raw_college_applications <- read_excel(file.path(
  box_file_dir,
  "College Data",
  "College-Going Database",
  "UCLACS",
  "College Application Trackers - Source Data",
  "UCLA CS CLASS OF 2025_College Applications.xlsx")
)

if (nrow(raw_college_applications) == 0) {
  stop("⚠️ College applications file loaded but is empty — check file path and filename")
}
cat("✅ College applications loaded:", nrow(raw_college_applications), "rows\n")

# Read in Decisions tab from the same source file
# ⚠️ UPDATE: change to current year's file
decisions_raw <- read_excel(file.path(
  box_file_dir,
  "College Data",
  "College-Going Database",
  "UCLACS",
  "College Application Trackers - Source Data",
  "UCLA CS CLASS OF 2025_College Applications.xlsx"  
),
sheet = "college decisions"
) %>%
  rename_with(str_to_lower) %>%
  rename_with(~ str_replace_all(., " ", "_")) %>%
  mutate(across(where(is.character), str_trim))

if (nrow(decisions_raw) == 0) {
  stop("⚠️ Decisions tab loaded but is empty — check file path and tab name")
}
cat("✅ College decisions tab loaded:", nrow(decisions_raw), "rows\n")

# Institution lookup reference table
institution_lookup <- read_csv(file.path(
  box_file_dir,
  "College Data",
  "Postsecondary Database",
  "institution_lookup.csv"
))

if (nrow(institution_lookup) == 0) {
  stop("⚠️ institution_lookup.csv loaded but is empty — check file path and contents")
}
cat("✅ Institution lookup loaded:", nrow(institution_lookup), "rows\n")

# Manual crosswalk (abbreviations + unresolved names from prior runs)
manual_crosswalk_path <- file.path(
  box_file_dir,
  "College Data",
  "Postsecondary Database",
  "college_name_app_crosswalk.csv"
)

if (!file.exists(manual_crosswalk_path)) {
  warning("⚠️ college_name_app_crosswalk.csv not found — Layer 3 matching will be skipped.
           Run through Layer 2, then generate and fill the crosswalk CSV before re-running.")
  manual_crosswalk <- tibble(college_name = character(), college_name_key = character())
} else {
  manual_crosswalk <- read_csv(manual_crosswalk_path) %>%
    select(college_name, college_name_key) %>%
    filter(!is.na(college_name_key) & college_name_key != "") %>%
    filter(!college_name %in% c("-", "Not Eligible"))
  cat("✅ Manual crosswalk loaded:", nrow(manual_crosswalk), "rows\n")
}

# ⚠️ PSD lookup for student ID join (Class of 2025 already have IDs)
# UPDATE: delete for future cohorts
psd_id_lookup <- read_csv(file.path(
  box_file_dir,
  "College Data",
  "Postsecondary Database",
  "UCLA Community School PSD",
  "Master Student List",
  "master-student-list-rfk-2012-2025.csv"  # ⚠️ UPDATE: change to current PSD file
)) %>%
  rename_with(str_to_lower) %>%
  rename_with(~ str_replace_all(., " ", "_")) %>%
  select(any_of(c("last_name", "first_name", "middle_name", "psd_id", "student_id"))) %>%
  mutate(across(where(is.character), str_trim))

if (nrow(psd_id_lookup) == 0) {
  stop("⚠️ PSD ID lookup file loaded but is empty — check file path and filename")
}
cat("✅ PSD lD lookup loaded:", nrow(psd_id_lookup), "rows\n")

## ---------------------------
## Part 2 - Rename columns positionally
## ---------------------------
# 1. Rename columns
# NOTE: ⚠️ If source file columns shift, update position numbers here
clean_college_apps <- raw_college_applications %>%
  rename(
    last_name     = 1,   # "Last Name"
    first_name    = 2,   # "First Name"
    middle_name   = 3,   # "Middle Name"
    grade_level   = 4,   # "Grade Level"
    cc_period     = 5,   # "CC period"
    area_of_study = 6,   # "Area of Study"
    
    # CSU colleges + decisions
    csu1          = 7,
    csu1_dec      = 8,
    csu2          = 9,
    csu2_dec      = 10,
    csu3          = 11,
    csu3_dec      = 12,
    csu4          = 13,
    csu4_dec      = 14,
    
    # UC colleges + decisions
    uc1           = 17,
    uc1_dec       = 18,
    uc2           = 19,
    uc2_dec       = 20,
    uc3           = 21,
    uc3_dec       = 22,
    uc4           = 23,
    uc4_dec       = 24,
    uc5           = 25,
    uc5_dec       = 26,
    uc6           = 27,
    uc6_dec       = 28,
    
    # Private colleges + decisions
    priv1         = 30,
    priv1_dec     = 31,
    priv2         = 32,
    priv2_dec     = 33,
    priv3         = 34,
    priv3_dec     = 35,
    priv4         = 36,
    priv4_dec     = 37,
    priv5         = 38,
    priv5_dec     = 39,
    priv6         = 40,
    priv6_dec     = 41,
    priv7         = 42,
    priv7_dec     = 43,
    priv8         = 44,
    priv8_dec     = 45,
    priv9         = 46,
    priv9_dec     = 47,
    priv10        = 48,
    priv10_dec    = 49,
    priv11        = 50,
    priv11_dec    = 51,
    priv12        = 52,
    priv12_dec    = 53,
    priv13        = 54,
    priv13_dec    = 55,
    priv14        = 56,
    priv14_dec    = 57,
    priv15        = 58,
    priv15_dec    = 59,
    priv16        = 60,
    priv16_dec    = 61,
    priv17        = 62,
    priv17_dec    = 63
  ) %>%
  mutate(across(where(is.character), str_trim))  # trim whitespace on all text fields

## ---------------------------
## Part 3 - Helper function: pivot school + decision column pairs to long format
## ---------------------------
#1. 
pivot_school_pairs <- function(df, id_cols, school_cols, dec_cols, sys_type_label) {
  school_long <- df %>%
    select(all_of(c(id_cols, school_cols))) %>%
    pivot_longer(cols = all_of(school_cols), names_to = "slot", values_to = "college_name")
  
  dec_long <- df %>%
    select(all_of(c(id_cols, dec_cols))) %>%
    pivot_longer(cols = all_of(dec_cols), names_to = "slot_dec", values_to = "decision")
  
  bind_cols(school_long, dec_long %>% select(decision)) %>%
    filter(!is.na(college_name) & college_name != "") %>%
    mutate(sys_type = sys_type_label) %>%
    select(all_of(id_cols), college_name, decision, sys_type)
}

# 2. Identify demographic  columns
id_cols <- c("last_name", "first_name", "middle_name", "grade_level", "cc_period")


# 3. Pivot to long format
csu_long <- pivot_school_pairs(
  df             = clean_college_apps,
  id_cols        = id_cols,
  school_cols    = c("csu1", "csu2", "csu3", "csu4"),
  dec_cols       = c("csu1_dec", "csu2_dec", "csu3_dec", "csu4_dec"),
  sys_type_label = "CSU"
)

uc_long <- pivot_school_pairs(
  df             = clean_college_apps,
  id_cols        = id_cols,
  school_cols    = c("uc1", "uc2", "uc3", "uc4", "uc5", "uc6"),
  dec_cols       = c("uc1_dec", "uc2_dec", "uc3_dec", "uc4_dec", "uc5_dec", "uc6_dec"),
  sys_type_label = "UC"
)

priv_long <- pivot_school_pairs(
  df             = clean_college_apps,
  id_cols        = id_cols,
  school_cols    = c("priv1","priv2","priv3","priv4","priv5","priv6","priv7","priv8","priv9",
                     "priv10","priv11","priv12","priv13","priv14","priv15","priv16","priv17"),
  dec_cols       = c("priv1_dec","priv2_dec","priv3_dec","priv4_dec","priv5_dec","priv6_dec",
                     "priv7_dec","priv8_dec","priv9_dec","priv10_dec","priv11_dec","priv12_dec",
                     "priv13_dec","priv14_dec","priv15_dec","priv16_dec","priv17_dec"),
  sys_type_label = "Private"
)

college_df <- bind_rows(csu_long, uc_long, priv_long) %>%
  arrange(last_name, first_name, sys_type, college_name)

cat("✅ Wide to long complete:", nrow(college_df), "rows\n")

## -----------------------------------------------------------------------------
## Part 4 -  Match college names to institution_lookup
## -----------------------------------------------------------------------------
# GUIDANCE: Three layers of matching : exact → fuzzy → manual crosswalk

# 1. Prep institution look up table
lookup_clean <- institution_lookup %>%
  rename(college_name_clean = college_name) %>%
  mutate(college_name_key = str_to_upper(str_trim(college_name_clean))) %>%
  distinct(college_name_key, .keep_all = TRUE)  # guard against lookup duplicates

# 2. LAYER 1: Exact match
college_df_prepped <- college_df %>%
  mutate(college_name_upper = str_to_upper(str_trim(college_name)))

exact_matched <- college_df_prepped %>%
  left_join(
    lookup_clean %>% select(college_name_key, college_name_clean, cc_4yr,
                            public_private, college_code, college_state, system_type),
    by = c("college_name_upper" = "college_name_key")
  )

matched   <- exact_matched %>% filter(!is.na(college_name_clean))
unmatched <- exact_matched %>% filter(is.na(college_name_clean))

cat("Layer 1 — Exact matched:", nrow(matched), "\n")
cat("Layer 1 — Still unmatched:", nrow(unmatched), "\n")

# 3. LAYER 2: Fuzzy match 

# GUIDANCE: Jaro-Winkler similarity; handles typos/misspellings
# ⚠️ Does NOT work well for abbreviations (LMU, USC) — those go to Layer 3
# ⚠️ After first run, spot-check fuzzy_auto to confirm no bad matches

unmatched_names <- unmatched %>%
  filter(!college_name %in% manual_crosswalk$college_name) %>%
  distinct(college_name, college_name_upper)

lookup_names <- lookup_clean$college_name_key

fuzzy_results <- unmatched_names %>%
  rowwise() %>%
  mutate(
    sim_scores = list(stringsim(college_name_upper, lookup_names, method = "jw")),
    best_idx   = which.max(sim_scores),
    best_match = lookup_names[best_idx],
    best_score = max(unlist(sim_scores))
  ) %>%
  ungroup() %>%
  select(college_name, college_name_upper, best_match, best_score)

# 4. Sepearate fuzzy_auto additions and fuzzy_review
fuzzy_auto   <- fuzzy_results %>% filter(best_score >= 0.90)
fuzzy_review <- fuzzy_results %>% filter(best_score <  0.90)

cat("Layer 2 — Fuzzy auto-accepted:", nrow(fuzzy_auto), "\n")
cat("Layer 2 — Below threshold (needs manual review):", nrow(fuzzy_review), "\n")

# 5.  LAYER 3: Manual crosswalk 
# FIRST RUN ONLY: uncomment write_csv block below to generate the crosswalk file.
# Fill in college_name_key column in Excel for each row, save, then re-comment.
# Subsequent runs: file loads automatically at the top of the script.


# 5. Combine all three layers 
college_df_matched <- bind_rows(
  
  ## Layer 1: exact matches
  matched %>%
    select(-college_name_upper),
  
  ## Layer 2: fuzzy auto-accepted
  unmatched %>%
    filter(!college_name %in% manual_crosswalk$college_name) %>%
    select(-college_name_clean) %>%
    left_join(fuzzy_auto %>% select(college_name, best_match),
              by = "college_name") %>%
    left_join(lookup_clean %>% select(college_name_key, college_name_clean, cc_4yr,
                                      public_private, college_code, college_state, system_type),
              by = c("best_match" = "college_name_key"),
              relationship = "many-to-many") %>%
    select(-best_match, -college_name_upper),
  
  ## Layer 3: manual crosswalk
  unmatched %>%
    filter(college_name %in% manual_crosswalk$college_name) %>%
    select(-college_name_clean) %>%
    left_join(manual_crosswalk %>% select(college_name, college_name_key),
              by = "college_name") %>%
    left_join(lookup_clean %>% select(college_name_key, college_name_clean, cc_4yr,
                                      public_private, college_code, college_state, system_type),
              by = "college_name_key",
              relationship = "many-to-many") %>%
    select(-college_name_key, -college_name_upper)
  
) %>%
  filter(!college_name %in% c("-", "Not Eligible"))

cat("✅ Matching complete:", nrow(college_df_matched), "rows\n")

# 6.  Final unmatched check 
still_unmatched <- college_df_matched %>%
  filter(is.na(college_name_clean)) %>%
  distinct(college_name, sys_type)

if (nrow(still_unmatched) > 0) {
  cat("⚠️", nrow(still_unmatched), "college names still unresolved — add to crosswalk CSV:\n")
  print(still_unmatched)
} else {
  cat("✅ All college names matched.\n")
}

## -----------------------------------------------------------------------------
## Join PSD student IDs for Class of 2025 - remove chunk after
## Class of 2025 already have IDs — join by name from PSD lookup
## For future cohorts where IDs don't exist yet, IDs will be NA and can be
## assigned later once students are entered into PSD
## -----------------------------------------------------------------------------
college_df_matched <- college_df_matched %>%
  mutate(across(c(last_name, first_name, middle_name), str_trim)) %>%
  left_join(
    psd_id_lookup %>% select(last_name, first_name, middle_name, psd_id),
    by = c("last_name", "first_name", "middle_name")
  )

# Check how many students matched to a PSD ID
psd_match_rate <- college_df_matched %>%
  distinct(last_name, first_name, middle_name, psd_id) %>%
  summarise(
    total_students  = n(),
    matched_psd     = sum(!is.na(psd_id)),
    unmatched_psd   = sum(is.na(psd_id))
  )

cat("PSD ID join results:\n")
print(psd_match_rate)

# Flag students with no PSD ID for manual review
psd_unmatched <- college_df_matched %>%
  filter(is.na(psd_id)) %>%
  distinct(last_name, first_name, middle_name)

if (nrow(psd_unmatched) > 0) {
  cat("⚠️ Students with no PSD ID match — review names for typos:\n")
  print(psd_unmatched)
}

## -----------------------------------------------------------------------------
## PART 5 - Final column selection and ordering
## -----------------------------------------------------------------------------
# 1. Removes any residual .x / .y columns from joins
college_df_final <- college_df_matched %>%
  select(
    psd_id,
    last_name,
    first_name,
    middle_name,
    college_name,
    college_name_clean,
    decision,
    cc_4yr,
    public_private,
    college_code,
    college_state,
    system_type) %>%
  arrange(last_name, first_name)

cat("✅ Final dataset:", nrow(college_df_final), "rows,", ncol(college_df_final), "columns\n")
glimpse(college_df_final)

## -----------------------------------------------------------------------------
## PART 6 - Recode decision to admitted
## -----------------------------------------------------------------------------
# 1. Recode decision column to match college-going database format
college_df_final <- college_df_final %>%
  mutate(
    admitted = case_when(
      decision == "Accepted"              ~ "Yes",
      decision == "Decided"              ~ "Yes",
      decision == "Denied"               ~ "No",
      decision == "Canceled or withdrawn" ~ "No",
      decision %in% c("Waitlisted",
                      "Waitlist",
                      "Wailisted",
                      "Waitlsted",
                      "Still waiting")   ~ "Waitlist",
      TRUE                               ~ NA_character_
    )
  ) %>%
  select(-decision)


## -----------------------------------------------------------------------------
## Part 7 - FUTURE Assign PSD IDs
## -----------------------------------------------------------------------------

## placeholder for future cohort

## -----------------------------------------------------------------------------
## Part 8 - Join Decisions tab (committed college per student)
## -----------------------------------------------------------------------------

# 1. Clean decisions_raw
decisions_clean <- decisions_raw %>%
  rename(
    committed_raw = college_decisions,  # raw name as entered by counselor
    committed     = college_name        # cleaned/matched name
  ) %>%
  select(last_name, first_name, middle_name,
         committed_raw, committed,
         degree_intent, scholarship_monies, notes)

# 2. Join to college_df_final by student name
college_df_final <- college_df_final %>%
  left_join(
    decisions_clean,
    by = c("last_name", "first_name", "middle_name")
  )

# 3. Check join results
committed_match <- college_df_final %>%
  distinct(last_name, first_name, middle_name, committed) %>%
  summarise(
    total_students    = n(),
    matched_committed = sum(!is.na(committed)),
    unmatched         = sum(is.na(committed))
  )

cat("Decisions join results:\n")
print(committed_match)

# 4. Flag unmatched for review
committed_unmatched <- college_df_final %>%
  filter(is.na(committed)) %>%
  distinct(last_name, first_name, middle_name)

if (nrow(committed_unmatched) > 0) {
  cat("⚠️ Students with no committed college — check Decisions tab:\n")
  print(committed_unmatched)
} else {
  cat("✅ All students matched to a committed college.\n")
}

## -----------------------------------------------------------------------------
## Part 9 - Inspect before export
## -----------------------------------------------------------------------------
# 1. Quick checks — review before exporting
cat("\nDecision value counts:\n")
print(college_df_final %>% count(admitted, sort = TRUE))

cat("\nSystem type counts:\n")
print(college_df_final %>% count(sys_type))

cat("\nRows with any NA in key columns:\n")
college_df_final %>%
  filter(is.na(college_name_clean) | is.na(psd_id)) %>%
  nrow() %>%
  cat("\n")

## -----------------------------------------------------------------------------
## Part 9 - Export college application file as a csv
## -----------------------------------------------------------------------------

# 1. Export college applicationfile
# NAMING CONVENTION: "YYYY-schoolsite-collegeapp-data.csv"
# Example: "2025-rfk-collegeapp-data.csv"
output_path_data <- file.path(
  box_file_dir,
  "College Data",
  "College-Going Database",
  "UCLACS College-Going Database",
  paste0(cohort_year, "-rfk-collegeapp-data.csv")  # e.g. 2025_collegeapp_data.csv
)

# 2. Export college application review file
output_path_review <- file.path(
  box_file_dir,
  "College Data",
  "College-Going Database",
  "UCLACS College-Going Database",
  paste0(cohort_year, "collegeapp-psd-review.xlsx")  # only generated when needed
)

# Main export
write_csv(college_df_final, file = output_path_data)
cat("✅ File exported to:", output_path_data, "\n")

# Review file — only exported if there are unmatched PSD IDs
if (nrow(psd_unmatched) > 0) {
  write_xlsx(psd_unmatched, path = output_path_review)
  cat("⚠️ Review file exported —", nrow(psd_unmatched),
      "students need manual PSD ID entry:", output_path_review, "\n")
}

