################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 03-transform-missing-list.R >
## [ AUTH ] < Jeffrey Yo / yjeffrey77 >
## [ INIT ] < 04/16/2026, updated 08/17,2026 >
##

# Transforms missing-list CSV into a multi-tab Excel workbook.
# Each tab = one graduating year (hs_grad_year).
# One row per college record per student.
# Notes column = "Did student attend/graduate/transfer from {college_name}?"

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(readr)
library(dplyr)
library(openxlsx)
library(data.table)
library(writexl)

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

## -----------------------------------------------------------------------------
## load all raw data sets
## -----------------------------------------------------------------------------

#load recently updated missing list - internal file from 02-create-missing-list script

##Path for RFK
## "College and Career RPP", "1. NSC Dataset", "RFK", "RFK PSD", "Missing List - Internal", "YYYYMMDD-rfk-missingListInternal-authorlastname.csv"

##Path For Mann
## "College and Career RPP", "1. NSC Dataset", "Mann", "Mann PSD", "Missing List - Internal", "YYYYMMDD-mann-missingListInternal-authorlastname.csv"

missing_list <- read_csv(file.path(box_file_dir,
                                   "College and Career RPP",
                                   "1. NSC Dataset",
                                   "Mann",
                                   "Mann PSD",
                                   "Missing List - Internal",
                                   #⚠️ UPDATE: change to current file
                                   "20260817-mann-missingListInternal-sanchez.csv"
                                   ))


## -----------------------------------------------------------------------------
## Part 1 - Create Teacher Friendly List
## -----------------------------------------------------------------------------

# Build the Notes column

#mark No Enrollment and Missing data as NA?
df<-missing_list %>% mutate(
  college_name = case_when(college_name %in% c("MISSING DATA","NO ENROLLMENT") ~ NA,
                           TRUE ~ college_name)
)

df <- df %>%
  mutate(
    Notes = case_when(
      !is.na(college_name) & college_name != "" ~
        paste0("Did student attend/graduate/transfer from ", college_name, "?"),
      TRUE ~ NA_character_
    )
  )



# Select & rename columns to match the template ─────────────────────────
#   Template columns: psd_id | first_name | last_name | Notes |
#                     College Enrollment or Career/Vocation |
#                     Teacher/College Counselor | School Notes

output_df <- df %>%
  transmute(
    psd_id                                  = psd_id,
    first_name                              = first_name,
    last_name                               = last_name,
    Notes                                   = Notes,
    `College Enrollment or Career/Vocation` = college_name,   # adjust if needed
    `Teacher/College Counselor`             = NA_character_,   # not in source CSV
    `School Notes`                          = NA_character_,   # not in source CSV
    hs_grad_year                            = hs_grad_year     # used for splitting; dropped later
  )

# Split by graduating year ─

# Treat missing grad year as "Unknown"
output_df$hs_grad_year<-as.character(output_df$hs_grad_year)

output_df <- output_df %>%
  mutate(hs_grad_year = if_else(is.na(hs_grad_year) | hs_grad_year == "",
                                "Unknown", hs_grad_year))

year_groups <- split(output_df, output_df$hs_grad_year)

# Sort tabs chronologically (Unknown goes last)
tab_order <- sort(names(year_groups))
unknown_idx <- which(tab_order == "Unknown")
if (length(unknown_idx) > 0) {
  tab_order <- c(tab_order[-unknown_idx], "Unknown")
}

# Define styles 

header_style <- createStyle(
  fontName      = "Arial",
  fontSize      = 11,
  fontColour    = "#FFFFFF",
  fgFill        = "#2F5496",      # dark blue header
  halign        = "CENTER",
  textDecoration = "bold",
  border        = "Bottom",
  borderColour  = "#FFFFFF",
  wrapText      = TRUE
)

row_style <- createStyle(
  fontName = "Arial",
  fontSize = 10
)

alt_row_style <- createStyle(
  fontName = "Arial",
  fontSize = 10,
  fgFill   = "#EEF2FA"            # light blue stripe
)

## -----------------------------------------------------------------------------
## Part 2 - Write workbook
## -----------------------------------------------------------------------------

wb <- createWorkbook()

# Columns to write (drop the hs_grad_year helper column)
final_cols <- c("psd_id", "first_name", "last_name", "Notes",
                "College Enrollment or Career/Vocation",
                "Teacher/College Counselor", "School Notes")

col_widths <- c(18, 14, 14, 55, 35, 25, 35)   # approximate widths per column

for (yr in tab_order) {
  
  sheet_data <- year_groups[[yr]] %>%
    select(all_of(final_cols))
  
  addWorksheet(wb, sheetName = yr)
  
  # Write header + data
  writeData(wb, sheet = yr, x = sheet_data, startRow = 1, startCol = 1,
            headerStyle = header_style, borders = "all",
            borderColour = "#C9C9C9")
  
  n_rows <- nrow(sheet_data)
  
  # Body font
  if (n_rows > 0) {
    addStyle(wb, sheet = yr, style = row_style,
             rows = 2:(n_rows + 1), cols = 1:length(final_cols),
             gridExpand = TRUE)
  }
  
  # Alternating row shading
  if (n_rows > 1) {
    even_rows <- seq(3, n_rows + 1, by = 2)
    if (length(even_rows) > 0) {
      addStyle(wb, sheet = yr, style = alt_row_style,
               rows = even_rows, cols = 1:length(final_cols),
               gridExpand = TRUE, stack = TRUE)
    }
  }
  
  # Column widths
  setColWidths(wb, sheet = yr, cols = 1:length(final_cols), widths = col_widths)
  
  # Freeze the header row
  freezePane(wb, sheet = yr, firstRow = TRUE)
}

## -----------------------------------------------------------------------------
## Part 3 - Export Data
## -----------------------------------------------------------------------------

# NAMING CONVENTION: "20YY-20YY Postsecondary_Paths_FollowUp_List.xlsx"
# Example: "2025-2026 Postsecondary_Paths_FollowUp_List.xlsx"

##Path for RFK
## "College and Career RPP", "1. NSC Dataset", "RFK", "RFK PSD", "Missing List - External", "20YY-20YY Postsecondary_Paths_FollowUp_List.xlsx"

##Path For Mann
## "College and Career RPP", "1. NSC Dataset", "Mann", "Mann PSD", "Missing List - External", "20YY-20YY Postsecondary_Paths_FollowUp_List.xlsx"

write_xlsx(output_df,
           path = file.path(box_file_dir,
                            "College and Career RPP",
                            "1. NSC Dataset",
                            "Mann",
                            "Mann PSD",
                            "Missing List - External",
                            # ⚠️ UPDATE: change to current date and author name
                            # following naming convention
                            "2025-2026 Postsecondary_Paths_Follow_Up_List.xlsx"
           ))

# Confirm the file was exported to Box folder
cat("✅ Export complete:", nrow(output_df), "rows written.\n")

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
# Select & rename columns to match the template ─────────────────────────
#   Template columns: psd_id | first_name | last_name | Notes |
#                     College Enrollment or Career/Vocation |
#                     Teacher/College Counselor | School Notes

output_df <- df %>%
  transmute(
    psd_id                                  = psd_id,
    first_name                              = first_name,
    last_name                               = last_name,
    Notes                                   = Notes,
    `College Enrollment or Career/Vocation` = college_name,   # adjust if needed
    `Teacher/College Counselor`             = NA_character_,   # not in source CSV
    `School Notes`                          = NA_character_,   # not in source CSV
    hs_grad_year                            = hs_grad_year     # used for splitting; dropped later
  )

# Split by graduating year ─

# Treat missing grad year as "Unknown"
output_df$hs_grad_year<-as.character(output_df$hs_grad_year)

output_df <- output_df %>%
  mutate(hs_grad_year = if_else(is.na(hs_grad_year) | hs_grad_year == "",
                                "Unknown", hs_grad_year))

year_groups <- split(output_df, output_df$hs_grad_year)

# Sort tabs chronologically (Unknown goes last)
tab_order <- sort(names(year_groups))
unknown_idx <- which(tab_order == "Unknown")
if (length(unknown_idx) > 0) {
  tab_order <- c(tab_order[-unknown_idx], "Unknown")
}

# Define styles 

header_style <- createStyle(
  fontName      = "Arial",
  fontSize      = 11,
  fontColour    = "#FFFFFF",
  fgFill        = "#2F5496",      # dark blue header
  halign        = "CENTER",
  textDecoration = "bold",
  border        = "Bottom",
  borderColour  = "#FFFFFF",
  wrapText      = TRUE
)

row_style <- createStyle(
  fontName = "Arial",
  fontSize = 10
)

alt_row_style <- createStyle(
  fontName = "Arial",
  fontSize = 10,
  fgFill   = "#EEF2FA"            # light blue stripe
)

## -----------------------------------------------------------------------------
## Part 2 - Write workbook
## -----------------------------------------------------------------------------

wb <- createWorkbook()

# Columns to write (drop the hs_grad_year helper column)
final_cols <- c("psd_id", "first_name", "last_name", "Notes",
                "College Enrollment or Career/Vocation",
                "Teacher/College Counselor", "School Notes")

col_widths <- c(18, 14, 14, 55, 35, 25, 35)   # approximate widths per column

for (yr in tab_order) {
  
  sheet_data <- year_groups[[yr]] %>%
    select(all_of(final_cols))
  
  addWorksheet(wb, sheetName = yr)
  
  # Write header + data
  writeData(wb, sheet = yr, x = sheet_data, startRow = 1, startCol = 1,
            headerStyle = header_style, borders = "all",
            borderColour = "#C9C9C9")
  
  n_rows <- nrow(sheet_data)
  
  # Body font
  if (n_rows > 0) {
    addStyle(wb, sheet = yr, style = row_style,
             rows = 2:(n_rows + 1), cols = 1:length(final_cols),
             gridExpand = TRUE)
  }
  
  # Alternating row shading
  if (n_rows > 1) {
    even_rows <- seq(3, n_rows + 1, by = 2)
    if (length(even_rows) > 0) {
      addStyle(wb, sheet = yr, style = alt_row_style,
               rows = even_rows, cols = 1:length(final_cols),
               gridExpand = TRUE, stack = TRUE)
    }
  }
  
  # Column widths
  setColWidths(wb, sheet = yr, cols = 1:length(final_cols), widths = col_widths)
  
  # Freeze the header row
  freezePane(wb, sheet = yr, firstRow = TRUE)
}

## -----------------------------------------------------------------------------
## Part 3 - Export Data
## -----------------------------------------------------------------------------

output_path<-file.path(code_file_dir,
                       "2025-2026 Postsecondary_Paths_Follow_Up_List.xlsx")

saveWorkbook(wb, file = output_path, overwrite = TRUE)
message("✓ Written to: ", output_path)

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
