################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 03-transform-missing-list.R >
## [ AUTH ] < Jeffrey Yo / yjeffrey77 >
## [ INIT ] < 04/16/2026 >
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
missing_list <- fread(file.path(code_file_dir,
                                "16april2026-rfk-missing-list-test.csv"))


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

output_path<-file.path(code_file_dir,
                       "2025-2026 Postsecondary_Paths_Follow_Up_List.xlsx")

saveWorkbook(wb, file = output_path, overwrite = TRUE)
message("✓ Written to: ", output_path)

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------