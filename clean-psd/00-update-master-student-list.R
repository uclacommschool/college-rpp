################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 00 - Update Master Student List >
## [ AUTH ] < Ariana Dimagiba / aridimagiba >
## [ INIT ] < 03/26/2026 >
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

## ---------------------------
## directory paths
## ---------------------------

code_file_dir<-file.path(".")

data_file_dir<-file.path("..","..")
## -----------------------------------------------------------------------------
## load all raw data sets
## -----------------------------------------------------------------------------
recent_nsc_graduate_file <-read_delim(file.path("..", "uclacs_nscgradfile_class2025.txt"),
                                     col_names = FALSE) 
#load most recent class graduate file
master_stu_list<- read_excel(file.path("..", "uclacs_master_studentlist_class12-24.xlsx")) 
#load master student directory file

## -----------------------------------------------------------------------------
## Part 1 Clean recent_nsc_graduate_file
## -----------------------------------------------------------------------------

#1 Check columns
ncol(recent_nsc_graduate_file)
glimpse(recent_nsc_graduate_file)
names(recent_nsc_graduate_file)

#2 Create clean_grad_file function
clean_grad_file <- function(df, grad_year) {
  df %>%
    select(3:5, 10:11, 16:18) %>%
    slice(-1) %>%
    setNames(c(
      "first_name",
      "middle_name",
      "last_name",
      "psd_id",
      "hs_diploma",
      "gender",
      "race_ethnicity",
      "poverty_indicator"
    )) %>%
    mutate(
      hs_grad_year = grad_year,
      student_id = psd_id,
      notes = NA_character_
    ) %>%
    select(
      student_id, first_name, middle_name, last_name, hs_grad_year,
      gender, race_ethnicity, poverty_indicator, hs_diploma, psd_id, notes
    )
}

#2 UPDATE Add data frame name and graduation year
clean_grad <- clean_grad_file(recent_nsc_graduate_file, 2025)

#3 Check names for both files
names(clean_grad)
names(master_stu_list)

identical(names(master_stu_list), names(clean_grad))

#if identicial returns FALSE identifiy mismatches
setdiff(names(master_stu_list), names(clean_grad))
setdiff(names(clean_grad), names(master_stu_list))

#4 Bind new cohort list to new master studnet lists
new_master_student <- bind_rows(master_stu_list, clean_grad) %>%
  distinct()

glimpse(new_master_student)
#5 Write new master csv file ----
write.csv(
  new_master_student,
  file = "master-student-list-rfk-2012-2025.csv",
  row.names = FALSE
  ) 

# NAMING CONVENTION:
# - Rename output file using:
#   "master-student-list-schoolsitename-yearrange.csv"
# - Example:
#   "master-stduent-list-rfk-2012-2025.csv"