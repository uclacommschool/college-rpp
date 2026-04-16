################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 00 - Update Master Student List >
## [ AUTH ] < Ariana Dimagiba / aridimagiba >
## [ INIT ] < 04/14/2026 >
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

box_file_dir<-file.path("C:/Users/jyo/Box","College Data")

## -----------------------------------------------------------------------------
## load all raw data sets
## -----------------------------------------------------------------------------
#Type in most recent class graduate file name in "nscgradfile.txt"to load graduate file 

# recent_nsc_graduate_file <-read_delim(file.path("..", "nscgradfile.txt"),
#                                       col_names = FALSE) 
# 
# #Type in  master student directory file name "master_studentlist.xlsx to load most recent master student list
# master_stu_list<- read_excel(file.path("..", "master_studentlist.xlsx")) 

recent_nsc_graduate_file <-read_delim(file.path(box_file_dir,
                                                "National Student Clearinghouse",
                                                "UCLACS Graduate & Request Files",
                                                "uclacs_nscgradfile_class2025.txt"),
                                      col_names = FALSE)

#Type in  master student directory file name "master_studentlist.xlsx to load most recent master student list
master_stu_list<- read_excel(file.path(box_file_dir,
                                       "Postsecondary Database",
                                       "UCLA Community School PSD",
                                       "uclacs_master_studentlist_class12-24.xlsx"))

## -----------------------------------------------------------------------------
## Part 1 Clean recent_nsc_graduate_file
## -----------------------------------------------------------------------------

#1 Check columns
ncol(recent_nsc_graduate_file)
glimpse(recent_nsc_graduate_file)
names(recent_nsc_graduate_file)

#2a Create clean_grad_file function

## Guidance: 'clean_grad_file' function selects the data needed to be added 
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


#2b UPDATE Add data frame name and graduation year
clean_grad <- clean_grad_file(recent_nsc_graduate_file, 2025)

#3 Check names for both files
names(clean_grad)
names(master_stu_list)

identical(names(master_stu_list), names(clean_grad))

#if identical returns FALSE identify mismatches
setdiff(names(master_stu_list), names(clean_grad))
setdiff(names(clean_grad), names(master_stu_list))

#4 Bind new cohort list to new master student lists
new_master_student <- bind_rows(master_stu_list, clean_grad) %>%
  distinct()

glimpse(new_master_student)
#5 Write new master csv file ----

# write.csv(
#   new_master_student,
#   file = "master-student-list-rfk-2012-2025.csv",
#   row.names = FALSE
# ) 

write.csv(
  new_master_student,
  file = file.path(".","clean-psd", "master-student-list-rfk-2012-2025.csv"),
  row.names = FALSE
) 
#NOTE: This should eventually be written directly to the box file. 

# NAMING CONVENTION:
# - Rename output file using:
#   "master-student-list-schoolsitename-yearrange.csv"
# - Example:
#   "master-student-list-rfk-2012-2025.csv"