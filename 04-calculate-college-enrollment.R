################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < 03-calculate-outcomes.R >
## [ AUTH ] < Ariana Dimagiba >
## [ INIT ] < 9/4/25 >
##
################################################################################

#Goal: Calculates the outcomes of the data.

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(readxl)
library(janitor)
library(data.table)
library(dplyr)
library(lubridate)

## ---------------------------
## directory paths
## ---------------------------

#see current directory
getwd()

#set current directory
code_file_dir<-file.path(".")

data_file_dir<-file.path("..","..")

box_file_dir<-file.path(Sys.getenv("HOME"), "Library", "CloudStorage", "Box-Box", "College Data")

## ---------------------------
## helper functions
## ---------------------------


## ---------------------------
## load & inspect data
## ---------------------------

#cleaned_missing
psd<-fread(file.path(box_file_dir,"Postsecondary Database",
                     "Mann UCLA Community School PSD", 
                     "09sept-2025-mann-psd-dimagiba.csv"))

## -----------------------------------------------------------------------------
## Part 1 - Clean PSD
## -----------------------------------------------------------------------------

df <- psd %>%
  # Step 1: Clean fields and validate college name
  mutate(
    college_name = tolower(trimws(as.character(college_name))),
    college_valid = !(is.na(college_name) | college_name %in% c("no enrollment", "missing data", "", "na")),
    
    enrollment_begin = mdy(as.character(enrollment_begin)),
    hs_grad_date = mdy(as.character(hs_grad_date)),
    
    record_year = as.numeric(as.character(record_year)),
    record_term = tolower(trimws(as.character(record_term))),
    
    has_valid_date = !is.na(enrollment_begin),
    enrolled_by_date = ifelse(has_valid_date, 1, 0),
    
    term_valid = record_term %in% c("fall", "spring", "winter", "summer", "enrolled anytime after fall"),
    year_valid = !is.na(record_year),
    enrolled_by_term = ifelse(term_valid & year_valid & college_valid, 1, 0),
    
    college_enrollment = ifelse(enrolled_by_date == 1 | enrolled_by_term == 1, 1, 0),
    
    # Step 2: Calculate cutoff date (1 year after HS grad)
    one_year_after_grad = hs_grad_date + years(1),
    
    # Step 3: Flag records with valid enrollment within 1 year
    enrolled_within_1yr = ifelse(
      !is.na(enrollment_begin) &
        enrollment_begin >= hs_grad_date &
        enrollment_begin <= one_year_after_grad &
        college_valid,
      1, 0
    )
  )

student_1yr_enrollment <- df %>%
  group_by(student_id, hs_grad_year) %>%
  summarise(
    enrolled_within_1yr = max(enrolled_within_1yr, na.rm = TRUE),
    .groups = "drop"
  )

student_1yr_enrollment %>%
  group_by(hs_grad_year) %>%
  summarise(
    total_students = n(),
    enrolled_within_1yr = sum(enrolled_within_1yr),
    enrollment_rate = round(100 * enrolled_within_1yr / total_students, 1)
  )

