################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 02-create-missing-list-from-psd.R >
## [ AUTH ] < Ariana Dimagiba / aridimagiba >
## [ INIT ] < 03/25/2026, updated 08/25/2025 >
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

## -----------------------------------------------------------------------------
## Part 1 - Create Missing List Dataframe
## -----------------------------------------------------------------------------

#1 Use the clean "nsc_data" df from "01-merge" script to confirm range of cohorts 
nsc_data %>%
  filter(!is.na(hs_grad_year)) %>%
  summarise(
    min_year = min(hs_grad_year),
    max_year = max(hs_grad_year),
    n_hs_grad_years = n_distinct(hs_grad_year)
  )

#2 Create "missing" df to identify students who did not show up in the new data pull
#Use master_stu_list and nsc_data to see who don't have records
missing_df <- master_stu_list %>%
  anti_join(nsc_data, by = "psd_id")

# data summary check
missing_df %>%
  summarise(
    n_missing = n(),
    pct_missing = n() / nrow(master_stu_list)
  )

#check for duplicates in NSC
nsc_data %>%
  count(psd_id) %>%
  filter(n > 1)

#check for missing IDs
nsc_data %>%
  count(psd_id) %>%
  filter(n > 1)

#3 Filter students who have graduated from a 4-year college
completed_4yr_college 

#2 Identify students who we should should be stop tracking
#3 

missing_fall<- select(fall_nsc, student_id,college_name)
missing_fall<- right_join(missing_fall, missing_master, by = "student_id") %>%
  filter(is.na(college_name))

names(missing_fall)
names(completed_fouryear)

missing_psd<-left_join(missing_fall, completed_fouryear, by = "student_id") %>%
  filter(is.na(degree_title))

write.xlsx(missing_psd,
           file = "missing_april2025_dimagiba.xlsx")

#read "psd_missing_nov2021.xlsx" to have a basic structure in adding in the 
#missing dataframe

psd_missing <- psd_missing_func("psd_dimagiba_march2025.xlsx")

#write out missing df as an excel file
write.xlsx(psd_missing,
           file = "psd_missing_april2025_dimagiba.xlsx") #has only nsc data from may2022


write.csv(final_psd, "psd_dimagiba_march2025.xlsx")
