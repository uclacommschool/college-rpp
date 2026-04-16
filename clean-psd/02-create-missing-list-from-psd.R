################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 02-create-missing-list-from-psd.R >
## [ AUTH ] < Ariana Dimagiba / aridimagiba >
## [ INIT ] < 03/25/2026, updated 04/15/2026 >
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

#load recently updated psd file from 01-merge script
current_psd <- read_csv(file.path("..", "updated_psd.csv"))
#load recently updated nsc_data df from 01-merge script


## -----------------------------------------------------------------------------
## Part 1 Filter nsc_data df for missing data
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


## -----------------------------------------------------------------------------
## Part 1 - Filter students who not active tracking
## -----------------------------------------------------------------------------
# GUIDANCE 
# - Remove students who have completed a 4-year degree
# - Remove students who we already identfied as Inactive or "stop-track"
# - Remove students who are newly inactive with missing data for three consecutive years

#3 Filter students who have graduated from a 4-year college
completed_4yr_college 

#3 Flag students who have graduated from a 4-year college 
missing_df <- missing_df %>%
  mutate(
    flag_4yr_grad = he_graduated == "Y" & `2yr_4yr` == "4-year"
  )

#4. Flag and filter students were previously assigned to stop tracking using
# _ notes variable in recently updated psd from 01-merge

#5.Flag new students who have been missing for 3 consecutive years as stop-track

#5b. Filter and create a new df for new stop-track students
# - will need to merge them in a later script when you merge missing data to psd



### OLD CODE ###
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