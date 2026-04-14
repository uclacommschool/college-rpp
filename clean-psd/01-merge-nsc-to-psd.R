################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 01-merge-nsc-to-psd.R >
## [ AUTH ] < Jeffrey Yo / yjeffrey77, Ariana Dimagiba / aridimagiba >
## [ INIT ] < 4/30/2022, updated 04/14/2026 >
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
## helper functions
## -----------------------------------------------------------------------------

#run the psd_rfk_function_list.R script, which contains all the helper 
#functions to clean and create the NSC data with the existing PSD data.

#use "source" function to run the script: 
source(file.path(".","psd_rfk_function_list.R"))

## -----------------------------------------------------------------------------
## load all raw data sets
## -----------------------------------------------------------------------------

#load new nsc student detail csv file
nsc_detail_report <-read_csv(file.path("..", "10042443_10042443-205673-DETAIL-EFFDT-20251120-RUNDT-20251203.csv"))
#load master student directory file
master_stu_list<- read_csv(file.path("..", "master-student-list-rfk-2012-2025.csv")) 
#load most recent psd file
previous_psd <- read_csv(file.path("..", "5sept2025-psd-yo.csv"))

## -----------------------------------------------------------------------------
##  Part 1 clean nsc data set
## -----------------------------------------------------------------------------

#1a. manipulating nsc data clean_names_nsc_data function
nsc_data <-clean_names_nsc_data(nsc_detail_report)

#1b. check
names(nsc_data)

#2. get stu id mutate (stu_id_nsc_data function)
nsc_data <- stu_id_nsc_data(nsc_data)

#3. Check college name duplicates and edit duplicates ----
college_names <- nsc_data %>% 
  group_by(college_name) %>% 
  summarize(num_names = n(), .groups = "drop") %>%
  arrange(desc(num_names))

print(college_names) 

#4a. Standardize selected college names using college_code
nsc_data<- nsc_data %>%
  mutate(college_name2 = case_when(
    college_code == "001149-00" ~ "CALIFORNIA STATE POLYTECHNIC UNIVERSITY, HUMBOLDT",
    college_code == "001143-00" ~ "CALIFORNIA STATE POLYTECHNIC UNIVERSITY, SAN LUIS OBISPO",
    college_code == "001144-00"~ "CALIFORNIA STATE POLYTECHNIC UNIVERSITY, POMONA",
    TRUE ~ as.character(college_name))) %>%
  select(
    student_id,  first_name, middle_name, last_name, name_suffix, 
    req_return_field, record_found,high_school_code, hs_grad_date, college_code, 
    college_name2, college_state, cc_4year, public_private,enrollment_begin, 
    enrollment_end, enrollment_status, he_graduated, coll_grad_date, 
    degree_title, major, college_sequence,program_code
    ) %>% 
  rename(college_name = 'college_name2')

#Check for new colleges
nsc_data %>% 
  group_by(college_name) %>% 
  summarize(num_names=n()) %>% 
  print( n = 93)  #check for new colleges

#4b. Count only nsc enrollment records not graduation or missing records
str_view(string =nsc_data$enrollment_begin, pattern = '^\\d{4}') #record_year
str_view(string =nsc_data$enrollment_begin,
         pattern = '^(\\d{4})(\\d\\d)(\\d\\d)') #record term

#5a. Run psd_var_nsc function
nsc_data <- psd_var_nsc(nsc_data)
names(nsc_data)

#5b.Check system type if new school that hasn't assigned a system_type
system_types <- nsc_data %>% 
  group_by(system_type) %>% 
  summarize(num_names = n(), .groups = "drop") %>%
  print(n=100)  

## -----------------------------------------------------------------------------
## Part 2 - clean master student list
## -----------------------------------------------------------------------------

#REQUIRED VALIDATION:
# - Confirm that the most recent graduating class is included in the mas_stu_list.
# - This step is REQUIRED before proceeding.

#1. Check available graduation years in the dataset
unique(master_stu_list$hs_grad_year)
names(master_stu_list)

# OPTIONAL: count students by graduating class
master_stu_list %>%
  count(hs_grad_year)

#2. Prepare master list to merge with NSC data
master_stu_df <- master_stu_list %>%
  select(student_id, gender, race_ethnicity, poverty_indicator,hs_diploma,psd_id)

# GUIDANCE:
# - Verify that the expected most recent class year (e.g., current senior cohort) appears
# - If the most recent class is missing: 
#   - Check that the correct master student list file was loaded
#   - Confirm the file has been updated with information from the counselor
# - Do NOT proceed until the most recent class is present.

## -----------------------------------------------------------------------------
## Part 3 - merge clean nsc data with master student list
## -----------------------------------------------------------------------------

#1. Merge nsc data with masterlist using merge_nsc_master function
nsc_data<-merge_nsc_master(nsc_data, master_stu_df)

#2. Check merge for duplicate column names
names(nsc_data)

#create object of obs that didn't merge
nsc_data_anti <- nsc_data %>%
  anti_join(master_stu_df, by = "student_id") #assess join
names(nsc_data_anti)
nsc_data_anti %>% select(last_name,first_name,student_id) %>% count(student_id)
nsc_data_anti <- nsc_data_anti %>% filter(record_found == "N")
rm(nsc_data_anti)#nsc_data_antit #remove data frames

## -----------------------------------------------------------------------------
## Part 4 - Select and bind new NSC file records with most recent PSD
## -----------------------------------------------------------------------------

#1. Count how many NEW NSC enrollment and graduation records to merge into PSD by date

##CODE THAT ALWAYS CHANGES WHEN UPDATING
nsc_enrollment_data<- nsc_data %>% filter(between(enrollment_begin, as.Date('2025-07-08'), as.Date('2025-10-27')))  #filters enrollment records by date
nsc_grads_data<-nsc_data %>%filter(between(coll_grad_date,as.Date('2024-06-18'), as.Date('2025-12-02'))) #filters graduation records by date

## -----------------------------------------------------------------------------
## Part 5 - Select and bind new NSC file records with most recent PSD
## -----------------------------------------------------------------------------

#1. Load and cleans most recent psd from previous session ----
psd_data<-psd_data_clean(previous_psd)  
names(psd_data)

#psd_data <-left_join(psd_data, psd_ids, by = "student_id") 
#In January 2024 created psd specific id to differentiate 

#2. Confirm all dataframes have the same 34 columns
# Compare column names across datasets
names(psd_data)
names(nsc_enrollment_data)
names(nsc_grads_data)

#3. Bind to enrollment and graduation records to most up-to-date PSD ----

psd_data_nsc_only<-bind_rows(
  psd = psd_data,
  enrollment = nsc_enrollment_data,
  graduation = nsc_grads_data
)

psd_data_nsc_only$student_id <- psd_data_nsc_only$student_id[,1]

#4. Sort by consistency and readability ----
psd_data_nsc_only <- psd_data_nsc_only %>%
  arrange(hs_grad_date,last_name, first_name, middle_name, enrollment_begin)

#5. Write new psd csv file ----
write.csv(psd_data_nsc_only,file = "ddmonthYYYY-schoolsitename-psd-name.csv") 

# NAMING CONVENTION:
# - Rename output file using:
#   "DDmonthYYYY-schoolsitename-psd-authorfamilyname.csv"
# - Example:
#   "10april2026-rfk-psd-dimagiba.csv"