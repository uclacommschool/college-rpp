################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 01-merge-nsc-to-psd.R >
## [ AUTH ] < Jeffrey Yo / yjeffrey77, Ariana Dimagiba / aridimagiba >
## [ INIT ] < 4/30/2022, updated 03/25/2026 >
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
#load master student directory file
#load most recent psd file

## -----------------------------------------------------------------------------
##  Part 1 clean nsc data set
## -----------------------------------------------------------------------------

#1a. Manipulating nsc data (clean_names_nsc_data function
nsc_data <-clean_names_nsc_data(nsc_detail_report)

#1b. Check the names of the variables in the nsc data file
names(nsc_data)

#2. Get stu id mutate (stu_id_nsc_data function)
nsc_data <- stu_id_nsc_data(nsc_data)

#3.Check college name duplicates and edit duplicates ----
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
    college_code == "001144-00" ~ "CALIFORNIA STATE POLYTECHNIC UNIVERSITY, POMONA",
    TRUE ~ as.character(college_name)
           )
         ) %>%
   select(
    student_id,  first_name, middle_name, last_name, name_suffix, 
    req_return_field, record_found, high_school_code, hs_grad_date, 
    college_code, college_name2, college_state, cc_4year, public_private,
    enrollment_begin, enrollment_end, enrollment_status, he_graduated, 
    coll_grad_date, degree_title, major, college_sequence,program_code
    ) %>% 
  rename(college_name = college_name2)

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

#5b.Check system type 
system_types <- nsc_data %>% 
  group_by(college_name) %>% 
  summarize(num_names = n(), .groups = "drop") %>%
  arrange(desc(num_names))

print(system_types)
## -----------------------------------------------------------------------------
## Part 2 - check master student list
## -----------------------------------------------------------------------------

#REQUIRED VALIDATION:
# - Confirm that the most recent graduating class is included in the mas_stu_list.
# - This step is REQUIRED before proceeding.

#1 Check available graduation years in the dataset
unique(master_stu_list$hs_grad_year)

# OPTIONAL: count students by graduating class
master_stu_list %>%
  count(hs_grad_year)

# GUIDANCE:
# - Verify that the expected most recent class year (e.g., current senior cohort) appears
# - If the most recent class is missing: 
#   - Check that the correct master student list file was loaded
#   - Confirm the file has been updated with information from the counselor
# - Do NOT proceed until the most recent class is present.

## -----------------------------------------------------------------------------
## Part 3 - merge clean nsc data with master student list
## -----------------------------------------------------------------------------

#6. Merge nsc data with masterlist using merge_nsc_master function
nsc_data<-merge_nsc_master(nsc_data, master_stu_list)
names(nsc_data)

#7. Check merge 
names(nsc_data)

#create object of obs that didn't merge
nsc_data_anti <- nsc_data %>%
  anti_join(master_stu_list, by = "student_id") #assess join
names(nsc_data_anti)
nsc_data_anti %>% select(last_name,first_name,student_id) %>% count(student_id)
nsc_data_anti <- nsc_data_anti %>% filter(record_found == "N")
rm(nsc_data_anti)#nsc_data_antit #remove data frames

## -----------------------------------------------------------------------------
## Part 4 - Select and bind new NSC file records with most recent PSD
## -----------------------------------------------------------------------------

# 8.Count how many NEW NSC enrollment and graduation records to merge into PSD by date
## CODE THAT ALWAYS CHANGES WHEN UPDATING ----

# Check how many NEW college enrollment records
nsc_data %>% 
  filter(record_found == "Y", he_graduated == "N") %>% 
  filter(
    between(
      enrollment_begin, 
      as.Date('YYYY-MM-DD'),  # START DATE: earliest NEW enrollment record date
      as.Date('YYYY-MM-DD')   # END DATE: latest NEW enrollment record date
    )
  ) %>%
  summarise(n_new_enrollment_records = n())

# Check how many NEW graduation records
nsc_data %>% 
  filter(record_found == "Y", he_graduated == "Y") %>% 
  filter(
    between(
      coll_grad_date, 
      as.Date('YYYY-MM-DD'),  # START DATE: earliest NEW graduation record date
      as.Date('YYYY-MM-DD')   # END DATE: latest NEW graduation record date
    )
  ) %>%
  summarise(n_new_graduation_records = n())

# 9. Create new data frames for NEW enrollment and graduation records ----
# Filter NSC enrollment records within the date range of NEW records only
nsc_enrollment_data <- nsc_data %>% 
  filter(
    between(
      enrollment_begin, 
      as.Date('YYYY-MM-DD'),  # START DATE: earliest NEW enrollment record date
      as.Date('YYYY-MM-DD')   # END DATE: latest NEW enrollment record date
    )
  )

# Filter NSC graduation records within the date range of NEW records only
nsc_grads_data <- nsc_data %>%
  filter(
    between(
      coll_grad_date,
      as.Date('YYYY-MM-DD'),  # START DATE: earliest NEW graduation record date
      as.Date('YYYY-MM-DD')   # END DATE: latest NEW graduation record date
    )
  )

# GUIDANCE:
# - Replace 'YYYY-MM-DD' with the actual start and end dates for NEW records only.
# - Do NOT rely only on the min/max dates in the current NSC file.
# - The NSC file may include older records that were already added to PSD.
# - These dates should reflect only the range of NEW records to be added to PSD.
# - Always use YYYY-MM-DD format to avoid parsing errors.

# STEP 1: Identify full date ranges in current NSC file
#   min(nsc_data$enrollment_begin, na.rm = TRUE)
#   max(nsc_data$enrollment_begin, na.rm = TRUE)
#   min(nsc_data$coll_grad_date, na.rm = TRUE)
#   max(nsc_data$coll_grad_date, na.rm = TRUE)

# STEP 2: Compare with existing PSD data to determine cutoff
#   max(previous_psd$enrollment_begin, na.rm = TRUE)
#   max(previous_psd$coll_grad_date, na.rm = TRUE)

# STEP 3: Manually set dates
# - Review the NSC file date ranges and compare them with the most recent records already in PSD.
# - Set the START DATE to the earliest record that is truly NEW and not already in PSD.
# - Set the END DATE to the latest relevant new record in the current NSC file.

# OPTIONAL CHECKS:
# Confirm no overlap with PSD enrollment records
# nsc_enrollment_data %>%
#   filter(enrollment_begin <= max(previous_psd$enrollment_begin, na.rm = TRUE))
# Should return 0 rows, or only expected cases if overlap is intentional.

# Confirm no overlap with PSD graduation records
# nsc_grads_data %>%
#   filter(coll_grad_date <= max(previous_psd$coll_grad_date, na.rm = TRUE))
# Should return 0 rows, or only expected cases if overlap is intentional.

# Confirm resulting date ranges
# range(nsc_enrollment_data$enrollment_begin, na.rm = TRUE)
# range(nsc_grads_data$coll_grad_date, na.rm = TRUE)

## -----------------------------------------------------------------------------
## Part 5 - Select and bind new NSC file records with most recent PSD
## -----------------------------------------------------------------------------

#10. Load and cleans most recent psd from previous session ----
psd_data<-psd_data_clean(previous_psd)  
names(psd_data)
#psd_data <-left_join(psd_data, psd_ids, by = "student_id") 
#In January 2024 created psd specific id to differentiate 

#11. Bind to enrollment and graduation records to most up-to-date PSD ----
names(psd_data)
names(nsc_enrollment_data)
names(nsc_grads_data)

#12. Sort by consistency and readbility ----
psd_data_nsc_only <- psd_data_nsc_only %>%
  arrange(hs_grad_date,last_name, first_name, middle_name, enrollment_begin)

#13. Write new psd csv file ----
write.csv(psd_data_nsc_only,file = "new-psd-file.csv") 

# NAMING CONVENTION:
# - Rename output file using:
#   "DDmonthYYYY-schoolsitename-psd-authorfamilyname.csv"
# - Example:
#   "10april2026-rfk-psd-dimagiba.csv"