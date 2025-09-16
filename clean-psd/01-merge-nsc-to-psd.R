################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 01-merge-nsc-to-psd.R >
## [ AUTH ] < Jeffrey Yo / yjeffrey77 >
## [ INIT ] < 4/30/2022, updated 08/25/2025 >
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
nsc_detail_report <-read_csv(file.path("..", "10102683hsst_10102683-198894-DETAIL-EFFDT-20250821-RUNDT-20250822.csv"))
master_stu_list<- read_excel(file.path("..", "mann-master-student-list-21-24.xlsx")) 
previous_psd <- read_csv(file.path("..", "09sept-2025-mann-psd-dimagiba.csv"))
## -----------------------------------------------------------------------------
##  Part 1 clean nsc data set
## -----------------------------------------------------------------------------

#1a. manipulating nsc data (clean_names_nsc_data function
nsc_data <-clean_names_nsc_data(nsc_detail_report)

#1b. check
names(nsc_data)

#2. get stu id mutate (stu_id_nsc_data function)
nsc_data <- stu_id_nsc_data(nsc_data)

#3. #check
nsc_data%>% filter(record_found == "Y", he_graduated == "N") %>% count()

#4.
# **check college name duplicates and edit duplicates ----
#count college names to look for duplicates
college_names<- nsc_data %>% group_by(college_name) %>% summarize(num_names=n()) 

nsc_data<- nsc_data %>%
  mutate(college_name2 = case_when(college_code == "001149-00" ~ "CALIFORNIA STATE POLYTECHNIC UNIVERSITY, HUMBOLDT",
                                   college_code == "001143-00" ~ "CALIFORNIA STATE POLYTECHNIC UNIVERSITY, SAN LUIS OBISPO",
                                   college_code == "001144-00"~ "CALIFORNIA STATE POLYTECHNIC UNIVERSITY, POMONA",
                                   TRUE ~ as.character(college_name))) %>%
  select(student_id,  first_name, middle_name, last_name, name_suffix, req_return_field, record_found,
         high_school_code, hs_grad_date, college_code, college_name2, college_state, cc_4year, public_private,
         enrollment_begin, enrollment_end, enrollment_status, he_graduated, coll_grad_date, degree_title, major, college_sequence,
         program_code) %>% 
  rename(college_name = 'college_name2')

nsc_data %>% group_by(college_name) %>% summarize(num_names=n()) %>% print( n = 93)  #check for new colleges

#count only enrollment not graduation or missing
str_view(string =nsc_data$enrollment_begin, pattern = '^\\d{4}') #record_year
str_view(string =nsc_data$enrollment_begin,
         pattern = '^(\\d{4})(\\d\\d)(\\d\\d)') #record term

#5. run psd_var_nsc function
nsc_data <- psd_var_nsc(nsc_data)
names(nsc_data)

#5. check system type 
system_types<- nsc_data %>% group_by(system_type) %>% summarize(num_names=n())  

## -----------------------------------------------------------------------------
## Part 2 - clean master student list
## -----------------------------------------------------------------------------
master_stu_list <- master_file(master_stu_list)
names(master_stu_list)
## -----------------------------------------------------------------------------
## Part 2 - merge clean nsc data with master student list
## -----------------------------------------------------------------------------

#7. merge nsc data with masterlist using merge_nsc_master function
nsc_data<-merge_nsc_master(nsc_data, master_stu_list)
names(nsc_data)
#8. check merge 
names(nsc_data)
#create object of obs that didn't merge
nsc_data_anti <- nsc_data %>%
  anti_join(master_stu_list, by = "student_id") #assess join
names(nsc_data_anti)
nsc_data_anti %>% select(last_name,first_name) %>% count(student_id)
nsc_data_anti <- nsc_data_anti %>% filter(record_found == "N")
rm(nsc_data_anti)#nsc_data_antit #remove data frames

#9.select nsc records to merge into psd by date 

##CODE THAT ALWAYS CHANGES WHEN UPDATING
nsc_enrollment_data<- nsc_data %>% filter(between(enrollment_begin, as.Date('2022-04-01'), as.Date('2023-04-30')))  #filters enrollment records by date
nsc_grads_data<-nsc_data %>%filter(between(coll_grad_date,as.Date('2022-04-21'), as.Date('2025-08-30'))) #filters graduation records by date
##

#10. load and cleans most recent psd from previous session
psd_data<-psd_data_clean(previous_psd)  
names(psd_data)
#psd_data <-left_join(psd_data, psd_ids, by = "student_id") #January 2024 created psd specific id


#11. bind to enrollment term to most up to date psd
names(psd_data)
names(nsc_enrollment_data)
names(nsc_grads_data)

psd_data_nsc_only <- bind_rows(psd_data, nsc_enrollment_data, nsc_grads_data)

#12. sort by graduate date, last name, first name, middle name, enrollment date
psd_data_nsc_only <- psd_data_nsc_only %>%
  arrange(hs_grad_date,last_name, first_name,
          middle_name, enrollment_begin)

#13. write data
write.csv(psd_data_nsc_only,file = "15sept2025-mann-psd-dimagiba.csv") #missing follow updata 2023-2024

#create missing list for class of 2014-2022
#merge master list  with nsc records to find missing students, didnt save code from nov2021.

## -----------------------------------------------------------------------------
## Part 3 - Create missing Dataframe
## -----------------------------------------------------------------------------
missing_master <- read_excel(file.path(".", "uclacs_all_studentlist_2012-2024.xlsx")) %>%
  filter(hs_diploma == "REGULAR DIPLOMA" , hs_grad_year %in% c(2017:2024))

#check for students who completed a 4-year degree. students with an AA or certificate may transfer
completed_fouryear <- psd_2025 %>% filter(he_graduated == "Y", cc_4year == "4-year", hs_grad_year %in% c(2017:2024)) %>%
  filter(degree_title != "NA") %>%
  select(student_id,coll_grad_date,degree_title,major,he_graduated,status_source)



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

## -----------------------------------------------------------------------------
## Part 4 - Manually add missing cases
## -----------------------------------------------------------------------------

#From here manually add the data from the 
#"UCLACS Fall 2021 College Enrollments and Follow Up Data" excel sheet
#using excel

## -----------------------------------------------------------------------------
## Part 5 - Manually add missing cases
## -----------------------------------------------------------------------------

#read in the updated missing dataframe
psd_missing_updated <- read_excel("mergelist20232024.xlsx")

#check to see if they the column names match
names(psd_2025) #jeffrey I added the most up to date psd file name here for you
names(psd_missing_updated) #change df to file where you're keeping the missing student data to merge with psd

#add missing students data from missing data list----
final_psd <- rbind(psd_2025, psd_missing_updated)

#12. sort by graduate date, last name, first name, middle name, enrollment date
final_psd <- final_psd %>%
  arrange(hs_grad_date,last_name, first_name,middle_name, enrollment_begin)

write.xlsx(final_psd, "psd_dimagiba_march2025.xlsx")

## -----------------------------------------------------------------------------
## Part 6 - Other Code
## -----------------------------------------------------------------------------

###trouble shooting for ariana
# ** since there are duplicates in psd_jan2022 file reload psd_jan2022_nsc_only----
psd_jan2022_nsc_only <- read_excel("psd_dimagiba_jan2022_nsc.xlsx")


#add postsecondary plans for class of 2019-2021 to psd

