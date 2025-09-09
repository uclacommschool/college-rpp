################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < 02-merge-missing-data.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 9/4/25 >
##
################################################################################

#Goal: Add the cleaned missing data into the PSD.

#Script will add these 3 datasets together:

#1.The cleaned missing list.
#2.The Missing List containing all the "Stop Tracking Folks"
#3.The 2024 Missing List Ariana already cleaned.
#4.The existing PSD.

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(readxl)
library(googlesheets4)
library(janitor)
library(data.table)
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

#The 2024 Missing List Ariana already cleaned.
psd_missing_24_ariana<-read_excel(file.path(box_file_dir,"Postsecondary Database",
                            "UCLA Community School PSD", "UCLACS Follow Up",
                            "2024-2025","psd_missing merge_april2025_dimagiba.xlsx"))


#PSD
psd<-read_excel(file.path(box_file_dir,"Postsecondary Database",
                                     "UCLA Community School PSD", 
                                     "29aug2025-psd-dimagiba.xlsx"))

#The Missing List containing all the "Stop Tracking Folks"
psd_stop_track_missing<-read_excel(file.path(box_file_dir,"Postsecondary Database",
                                             "UCLA Community School PSD", "UCLACS Follow Up",
                                             "2024-2025","missing_april2025_dimagiba.xlsx"))

#cleaned_missing
psd_missing<-fread(file.path(box_file_dir,"Postsecondary Database",
                    "UCLA Community School PSD", "UCLACS Follow Up",
                    "missing_list_jy_draft_ad_edits.csv"))

## -----------------------------------------------------------------------------
## Part 1 - Clean the clean_missing file
## -----------------------------------------------------------------------------

#remove unnecessary variables
psd_missing<-psd_missing %>% 
  select(-c(V1,cohort, first_name_sm,last_name_sm,
  ariana_notes, college_enrollment_or_career_vocation,
  teacher_college_counselor, notes_sm, college_code_sch,
  college_state_sch, cc_4year_sch, public_private_sch,system_type_sch))

#check
colnames(psd_missing) == colnames(psd)

#filter out Class of 2024
psd_missing<-psd_missing %>% filter(hs_grad_year != "2024")

#clean dates for hs_grad_date and coll_grad_date
psd_missing <-psd_missing %>% mutate(coll_grad_date = mdy(coll_grad_date))

## -----------------------------------------------------------------------------
## Part 1.2 - Clean the stop track students list
## -----------------------------------------------------------------------------

psd_stop_track_missing<-clean_names(psd_stop_track_missing)

#keep only 2025 stop track cases
psd_stop_track_missing<-psd_stop_track_missing %>% 
  filter(ariana_notes == "2025 stop track"|college_enrollment=="2025 stop track"|
           source == "2025 stop track"|source_note == "2025 stop track")

psd_stop_track_missing<-psd_stop_track_missing %>% 
  mutate(hs_grad_year = as.character(hs_grad_year))

#check
check<-psd_stop_track_missing %>% 
  count(ariana_notes, college_enrollment, source,source_note)


#create a psd_template
psd_temp<-psd %>% filter(psd_id %in% psd_stop_track_missing$psd_id)

psd_temp<-psd_temp %>% full_join(psd_stop_track_missing,
                             by = c("student_id", "psd_id",
                                    "first_name", "middle_name",
                                    "last_name", "hs_grad_year",
                                    "hs_diploma", "record_found"))

psd_temp<-psd_temp %>% filter(!is.na(ariana_notes))

psd_temp<-psd_temp %>% select(colnames(psd))

psd_temp<-psd_temp %>% 
  mutate(across(c(name_suffix, record_found, req_return_field,
                  high_school_code, college_state,enrollment_begin,
                  enrollment_end, enrollment_status, he_graduated,
                  coll_grad_date, degree_title, major,college_sequence,
                  program_code), ~ NA))
  
psd_temp<-psd_temp %>% 
  mutate(
    college_code = "MISSING DATA", college_name = "MISSING DATA",
    cc_4year = "MISSING DATA", public_private = "MISSING DATA", 
    status_source = "MISSING DATA",system_type = "MISSING DATA",
    record_year = "2025", record_term = "fall", notes = "2025 stop track"
  )

psd_temp<-psd_temp %>% unique()

## -----------------------------------------------------------------------------
## Part 2.1 - Merge all Data sources together
## -----------------------------------------------------------------------------

full_psd<-rbind(psd, psd_temp, psd_missing_24_ariana, psd_missing) 

#clean full_psd

## -----------------------------------------------------------------------------
## Part 2.2 - Clean full_psd dataset
## -----------------------------------------------------------------------------

#test<-psd_merge_list %>% filter(record_term == "enrolled at anytime")

#record_term values
record_term_v<-data.frame(full_psd %>% count(record_term))

full_psd<-full_psd %>%
  mutate(record_term = case_when(
    record_term %in% c(record_term_v$record_term[2],
                       "enrolled at anytime",
                       "enrolled at anytime after fall",
                       record_term_v$record_term[5])  ~ "enrolled anytime after fall",
    
    record_term == "NA" ~ NA,
    TRUE ~ record_term
  ))

#check
full_psd %>% count(record_term)

#change the order of this term (Note: this needs to change every term)
full_psd$record_term <- factor(full_psd$record_term,
                                     levels = c("NA", "plans", "winter", "spring", "summer", 
                                                "enrolled anytime after fall", "fall"),
                                     ordered = TRUE)

full_psd$record_year <- factor(full_psd$record_year,
                                     levels = c("NA", str_c(2012:2025)),
                                     ordered = TRUE)

#check
test<-full_psd %>% arrange(record_term)


#clean 
sch_info_list<-full_psd %>% 
  count(college_code, college_name,college_state,
        cc_4year, public_private,system_type)

full_psd<-full_psd %>% 
  mutate(
    college_code = case_when(
      college_name == "CCC" ~ "CCC",
      is.na(college_code) ~ "NA",
      TRUE ~ college_code
    ),
    college_name = case_when(
      college_name == "CALIFORNIA STATE UNIVERSITY - LOS ANGELE" ~ "CALIFORNIA STATE UNIVERSITY - LOS ANGELES",
      college_name == "CALIFORNIA STATE UNIVERSITY - DOMINGUEZ" ~ "CALIFORNIA STATE UNIVERSITY - DOMINGUEZ HILLS",
      college_name == "CALIFORNIA STATE POLYTECHNIC UNIVERSITY, POMONA" ~ "CALIFORNIA STATE POLYTECHNIC UNIVERSITY - POMONA",
      college_name %in% c("CALIFORNIA STATE POLYTECHNIC UNIVERSITY - HUMBOLDT") ~"CALIFORNIA STATE POLYTECHNIC UNIVERSITY, HUMBOLDT",
      college_name %in% c("CALIFORNIA STATE UNIVERSITY- NORTHRIDGE") ~"CALIFORNIA STATE UNIVERSITY - NORTHRIDGE",
      college_name %in% c("MOUNT ST MARY'S COLLEGE","MOUNT ST MARY'S UNIVERSITY") ~"MOUNT SAINT MARY'S UNIVERSITY",
      college_name %in% c("KALAMAZOO UNIVERSITY") ~"KALAMAZOO COLLEGE",
      college_name %in% c("UNIVERSITY OF ALASKA ANCHORAGE") ~"UNIVERSITY OF ALASKA - ANCHORAGE",
      college_code == "NO ENROLLMENT" ~ "NO ENROLLMENT",
      is.na(college_name) ~"NA",
      TRUE ~ college_name
    ),
    college_state = case_when(
      college_name %in% c("CALIFORNIA STATE POLYTECHNIC UNIVERSITY - POMONA",
                          "CALIFORNIA STATE UNIVERSITY - NORTHRIDGE") ~ "CA",
      college_name %in% c("PRINCETON UNIVERSITY") ~ "NJ",
      college_code %in% c("MISSING DATA") ~ "MISSING DATA",
      college_code %in% c("NO ENROLLMENT") ~ "NO ENROLLMENT",
      college_name %in% c("MAPUA INSTITUTE OF TECHNOLOGY") ~ "PHILIPPINES",
      is.na(college_state) ~ "NA",
      TRUE ~ college_state
    ),
    cc_4year = case_when(
      college_name %in% c("CALIFORNIA STATE POLYTECHNIC UNIVERSITY - POMONA",
                          "CALIFORNIA STATE UNIVERSITY - NORTHRIDGE") ~ "4-year",
      college_name %in% c("GLENDALE COMMUNITY COLLEGE") ~ "2-year",
      college_name %in% c("NO ENROLLMENT") ~ "NO ENROLLMENT",
      is.na(cc_4year) ~ "NA",
      TRUE ~ cc_4year
    ),
    public_private = case_when(
      college_name %in% c("CALIFORNIA STATE UNIVERSITY - DOMINGUEZ HILLS",
                          "CALIFORNIA STATE POLYTECHNIC UNIVERSITY - POMONA",
                          "CALIFORNIA STATE UNIVERSITY - NORTHRIDGE") ~ "Public",
      college_name %in% c("NO ENROLLMENT") ~ "NO ENROLLMENT",
      is.na(public_private) ~ "NA",
      TRUE ~ public_private
    ),
    system_type = case_when(
      college_name %in% c("CALIFORNIA STATE POLYTECHNIC UNIVERSITY - POMONA",
                          "CALIFORNIA STATE UNIVERSITY - LOS ANGELES",
                          "CALIFORNIA STATE UNIVERSITY - NORTHRIDGE"
      ) ~ "CSU",
      college_name %in% c("UNIVERSITY OF CALIFORNIA-LOS ANGELES") ~ "UC",
      college_name %in% c("KALAMAZOO COLLEGE") ~ "OUT_4YR",
      college_name %in% c("POMONA COLLEGE", "NATIONAL UNIVERSITY") ~ "INP_NP",
      college_name %in% c("NO ENROLLMENT") ~ "NO ENROLLMENT",
      system_type %in% c("CHAFFEY COLLEGE", "LOS ANGELES HARBOR COLLEGE") ~ "CCC",
      system_type %in% c("SAINT PAUL COLLEGE") ~ "OUT_CC",
      is.na(system_type) ~ "NA",
      TRUE ~ system_type
    )
  )

#change the order of this term (Note: this needs to change every term)
full_psd$record_term <- factor(full_psd$record_term,
                                     levels = c("NA", "plans", "winter", "spring", "summer", 
                                                "enrolled anytime after fall", "fall"),
                                     ordered = TRUE)

full_psd$record_year <- factor(full_psd$record_year,
                                     levels = c("NA", str_c(2012:2025)),
                                     ordered = TRUE)


#arrange group
full_psd<-full_psd %>% arrange(hs_grad_year, psd_id, record_year, record_term)

## -----------------------------------------------------------------------------
## Part 3 - Save and Export Files
## -----------------------------------------------------------------------------

write.csv(full_psd, 
          file.path(box_file_dir,"Postsecondary Database",
                    "UCLA Community School PSD", 
                    "5sept2025-psd-yo.csv"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
