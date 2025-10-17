################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < 03-calculate-outcomes.R >
## [ AUTH ] < Jeffrey Yo >
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

## ---------------------------
## directory paths
## ---------------------------

#see current directory
getwd()

#set current directory
code_file_dir<-file.path(".")

data_file_dir<-file.path("..","..")

box_file_dir<-"C:/Users/jyo/Box/College Data"

## ---------------------------
## helper functions
## ---------------------------


## ---------------------------
## load & inspect data
## ---------------------------

#cleaned_missing
psd<-fread(file.path(box_file_dir,"Postsecondary Database",
                       "UCLA Community School PSD", 
                       "9sept2025-psd-yo.csv"))

## -----------------------------------------------------------------------------
## Part 1 - Clean PSD
## -----------------------------------------------------------------------------

title_types<-psd %>% count(degree_title)

#clean degree title
psd<-psd %>% mutate(
  degree_type = case_when(
    degree_title %in% c("AA TRANSFER","AS TRANSFER",
                        "AS-TRANSFER","ASSOCIATE DEGREE",
                        "ASSOCIATE IN ARTS","ASSOCIATE IN ARTS&SCIENCES-DTA",
                        "ASSOCIATE OF ARTS","ASSOCIATE'S IN ARTS",
                        "Associate Degree") ~ "AA",
    
    str_detect(degree_title, regex("bachelor", ignore_case = TRUE)) ~ "BA",
    degree_title %in% c("B.A.","BACCALAUREATE DEGREE", "BS",
                        "Bachelor of Science","BACELOR OF ARTS") ~ "BA",
    degree_title %in% c("CREDENTIAL") ~ "CREDENTIAL",
    str_detect(degree_title, regex("CERTIFICATE", ignore_case = TRUE)) ~ "CERTIFICATE",
    str_detect(degree_title, regex("MASTER", ignore_case = TRUE)) ~ "ADVANCED",
    degree_title %in% c("PSYC COGNATES") ~ "BA",
    TRUE ~ NA
  )
)

title_types2<-psd %>% count(degree_title, degree_type)

#change into a list
psd_list <- psd %>% split(~hs_grad_year)



college_outcomes<-psd_list[["2017"]] 

class_size<-college_outcomes %>% count(student_id)
class_size<-nrow(class_size)
grad_class<-college_outcomes %>% filter(he_graduated == "Y")


test<-grad_class %>% 
  mutate(completion_year = case_when(
    coll_grad_date >= as.Date("2018-01-01") & coll_grad_date <= as.Date("2021-08-15") ~ "4-year",
    coll_grad_date >= as.Date("2021-08-16") & coll_grad_date <= as.Date("2022-08-15") ~ "5-year",
    coll_grad_date >= as.Date("2022-08-16") & coll_grad_date <= as.Date("2023-08-15") ~ "6-year",
    coll_grad_date >= as.Date("2023-08-16") & coll_grad_date <= as.Date("2024-08-15") ~ "7-year",
    coll_grad_date >= as.Date("2024-08-16") & coll_grad_date <= as.Date("2025-08-15") ~ "8-year",
    (is.na(coll_grad_date) & (as.numeric(record_year) - as.numeric(hs_grad_year))<=4) ~ "4-year",
    (is.na(coll_grad_date) & (as.numeric(record_year) - as.numeric(hs_grad_year))==5) ~ "5-year",
    (is.na(coll_grad_date) & (as.numeric(record_year) - as.numeric(hs_grad_year))==6) ~ "6-year",
    (is.na(coll_grad_date) & (as.numeric(record_year) - as.numeric(hs_grad_year))==7) ~ "7-year",
    (is.na(coll_grad_date) & (as.numeric(record_year) - as.numeric(hs_grad_year))==8) ~ "8-year",
  ))

test2<-test %>% 




#update degree titel
test<-test %>% 
  mutate(
    
    
    
  )



test2<-test %>% group_by(completion_year) %>% 
  summarize(
    n = n(),
    perc = n()/class_size
  )


# %>% group_by(hs_diploma) %>% 
#   summarize(n = n(),
#             perc = (n()/nrow(psd_list[["2023"]]) * 100) %>% round(2))



#fitler he_grad yes, college_grad

create_grad_count_perc<-function(df){
  
  update_df<-df %>% group_by(hs_diploma) %>% 
    summarize(n = n(),
              perc = (n()/nrow(df) * 100) %>% round(2))
  
}

#Complete Certificate
#Complete 2-year
#complete 4-year


college_outcomes<-psd_list[["2023"]] %>% group_by(hs_diploma) %>% 
  summarize(n = n(),
            perc = (n()/nrow(psd_list[["2023"]]) * 100) %>% round(2))
  


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

full_psd<-rbind(psd, psd_temp, psd_missing_24_ariana) 

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
                    "9sept2025-psd-yo.csv"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
