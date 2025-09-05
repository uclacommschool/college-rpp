################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < clean-missing-list.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 9/3/25 >
##
################################################################################

#Goal: Cleans the missing list script. 

#To clean this, first you need to transform the school friendly missing list
#to the R friendly missing list. Then, merge the R friendly missing list 
#to the merge list. Finally, add the merge list to the Post-secondary database.

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(readxl)
library(googlesheets4)
library(janitor)

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

# Opens browser for authentication (only once per session)
gs4_auth()  

## ---------------------------
## load & inspect data
## ---------------------------

#missing list
psd_missing_list_example<-read_excel(file.path(box_file_dir,"Postsecondary Database",
                            "UCLA Community School PSD", "UCLACS Follow Up",
                            "2024-2025","psd_missing merge_april2025_dimagiba.xlsx"))


psd_merge_list<-read_excel(file.path(box_file_dir,"Postsecondary Database",
                                     "UCLA Community School PSD", 
                                     "29aug2025-psd-dimagiba.xlsx"))

## -----------------------------------------------------------------------------
## Part 1.1 - Read in School Facing Missing List
## -----------------------------------------------------------------------------

#Note: only do this once the missing list is finished

#read in the missing file into R
sheet_url<-'https://docs.google.com/spreadsheets/d/1r6BHHnzuELScjRpUYgmk4WwD4acs1CcJjxOswCwDz_U/edit?gid=1289270796#gid=1289270796'
sheets <- sheet_names(sheet_url)

sch_missing_list<-map(sheets,
                      function(x){
                        read_sheet(sheet_url, sheet = x)
                      })

names(sch_missing_list)<-sheets

## -----------------------------------------------------------------------------
## Part 1.2 - Clean School Facing Missing List
## -----------------------------------------------------------------------------

#clean sch_missing_list
sch_missing_list<-map(sch_missing_list, clean_names)

#create updated miss to avoid having to keep communicating with google sheets
sch_missing_list_v2<-sch_missing_list

#make sure columns are the same length and the same

#see column names
map(sheets, function(x){sch_missing_list_v2[[x]] %>% colnames()})

#create column names string
col_names<-c("psd_id","first_name","last_name","ariana_notes",
             "college_enrollment_or_career_vocation",
             "tearcher_college_counselor","notes")

#update each group (Note: may need to do things manually)
sch_missing_list_v2[[1]]<-sch_missing_list[[1]] %>% select(-c(psd_id_8))
colnames(sch_missing_list_v2[[1]])<-col_names

sch_missing_list_v2[[2]]<-sch_missing_list[[2]] %>% select(psd_id, everything())

sch_missing_list_v2[[3]]<-sch_missing_list[[3]] %>% select(-c(psd_id_8))
colnames(sch_missing_list_v2[[3]])<-col_names

sch_missing_list_v2[[4]]<-sch_missing_list[[4]] %>% select(psd_id, everything())

sch_missing_list_v2[[5]]<-sch_missing_list[[5]] %>% select(-c(psd_id_8))
colnames(sch_missing_list_v2[[5]])<-col_names

sch_missing_list_v2[[6]]<-sch_missing_list[[6]] %>% select(-c(psd_id_8))
colnames(sch_missing_list_v2[[6]])<-col_names

sch_missing_list_v2[[7]]<-sch_missing_list[[7]] %>% select(-c(psd_id_9, first_name_3))
colnames(sch_missing_list_v2[[7]])<-col_names

sch_missing_list_v2[[8]]<-sch_missing_list[[8]]

#see column names
map(sheets, function(x){sch_missing_list_v2[[x]] %>% colnames()})

#merge list into one group
sch_missing_list_v2<-bind_rows(sch_missing_list_v2, .id = "cohort")

#filter away any
sch_missing_list_v2<-sch_missing_list_v2 %>% filter(!is.na(psd_id))

#update column names to better differentiate these columns from other 
#columns (add "sm")
colnames(sch_missing_list_v2)<-c("cohort", "psd_id", "first_name_sm",
                                 "last_name_sm","ariana_notes",
                                 "college_enrollment_or_career_vocation",
                                 "teacher_college_counselor","notes_sm")

## -----------------------------------------------------------------------------
## Part 1.3 - Clean the psd_merge_list dataset
## -----------------------------------------------------------------------------

#test<-psd_merge_list %>% filter(record_term == "enrolled at anytime")

#record_term values
record_term_v<-data.frame(psd_merge_list %>% count(record_term))

psd_merge_list<-psd_merge_list %>%
  mutate(record_term = case_when(
  record_term %in% c(record_term_v$record_term[2],
                     "enrolled at anytime",
                     "enrolled at anytime after fall",
                     record_term_v$record_term[5])  ~ "enrolled anytime after fall",
  
  record_term == "NA" ~ NA,
  TRUE ~ record_term
  ))

#check
psd_merge_list %>% count(record_term)
  
#change the order of this term (Note: this needs to change every term)
psd_merge_list$record_term <- factor(psd_merge_list$record_term,
                          levels = c("NA", "plans", "winter", "spring", "summer", 
                                     "enrolled anytime after fall", "fall"),
                          ordered = TRUE)

psd_merge_list$record_year <- factor(psd_merge_list$record_year,
                                     levels = c("NA", str_c(2012:2025)),
                                     ordered = TRUE)

#check
test<-psd_merge_list %>% arrange(record_term)


#clean 
sch_info_list<-psd_merge_list %>% 
  count(college_code, college_name,college_state,
        cc_4year, public_private,system_type)

psd_merge_list<-psd_merge_list %>% 
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

#what do these values mean again:
#CALIFORNIA STATE POLYTECHNIC UNIVERSITY, HUMBOLDT


# sch_info_list<-test %>% count(college_code, college_name,
#                                         college_state, cc_4year, public_private, system_type)  

sch_info_list<-psd_merge_list %>% count(college_code, college_name, college_state,
                              cc_4year, public_private, system_type)  

sch_count<-sch_info_list %>% count(college_name) %>% filter(n>1)

#check
psd_merge_list %>% count(public_private) 
#Goal: make it back to 144 categories

## -----------------------------------------------------------------------------
## Part 2.1 - Transform School Facing List to PSD Friendly List
## -----------------------------------------------------------------------------

#determine missing list
test<-psd_merge_list %>% filter(hs_grad_year == "2024")

#To clean this there are many iterations to do. 

#First, among those identified in the missing list, filter the merged PSD
#to only contain those cases from the previous term.

#We will use some of the information from the previous cases to fill in the
#missing cases.

#identify psd_ids of missing cases
psd_ids_missing<-sch_missing_list_v2$psd_id
psd_ids_missing<-unique(psd_ids_missing)

#filter by those missing cases
previous_term<-psd_merge_list %>% filter(psd_id %in% psd_ids_missing)

#For each psd_id, arrange by record_year and record_term, then pick the last row

previous_term <- previous_term %>%
  group_by(psd_id) %>%
  arrange(record_year, record_term, .by_group = TRUE) %>%   # sort within each student
  slice_tail(n = 1) %>%                       # keep the last row per group
  ungroup()

check<-previous_term %>% count(record_year, record_term)

## -----------------------------------------------------------------------------
## Part 2.2 - Create Intermediate clean dataframe Part 1
## -----------------------------------------------------------------------------

#Merge the previous_term dataset with the missing list
psd_missing_list<-full_join(previous_term, sch_missing_list_v2,
                by = "psd_id")

colnames(psd_missing_list)

#filter out cases that have a record year of 2025
psd_missing_list<-psd_missing_list %>% filter(record_year != "2025")

#Update values from student_id to hs_grade_date
psd_missing_list<-psd_missing_list %>% mutate(
  student_id = case_when(
    is.na(student_id) ~ psd_id,
    TRUE ~ student_id),
  first_name = case_when(
    is.na(first_name) ~ first_name_sm,
    TRUE ~ first_name),
  last_name = case_when(
    is.na(last_name) ~ last_name_sm,
    TRUE ~ last_name),
  record_found = NA,
  req_return_field = NA,
  high_school_code = "051662",
  hs_grad_date = case_when(
    is.na(hs_grad_date) ~ as.Date("2024-06-10"),
    TRUE ~ hs_grad_date
  )
)

## -----------------------------------------------------------------------------
## Part 2.3 - Create Intermediate clean dataframe Part 2
## -----------------------------------------------------------------------------

#Update values from college_code to public_private
#First mark all these values as NA

psd_missing_list<-psd_missing_list %>% 
  mutate(
    college_code = NA,college_name = NA,college_state = NA,
    cc_4year = NA, public_private = NA,
    enrollment_begin = NA, enrollment_end = NA,
    enrollment_status = NA, he_graduated = NA,
    coll_grad_date = NA, degree_title = NA, degree_title = NA,
    degree_title = NA, major = NA, college_sequence = NA,
    program_code = NA, status_source = NA,
   # record_year = "2025", record_term = "enrolled anytime after fall",
    system_type = NA, notes = NA)

#use existing psd_merge_list to create school info list

sch_info_list<-psd_merge_list %>% count(college_code, college_name,
                                        college_state, cc_4year, public_private,
                                        system_type)
#update column order and column names
sch_info_list<-sch_info_list %>% select(college_name, everything())
colnames(sch_info_list)<-c("college_name",
                           str_c(colnames(sch_info_list)[2:length(colnames(sch_info_list))],"_sch"))
sch_info_list<-sch_info_list %>% select(-c(n_sch))

#filter out Duplicate MISSING DATA case
sch_info_list<-sch_info_list %>% 
  filter(!c(college_name == "MISSING DATA" & system_type_sch != "MISSING DATA"))

#update the college_name using college_enrollment_or_career_vocation
psd_missing_list<-psd_missing_list %>% mutate(
  college_name = case_when(
    college_enrollment_or_career_vocation %in% c("CAL POLY POMONA", "Cal Poly Pomona") ~
      "CALIFORNIA STATE POLYTECHNIC UNIVERSITY - POMONA",
    college_enrollment_or_career_vocation %in% 
      c("CAL STATE NORTHRIDGE", "CSUN","CSUN, MAY HAVE GRADUATED","GRADUATED, CSUN in 2025") ~
      "CALIFORNIA STATE UNIVERSITY - NORTHRIDGE",
    college_enrollment_or_career_vocation == "CSU Channel Islands" ~ "CALIFORNIA STATE UNIV CHANNEL ISLANDS",
    college_enrollment_or_career_vocation %in% 
      c("CSUDH","GRADUATED FROM DOMINGUEZ HILLS","TRANSFERRED TO CSUDH") ~
      "CALIFORNIA STATE UNIVERSITY - DOMINGUEZ HILLS",
    college_enrollment_or_career_vocation %in% c("CSULA","CSULA WAS GRADUATING 2025",
                                                 "GRADUATED CSULA","GRADUATED FROM CSULA") ~
      "CALIFORNIA STATE UNIVERSITY - LOS ANGELES",
    college_enrollment_or_career_vocation == "Chico State" ~ "CALIFORNIA STATE UNIVERSITY - CHICO",
    college_enrollment_or_career_vocation == "LACC" ~ "LOS ANGELES CITY COLLEGE",
    college_enrollment_or_career_vocation %in% 
      c("LATTC","GRADUATED FROM LATTC") ~ "LOS ANGELES TRADE TECHNICAL",
    college_enrollment_or_career_vocation == "SMC" ~ "SANTA MONICA COLLEGE",
    college_enrollment_or_career_vocation == "UC Riverside" ~ "UNIVERSITY OF CALIFORNIA - RIVERSIDE",
    college_enrollment_or_career_vocation == "UC Santa Cruz" ~ "UNIVERSITY OF CALIFORNIA-SANTA CRUZ",
    college_enrollment_or_career_vocation == "UCD" ~ "UNIVERSITY OF CALIFORNIA-DAVIS",
    college_enrollment_or_career_vocation %in% 
      c("UCLA","GRADUATED UCLA","UCLA; MAY HAVE GRADUATED") ~ "UNIVERSITY OF CALIFORNIA-LOS ANGELES",
    college_enrollment_or_career_vocation %in% c("UC Riverside", "UCR") ~ "UNIVERSITY OF CALIFORNIA - RIVERSIDE",
    college_enrollment_or_career_vocation == "Woodbury University" ~ "WOODBURY UNIVERSITY",
    college_enrollment_or_career_vocation == "GRADUATED FROM MILLS COLLEGE" ~ "MILLS COLLEGE",
    college_enrollment_or_career_vocation == "Graduated from University of Greenwich" ~ "UNIVERSITY OF GREENWICH",
    college_enrollment_or_career_vocation %in% 
      c("NOT ENROLLED/WORKING","Not Enrolled/Working") ~ "NO ENROLLMENT",
    is.na(college_enrollment_or_career_vocation) ~ "MISSING DATA",

  )
)

check<-psd_missing_list %>% count(college_name, college_enrollment_or_career_vocation)

#Merge sch_info_list with the missing list to get college information
psd_missing_list<-psd_missing_list %>% left_join(sch_info_list, by = "college_name")

#update "college_code","college_state","cc_4year",public_private, system_type 
psd_missing_list<-psd_missing_list %>% mutate(
  college_code = college_code_sch,
  college_state = college_state_sch,
  cc_4year = cc_4year_sch,
  public_private = public_private_sch,
  system_type = system_type_sch
) 
  
#update he_graduated and status_source
psd_missing_list<-psd_missing_list %>% mutate(
  he_graduated = case_when(
    college_enrollment_or_career_vocation %in% 
      c("GRADUATED","GRADUATED CSULA", "GRADUATED FROM COAST GUARD ACADEMY IN 2024",
        "GRADUATED FROM CSULA","GRADUATED FROM DOMINGUEZ HILLS",
        "GRADUATED FROM LATTC","GRADUATED FROM MILLS COLLEGE",
        "GRADUATED UCLA","GRADUATED, CSUN in 2025","Graduated from University of Greenwich",
        "NOT ENROLLED/GRADUATED") ~ "Y",
    is.na(college_name) ~ NA,
    college_name == "MISSING DATA" ~ NA,
    TRUE ~ "N"),
  status_source = "staff"
)

check<-psd_missing_list %>% count(he_graduated, college_name)
check<-psd_missing_list %>% 
  count(he_graduated, college_enrollment_or_career_vocation)  

#update the teacher_college_counselor column
psd_missing_list<-psd_missing_list %>% mutate(
  teacher_college_counselor = case_when(
    is.na(teacher_college_counselor) & !is.na(notes_sm) ~ "Cesare",
    TRUE ~ teacher_college_counselor
  )
)

#update notes column
psd_missing_list<-psd_missing_list %>% mutate(
  notes = case_when(
    is.na(college_name)|(college_name %in% c("MISSING DATA","NO ENROLLMENT")) ~ NA,
    TRUE ~ str_c("Cesare confirmed student is attending ", college_name)
  )
)

#make a record_year = 2024 and record_term = fall version
psd_missing_list1<-psd_missing_list %>% 
  mutate(record_year = "2024", record_term = "fall")


#make a record_year = 2025 and record_term = enrolled anytime after fall version
psd_missing_list2<-psd_missing_list %>% 
  mutate(record_year = "2025", 
         record_term = "enrolled anytime after fall version")

#merge into a clean dataset
clean_data<-rbind(psd_missing_list1, psd_missing_list2) %>% 
  arrange(student_id, record_year)

#note any remaining cases
left_missing_df<-clean_data %>% filter(is.na(college_name))

## -----------------------------------------------------------------------------
## Part 3 - Save and Export Files
## -----------------------------------------------------------------------------

# write.csv(left_missing_df, 
#           file.path(box_file_dir,"Postsecondary Database",
#                     "UCLA Community School PSD", "UCLACS Follow Up",
#                     "unknown_cases_jy.csv"))

#write.csv(data.frame(colnames(psd_merge_list)), "col_list.csv")

write.csv(clean_data, 
          file.path(box_file_dir,"Postsecondary Database",
                    "UCLA Community School PSD", "UCLACS Follow Up",
                    "missing_list_jy_draft.csv"))

# write.csv(previous_term, 
#           file.path(box_file_dir,"Postsecondary Database",
#                     "UCLA Community School PSD", "UCLACS Follow Up",
#                     "previous_term_jy.csv"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
