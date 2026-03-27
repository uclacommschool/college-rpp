################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < psd_rfk_function_list.R >
## [ AUTH ] < Jeffrey Yo / yjeffrey77 >
## [ INIT ] < 4/30/2022, updated 02/07/2025 >
##
################################################################################

#Goal: To have a Rscript that only contains the functions used to clean and
# merge the NSC and PSD data.

#By having these functions in a separate script, it will make it easier to  
#modify these functions in the future.  

# This is better than having the functions and the code to clean 
# the PSD files as that would make a very long R script.

################################################################################


## -----------------------------------------------------------------------------
## Part 1 - Create Helper Functions
## -----------------------------------------------------------------------------

# manipulating nsc data ----
#create clean_nsc_data function, which changes variable names of nsc dataset
clean_names_nsc_data<-function(nsc_data){
  # *read in NSC data ----
  #nsc_data<- read_csv(file.path(data_path)) #make sure to change directory to access nsc files
  nsc_data<- clean_names(nsc_data)
  #names(nsc_data)<- tolower(names(nsc_data)) #lower cases variables
  nsc_data <- nsc_data %>% rename(record_found = record_found_y_n, #rename variables with / 
                                  college_code = college_code_branch,
                                  cc_4year = x2_year_4_year,
                                  public_private = public_private,
                                  he_graduated = graduated, #specify higher ed graduation
                                  coll_grad_date = graduation_date,
                                  hs_grad_date = high_school_grad_date,
                                  req_return_field = requester_return_field) 
  return(nsc_data)
}


# **get stu id mutate ----
#Add student id variable using stu_id_nsc_data function

stu_id_nsc_data<-function(nsc_data){
  nsc_data <- nsc_data %>% 
    mutate(student_id = str_match(string = your_unique_identifier,
                                  pattern = "[\\d\\w\\d]+[^[:punct:]]"))
  nsc_data <- select(nsc_data, student_id,  first_name, middle_name, last_name, name_suffix, req_return_field, record_found,
                     high_school_code, hs_grad_date, college_code, college_name, college_state, cc_4year, public_private,
                     enrollment_begin, enrollment_end, enrollment_status, he_graduated, coll_grad_date, degree_title, major, college_sequence,
                     program_code) #select df without nsc id
  nsc_data <- arrange(nsc_data,hs_grad_date,last_name,
                      first_name, middle_name, enrollment_begin)
  return(nsc_data)
}

#clean create new variables using psd_var_nsc function
#function transforms nsc data into a psd df with these variables

#(student_id,first_name, middle_name, last_name, name_suffix, record_found,
#req_return_field, high_school_code,hs_grad_date, college_code, college_name,
#college_state, cc_4year, public_private,enrollment_begin,enrollment_end,
#enrollment_status, he_graduated, coll_grad_date,degree_title, major,
#college_sequence,program_code, status_source, record_year, record_term,
#system_type,hs_grad_year)
psd_var_nsc<-function(nsc_data){
  nsc_data <- nsc_data %>% # *Add psd specific variables----
  mutate(status_source = recode(record_found, "Y" = "NSC")) %>% #status_source
    mutate(hs_grad_year=str_match(string = nsc_data$hs_grad_date, pattern = '^\\d{4}')) %>% #add grad year
    mutate(enrollment_begin_date = ymd(enrollment_begin), # **change strings to dates ----
           enrollment_end_date = ymd(enrollment_end),
           coll_grad_date_date = ymd(coll_grad_date),
           hs_grad_date_date = ymd(hs_grad_date)) %>% 
    mutate(system_type = recode(college_name, "ACADEMY OF ART UNIVERSITY" =	"INP_NP", # ** college_name ---- 
                                "ALLAN HANCOCK COLLEGE" = "CCC",
                                "ANTELOPE VALLEY COLLEGE"	= "CCC",
                                "ARIZONA STATE UNIVERSITY" = "OUT_4YR",
                                "ART CENTER COLLEGE OF DESIGN"	 =	"INP_NP",
                                "BERKELEY CITY COLLEGE" = "CCC",
                                "BUTTE COLLEGE"	= "CCC",
                                "CALIFORNIA STATE POLYTECHNIC UNIVERSITY, SAN LUIS OBISPO"	= "CSU",
                                "CALIFORNIA STATE UNIV CHANNEL ISLANDS"	= "CSU",
                                "CALIFORNIA STATE UNIVERSITY - BAKERSFIEL"	= "CSU",
                                "CALIFORNIA STATE UNIVERSITY - CHICO"	= "CSU",
                                "CALIFORNIA STATE UNIVERSITY - DOMINGUEZ"	= "CSU",
                                "CALIFORNIA STATE UNIVERSITY - EAST BAY"	= "CSU",
                                "CALIFORNIA STATE UNIVERSITY - FULLERTON"	= "CSU",
                                "CALIFORNIA STATE UNIVERSITY - LONG BEACH"	= "CSU",
                                "CALIFORNIA STATE UNIVERSITY - LOS ANGELE"	= "CSU",
                                "CALIFORNIA STATE UNIVERSITY - SACRAMENTO"	= "CSU",
                                "CALIFORNIA STATE UNIVERSITY - SAN BERNAR"	= "CSU",
                                "CALIFORNIA STATE UNIVERSITY - SAN MARCOS"	= "CSU",
                                "CALIFORNIA STATE UNIVERSITY- NORTHRIDGE"	= "CSU",
                                "CALIFORNIA STATE UNIVERSITY - FRESNO" = "CSU",
                                "CALIFORNIA STATE POLYTECHNIC UNIVERSITY, HUMBOLDT" = "CSU",
                                "CALIFORNIA STATE POLYTECHNIC UNIVERSITY, POMONA" = "CSU",
                                "CARNEGIE MELLON UNIVERSITY" = "OUT_4YRP",
                                "CHABOT COLLEGE" = "CCC",
                                "CHAMBERLAIN UNIVERSITY" =	"OUT_FP",
                                "CHAPMAN UNIVERSITY-ORANGE" =	"INP_NP",
                                "CITY COLLEGE OF SAN FRANCISCO"= "CCC",
                                "CITY OF CHICAGO - HAROLD WASHINGTON COLL"= "OUT_CC",
                                "COCONINO COMMUNITY COLLEGE"= "OUT_CC",
                                "COLLEGE OF SOUTHERN IDAHO"= "OUT_CC",
                                "COLLEGE OF THE CANYONS"= "CCC",
                                "COLUMBIA UNIVERSITY"= "OUT_4YRP",
                                "CYPRESS COLLEGE"= "CCC",	
                                "DE ANZA COLLEGE"= "CCC",
                                "EAST LOS ANGELES COLLEGE"= "CCC",
                                "EL CAMINO COLLEGE"= "CCC",
                                "FULLERTON COLLEGE" = "CCC",
                                "GLENDALE COMMUNITY COLLEGE"= "CCC",
                                "IRVINE VALLEY COLLEGE"= "CCC",
                                "KALAMAZOO COLLEGE" = "OUT_4YR",
                                "LASSEN COLLEGE"= "CCC",
                                "LONG BEACH CITY COLLEGE"= "CCC",
                                "LOS ANGELES CITY COLLEGE"= "CCC",
                                "LOS ANGELES SOUTHWEST COLLEGE"= "CCC",
                                "LOS ANGELES TRADE TECHNICAL"= "CCC",
                                "LOS ANGELES VALLEY COLLEGE"= "CCC",
                                "LOS ANGELES PACIFIC UNIVERSITY" = "INP_FP",
                                "MERCED COLLEGE"= "CCC",
                                "MOUNT SAINT MARY'S UNIVERSITY" =	"INP_NP",
                                "MOUNT ST MARY'S UNIVERSITY" = "INP_NP",
                                "MOUNT SAN ANTONIO COLLEGE"= "CCC",
                                "MOUNT ST MARY'S COLLEGE" =	"INP_NP",
                                "NORTHERN ARIZONA UNIVERSITY" = "OUT_4YR",
                                "NORTHWESTERN UNIVERSITY" = "OUT_4YRP",
                                "ORANGE COAST COLLEGE"= "CCC",
                                "OTIS COLLEGE OF ART AND DESIGN" =	"INP_NP",
                                "PALOMAR COLLEGE"= "CCC",
                                "PASADENA CITY COLLEGE"= "CCC",
                                "POMONA COLLEGE" =	"INP_NP",
                                "REGENT UNIVERSITY"= "OUT_4YRP",
                                "SACRAMENTO CITY COLLEGE-LOS RIOS CC DIST"= "CCC",
                                "SAN FRANCISCO STATE UNIVERSITY"	= "CSU",
                                "SAN JOSE STATE UNIVERSITY"	= "CSU",
                                "SANTA MONICA COLLEGE"= "CCC",
                                "SHASTA COLLEGE"= "CCC",
                                "ST. OLAF COLLEGE" = "OUT_4YRP",
                                "SUNY STONY BROOK UNIVERSITY" = "OUT_4YR",
                                "UNIVERSITY OF ALASKA - ANCHORAGE" = "OUT_4YR",
                                "UNIVERSITY OF ALASKA ANCHORAGE" = "OUT_4YR",
                                "UNIVERSITY OF ARIZONA" = "OUT_4YR",
                                "UNIVERSITY OF CALIFORNIA - BERKELEY"= "UC",
                                "UNIVERSITY OF CALIFORNIA - IRVINE"= "UC",
                                "UNIVERSITY OF CALIFORNIA - MERCED"= "UC",
                                "UNIVERSITY OF CALIFORNIA - RIVERSIDE"= "UC",
                                "UNIVERSITY OF CALIFORNIA-DAVIS"= "UC",
                                "UNIVERSITY OF CALIFORNIA-LOS ANGELES"= "UC",
                                "UNIVERSITY OF CALIFORNIA-SAN DIEGO"= "UC",
                                "UNIVERSITY OF CALIFORNIA-SANTA BARBARA"= "UC",
                                "UNIVERSITY OF CALIFORNIA-SANTA CRUZ"= "UC",
                                "UNIVERSITY OF NEVADA LAS VEGAS"	= "OUT_4YR",
                                "UNIVERSITY OF PHOENIX" = "OUT_FP",
                                "UNIVERSITY OF SOUTHERN CALIFORNIA" = "INP_NP",
                                "WELLESLEY COLLEGE"	= "OUT_4YRP",
                                "WESLEYAN UNIVERSITY"	= "OUT_4YRP",
                                "WEST COAST UNIVERSITY- NORTH HOLLYWOOD"	= "INP_FP",
                                "WEST LOS ANGELES COLLEGE"= "CCC",
                                "WHATCOM COMMUNITY COLLEGE"= "NON_CCC",
                                "WILLIAM RAINEY HARPER COLLEGE" = "NON_CCC",
                                "YALE UNIVERSITY"	= "OUT_4YRP",
                                "UNIVERSIDAD POLITECNICA PUERTO RICO" = "OUT_4YR",
                                "UNIVERSITY OF SAN FRANCISCO" = "INP_NP",
                                "UNIVERSITY OF CHICAGO"  = "OUT_4YRP",
                                "BOSTON UNIVERSITY"  = "OUT_4YRP",
                                "FRESNO CITY COLLEGE" = "CCC",
                                "LAMAR STATE COLLEGE - PORT ARTHUR" = "OUT_CC",
                                "UNITED EDUCATION INSTITUTE- HUNTINGTON P" = "INP_FP",
                                "LAKE TAHOE COMMUNITY COLLEGE" = "CCC",
                                "TEXAS STATE TECHNICAL COLLEGE - WACO" = "OUT_CC",
                                "CALIFORNIA STATE POLYTECHNIC UNIVERSITY" = "CSU",
                                "FOOTHILL COLLEGE" = "CCC",
                                "LOS ANGELES MISSION COLLEGE" = "CCC",
                                "CERRITOS COLLEGE" = "CCC",
                                "COASTLINE COMMUNITY COLLEGE" = "CCC",
                                "COLORADO TECHNICAL UNIVERSITY" = "OUT_FP",
                                "GUILFORD TECHNICAL COMMUNITY COLLEGE" = "OUT_CC",
                                "LAMAR INSTITUTE OF TECHNOLOGY" = "OUT_CC",
                                "LOS ANGELES PIERCE COLLEGE" = "CCC",
                                "NORTH HENNEPIN COMMUNITY COLLEGE" = "OUT_CC",
                                "PEPPERDINE UNIVERSITY - ONLINE PSYCHOLOGY" = "INP_NP",
                                "PRINCETON UNIVERSITY" = "OUT_4YRP",
                                "RIO HONDO COLLEGE" = "CCC",
                                "SADDLEBACK COLLEGE" = "CCC",
                                "SANTA BARBARA CITY COLLEGE" = "CCC",
                                "SCRIPPS COLLEGE" = "INP_NP",
                                "SWARTHMORE COLLEGE" = "OUT_4YRP",
                                "UNIVERSITY OF LA VERNE SEM TRADITIONAL" = "INP_NP",
                                "PEPPERDINE UNIVERSITY - ONLINE PSYCHOLOG" = "INP_NP",
                                "AMERICAN PUBLIC UNIVERSITY SYSTEM" = "OUT_FP")) %>% # ** system_type----
  select(student_id,first_name, middle_name, last_name, name_suffix, record_found, req_return_field, high_school_code,
         hs_grad_date_date, college_code, college_name, college_state, cc_4year, public_private,enrollment_begin_date,
         enrollment_end_date,enrollment_status, he_graduated, coll_grad_date_date,degree_title, major, college_sequence,
         program_code,status_source, system_type,hs_grad_year) %>%
    rename(hs_grad_date='hs_grad_date_date', #rename date variables
           enrollment_begin = 'enrollment_begin_date',
           enrollment_end = 'enrollment_end_date',
           coll_grad_date = 'coll_grad_date_date')
  
  #**record_year ----
  nsc_data <- nsc_data %>%
    mutate(enrollment_year=year(enrollment_begin)) #figure out enrollment year
  nsc_data<-nsc_data  %>%
    mutate(coll_grad_year=year(coll_grad_date)) #figure out grad year 
  nsc_data <-nsc_data %>%
    mutate(record_year = if_else(is.na(enrollment_year),
                                 coll_grad_year,
                                 enrollment_year)) #combine enrollment year and grad year to create record year
  nsc_data %>% count(enrollment_year, coll_grad_year, record_year)  #check counts for each
  
  #** record_term ----
  nsc_data <-nsc_data%>% mutate(record_term = case_when(
    month(enrollment_begin) == 8 & month(enrollment_end) == 12 ~ "fall",
    month(enrollment_begin) == 9 & month(enrollment_end) == 12 ~ "fall",
    month(enrollment_begin) == 10 & month(enrollment_end) == 12 ~ "fall",
    month(enrollment_begin) == 7 & month(enrollment_end) == 11 ~ "fall",
    month(enrollment_begin) == 8 & month(enrollment_end) == 9 & system_type == "UC" ~ "summer",
    month(enrollment_begin) == 8 & month(enrollment_end) == 9 & system_type != "UC" ~ "fall",
    month(enrollment_begin) == 8 & month(enrollment_end) == 8  ~ "fall",
    month(enrollment_begin) == 8 & month(enrollment_end) == 8  ~ "fall",
    month(enrollment_begin) == 8 & month(enrollment_end) == 11  ~ "fall",
    month(enrollment_begin) == 8 & month(enrollment_end) == 10  ~ "fall",
    month(enrollment_begin) >= 9 & month(enrollment_end) <= 11  ~ "fall",
    month(enrollment_begin) >= 8 & month(enrollment_end) <= 3  ~ "winter",
    month(enrollment_begin) == 1 & month(enrollment_end) <= 4 ~ "winter",
    month(enrollment_begin) == 1 & month(enrollment_end) == 5 ~ "spring",
    month(enrollment_begin) >= 2 & month(enrollment_end) <= 5 ~ "spring",
    month(enrollment_begin) <= 4 & month(enrollment_end) == 6 ~ "spring",
    month(enrollment_begin) >= 5 & month(enrollment_end) <= 9 ~ "summer",
    month(coll_grad_date) %in% 7:9  & he_graduated  == "Y"  ~ "summer",
    month(coll_grad_date) %in% 10:12  & he_graduated  == "Y" ~ "fall",
    month(coll_grad_date) %in% 1:4  & he_graduated  == "Y" ~ "winter",
    month(coll_grad_date) %in% 5:6  & he_graduated  == "Y"  ~ "spring"))
  
  #final nsc data with added variables. missing demographics ----     
  nsc_data<- select(nsc_data, student_id,first_name, middle_name, last_name, name_suffix, record_found, req_return_field, high_school_code,
                    hs_grad_date, college_code, college_name, college_state, cc_4year, public_private,enrollment_begin,
                    enrollment_end,enrollment_status, he_graduated, coll_grad_date,degree_title, major, college_sequence,
                    program_code, status_source, record_year, record_term,system_type,hs_grad_year) 
  
  return(nsc_data)
}

#function master_file loads masterlist
master_file<-function(data_path_master){
  masterlist<- read_excel(file.path(data_path_master))
  master_stu_list <- select(masterlist, student_id, gender, race_ethnicity,
                            poverty_indicator, hs_diploma, notes, psd_id,)
  #relocate(psd_id, .before = "first_name" )
  return(master_stu_list)
}

#function merge_nsc_master merges nsc data with masterlist
merge_nsc_master<-function(nsc_data, master_data){
  # psd with student demos----
  merge_data<- inner_join(nsc_data, master_data,  by = "student_id") %>%
  #relocate(psd_id, .before = "first_name" )
  return(merge_data)
}

#function psd_data_clean loads and cleans most recent psd from previous session
psd_data_clean<-function(psd_data){
  #psd_data <- read_excel(file.path(psd_file_path))
  psd_data <- data.frame(psd_data)
  # **change strings to dates ----
  test <- psd_data %>% mutate(enrollment_begin_date = mdy(enrollment_begin), 
                              enrollment_end_date = mdy(enrollment_end),
                              coll_grad_date_date = mdy(coll_grad_date),
                              hs_grad_date_date = mdy(hs_grad_date)) 
  attributes(test$enrollment_begin_date)
  test <- test %>% select("student_id","first_name","middle_name","last_name","name_suffix",      
                          "record_found","req_return_field" , "high_school_code","hs_grad_date_date", "college_code" ,    
                          "college_name","college_state","cc_4year", "public_private","enrollment_begin_date",
                          "enrollment_end_date","enrollment_status","he_graduated" ,"coll_grad_date_date", "degree_title",     
                          "major" ,"college_sequence" , "program_code" ,"status_source","record_year" ,"record_term" ,
                          "system_type", "hs_grad_year","gender", "race_ethnicity", 
                          "poverty_indicator" ,"hs_diploma","notes", "psd_id") 
  
  psd_data <-  test %>%rename(hs_grad_date ='hs_grad_date_date', #rename date variables
                              enrollment_begin = 'enrollment_begin_date',
                              enrollment_end = 'enrollment_end_date',
                              coll_grad_date = 'coll_grad_date_date')
  return(psd_data)
}

## -----------------------------------------------------------------------------
## Part 3 - Create missing Dataframe function
## -----------------------------------------------------------------------------

psd_missing_func <-function(missing_data_file_path){
  
  psd_missing <- read_excel(missing_data_file_path)
  
  #add columns to make dataframe match "psd_data_nsc_only" dataframe
  psd_missing <- psd_missing %>% mutate(
    student_id = NA, first_name = NA, middle_name = NA,
    last_name = NA, name_suffix = NA, record_found = NA, 
    req_return_field = NA, high_school_code = NA, hs_grad_date = NA,
    college_code = NA, college_name = NA, college_state = NA,    
    cc_4year = NA, public_private = NA, enrollment_begin = NA, 
    enrollment_end = NA, enrollment_status = NA, he_graduated = NA,     
    coll_grad_date = NA, degree_title = NA, major = NA,            
    college_sequence = NA, program_code = NA, status_source = NA,    
    record_year = NA, record_term = NA, system_type = NA,
    hs_grad_year = NA, gender = NA, race_ethnicity = NA, poverty_indicator = NA,
    hs_diploma = NA, notes = NA, psd_id = NA)
  
  #arrange columns to match "psd_data_nsc_only" dataframe
  col_order <- names(psd_data_nsc_only)
  psd_missing <- psd_missing[, col_order]
  
  return(psd_missing)
  }

## -----------------------------------------------------------------------------
## Part 4 - Reference Functions
## -----------------------------------------------------------------------------

#To check and reference the excel sheet with the psd_may2022_nsc_only
#dataframe, use the following functions:

#check_person - creates dataframe filtered by the person's first and last names
check_person <- function(data, firstname, lastname){
  data %>% 
    filter((first_name == firstname) & (last_name == lastname))
}

#check_person_first - creates dataframe filtered by person's first name
check_person_first <- function(data, firstname){
  data %>% 
    filter(first_name == firstname)
}

#check_person_last - creates dataframe filtered by person's last name
check_person_last <- function(data, lastname){
  data %>% 
    filter(last_name == lastname)
}

#check_id - creates dataframe filtered by student id
check_id <- function(data, stu_id){
  data %>% 
    filter(student_id == stu_id)
}


#function master_file loads masterlist
missing_master<-function(data_path_master){
  masterlist<- read_excel(file.path(data_path_master))
  master_stu_list <- select(masterlist,student_id, first_name, middle_name,last_name,hs_grad_year)
  return(master_stu_list)
}

stop_track<-function(data_path_master){
  stop_track<- read_excel(file.path(data_path_master))
  stop_track<- select(stop_track,student_id,notes)
  return(stop_track)
}

#check in the test dataframe, which uses the functions listed here:
# test<-check_id(psd_data_nsc_only, "051896M004")
# test<-check_person_last(psd_data_nsc_only, "PINEDA")
# test<-check_person_first(psd_data_nsc_only, "GUSTAVO")
# test<-check_person(psd_data_nsc_only, "KARL", "PINEDA")
# 
# #creates tables of the first and last names
# table(psd_data_nsc_only$first_name)
# table(psd_data_nsc_only$last_name)
