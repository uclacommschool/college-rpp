################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < create_masters_pathways_df.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 1/14/25 >
##
################################################################################

#Goal: Create Functions and Code to transform the Community School's 
#Postsecondary Database to a Masters Pathway Dataset

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(readxl)
library(openxlsx)
library(janitor)

## ---------------------------
## directory paths
## ---------------------------

#see current directory
getwd()

#set current directory
code_file_dir<-file.path(".")

data_file_dir<-file.path("..","..")

## ---------------------------
## helper functions
## ---------------------------


## ---------------------------
## load & inspect data
## ---------------------------

#pathsway database
psd<-read.csv(file.path(".", "sample_file.csv")) %>% select(-c(X))

#read in access_master dataset
access_master<-read_excel(file.path(".", "access_master.xlsx"),
                          sheet = 'master')
access_master<-clean_names(access_master)

## -----------------------------------------------------------------------------
## Part 1.1 - Create Functions  
## -----------------------------------------------------------------------------

#Create a function that separates datasets for each cohort 

#function that separates one cohort
create_cohort<-function(df, cohort_year){
  
  df_update<-df %>% filter(hs_grad_year == cohort_year)
  return(df_update)
}

#function that separates all cohorts and puts them in separate database for
#each list
create_cohort_data_list<-function(df){

  cohort_years<-as.numeric(sort(unique(df$hs_grad_year)))
  
  df_list<-map(cohort_years,
            function(x){create_cohort(df, x)})  
  
  names(df_list)<-cohort_years
  
  return(df_list)
}

#Create function that identifies college enrollment records to determine 
#if the student enrolled in college between 
#8/15 - 8/14 the following year. If yes, it would take the certain variables 
#to start a new dataset.

# Function to combine rows based on student_id
combine_college_years <- function(df) {
  df %>%
    group_by(student_id, name, cohort_year) %>% # Group by unique student info
    summarise(
      college_year = paste(unique(college_year), collapse = ", "),
      .groups = "drop" # Ungroup after summarising
    )
}

# Function to create process the data by year
filter_college_year <- function(df, start_date, end_date, college_year_name) {
  
  #add college grad date to enrollment end variable
  df<-df %>% mutate(
    enrollment_begin = case_when(
      is.na(enrollment_begin) ~ coll_grad_date,
      TRUE ~ enrollment_begin 
    ) 
  )
  
  # Convert the dates to Date format
  df$enrollment_begin <- as.Date(df$enrollment_begin, format = "%Y-%m-%d")
  df$enrollment_end <- as.Date(df$enrollment_end, format = "%Y-%m-%d")
  #df$coll_grad_date <- as.Date(df$coll_grad_date, format = "%Y-%m-%d")
  
  # Define the range of interest
  range_start <- as.Date(start_date)
  range_end <- as.Date(end_date)
  
  # Filter rows based on enrollment dates
  filtered_df <- df %>%
    filter(
      (enrollment_begin >= range_start & enrollment_begin <= range_end)) %>%
    mutate(college_year = case_when(he_graduated == "Y" ~ "GRADUATED",
                                    TRUE ~ college_name),
      name = str_c(last_name, first_name, sep = ", "),
      cohort_year = hs_grad_year
           ) %>%
    select(student_id, name, cohort_year, college_year) %>%
    distinct()
  
  filtered_df<-combine_college_years(filtered_df)

  #update column names
  colnames(filtered_df)<-c("student_id", "name", "cohort_year",
                           college_year_name)

  return(filtered_df)
}

#Function to create the college track dataframe
create_student_college_track_df<-function(df, cohort_yr){
  
  #create yearly dataframes
  year1<-filter_college_year(df,
                            str_c(cohort_yr, "-08-15"), #2016-08-15
                            str_c((cohort_yr+1),"-08-14"),"college_year1") #2017-08-15
  year2<-filter_college_year(df,
                             str_c((cohort_yr+1), "-08-15"),
                             str_c((cohort_yr+2),"-08-14"),"college_year2")
  year3<-filter_college_year(df,
                             str_c((cohort_yr+2), "-08-15"),
                             str_c((cohort_yr+3),"-08-14"),"college_year3")
  year4<-filter_college_year(df,
                             str_c((cohort_yr+3), "-08-15"),
                             str_c((cohort_yr+4),"-08-14"),"college_year4")
  year5<-filter_college_year(df,
                             str_c((cohort_yr+4), "-08-15"),
                             str_c((cohort_yr+5),"-08-14"),"college_year5")
  year6<-filter_college_year(df,
                             str_c((cohort_yr+5), "-08-15"),
                             str_c((cohort_yr+6),"-08-14"),"college_year6")
  
  #merge datasets
  update_df<-full_join(year1, year2,
                   by = c("student_id", "name", "cohort_year"))
  update_df<-full_join(update_df, year3,
                       by = c("student_id", "name", "cohort_year"))
  update_df<-full_join(update_df, year4,
                       by = c("student_id", "name", "cohort_year"))
  update_df<-full_join(update_df, year5,
                       by = c("student_id", "name", "cohort_year"))
  update_df<-full_join(update_df, year6,
                       by = c("student_id", "name", "cohort_year"))
  return(update_df)
}

#test<-create_student_college_track_df(cohort_list[["2016"]], 2016)

## -----------------------------------------------------------------------------
## Part 1.2 - Create Master Pathways Database  
## -----------------------------------------------------------------------------

#create cohort list
cohort_list<-create_cohort_data_list(psd)

#create college enrollment record list
college_enroll_list<-map(names(cohort_list),
          function(cohort_yr){
            create_student_college_track_df(cohort_list[[cohort_yr]],
                                            as.numeric(cohort_yr))})
names(college_enroll_list)<-names(cohort_list)

#create master pathways dataset
master_pathways_df<-college_enroll_list %>% bind_rows()

## -----------------------------------------------------------------------------
## Part 2.1 - Create Master Pathways Dataset (Preparation)
## -----------------------------------------------------------------------------

#create planned dataset
plans_psd<-psd %>% filter(record_term == "plans") %>% 
  mutate(name = str_c(last_name, first_name, sep = ", "),
          cohort_year = hs_grad_year,
         college_year_plan = college_name) %>%
  select(student_id, name, cohort_year, college_year_plan)

#check for duplicates
plans_count<-plans_psd %>% count(student_id)

#merge planned dataset together
master_pathways_df<-full_join(master_pathways_df,plans_psd,
                                   by = c("student_id", "name", "cohort_year")) %>% 
  select(student_id, name, cohort_year, college_year_plan, everything())

## -----------------------------------------------------------------------------
## Part 2.2 - Create Pathways List 
## -----------------------------------------------------------------------------

#create pathways list
pathways_list<-vector("list", 6)
names(pathways_list)<-c("plan_y1","y1_y2","y2_y3","y3_y4", "y4_y5","y5_y6")

pathways_list[["plan_y1"]]<-master_pathways_df %>% 
  count(college_year_plan, college_year1)

pathways_list[["y1_y2"]]<-master_pathways_df %>% 
  count(college_year1, college_year2)

pathways_list[["y2_y3"]]<-master_pathways_df %>% 
  count(college_year2, college_year3)

pathways_list[["y3_y4"]]<-master_pathways_df %>% 
  count(college_year3, college_year4)

pathways_list[["y4_y5"]]<-master_pathways_df %>% 
  count(college_year4, college_year5)

pathways_list[["y5_y6"]]<-master_pathways_df %>% 
  count(college_year5, college_year6)

## -----------------------------------------------------------------------------
## Part 2.3 - Create School Strings
## -----------------------------------------------------------------------------

csu_string<-str_c("CALIFORNIA STATE","HUMBOLDT STATE UNIVERSITY",
                  "SAN FRANCISCO STATE UNIVERSITY",
                  "SAN JOSE STATE UNIVERSITY",sep="|")

ccc_string<-str_c("COLLEGE|TECHNICAL|UNITED EDUCATION INSTITUTE- HUNTINGTON P",
              "CITY OF CHICAGO - HAROLD WASHINGTON COLL", sep="|")

uc_string<-c("UNIVERSITY OF CALIFORNIA")

out_4yr<-str_c("UNIVERSITY OF NEVADA LAS VEGAS",
               "SUNY STONY BROOK UNIVERSITY",
               "NORTHERN ARIZONA UNIVERSITY",
               "UNIVERSITY OF ALASKA - ANCHORAGE",
               "UNIVERSITY OF ALASKA ANCHORAGE",
               "UNIVERSITY OF ARIZONA", sep="|")

private_string<-str_c("CARNEGIE MELLON UNIVERSITY",
               "UNIVERSITY OF SOUTHERN CALIFORNIA","UNIVERSITY OF CHICAGO",
               "COLUMBIA UNIVERSITY", "UNIVERSITY OF SAN FRANCISCO",
               "YALE UNIVERSITY","ST. OLAF COLLEGE",
               "POMONA COLLEGE","KALAMAZOO COLLEGE","MILLS COLLEGE",
               "CHAPMAN UNIVERSITY-ORANGE","MOUNT SAINT MARY'S UNIVERSITY",
               "WESLEYAN UNIVERSITY","MOUNT ST MARY'S COLLEGE",
               "UNIVERSIDAD POLITECNICA PUERTO RICO",
               "WELLESLEY COLLEGE","OTIS COLLEGE OF ART AND DESIGN",
               "ART CENTER COLLEGE OF DESIGN",
               "WEST COAST UNIVERSITY- NORTH HOLLYWOOD",
               "ACADEMY OF ART UNIVERSITY","BOSTON UNIVERSITY",
               "UNIVERSITY OF PHOENIX","REGENT UNIVERSITY",
               sep="|")

## -----------------------------------------------------------------------------
## Part 3.1 - Add Labels - Plan to Year 1
## -----------------------------------------------------------------------------

pathways_list[["plan_y1"]] <-
  pathways_list[["plan_y1"]] %>% mutate(
  plan_y1 = case_when(
       str_detect(college_year_plan, csu_string) &
       str_detect(college_year1, csu_string) ~ "csuplan to csu1",    
       
       str_detect(college_year_plan, csu_string) &
         is.na(college_year1) ~ "csuplan to miss1", 
       
       str_detect(college_year_plan, csu_string) &
         str_detect(college_year1, ccc_string) &
         !str_detect(college_year1, private_string) ~ "csuplan to ccc1", 
       
       str_detect(college_year_plan, csu_string) &
         str_detect(college_year1, uc_string) ~ "csuplan to uc1", 

       str_detect(college_year_plan, ccc_string) &
         !str_detect(college_year_plan, private_string) &
         str_detect(college_year1, ccc_string) &
         !str_detect(college_year1, private_string) ~ "cccplan to ccc1", 
       
       str_detect(college_year_plan, ccc_string) &
         !str_detect(college_year_plan, private_string) &
         is.na(college_year1) ~ "cccplan to miss1",
       
       str_detect(college_year_plan, uc_string) &
         str_detect(college_year1, uc_string) ~ "ucplan to uc1",
       
       str_detect(college_year_plan, uc_string) &
         is.na(college_year1) ~ "ucplan to miss1",
       
       is.na(college_year_plan) &
         str_detect(college_year1, ccc_string) &
         !str_detect(college_year1, private_string) ~ "missplan to ccc1",
       
       is.na(college_year_plan) &
         str_detect(college_year1, csu_string) ~ "missplan to csu1",
       
       is.na(college_year_plan) &
         str_detect(college_year1, uc_string) ~ "missplan to uc1",
       
       is.na(college_year_plan) &
         str_detect(college_year1, out_4yr) ~ "missplan to OUT4YR1",
       
       str_detect(college_year_plan, private_string) &
         str_detect(college_year1, private_string) ~ "private plan to private1",
       
       str_detect(college_year_plan, out_4yr) &
         str_detect(college_year1, out_4yr) ~ "OUT4YRplan to OUT4YR1",
       
       is.na(college_year_plan) &
         str_detect(college_year1, private_string) ~ "missplan to private1",

       str_detect(college_year_plan, private_string) &
         is.na(college_year1) ~ "private plan to miss1",
       
       str_detect(college_year_plan, ccc_string) &
         !str_detect(college_year_plan, private_string) &
         str_detect(college_year1, out_4yr) ~ "cccplan to OUT4YR1",
       
       (is.na(college_year_plan)|str_detect(college_year_plan, "MISSING DATA")) &
         is.na(college_year1) ~ "missplan to miss1",
       
       str_detect(college_year_plan, "NO PLANS") &
         is.na(college_year1) ~ "noplan to miss1",
       
       str_detect(college_year_plan, "NO ENROLLMENT") &
         is.na(college_year1) ~ "noenrollplan to miss1",
       
       str_detect(college_year_plan, "WORK") &
         is.na(college_year1) ~ "workplan to miss1",
       
       str_detect(college_year_plan, "WORK") &
         str_detect(college_year1, ccc_string) &
         !str_detect(college_year1, private_string) ~ "workplan to ccc1",
       
       str_detect(college_year_plan, "WORK") &
         str_detect(college_year1, csu_string) ~ "workplan to csu1",
       
       str_detect(college_year_plan, csu_string) &
         str_detect(college_year1, private_string) ~ "csuplan to private1",
       
       #NOTE: Added my own categories
       str_detect(college_year_plan, "FOR-PROFIT") &
         is.na(college_year1) ~ "forprofitplan to miss1",
       
       str_detect(college_year_plan, "GAP YEAR") &
         is.na(college_year1) ~ "gapyearplan to miss1",
       
       str_detect(college_year_plan, "MILITARY") &
         is.na(college_year1) ~ "militaryplan to miss1",
       
    TRUE ~ NA
  )
)

#Check
#View(pathways_list[["plan_y1"]])

missing_case<-pathways_list[["plan_y1"]] %>% filter(is.na(plan_y1))

#Duplicates
#CALIFORNIA STATE UNIVERSITY - CHICO, SANTA MONICA COLLEGE 
#CALIFORNIA STATE UNIVERSITY - BAKERSFIEL, CALIFORNIA STATE UNIVERSITY - EAST BAY
#CALIFORNIA STATE UNIVERSITY - LOS ANGELE, LOS ANGELES CITY COLLEGE
#CALIFORNIA STATE UNIVERSITY - LOS ANGELE, SANTA MONICA COLLEGE
#CALIFORNIA STATE UNIVERSITY- NORTHRIDGE, GLENDALE COMMUNITY COLLEGE
#CALIFORNIA STATE UNIVERSITY- NORTHRIDGE, SANTA MONICA COLLEGE
#LOS ANGELES CITY COLLEGE, CALIFORNIA STATE UNIVERSITY- NORTHRIDGE
#SANTA MONICA COLLEGE, CALIFORNIA STATE UNIVERSITY- NORTHRIDGE
#CYPRESS COLLEGE, LOS ANGELES CITY COLLEGE
#SANTA MONICA COLLEGE, GLENDALE COMMUNITY COLLEGE
#UNIVERSITY OF CALIFORNIA - IRVINE, LOS ANGELES CITY COLLEGE
#UNIVERSITY OF CALIFORNIA - MERCED, LOS ANGELES CITY COLLEGE
#SACRAMENTO CITY COLLEGE-LOS RIOS CC DIST, UNIVERSITY OF CALIFORNIA-DAVIS
#SACRAMENTO CITY COLLEGE-LOS RIOS CC DIST, UNIVERSITY OF CALIFORNIA-DAVIS, LOS ANGELES CITY COLLEGE
#UNIVERSITY OF CALIFORNIA-SAN DIEGO, SANTA MONICA COLLEGE
#UNIVERSITY OF SOUTHERN CALIFORNIA, LOS ANGELES CITY COLLEGE, SANTA MONICA COLLEGE
#LOS ANGELES CITY COLLEGE, PASADENA CITY COLLEGE
#LOS ANGELES TRADE TECHNICAL, GLENDALE COMMUNITY COLLEGE
#UNIVERSITY OF CALIFORNIA - MERCED, LOS ANGELES CITY COLLEGE

## -----------------------------------------------------------------------------
## Part 3.2 - Add Labels - Year 1 to Year 2
## -----------------------------------------------------------------------------

#View(pathways_list[["y1_y2"]])

#update year
pathways_list[["y1_y2"]] <-
  pathways_list[["y1_y2"]] %>% mutate(
    y1_y2 = case_when(
      
      is.na(college_year1) &
        str_detect(college_year2, 'GRADUATED') ~ "miss1 to graduate",
      
      str_detect(college_year1, ccc_string) &
        !str_detect(college_year1, private_string) &
        str_detect(college_year2, 'GRADUATED$') ~ "ccc1 to graduate",
      
      str_detect(college_year1, csu_string) &
        str_detect(college_year2, 'GRADUATED$') ~ "csu1 to graduate",
      
      str_detect(college_year1, csu_string) &
        str_detect(college_year2, csu_string) ~ "csu1 to csu2",    
      
      str_detect(college_year1, csu_string) &
        is.na(college_year2) ~ "csu1 to miss2", 
      
      str_detect(college_year1, csu_string) &
        str_detect(college_year2, ccc_string) &
        !str_detect(college_year2, private_string) ~ "csu1 to ccc2", 
      
      str_detect(college_year1, csu_string) &
        str_detect(college_year2, uc_string) ~ "csu1 to uc2", 
      
      str_detect(college_year1, ccc_string) &
        !str_detect(college_year1, private_string) &
        str_detect(college_year2, ccc_string) &
        !str_detect(college_year2, private_string) ~ "ccc1 to ccc2", 
      
      str_detect(college_year1, ccc_string) &
        !str_detect(college_year1, private_string) &
        is.na(college_year2) ~ "ccc1 to miss2",
      
      str_detect(college_year1, uc_string) &
        str_detect(college_year2, uc_string) ~ "uc1 to uc2",
      
      str_detect(college_year1, uc_string) &
        is.na(college_year2) ~ "uc1 to miss2",
      
      is.na(college_year1) &
        str_detect(college_year2, ccc_string) &
        !str_detect(college_year2, private_string) ~ "miss1 to ccc2",
      
      is.na(college_year1) &
        str_detect(college_year2, csu_string) ~ "miss1 to csu2",
      
      is.na(college_year1) &
        str_detect(college_year2, uc_string) ~ "miss1 to uc2",
      
      is.na(college_year1) &
        str_detect(college_year2, out_4yr) ~ "miss1 to OUT4YR2",
      
      str_detect(college_year1, private_string) &
        str_detect(college_year2, private_string) ~ "private1 to private2",
      
      str_detect(college_year1, out_4yr) &
        str_detect(college_year2, out_4yr) ~ "OUT4YR1 to OUT4YR2",
      
      is.na(college_year1) &
        str_detect(college_year2, private_string) ~ "miss1 to private2",
      
      str_detect(college_year1, private_string) &
        is.na(college_year2) ~ "private1 to miss2",
      
      str_detect(college_year1, ccc_string) &
        !str_detect(college_year1, private_string) &
        str_detect(college_year2, out_4yr) ~ "ccc1 to OUT4YR2",
      
      (is.na(college_year1)|str_detect(college_year1, "MISSING DATA")) &
        is.na(college_year2) ~ "miss1 to miss2",
      
      str_detect(college_year1, csu_string) &
        str_detect(college_year2, private_string) ~ "csu1 to private2",
      
      str_detect(college_year1, uc_string) &
        str_detect(college_year2, out_4yr) ~ "uc1 to OUT4YR2",
      
      str_detect(college_year1, uc_string) &
        str_detect(college_year2, ccc_string) &
        !str_detect(college_year2, private_string)~ "uc1 to ccc2",
      
      str_detect(college_year1, uc_string) &
        str_detect(college_year2, csu_string) ~ "uc1 to csu2",
      
      str_detect(college_year1, ccc_string) &
        !str_detect(college_year1, private_string) &
        str_detect(college_year2, 'MISSING DATA') ~ "uc1 to miss2",
      
      str_detect(college_year1, out_4yr) &
        is.na(college_year2) ~ "OUT4YR1 to miss2",
      
      TRUE ~ NA
    )
  )

#check
#View(pathways_list[["y1_y2"]])

missing_case<-pathways_list[["y1_y2"]] %>% filter(is.na(y1_y2))

#duplicates
#CALIFORNIA STATE POLYTECHNIC, IRVINE VALLEY COLLEGE
#CALIFORNIA STATE UNIVERSITY - EAST BAY, PASADENA CITY COLLEGE
#CALIFORNIA STATE UNIVERSITY - CHICO, SANTA MONICA COLLEGE
#CALIFORNIA STATE UNIVERSITY - FULLERTON, CYPRESS COLLEGE
#CALIFORNIA STATE UNIVERSITY - LONG BEACH, LONG BEACH CITY COLLEGE, FRESNO CITY COLLEGE
#CALIFORNIA STATE UNIVERSITY - LONG BEACH, LOS ANGELES CITY COLLEGE
#LOS ANGELES SOUTHWEST COLLEGE, LOS ANGELES CITY COLLEGE
#CALIFORNIA STATE UNIVERSITY - SACRAMENTO, SACRAMENTO CITY COLLEGE-LOS RIOS CC DIST
#CALIFORNIA STATE UNIVERSITY - EAST BAY, CALIFORNIA STATE UNIVERSITY- NORTHRIDGE
#CALIFORNIA STATE UNIVERSITY- NORTHRIDGE, LOS ANGELES CITY COLLEGE
#CALIFORNIA STATE UNIVERSITY- NORTHRIDGE, LOS ANGELES CITY COLLEGE, SANTA MONICA COLLEGE
#CALIFORNIA STATE UNIVERSITY- NORTHRIDGE, SANTA MONICA COLLEGE
#GLENDALE COMMUNITY COLLEGE, LOS ANGELES TRADE TECHNICAL

## -----------------------------------------------------------------------------
## Part 3.3 - Add Labels - Year 2 to Year 3
## -----------------------------------------------------------------------------

#View(pathways_list[["y2_y3"]])

#update year
pathways_list[["y2_y3"]] <-
  pathways_list[["y2_y3"]] %>% mutate(
    y2_y3 = case_when(
      
      is.na(college_year2) &
        str_detect(college_year3, 'GRADUATED') ~ "miss2 to graduate",
      
      str_detect(college_year2, ccc_string) &
        !str_detect(college_year2, private_string) &
        str_detect(college_year3, 'GRADUATED$') ~ "ccc2 to graduate",
      
      str_detect(college_year2, csu_string) &
        str_detect(college_year3, 'GRADUATED$') ~ "csu2 to graduate",
      
      str_detect(college_year2, 'GRADUATED$') &
        is.na(college_year3) ~ "graduate to miss3",
      
      str_detect(college_year2, ccc_string) &
        !str_detect(college_year2, private_string) &
        str_detect(college_year3, csu_string) ~ "ccc2 to csu3",
      
      str_detect(college_year2, ccc_string) &
        !str_detect(college_year2, private_string) &
        str_detect(college_year3, uc_string) ~ "ccc2 to uc3",
      
      str_detect(college_year2, csu_string) &
        str_detect(college_year3, csu_string) ~ "csu2 to csu3",    
      
      str_detect(college_year2, csu_string) &
        is.na(college_year3) ~ "csu2 to miss3", 
      
      str_detect(college_year2, csu_string) &
        str_detect(college_year3, ccc_string) &
        !str_detect(college_year3, private_string) ~ "csu2 to ccc3", 
      
      str_detect(college_year2, csu_string) &
        str_detect(college_year3, uc_string) ~ "csu2 to uc3", 
      
      str_detect(college_year2, ccc_string) &
        !str_detect(college_year2, private_string) &
        str_detect(college_year3, ccc_string) &
        !str_detect(college_year3, private_string) ~ "ccc2 to ccc3", 
      
      str_detect(college_year2, ccc_string) &
        !str_detect(college_year2, private_string) &
        is.na(college_year3) ~ "ccc2 to miss3",
      
      str_detect(college_year2, uc_string) &
        str_detect(college_year3, uc_string) ~ "uc2 to uc3",
      
      str_detect(college_year2, uc_string) &
        is.na(college_year3) ~ "uc2 to miss3",
      
      is.na(college_year2) &
        str_detect(college_year3, ccc_string) &
        !str_detect(college_year3, private_string) ~ "miss2 to ccc3",
      
      is.na(college_year2) &
        str_detect(college_year3, csu_string) ~ "miss2 to csu3",
      
      is.na(college_year2) &
        str_detect(college_year3, uc_string) ~ "miss2 to uc3",
      
      is.na(college_year2) &
        str_detect(college_year3, out_4yr) ~ "miss2 to OUT4YR3",
      
      str_detect(college_year2, private_string) &
        str_detect(college_year3, private_string) ~ "private2 to private3",
      
      str_detect(college_year2, out_4yr) &
        str_detect(college_year3, out_4yr) ~ "OUT4YR2 to OUT4YR3",
      
      is.na(college_year2) &
        str_detect(college_year3, private_string) ~ "miss2 to private3",
      
      str_detect(college_year2, private_string) &
        is.na(college_year3) ~ "private2 to miss3",
      
      str_detect(college_year2, ccc_string) &
        !str_detect(college_year2, private_string) &
        str_detect(college_year3, out_4yr) ~ "ccc2 to OUT4YR3",
      
      (is.na(college_year2)|str_detect(college_year2, "MISSING DATA")) &
        is.na(college_year3) ~ "miss2 to miss3",
      
      str_detect(college_year2, csu_string) &
        str_detect(college_year3, private_string) ~ "csu2 to private3",
      
      str_detect(college_year2, uc_string) &
        str_detect(college_year3, out_4yr) ~ "uc2 to OUT4YR3",
      
      str_detect(college_year2, uc_string) &
        str_detect(college_year3, ccc_string)&
        !str_detect(college_year3, private_string)~ "uc2 to ccc3",
      
      str_detect(college_year2, uc_string) &
        str_detect(college_year3, csu_string) ~ "uc2 to csu3",
      
      str_detect(college_year2, ccc_string) &
        !str_detect(college_year2, private_string) &
        str_detect(college_year3, 'MISSING DATA') ~ "uc2 to miss3",
      
      str_detect(college_year2, out_4yr) &
        is.na(college_year3) ~ "OUT4YR2 to miss3",
      
      TRUE ~ NA
    )
  )

#check
#View(pathways_list[["y2_y3"]])

missing_case<-pathways_list[["y2_y3"]] %>% filter(is.na(y2_y3))

test<-pathways_list[["y2_y3"]] %>% count(college_year2)

#Note: Marked Graduate to Miss pathway as NA
#There are some rows with 2 schools, not sure how to best categorize them
#May need your discretion to clean here.

## -----------------------------------------------------------------------------
## Part 3.4 - Add Labels - Year 3 to Year 4
## -----------------------------------------------------------------------------

#View(pathways_list[["y3_y4"]])

#update year

pathways_list[["y3_y4"]] <-
  pathways_list[["y3_y4"]] %>% mutate(
    y3_y4 = case_when(
      
      is.na(college_year3) &
        str_detect(college_year4, 'GRADUATED') ~ "miss3 to graduate",
      
      str_detect(college_year3, ccc_string) &
        !str_detect(college_year3, private_string) &
        str_detect(college_year4, 'GRADUATED$') ~ "ccc3 to graduate",
      
      str_detect(college_year3, csu_string) &
        str_detect(college_year4, 'GRADUATED$') ~ "csu3 to graduate",
      
      str_detect(college_year3, 'GRADUATED$') &
        is.na(college_year4) ~ "graduate to miss4",
      
      str_detect(college_year3, 'GRADUATED$') &
        str_detect(college_year4, 'GRADUATED$')~ "graduate to graduate",
      
      str_detect(college_year3, ccc_string) &
        !str_detect(college_year3, private_string) &
        str_detect(college_year4, csu_string) ~ "ccc3 to csu4",
      
      str_detect(college_year3, ccc_string) &
        !str_detect(college_year3, private_string) &
        str_detect(college_year4, uc_string) ~ "ccc3 to uc4",
      
      str_detect(college_year3, ccc_string) &
        !str_detect(college_year3, private_string) &
        str_detect(college_year4, private_string) ~ "ccc3 to private4",
      
      str_detect(college_year3, csu_string) &
        str_detect(college_year4, csu_string) ~ "csu3 to csu4",    
      
      str_detect(college_year3, csu_string) &
        is.na(college_year4) ~ "csu3 to miss4", 
      
      str_detect(college_year3, csu_string) &
        str_detect(college_year4, ccc_string) &
        !str_detect(college_year4, private_string) ~ "csu3 to ccc4", 
      
      str_detect(college_year3, csu_string) &
        str_detect(college_year4, uc_string) ~ "csu3 to uc4", 
      
      str_detect(college_year3, ccc_string) &
        !str_detect(college_year3, private_string) &
        str_detect(college_year4, ccc_string) &
        !str_detect(college_year4, private_string) ~ "ccc3 to ccc4", 
      
      str_detect(college_year3, ccc_string) &
        !str_detect(college_year3, private_string) &
        is.na(college_year4) ~ "ccc3 to miss4",
      
      str_detect(college_year3, uc_string) &
        str_detect(college_year4, uc_string) ~ "uc3 to uc4",
      
      str_detect(college_year3, uc_string) &
        is.na(college_year4) ~ "uc3 to miss4",
      
      is.na(college_year3) &
        str_detect(college_year4, ccc_string) &
        !str_detect(college_year4, private_string) ~ "miss3 to ccc4",
      
      is.na(college_year3) &
        str_detect(college_year4, csu_string) ~ "miss3 to csu4",
      
      is.na(college_year3) &
        str_detect(college_year4, uc_string) ~ "miss3 to uc4",
      
      is.na(college_year3) &
        str_detect(college_year4, out_4yr) ~ "miss3 to OUT4YR4",
      
      str_detect(college_year3, private_string) &
        str_detect(college_year4, private_string) ~ "private3 to private4",
      
      str_detect(college_year3, out_4yr) &
        str_detect(college_year4, out_4yr) ~ "OUT4YR3 to OUT4YR4",
      
      is.na(college_year3) &
        str_detect(college_year4, private_string) ~ "miss3 to private4",
      
      str_detect(college_year3, private_string) &
        is.na(college_year4) ~ "private3 to miss4",
      
      str_detect(college_year3, ccc_string) &
        !str_detect(college_year3, private_string) &
        str_detect(college_year4, out_4yr) ~ "ccc3 to OUT4YR4",
      
      (is.na(college_year3)|str_detect(college_year3, "MISSING DATA")) &
        is.na(college_year4) ~ "miss3 to miss4",
      
      str_detect(college_year3, csu_string) &
        str_detect(college_year4, private_string) ~ "csu3 to private4",
      
      str_detect(college_year3, uc_string) &
        str_detect(college_year4, out_4yr) ~ "uc3 to OUT4YR4",
      
      str_detect(college_year3, uc_string) &
        str_detect(college_year4, ccc_string) &
        !str_detect(college_year4, private_string)~ "uc3 to ccc4",
      
      str_detect(college_year3, uc_string) &
        str_detect(college_year4, csu_string) ~ "uc3 to csu4",
      
      str_detect(college_year3, ccc_string) &
        !str_detect(college_year3, private_string) &
        str_detect(college_year4, 'MISSING DATA') ~ "uc3 to miss4",
      
      str_detect(college_year3, out_4yr) &
        is.na(college_year4) ~ "OUT4YR3 to miss4",
      
      TRUE ~ NA
    )
  )

#check
#View(pathways_list[["y3_y4"]])

missing_case<-pathways_list[["y3_y4"]] %>% filter(is.na(y3_y4))

#Note: Marked Graduate to Miss pathway as NA
#There are some rows with 2 schools, not sure how to best categorize them
#May need your discretion to clean here.

## -----------------------------------------------------------------------------
## Part 3.5 - Add Labels - Year 4 to Year 5
## -----------------------------------------------------------------------------

#View(pathways_list[["y4_y5"]])

pathways_list[["y4_y5"]] <-
  pathways_list[["y4_y5"]] %>% mutate(
    y4_y5 = case_when(
      
      is.na(college_year4) &
        str_detect(college_year5, 'GRADUATED') ~ "miss4 to graduate",
      
      str_detect(college_year4, ccc_string) &
        !str_detect(college_year4, private_string) &
        str_detect(college_year5, 'GRADUATED$') ~ "ccc4 to graduate",
      
      str_detect(college_year4, csu_string) &
        str_detect(college_year5, 'GRADUATED$') ~ "csu4 to graduate",
      
      str_detect(college_year4, 'GRADUATED$') &
        is.na(college_year5) ~ "graduate to miss5",
      
      str_detect(college_year4, 'GRADUATED$') &
        str_detect(college_year5, private_string)~ "graduate to private5",
      
      str_detect(college_year4, 'GRADUATED$') &
        str_detect(college_year5, 'GRADUATED$')~ "graduate to graduate",
      
      str_detect(college_year4, uc_string) &
        str_detect(college_year5, 'GRADUATED$')~ "uc4 to graduate",
      
      str_detect(college_year4, ccc_string) &
        !str_detect(college_year4, private_string) &
        str_detect(college_year5, csu_string) ~ "ccc4 to csu5",
      
      str_detect(college_year4, ccc_string) &
        !str_detect(college_year4, private_string) &
        str_detect(college_year5, uc_string) ~ "ccc4 to uc5",
      
      str_detect(college_year4, ccc_string) &
        !str_detect(college_year4, private_string) &
        str_detect(college_year5, private_string) ~ "ccc4 to private5",
      
      str_detect(college_year4, csu_string) &
        str_detect(college_year5, csu_string) ~ "csu4 to csu5",    
      
      str_detect(college_year4, csu_string) &
        is.na(college_year5) ~ "csu4 to miss5", 
      
      str_detect(college_year4, csu_string) &
        str_detect(college_year5, ccc_string) &
        !str_detect(college_year5, private_string) ~ "csu4 to ccc5", 
      
      str_detect(college_year4, csu_string) &
        str_detect(college_year5, uc_string) ~ "csu4 to uc5", 
      
      str_detect(college_year4, ccc_string) &
        !str_detect(college_year4, private_string) &
        str_detect(college_year5, ccc_string) &
        !str_detect(college_year5, private_string) ~ "ccc4 to ccc5", 
      
      str_detect(college_year4, ccc_string) &
        !str_detect(college_year4, private_string) &
        is.na(college_year5) ~ "ccc4 to miss5",
      
      str_detect(college_year4, uc_string) &
        str_detect(college_year5, uc_string) ~ "uc4 to uc5",
      
      str_detect(college_year4, uc_string) &
        is.na(college_year5) ~ "uc4 to miss5",
      
      is.na(college_year4) &
        str_detect(college_year5, ccc_string) &
        !str_detect(college_year5, private_string) ~ "miss4 to ccc5",
      
      is.na(college_year4) &
        str_detect(college_year5, csu_string) ~ "miss4 to csu5",
      
      is.na(college_year4) &
        str_detect(college_year5, uc_string) ~ "miss4 to uc5",
      
      is.na(college_year4) &
        str_detect(college_year5, out_4yr) ~ "miss4 to OUT4YR5",
      
      str_detect(college_year4, private_string) &
        str_detect(college_year5, private_string) ~ "private4 to private5",
      
      str_detect(college_year4, out_4yr) &
        str_detect(college_year5, out_4yr) ~ "OUT4YR4 to OUT4YR5",
      
      is.na(college_year4) &
        str_detect(college_year5, private_string) ~ "miss4 to private5",
      
      str_detect(college_year4, private_string) &
        is.na(college_year5) ~ "private4 to miss5",
      
      str_detect(college_year4, ccc_string) &
        !str_detect(college_year4, private_string) &
        str_detect(college_year5, out_4yr) ~ "ccc4 to OUT4YR5",
      
      (is.na(college_year4)|str_detect(college_year4, "MISSING DATA")) &
        is.na(college_year5) ~ "miss4 to miss5",
      
      str_detect(college_year4, csu_string) &
        str_detect(college_year5, private_string) ~ "csu4 to private5",
      
      str_detect(college_year4, uc_string) &
        str_detect(college_year5, out_4yr) ~ "uc4 to OUT4YR5",
      
      str_detect(college_year4, uc_string) &
        str_detect(college_year5, ccc_string) &
        !str_detect(college_year5, private_string)~ "uc4 to ccc5",
      
      str_detect(college_year4, uc_string) &
        str_detect(college_year5, csu_string) ~ "uc4 to csu5",
      
      str_detect(college_year4, ccc_string) &
        !str_detect(college_year4, private_string) &
        str_detect(college_year5, 'MISSING DATA') ~ "uc4 to miss5",
      
      str_detect(college_year4, out_4yr) &
        is.na(college_year5) ~ "OUT4YR4 to miss5",
      
      TRUE ~ NA
    )
  )

#check
#View(pathways_list[["y4_y5"]])

missing_case<-pathways_list[["y4_y5"]] %>% filter(is.na(y4_y5))

## -----------------------------------------------------------------------------
## Part 3.6 - Add Labels - Year 5 to Year 6
## -----------------------------------------------------------------------------

pathways_list[["y5_y6"]] <-
  pathways_list[["y5_y6"]] %>% mutate(
    y5_y6 = case_when(
      
      is.na(college_year5) &
        str_detect(college_year6, 'GRADUATED') ~ "miss5 to graduate",
      
      str_detect(college_year5, ccc_string) &
        !str_detect(college_year5, private_string) &
        str_detect(college_year6, 'GRADUATED$') ~ "ccc5 to graduate",
      
      str_detect(college_year5, csu_string) &
        str_detect(college_year6, 'GRADUATED$') ~ "csu5 to graduate",
      
      str_detect(college_year5, 'GRADUATED$') &
        is.na(college_year6) ~ "graduate to miss6",
      
      str_detect(college_year5, 'GRADUATED$') &
        str_detect(college_year6, 'GRADUATED$')~ "graduate to graduate",
      
      str_detect(college_year5, 'GRADUATED$') &
        str_detect(college_year6, private_string)~ "graduate to private5",
      
      str_detect(college_year5, uc_string) &
        str_detect(college_year6, 'GRADUATED$')~ "uc5 to graduate",
      
      str_detect(college_year5, ccc_string) &
        !str_detect(college_year5, private_string) &
        str_detect(college_year6, csu_string) ~ "ccc5 to csu6",
      
      str_detect(college_year5, ccc_string) &
        !str_detect(college_year5, private_string) &
        str_detect(college_year6, uc_string) ~ "ccc5 to uc6",
      
      str_detect(college_year5, ccc_string) &
        !str_detect(college_year5, private_string) &
        str_detect(college_year6, private_string) ~ "ccc5 to private6",
      
      str_detect(college_year5, csu_string) &
        str_detect(college_year6, csu_string) ~ "csu5 to csu6",    
      
      str_detect(college_year5, csu_string) &
        is.na(college_year6) ~ "csu5 to miss6", 
      
      str_detect(college_year5, csu_string) &
        str_detect(college_year6, ccc_string) &
        !str_detect(college_year6, private_string) ~ "csu5 to ccc6", 
      
      str_detect(college_year5, csu_string) &
        str_detect(college_year6, uc_string) ~ "csu5 to uc6", 
      
      str_detect(college_year5, ccc_string) &
        !str_detect(college_year5, private_string) &
        str_detect(college_year6, ccc_string) &
        !str_detect(college_year6, private_string) ~ "ccc5 to ccc6", 
      
      str_detect(college_year5, ccc_string) &
        !str_detect(college_year5, private_string) &
        is.na(college_year6) ~ "ccc5 to miss6",
      
      str_detect(college_year5, uc_string) &
        str_detect(college_year6, uc_string) ~ "uc5 to uc6",
      
      str_detect(college_year5, uc_string) &
        is.na(college_year6) ~ "uc5 to miss6",
      
      is.na(college_year5) &
        str_detect(college_year6, ccc_string) &
        !str_detect(college_year6, private_string) ~ "miss5 to ccc6",
      
      is.na(college_year5) &
        str_detect(college_year6, csu_string) ~ "miss5 to csu6",
      
      is.na(college_year5) &
        str_detect(college_year6, uc_string) ~ "miss5 to uc6",
      
      is.na(college_year5) &
        str_detect(college_year6, out_4yr) ~ "miss5 to OUT4YR6",
      
      str_detect(college_year5, private_string) &
        str_detect(college_year6, ccc_string) &
        !str_detect(college_year6, private_string)~ "private5 to ccc6",
      
      str_detect(college_year5, private_string) &
        str_detect(college_year6, private_string) ~ "private5 to private6",
      
      str_detect(college_year5, out_4yr) &
        str_detect(college_year6, out_4yr) ~ "OUT4YR5 to OUT4YR6",
      
      is.na(college_year5) &
        str_detect(college_year6, private_string) ~ "miss5 to private6",
      
      str_detect(college_year5, private_string) &
        is.na(college_year6) ~ "private5 to miss6",
      
      str_detect(college_year5, ccc_string) &
        !str_detect(college_year5, private_string) &
        str_detect(college_year6, out_4yr) ~ "ccc5 to OUT4YR6",
      
      (is.na(college_year5)|str_detect(college_year5, "MISSING DATA")) &
        is.na(college_year6) ~ "miss5 to miss6",
      
      str_detect(college_year5, csu_string) &
        str_detect(college_year6, private_string) ~ "csu5 to private6",
      
      str_detect(college_year5, uc_string) &
        str_detect(college_year6, out_4yr) ~ "uc5 to OUT4YR6",
      
      str_detect(college_year5, uc_string) &
        str_detect(college_year6, ccc_string) &
        !str_detect(college_year6, private_string)~ "uc5 to ccc6",
      
      str_detect(college_year5, uc_string) &
        str_detect(college_year6, csu_string) ~ "uc5 to csu6",
      
      str_detect(college_year5, ccc_string) &
        !str_detect(college_year5, private_string) &
        str_detect(college_year6, 'MISSING DATA') ~ "uc5 to miss6",
      
      str_detect(college_year5, out_4yr) &
        is.na(college_year6) ~ "OUT4YR5 to miss6",
      
      TRUE ~ NA
    )
  )

#check
#View(pathways_list[["y5_y6"]])
missing_case<-pathways_list[["y5_y6"]] %>% filter(is.na(y5_y6))

## -----------------------------------------------------------------------------
## Part 3.7 - Add Labels - Housekeeping
## -----------------------------------------------------------------------------

pathways_list<-map(pathways_list, function(x){
  df_update<-x %>% select(-c(n))
  return(df_update)} )

## -----------------------------------------------------------------------------
## Part 4.1 - Add Nodes to Master Pathways Database
## -----------------------------------------------------------------------------

#String used to go through each list
#str_c(str_c("y",c(1,2))[1], str_c("y",c(1,2))[2], sep = "_")

master_pathways_df<-left_join(master_pathways_df, pathways_list[["plan_y1"]],
                by = c("college_year_plan", "college_year1"))

for(i in 1:5){
  master_pathways_df<-master_pathways_df %>%
    left_join(pathways_list[[str_c(str_c("y",c(i,i+1))[1],
                                   str_c("y",c(i,i+1))[2], sep = "_")]],
              by = str_c("college_year", c(i, i+1)))
  
}

#Add count and filter functions
create_counts_df<-function(df){
  
  count_plan_y1<-df %>% count(plan_y1) %>% rename(path = plan_y1)
  count_y1_y2<-df %>% count(y1_y2) %>% rename(path = y1_y2)
  count_y2_y3<-df %>% count(y2_y3) %>% rename(path = y2_y3)
  count_y3_y4<-df %>% count(y3_y4) %>% rename(path = y3_y4)
  count_y4_y5<-df %>% count(y4_y5) %>% rename(path = y4_y5)
  count_y5_y6<-df %>% count(y5_y6) %>% rename(path = y5_y6)
  
  update_df<-rbind(count_plan_y1, count_y1_y2, count_y2_y3, count_y3_y4,
                   count_y4_y5, count_y5_y6)
  
  return(update_df)
}

filter_by_cohort<-function(df, cohort_num){
  
  df_update<-df %>% filter(cohort_year == cohort_num)
  
  return(df_update)
}


#filter by cohort
master_pathways_df_by_cohort<-map(unique(master_pathways_df$cohort_year),
                                  function(num){
                                    filter_by_cohort(master_pathways_df,num)
                                  })

names(master_pathways_df_by_cohort)<-unique(master_pathways_df$cohort_year)


#create create overall pathway count (all years)
overall_count<-create_counts_df(master_pathways_df) %>% 
  mutate(cohort = "all years") %>% 
  select(cohort, everything())

#create pathway counts by year  
year_counts_list<-map(master_pathways_df_by_cohort, create_counts_df)
year_counts<-bind_rows(year_counts_list, .id = "cohort")

#combine the overall and by year pathway counts together
pathway_counts<-rbind(overall_count, year_counts)

## -----------------------------------------------------------------------------
## Part 5 - Export Data
## -----------------------------------------------------------------------------

write.csv(master_pathways_df, file.path(".", "master_pathway_psd.csv"))
write.csv(pathway_counts, file.path(".", "pathway_counts.csv"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
