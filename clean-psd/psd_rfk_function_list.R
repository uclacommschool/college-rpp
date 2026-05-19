################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < psd_rfk_function_list.R >
## [ AUTH ] < Jeffrey Yo / yjeffrey77 >
## [ INIT ] < 4/30/2022, updated 04/15/2026 aridimagiba >
##

################################################################################

#Goal: To have a Rscript that only contains the functions used to clean and
# merge the NSC and PSD data.

#By having these functions in a separate script, it will make it easier to  
#modify these functions in the future.  

# This is better than having the functions and the code to clean 
# the PSD files as that would make a very long R script.

# EXECUTION ORDER (called by 01-merge-nsc-to-psd.R):
# clean_nsc_names() → add_student_id() → add_psd_variables() →
# merge_nsc_master() → assign_column_classes() → parse_dates() →
# check_type() / check_type_mismatch()

## -----------------------------------------------------------------------------
# FUNCTIONS IN THIS FILE:
# Part 1 - NSC Data Cleaning
#   clean_nsc_names()       — standardizes NSC column names
#   add_student_id()        — extracts and adds student ID
#   add_psd_variables()     — adds system_type, record_term, record_year
#
# Part 2 - Merging
#   merge_nsc_master()      — joins NSC data with master student list
#   assign_column_classes() — standardizes column data types
#   parse_dates()           — converts date columns to Date class
#   check_type()            — compares column types across data frames
#   check_type_mismatch()   — surfaces type mismatches before binding
#
# Part 3 - Reference/QA
#   check_person()          — filter by first and last name
#   check_person_first()    — filter by first name
#   check_person_last()     — filter by last name
#   check_id()              — filter by student ID
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Part 1 - NSC Data Cleaning
## -----------------------------------------------------------------------------

# FUNCTION: clean_names_nsc_data
# PURPOSE:  Standardizes NSC column names to PSD naming conventions
# INPUT:    nsc_detail_report — raw NSC StudentTracker detail CSV
# OUTPUT:   nsc_data — with renamed columns matching PSD schema
# CALLED IN: 01-merge-nsc-to-psd.R, Part 1 step 1a

clean_names_nsc_data<-function(nsc_data){
  nsc_data<- clean_names(nsc_data)
  # NOTE: clean_names() from janitor handles lowercase conversion
  # tolower() is redundant here
  nsc_data <- nsc_data %>% rename(record_found = record_found_y_n,
                                  college_code = college_code_branch,
                                  cc_4year = x2_year_4_year,
                                  public_private = public_private,
                                  he_graduated = graduated, 
                                  coll_grad_date = graduation_date,
                                  hs_grad_date = high_school_grad_date,
                                  req_return_field = requester_return_field) 
  return(nsc_data)
}

# FUNCTION: add_student_id
# PURPOSE:  Extracts student ID from NSC unique identifier field
# INPUT:    nsc_data — raw NSC data frame after clean_nsc_names()
# OUTPUT:   nsc_data — with student_id column added, sorted by grad date
# CALLED IN: 01-merge-nsc-to-psd.R, Part 1 step 2

add_student_id<-function(nsc_data){
  nsc_data <- nsc_data %>% 
    mutate(student_id = str_extract(string = your_unique_identifier,
                                  pattern = "[\\d\\w\\d]+[^[:punct:]]"))
  nsc_data <- select(nsc_data, student_id,  first_name, middle_name, last_name, name_suffix, req_return_field, record_found,
                     high_school_code, hs_grad_date, college_code, college_name, college_state, cc_4year, public_private,
                     enrollment_begin, enrollment_end, enrollment_status, he_graduated, coll_grad_date, degree_title, major, college_sequence,
                     program_code) #select df without nsc id
  nsc_data <- arrange(nsc_data,hs_grad_date,last_name,
                      first_name, middle_name, enrollment_begin)
  return(nsc_data)
}


# FUNCTION: add_psd_variables
# PURPOSE:  Creates psd specific variables not part of the nsc_data: status_source,
#           system_type, record_year, and record_term. Additionally, transforms 
#           date strings to ymd dates.
# INPUT:    nsc_data — raw NSC data frame after clean_nsc_names()
# OUTPUT:   nsc_data — with status_source,system_type, record_year, 
#           and record_term columns added. Additionally, transforms 
#           date strings to ymd dates.
# CALLED IN: 01-merge-nsc-to-psd.R, Part 1 step 5

add_psd_variables<-function(nsc_data,institution_lookup){
  nsc_data <- nsc_data %>%
  # status_source values:
  # NSC          — record confirmed by National Student Clearinghouse
  # staff        — record confirmed by school counselor
  # self-reported — student self-reported their enrollment
  # old_psd      — record from pre-2019 database before long format conversion
  # MISSING DATA — status unknown, follow-up needed
  mutate(status_source = recode(record_found, "Y" = "NSC")) %>% 
  # add grad year
  mutate(hs_grad_year=str_match(string = nsc_data$hs_grad_date, pattern = '^\\d{4}')) %>% 
  # Convert date strings to Date class 
  # NSC currently sends dates in YYYYMMDD format
  # Historical format handling (pre-2019) is managed by parse_dates()
    mutate(enrollment_begin_date = ymd(enrollment_begin),
           enrollment_end_date = ymd(enrollment_end),
           coll_grad_date_date = ymd(coll_grad_date),
           hs_grad_date_date = ymd(hs_grad_date)) %>% 
  #join institution (sys_type) attributes using college_code
  left_join(institution_lookup %>% 
          select(college_code,system_type),
          by = "college_code",
          relationship = "many-to-many") %>% 
  select(student_id,first_name, middle_name, last_name, name_suffix, record_found, req_return_field, high_school_code,
         hs_grad_date_date, college_code, college_name, college_state, cc_4year, public_private,enrollment_begin_date,
         enrollment_end_date,enrollment_status, he_graduated, coll_grad_date_date,degree_title, major, college_sequence,
         program_code,status_source, system_type,hs_grad_year) %>%
    rename(hs_grad_date='hs_grad_date_date', #rename date variables
           enrollment_begin = 'enrollment_begin_date',
          enrollment_end = 'enrollment_end_date',
           coll_grad_date = 'coll_grad_date_date')
  #add record_year 
  nsc_data <- nsc_data %>%
    mutate(enrollment_year=year(enrollment_begin)) #figure out enrollment year
  nsc_data<-nsc_data  %>%
    mutate(coll_grad_year=year(coll_grad_date)) #figure out grad year 
  nsc_data <-nsc_data %>%
    mutate(record_year = if_else(is.na(enrollment_year),
                                 coll_grad_year,
                                 enrollment_year)) #combine enrollment year and grad year to create record year
  # creates psd variable record_term 
  # record_term logic:
  # enrollment_begin and enrollment_end month combinations determine record_term
  # enrollment_begin date arrives from NSC as YYYYMMDD string — converted to date above
  # UC summer exception: Aug-Sep enrollment at UC = summer (quarter system)
  # Non-UC: Aug-Sep = fall (semester system starts in August)
  # Graduation records: term derived from coll_grad_date when he_graduated == "Y"
  nsc_data <-nsc_data%>% mutate(record_term = case_when(
    month(enrollment_begin) == 8 & month(enrollment_end) == 12 ~ "fall",
    month(enrollment_begin) == 9 & month(enrollment_end) == 12 ~ "fall",
    month(enrollment_begin) == 10 & month(enrollment_end) == 12 ~ "fall",
    month(enrollment_begin) == 7 & month(enrollment_end) == 11 ~ "fall",
    month(enrollment_begin) == 8 & month(enrollment_end) == 9 & system_type == "UC" ~ "summer",
    month(enrollment_begin) == 8 & month(enrollment_end) == 9 & system_type != "UC" ~ "fall",
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
  # Final column selection — demographics added downstream in merge_nsc_master  
  nsc_data<- select(nsc_data, student_id,first_name, middle_name, last_name, name_suffix, record_found, req_return_field, high_school_code,
                    hs_grad_date, college_code, college_name, college_state, cc_4year, public_private,enrollment_begin,
                    enrollment_end,enrollment_status, he_graduated, coll_grad_date,degree_title, major, college_sequence,
                    program_code, status_source, record_year, record_term,system_type,hs_grad_year) 
  
  return(nsc_data)
}

## -----------------------------------------------------------------------------
## Part 2 - Merging
## -----------------------------------------------------------------------------

# FUNCTION: merge_nsc_master
# PURPOSE:  Merges demographic data from the student master list to the nsc data 
#           frame
# INPUT:  nsc_data    — NSC data frame after add_psd_variables()
#         master_data — master student list with demographics
# OUTPUT:   nsc_data - with hs_grad_year, gender, race/ethnicity, poverty indicator, hs_diploma
#           psd_id
# CALLED IN: 01-merge-nsc-to-psd.R, Part 2 step 2

merge_nsc_master<-function(nsc_data, master_data){
  # psd with student demos----
  merge_data<- inner_join(nsc_data, master_data,  by = "student_id") %>%
    relocate(psd_id, .after = last_col())
  return(merge_data)
}

# FUNCTION: parse_dates
# PURPOSE:  Converts date columns to Date class for consistent analysis and merging
#           Handles three date formats present in PSD history:
#           - YYYYMMDD   — current NSC format (2019+)
#           - M/D/YY     — old NSC format (pre-2019)
#           - YYYY-MM-DD — saved PSD CSV format
# INPUT:    df — any PSD data frame with date columns (nsc_data or psd_data)
# OUTPUT:   df — with enrollment_begin, enrollment_end, coll_grad_date,
#           hs_grad_date converted to Date class
# CALLED IN: 01-merge-nsc-to-psd.R, Part 3 step 3b and Part 4 step 3

parse_dates <- function(df) {
  df %>%
    mutate(
      enrollment_begin = as.Date(parse_date_time(enrollment_begin, 
                                                 orders = c("Ymd", "mdy", "ymd"))),
      enrollment_end   = as.Date(parse_date_time(enrollment_end,   
                                                 orders = c("Ymd", "mdy", "ymd"))),
      coll_grad_date   = as.Date(parse_date_time(coll_grad_date,   
                                                 orders = c("Ymd", "mdy", "ymd"))),
      hs_grad_date     = as.Date(parse_date_time(hs_grad_date,     
                                                 orders = c("Ymd", "mdy", "ymd")))
    )
}

# FUNCTION: assign_column_classes 
# PURPOSE:  Assigns correct data types to all non-date PSD columns and 
#           standardizes text columns to uppercase for consistent filtering
#           and reporting. "notes" column is excluded from uppercase conversion
#           as it contains free-text staff input.
# INPUT:    df — any PSD data frame (nsc_data or psd_data)
# OUTPUT:   df — with correct column classes and uppercase text columns
# CALLED IN: 01-merge-nsc-to-psd.R, Part 3 step 3a and Part 4 step 3

# COLUMN CLASS REFERENCE
# Column              Source    Class       toupper()   Rationale
# -------             ------    -------     ---------   ---------
# student_id          PSD       character   NO          Case-sensitive ID — changing case breaks joins
# first_name          NSC       character   YES         Standardize for reporting
# middle_name         NSC       character   YES         Standardize for reporting
# last_name           NSC       character   YES         Standardize for reporting
# name_suffix         NSC       character   YES         Standardize for reporting
# record_found        NSC       character   YES         NSC flag value — standardize
# req_return_field    NSC       character   YES         NSC return field — standardize
# high_school_code    NSC       character   YES         Code stored as string not number
# college_code        NSC       character   NO          Federal school code — case matters for joins
# college_name        NSC       character   YES         Standardize for filtering and reporting
# college_state       NSC       character   YES         Standardize for filtering
# cc_4year            NSC       character   YES         Controlled vocabulary — standardize
# public_private      NSC       character   YES         Controlled vocabulary — standardize
# enrollment_begin    NSC       date        NO          Parsed by parse_dates()
# enrollment_end      NSC       date        NO          Parsed by parse_dates()
# enrollment_status   NSC       character   YES         NSC status value — standardize
# he_graduated        NSC       character   YES         NSC flag value — standardize
# coll_grad_date      NSC       date        NO          Parsed by parse_dates()
# degree_title        NSC       character   YES         Standardize for reporting
# major               NSC       character   YES         Standardize for reporting
# college_sequence    NSC       numeric     NO          Numeric sequence — toupper() not applicable
# program_code        NSC       character   YES         Standardize for reporting
# hs_grad_date        NSC       date        NO          Parsed by parse_dates()
# status_source       PSD       character   YES         Controlled vocabulary — standardize
# record_year         PSD       integer     NO          Derived from enrollment_begin — year as integer
# record_term         PSD       character   YES         Derived from enrollment_begin/end months
# system_type         PSD       character   YES         Joined from institution_lookup via college_code
# hs_grad_year        PSD       integer     NO          Derived from hs_grad_date — year as integer
# gender              MASTER    character   YES         Controlled vocabulary — standardize
# race_ethnicity      MASTER    character   YES         Controlled vocabulary — standardize
# poverty_indicator   MASTER    character   YES         Controlled vocabulary — standardize
# hs_diploma          MASTER    character   YES         Controlled vocabulary — standardize
# notes               MASTER    character   NO          Staff free text — preserve original formatting
# psd_id              PSD       character   NO          Case-sensitive ID — changing case breaks joins

assign_column_classes <- function(psd_data){
  psd_data <- psd_data %>% 
    mutate(
      student_id = as.character(student_id),
      first_name = toupper(as.character(first_name)),
      middle_name = toupper(as.character(middle_name)),
      last_name = toupper(as.character(last_name)),
      name_suffix = toupper(as.character(name_suffix)),
      record_found = toupper(as.character(record_found)),
      req_return_field = toupper(as.character(req_return_field)),
      high_school_code = toupper(as.character(high_school_code)),
      college_code = as.character(college_code),
      college_name = toupper(as.character(college_name)),
      college_state = toupper(as.character(college_state)),
      cc_4year = toupper(as.character(cc_4year)),
      public_private = toupper(as.character(public_private)),
      enrollment_status = toupper(as.character(enrollment_status)),
      he_graduated = toupper(as.character(he_graduated)),
      degree_title = toupper(as.character(degree_title)),
      major = toupper(as.character(major)),
      college_sequence = as.numeric(college_sequence),
      program_code = toupper(as.character(program_code)),
      status_source = toupper(as.character(status_source)),
      record_year = as.integer(record_year),
      record_term = toupper(as.character(record_term)),
      system_type = toupper(as.character(system_type)),
      hs_grad_year = as.integer(hs_grad_year),
      gender = toupper(as.character(gender)),
      race_ethnicity = toupper(as.character(race_ethnicity)),
      poverty_indicator = toupper(as.character(poverty_indicator)),
      hs_diploma = toupper(as.character(hs_diploma)),
      notes = as.character(notes),
      psd_id = as.character(psd_id)
    )
  
  return(psd_data)
}


# FUNCTION: check_type
# PURPOSE:  Compares column classes across multiple data frames to verify
#           consistency before binding. Returns a table showing the class
#           of each column in each data frame.
# INPUT:    df_list  — list of data frames to compare
#           df_names — optional character vector of names for each data frame
# OUTPUT:   data frame with columns: column, and one column per data frame
#           showing the class of each variable
# CALLED IN: 01-merge-nsc-to-psd.R, Part 5 step 3b
check_type <- function(df_list, df_names = NULL) {
  
  # Assign names if not provided
  if (is.null(df_names)) {
    df_names <- paste0("df", seq_along(df_list))
  }
  
  # Get all columns across all dfs
  cols <- Reduce(union, lapply(df_list, names))
  
  # Helper function
  get_class <- function(df, col) {
    if (col %in% names(df)) {
      class(df[[col]])[1]
    } else {
      NA
    }
  }
  
  # Build result
  result <- data.frame(column = cols, stringsAsFactors = FALSE)
  
  for (i in seq_along(df_list)) {
    result[[df_names[i]]] <- sapply(cols, function(x) get_class(df_list[[i]], x))
  }
  
  return(result)
}

# FUNCTION: check_type_mismatch
# PURPOSE:  Filters check_type() output to show only columns where class
#           types differ across data frames. Returns empty data frame if
#           all column classes match — confirms data frames are ready to bind.
# INPUT:    df_list  — list of data frames to compare
#           df_names — optional character vector of names for each data frame
# OUTPUT:   data frame showing only mismatched columns and their classes
#           Returns 0 rows if no mismatches found
# CALLED IN: 01-merge-nsc-to-psd.R, Part 5 step 3b
check_type_mismatch <- function(df_list, df_names = NULL) {
  tbl <- check_type(df_list, df_names)
  
  tbl %>%
    dplyr::filter(
      apply(tbl[-1], 1, function(x) length(unique(na.omit(x))) > 1)
    )
}

## -----------------------------------------------------------------------------
## Part 3 - Reference Functions
## -----------------------------------------------------------------------------
# NOTE: These are interactive QA tools for ad hoc investigation
# They are NOT called by any pipeline script
# Consider moving to psd_rfk_qa_tools.R in next cleanup pass
# Usage examples:
# check_id(current_psd, "2024AFLB74")
# check_person(current_psd, "MARIA", "GARCIA")
# check_person_last(current_psd, "GARCIA")

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
## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------