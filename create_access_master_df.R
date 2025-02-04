################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < create_access_master_df.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 2/2/25 >
##
################################################################################

#Goal: Creates the Access Master Excel file needed to read to create the 
#sankey chart.

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

#master pathway database
pathways_df<-read.csv(file.path(".", "master_pathway_psd.csv")) %>% select(-c(X))

pathways_counts<-read.csv(file.path(".", "pathway_counts.csv")) %>% select(-c(X))

#read in former access_master dataset as a reference
ref_data<-vector("list", 3)

ref_data[[1]]<-read_excel(file.path(data_file_dir, "access_master.xlsx"),
                          sheet = "nodes") 

ref_data[[2]]<-read_excel(file.path(data_file_dir, "access_master.xlsx"),
                          sheet = "links") 

ref_data[[3]]<-read_excel(file.path(data_file_dir, "access_master.xlsx"),
                          sheet = "master") 

ref_data[[3]]<-clean_names(ref_data[[3]])

names(ref_data)<-c("nodes", "links", "master")

## -----------------------------------------------------------------------------
## Part 1 - Make Master Access Dataframe - Preparation
## -----------------------------------------------------------------------------

#transform the pathways_counts document to the access_master (am) document

am_master<-pathways_counts %>% 
  mutate(title = path) %>% 
  separate(path, into = c("source_name", "target_name"), sep = " to ")

#change n to value
am_master<-am_master %>% rename(value = n)

#Create temporary source and target value dataframes 
#overall source values
temp_source_target_values<-am_master %>% select(-c(value, cohort)) %>% unique()

#source values by year
source_values_by_year<-map(c(1:6),
                           function(x){
                             df<-temp_source_target_values %>% filter(grepl(str_c(x,"$"), target_name)) %>% 
                               select(source_name) %>% unique()
                             return(df)
                           })
#source values by graduation
source_values_by_graduate<-temp_source_target_values %>% filter(grepl(str_c("graduate"), target_name)) %>% 
  select(source_name) %>% unique()

#target values
target_values<-temp_source_target_values %>% count(target_name)

#source name values
source_name_values<-temp_source_target_values %>% select(source_name) %>% unique()


#create strings based on the year
plan_year_string<-c("ucplan", "csuplan","cccplan", "private plan", "noplan",
                    "missplan", "OUT4YRplan",
                    "noenrollplan", "forprofitplan", "gapyearplan",
                    "militaryplan", "workplan")

year1_string<-c("ccc1", "csu1", "miss1", "private1", "uc1", "OUT4YR1")

#Made Year 2 - 6 strings. Note: ignored "graduate"
year2_string<-c("ccc2", "csu2", "miss2", "private2", "uc2", "OUT4YR2")
year3_string<-c("ccc3", "csu3", "miss3", "private3", "uc3", "OUT4YR3")
year4_string<-c("ccc4", "csu4", "miss4", "private4", "uc4", "OUT4YR4")
year5_string<-c("ccc5", "csu5", "miss5", "private5", "uc5", "OUT4YR5")
year6_string<-c("ccc6", "csu6", "miss6", "private6", "uc6", "OUT4YR6")

#Note/Question:
#If they graduate in their 6th year, would it be the college name or "graduate"
#in college_year6?

#check
#View(source_values_by_year[[4]])

test<-source_values_by_year[[1]] %>% filter(!(source_name %in% plan_year_string))
test<-source_values_by_year[[2]] %>% filter(!(source_name %in% year1_string))
test<-source_values_by_year[[3]] %>% filter(!(source_name %in% year2_string))
test<-source_values_by_year[[4]] %>% filter(!(source_name %in% year3_string))
test<-source_values_by_year[[5]] %>% filter(!(source_name %in% year4_string))
test<-source_values_by_year[[6]] %>% filter(!(source_name %in% year5_string))

# Define the custom order and arrange the dataframe for source_name
am_master<-am_master %>% 
  mutate(source_name = factor(source_name,
                              levels = 
                                c(plan_year_string,
                                  year1_string, year2_string, year3_string,
                                  year4_string, year5_string, "graduate")),  
           target_name = factor(target_name,
                                levels = 
                                  c(year1_string,year2_string, year3_string,
                                    year4_string,year5_string, year6_string,
                                    "graduate"))) %>%
  arrange(cohort,source_name, target_name)

#question do you get rid of graduate in source_name? Left it in for now.

## -----------------------------------------------------------------------------
## Part 2 - Make Master Access Dataframe - Source & Target Values
## -----------------------------------------------------------------------------

#create key to match double with the source and target
key_df_s<-am_master %>% select(source_name) %>% unique()
key_df_t<-am_master %>% select(target_name) %>% unique()

key_df_t<-key_df_t %>% mutate(source_name = target_name)

key_df<-full_join(key_df_s, key_df_t, by = c("source_name")) %>% 
  select(-c(target_name)) 

key_df<-key_df %>%
  mutate(
    source_name = factor(source_name,
                         levels = c(plan_year_string,
                                    year1_string, year2_string, year3_string,
                                      year4_string, year5_string,year6_string,
                                      "graduate"))) %>%
  arrange(source_name) %>% 
  mutate(id = seq_len(nrow(key_df))) %>% 
  rename(name = source_name)

#update the master df by target and source name 
am_master<-am_master %>% left_join(key_df, by = c("source_name" = "name")) %>% 
  rename(source = id)

am_master<-am_master %>% left_join(key_df, by = c("target_name" = "name")) %>% 
  rename(target = id)

## -----------------------------------------------------------------------------
## Part 3 - Make Master Access Dataframe - Group Values  
## -----------------------------------------------------------------------------

path_suffix<-c("plan", c(1:6))

group_key<-key_df %>% 
  mutate(
    group = case_when(
      name %in% c(str_c("uc", path_suffix)) ~ "a",
      name %in% c(str_c("csu", path_suffix)) ~ "b",
      name %in% c(str_c("ccc", path_suffix)) ~ "c",
      name %in% c(str_c("private ", path_suffix[1]),
                  str_c("private", path_suffix[-1])) ~ "d",
      name %in% c(str_c("OUT4YR", path_suffix)) ~ "e",
      name %in% c(str_c("miss", path_suffix)) ~ "f",
      
      #miscellanous plans
      name %in% c("noplan") ~ "g",
      name %in% c("noenrollplan") ~ "h",
      name %in% c("forprofitplan") ~ "i",
      name %in% c("gapyearplan") ~ "j",
      name %in% c("militaryplan") ~ "k",
      name %in% c("workplan") ~ "l",
      
      #graduated
      name %in% c("graduate") ~ "m",
    )
  )

#remove id column
group_key<-group_key %>% select(-c(id))

#check
check<-group_key %>% filter(is.na(group))
check<-group_key %>% filter(group == "b")

#update the master df by group
am_master<-am_master %>% left_join(group_key, by = c("source_name" = "name"))

## -----------------------------------------------------------------------------
## Part 4 - Create List of Master Access Database 
## -----------------------------------------------------------------------------

#create cohort string
cohort_string<-am_master$cohort %>% unique()

master_access_list<-map(cohort_string, function(x){
  am_master %>% filter(cohort == x)
})

names(master_access_list)<-cohort_string

#create links list by cohort

#create links function
create_links_df<-function(df){
  links_df<-df %>% select(source, target, value, group)
  return(links_df)
}

links_list<-map(master_access_list, create_links_df)

#create Master list that includes all the links, matster access dfs, and nodes df

master_list<-list(master_access_list,links_list,group_key)
names(master_list)<-c('master', 'links', 'nodes')

## -----------------------------------------------------------------------------
## Part 5 - Export Data & Save Data
## -----------------------------------------------------------------------------

#export function
export_master_list<-function(cohort){
  
  wb <- createWorkbook()
  #nodes
  addWorksheet(wb, "nodes")
  writeData(wb, "nodes", master_list[["nodes"]])
  
  #links
  addWorksheet(wb, "links")
  writeData(wb, "links", master_list[["links"]][[cohort]])
  
  #links
  addWorksheet(wb, "master")
  writeData(wb, "master", master_list[["master"]][[cohort]])
  
  saveWorkbook(wb, file.path(".", "access_master",
                             str_c("access_master_",cohort,".xlsx")),
                             overwrite = TRUE)
  
}

#export files
map(cohort_string, export_master_list)

#save files
save(master_list,file = file.path(".", "master_list.Rdata"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
