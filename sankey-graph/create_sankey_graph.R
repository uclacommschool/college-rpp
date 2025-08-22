################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < create_sankey_graph.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 2/3/25 >
##
################################################################################

#Goal: Creates sankey graph (adapted from Ariana's script).

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(networkD3)
library(tidyverse)
library(readxl)
library(knitr)
library(htmlwidgets)
library(magrittr)
library(webshot)

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

#load master list
load("master_list.Rdata")

## -----------------------------------------------------------------------------
## Part 1 - Create Specific Links and Nodes
## -----------------------------------------------------------------------------

create_sanky_chart<-function(link2, nodes2){
  
  links2 <- link2
  nodes2 <- nodes2
  
  min_source_value<-min(links2$source)
  
  # Convert to 0-based index
  links2$source <- links2$source - min_source_value
  links2$target <- links2$target - min_source_value
  
  my_color <- 'd3.scaleOrdinal() .domain(["a", "b", "c", "d", "e", "f", "g","h","i","j"]) .range(["#B7C6E4", "#FBE5A2",
  "#B1CE95", "#BFA9E6", "#F4AFA9", "#FBD4A2", "#DBDBDB", "#F4A261", "#0F5257","#98D2EB"])'
  
  #note: may need to adjust the number of groups and their corresponding colors
  
  
  sankey_pathways <- sankeyNetwork(Links = links2, Nodes = nodes2, Source = "source",
                                   Target = "target", Value = "value", NodeID = "name",
                                   fontSize = 8, nodeWidth = 10, fontFamily = "sans-serif",
                                   height = 400, width = 700, iterations = 0, 
                                   colourScale=my_color, LinkGroup="group", NodeGroup="group")
  
  return(sankey_pathways)
  
}
  
#store   
sankey_list<-map(master_list[["links"]],
          function(link_df){
            create_sanky_chart(link_df,master_list[["nodes"]])
          })

## -----------------------------------------------------------------------------
## Part 2 - Create Specific Links and Nodes for each cohort
## -----------------------------------------------------------------------------

#create updated links and master node dataframes
update_link_nodes<-function(nodes, master, option){
  
  #create missing cases based on source and target names
  missing_source<-left_join(nodes, master,
                            by = c("name" = "source_name", "group"))
  
  
  missing_target<-left_join(nodes, master,
                            by = c("name" = "target_name"))
  
  #create source2 to that only keeps needed cases for sankey chart
  target_df<-missing_target %>% select(name, target) %>% unique()
  target_df<-target_df %>% filter(!is.na(target))
  colnames(target_df)[2]<-"source2"
  
  missing_source<-left_join(missing_source, target_df, by = "name")
  
  missing_source<-missing_source %>% 
    mutate(source2 = case_when(is.na(source2) ~ source,TRUE ~ source2))
  
  #create source3 that updates source numbering
  
  # Identify indices where source is NA 
  na_indices <- which(is.na(missing_source$source2))
  
  missing_source$source3<-missing_source$source2
  
  # Increment subsequent numbers in source column
  for (idx in rev(na_indices)) {
    if (idx + 1 <= nrow(missing_source)) {
      missing_source$source3[(idx+1):nrow(missing_source)] <- 
        missing_source$source3[(idx+1):nrow(missing_source)] - 1
    }}
  
  #create target2, which included revised target numbering 
  target_df2<-missing_source %>% select(name, source3) %>% unique()
  colnames(target_df2)[2]<-"target2"
  
  missing_source<-left_join(missing_source, target_df2,
                            by = c("target_name" = "name"))
  
  if (option == "links"){
    
    missing_source<-missing_source %>% filter(!is.na(cohort))
    missing_source<-missing_source %>% select(-c(source, target,source2))
    colnames(missing_source)[c(7,8)]<-c("source", "target")
    
    link_df<-missing_source %>% select(source, target, value, group)
    
    return(link_df)
  }
  
  if (option == "nodes"){
    
    master_nodes<-missing_source %>% select(name, source3, group)
    master_nodes<-master_nodes %>% filter(!is.na(source3)) %>% unique() %>% 
      select(-c(source3))
    
    return(master_nodes)
  }
}

safe_eval <- function(expr) {
  tryCatch(
    expr, 
    error = function(e) NA
  )
}

update_link_df_list<-map(master_list[["master"]],
                         function(x){
                           update_link_nodes(master_list[["nodes"]],
                                             x,"links")} %>% 
                           safe_eval())

update_master_df_list<-map(master_list[["master"]],
                           function(x){
                             update_link_nodes(master_list[["nodes"]],
                                               x,"nodes")} %>% 
                             safe_eval())  

#store updated sankey charts
update_sankey_list<-map2(update_link_df_list,update_master_df_list,
                         function(link_df, master_df){
                           create_sanky_chart(link_df,master_df)
                         } %>%  safe_eval())

## -----------------------------------------------------------------------------
## Part 3 - Save and Export Files
## -----------------------------------------------------------------------------

save(sankey_list, update_sankey_list, file = "sankey_list.Rdata")
  
## save the widget
save_sankey<-function(df, cohort){
  
  saveWidget(df, file=file.path(".","sankey_chart",
  str_c("sankey_", cohort, ".html")))
}

map2(update_sankey_list, names(sankey_list), function(x,y) save_sankey(x,y))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
