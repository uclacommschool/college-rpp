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
                                   height = 400, width = 700, iterations = 1, 
                                   colourScale=my_color, LinkGroup="group", NodeGroup="group")
  
  return(sankey_pathways)
  
}
  

#store   
sankey_list<-map(master_list[["links"]],
          function(link_df){
            create_sanky_chart(link_df,master_list[["nodes"]])
          })
  
## -----------------------------------------------------------------------------
## Part 2 - Save and Export Files
## -----------------------------------------------------------------------------

save(sankey_list, "sankey_list.Rdata")
  
## save the widget
save_sankey<-function(df, cohort){
  
  saveWidget(df, file=file.path(".","sankey_chart",
  str_c("sankey_", cohort, ".html")))
}

map2(sankey_list, names(sankey_list), function(x,y) save_sankey(x,y))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
