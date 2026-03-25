################################################################################
##
## [ PROJ ] < College Data Project >
## [ FILE ] < 02-create-missing-list-from-psd.R >
## [ AUTH ] < Ariana Dimagiba / aridimagiba >
## [ INIT ] < 03/25/2026, updated 08/25/2025 >
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

data_file_dir<-file.path("..","..")w