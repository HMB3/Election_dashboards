####################################################################################################################
###################################### rmt DATA GENERATOR  ---- ###########################################
####################################################################################################################


library(RODBC)
library(RODBCext)
library(flextable)
library(flexdashboard)
library(kableExtra)
library(DT)
library(knitr)
library(formattable)
library(openxlsx)
library(tidyr)
library(dplyr)

options(scipen=999)

# test event LG2003 did not cover votecountingpoint table, using LG1701 instead

event_group_ID= 'LG2101'

# 1. set parameters  ---------------------------------------------------------
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))

setwd(paste0(Sys.getenv("source_control_local_path"), 'Election_Events/LG2101/01 R Launch Scripts'))

source('LG2101_F_drive_file_backups.R')

