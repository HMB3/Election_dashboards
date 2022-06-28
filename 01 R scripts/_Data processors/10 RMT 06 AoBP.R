

# AoBP Data Extraction ----------------------------------------------------

# Current version 5.0


start_time <- Sys.time()



# To do list --------------------------------------------------------------

# Initialisation ----------------------------------------------------------




#  Load packages and connections ------------------------------------------


# Import folder locations
# Load packages


library(openxlsx)
#library(tidyverse)
library(tidyr)
library(dplyr)
library(RODBC)
library(RODBCext)


event_group_ID <- 'LG2101'
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))


# Access backups and prepare storage --------------------------------------



# Set working directory to the most recent AoBP backup folder
# load file /data-----------------------------------------------------------


setwd(AoBP_data_folder)

BP_collection_summary <- read.csv('BP_collection_summary.csv') %>%
  select(ReturningOffice
         ,LGAreaCode
         ,LocationTypeCode
         ,VenuesCollected
         ,NumberOfVenues
         ,SourceFile = from_file
         )

pre_poll_allocations_all <- read.csv('Pre_poll_allocations.csv')
BP_collection_all <- read.csv('BP_collection_all.csv')
DidNtComplete <-data.frame()
#DidNtComplete <- read.csv('DidNtComplete.csv')




# Knit to dashboard -------------------------------------------------------

