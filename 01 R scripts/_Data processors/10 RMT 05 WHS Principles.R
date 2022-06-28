

# AoBP Data Extraction ----------------------------------------------------

# Current version 5.0


start_time <- Sys.time()



# To do list --------------------------------------------------------------

# Initialisation ----------------------------------------------------------




#  Load packages and connections ------------------------------------------

event_group_ID          <- 'LG2101'

# Import folder locations
# Load packages


library(openxlsx)
#library(tidyverse)
library(tidyr)
library(dplyr)
library(RODBC)
library(RODBCext)
library(DBI)
library(odbc)
library(glue)
library(DT)


# Access backups and prepare storage --------------------------------------
use_dbi <- TRUE


source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))

root_dashboard   <- paste0(Sys.getenv('source_control_local_path'), 
                           'Election_Events/LG2101/02 Dashboard/')
script_files     <-paste0(root_dashboard,  '01 R scripts/')

data_source      <- paste0(root_dashboard, '02 Source data/')
data_proc        <- paste0(root_dashboard, '01 R scripts/_Data processors/')
rmd_files        <- paste0(root_dashboard, '01 R scripts/_RMD files/')
data_files       <- paste0(root_dashboard, '02 Source data/')
image_files      <- paste0(root_dashboard, '01 R scripts/_RMD files/Dashboard_graphs')
sub_pages        <- paste0(root_dashboard, '01 R scripts/_RMD files/Sub pages')



## Set server locations
server_root_indexes      <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Indexes/'
server_root_subpages     <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Indexes/Sub pages/'
server_root_securedpages <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Secured pages/'


## Stop R triggering scientific notation for the y-axis
options(dplyr.summarise.inform = FALSE, scipen = 999)


setwd(database_connections)
source('hyperion.R')

# Set working directory to the most recent AoBP backup folder

setwd(AoBP_backup_folder)

backup_folders <- list.dirs(full.names = FALSE)

backup_folder <- paste(AoBP_backup_folder,
                       "/",
                       backup_folders[length(backup_folders)], sep = "")

setwd(backup_folder)




# Storage tables

# File lists
file_list <- list.files(pattern = "\\.xlsm$")


file_list <- grep("~|STH", file_list, value = TRUE, invert = TRUE)




AllVenues <- data.frame(stringsAsFactors = F)

for (i in seq_along(file_list)) {
  
  # for (i in 45:53) {
  print(paste0(i,' / ', length(file_list)))
  # Source main data table and reference data -------------------------------
  
  
  if(file.size(paste(backup_folder, file_list[i], sep = "/")) > 0) {
    
    # Parameters
    data_sheet <- read.xlsx(xlsxFile = file_list[i],
                            sheet = "Record Time")
    
    AoBP_RO <- gsub('RO: ','',data_sheet[4,2])
    AoBP_AreaCode <- gsub('LGA: ','',data_sheet[3, 2])
 
    # Extract data from main AoBP storage table
    
    RecordTime <- readxl::read_excel(file_list[i],
                             sheet = "Record Time",
                             skip = 10
                           # ,detectDates = TRUE
                           # ,col_types = c('text','text','text','text','date','date','text')
                           ) %>% as.data.frame()
    
    if(length(RecordTime) !=7) {
      
      RecordTime <- RecordTime %>% mutate(Comments = NA)
      
    }
    
    colnames(RecordTime) <- c('RO Office','AreaCode','3','VenueName','Time of Leaving Polling Place','Time of Leaving RO Office','Comments')
   
    RecordTime <- RecordTime %>% dplyr::filter(`RO Office` > 0) %>%
                                    mutate(`RO Office` = AoBP_RO
                                        ,AreaCode = AoBP_AreaCode
                                        ,`Time of Leaving Polling Place` = format(`Time of Leaving Polling Place`, format = "%H:%M:%S")
                                        ,`Time of Leaving RO Office` = format(`Time of Leaving RO Office`, format = "%H:%M:%S")
                                        ) %>%
      select(`RO Office`,AreaCode, VenueName, everything()) 
    
    
 
  #  RecordTime[, which(colnames(RecordTime) %in% cols.num)] <- sapply(RecordTime[, which(colnames(RecordTime) %in% cols.num)],reformat_time)
    # this bit has problems 
 
  }

  AllVenues <- AllVenues %>% bind_rows(RecordTime %>% mutate(Source_file = file_list[i]))
  
  }

AllVenues <- AllVenues %>% filter(!is.na(VenueName)) %>%
  select(`RO Office`
         ,AreaCode
         ,VenueName
         ,`Time of Leaving Polling Place`
         ,`Time of Leaving RO Office`
         ,Comments
         ,Source_file)


  # DidNtComplete
  
  listofNAVenues <- AllVenues %>% filter(is.na(`Time of Leaving Polling Place`) | `Time of Leaving Polling Place` == 'NA')
  
  listofROs <- listofNAVenues %>% group_by(`RO Office`, Source_file) %>% summarise(MissingCalls = n())
  
  
  # potentially can do latest time date etc analysis, pending real eg.
  
 StateSummary <- AllVenues %>% arrange()


# ema initial count entry time from processing data db --------------------

 
 results_timestamps_EN <- dbGetQuery(hyperion_connection,
                          glue_sql("
                        /****** Script for SelectTopNRows command from SSMS  ******/
SELECT *
  FROM [ProcessingData].[dashboard].[results_timestamps_EN_LG_LB]
  where [EventGroupID] like {paste0(event_group_ID,'%')}
                           
                           ", .con=hyperion_connection))
 
 
 

# Write to file -----------------------------------------------------------

 
 setwd(paste0(sub_pages))
 rmarkdown::render('10 RMT 05 WHS Principles.Rmd', 
                   output_dir = server_root_subpages)  

Sys.time()
print('Complete')
