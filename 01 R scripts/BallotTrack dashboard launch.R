################################### ----- LG2101 DASHBAORD PIPELINE ---- ######################################



## This pipeline is used to create all the dashboards for the 2021  Local Government election  


## New  dashboard strucure has 4 main sections ----


# 1. Data extraction from Database is in a seperate file, so that when integration to hyperion is done it can be 
#    amended accordingly (initial thought is to add hyperion view code to process codes and maintain the raw data 
#    code for hyperion failure readiness)


# 2. Data processing mapping done in this phase


# 3. RMD files in indexes, and in sub pages (two separate RMD files for each dashboard)


# 4. HTML pages will be knitted straight to the server, and all within the launch script. 
#    Currnetly one launch scripts for indexes and 1 for subs (i.e. pages)


## Set parameters for analysis ----
event_group_ID       <- 'LG2101'
api_key              <- "AIzaSyB-ltOEpu8ZwF7H-ZOIIp5UmXHwewu27eA"
from_staging         <- FALSE
flex_headline_size   <- 24
BallotTrack          <- TRUE
test_data            <- TRUE
knit_rmd             <- TRUE




## Source for folder locations/functions/db connections ----
## Jason wants us to connect to each database as required, rather than all of them, then disconnect
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))


## Load packages
library(fs)
library(flexdashboard)
library(flextable)
library(lubridate)
library(officer)
library(tidyr)
library(readr)
library(dplyr)
library(openxlsx)
library(RODBC)
library(RODBCext)
library(summarywidget)
library(bsselectR)
library(crosstalk)
library(DT)
library(stringr)
library(htmltools)
library(knitr)
library(ggplot2)
library(highcharter)
library(readxl)
library(memisc)


## Source the functions created just for the dashboard 
source(paste0(Sys.getenv("source_control_local_path"), 
              "Election_Events/LG2101/02 Dashboard/01 R scripts/Functions/dashboard_library.R"))


## Set directories for analysis
## Source the functions used only for the dashboard - I.E. graphs, etc.
root_dashboard   <- paste0(Sys.getenv('source_control_local_path'), 
                           'Election_Events/LG2101/02 Dashboard/')

script_files     <- paste0(root_dashboard, '01 R scripts/')
data_gen         <- paste0(root_dashboard, '01 R scripts/_Data generators/')
data_source      <- paste0(root_dashboard, '02 Source data/')
data_RO          <- paste0(root_dashboard, '02 Source data/RO files/')
data_output      <- paste0(root_dashboard, '04 Output data/')
data_proc        <- paste0(root_dashboard, '01 R scripts/_Data processors/')
rmd_files        <- paste0(root_dashboard, '01 R scripts/_RMD files/')
data_files       <- paste0(root_dashboard, '02 Source data/')
image_files      <- paste0(root_dashboard, '01 R scripts/_RMD files/Dashboard_graphs')
sub_pages        <- paste0(root_dashboard, '01 R scripts/_RMD files/Sub pages')


## Source the functions and databases
source(paste0(root_dashboard,              '01 R scripts/Functions/dashboard_plotting_library.R'))
source(paste0(root_dashboard,              '01 R scripts/Functions/dashboard_functions_library.R'))
source(paste0(Sys.getenv("source_control_local_path"),
              "Main/R Scripts - Production/Database connections/Hyperion.R"))


## Set server locations
server_root_indexes       <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Indexes/'
server_root_subpages      <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Indexes/Sub pages/'
server_root_securedpages  <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Secured pages/'
BT_server_data            <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Data/BallotTrack/'
server_messages           <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Data/Messages/'


## Sub page RMD files
server_root_subpage_files <- paste0(server_root_subpages, '08-Ballot-Track-Sub_files/')
server_root_subpage_html  <- paste0(server_root_subpages, '08-Ballot-Track-Sub.html')



## Clean out sub-page program files (after RMD self_contained = TRUE has been set this can be deleted)
if(dir.exists(server_root_subpage_files)) {
  
  directories_to_delete <- list.dirs(server_root_subpage_files, full.names = TRUE, recursive = FALSE)
  dir_delete(directories_to_delete)
}


## 
if(file.exists(server_root_subpage_html)) {
  file.remove(server_root_subpage_html)
}


## Remove previous sub RMD files if they exist.
sub_RMD = list.files(sub_pages, pattern = '08-Ballot-Track-Sub', full.names = TRUE)
file_delete(sub_RMD)


## Set global R options : 
## Stop R triggering scientific notation for the y-axis
## suppress summarise warnings
options(dplyr.summarise.inform = FALSE, scipen = 999)


## Capture messages and errors to a file 
zz <- file(paste0(server_messages, 'BallotTrack_messages.txt'), open = "wt")
sink(zz, type = "message")





## 08 Ballot Track --------------------------------------------------------


## Run the ballot track dashboard.


## Create BT data 
if (BallotTrack) {
  
  source(paste0(data_gen,  '08 Ballot Track data generator.R'))
  source(paste0(data_proc, '08 Ballot Track data processor.R'))
  
  ## Knit the sub-page to the sub-directory
  if(knit_rmd) {
    
    setwd(sub_pages)
    rmarkdown::render('08 Ballot Track Sub.Rmd', 
                      output_dir = server_root_subpages)
    
    ## Knit the main page to the index directory
    setwd(rmd_files)
    rmarkdown::render('08 Ballot Track.Rmd', 
                      output_dir = server_root_indexes)
    
  }
}


## reset message sink and close the file connection
sink(type = "message")
close(zz)





#################################################### TBC ###########################################################