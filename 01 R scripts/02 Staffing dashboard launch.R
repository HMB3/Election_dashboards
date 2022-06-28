################################### ----- LG2101 DASHBAORD PIPELINE ---- ######################################



## This pipeline is used to create all the dashboards for the 2021  Local Government election  


## New  dashboard strucure has 4 main sections ----


# 1. Data extraction from Database is in a seperate file, so that when integration to hyperion is done it can be 
#    amended accordingly (initial thought is to add hyperion view code to process codes and maintain the raw data 
#    code for hyperion failure readiness)


# 2. Data processing mapping done in this phase


# 3. RMD files in indexes, and in sub pages (two separate RMD files for each dashboard)


# 4. HTML pages will be knitted straight to the server, and all within the launch script. 
#    The initial plan is to have 2 launch scripts: 1 for indexes, and 1 for subs (i.e. pages)



## Set parameters for analysis ----
event_group_ID     <- 'LG2101'
api_key            <- "AIzaSyB-ltOEpu8ZwF7H-ZOIIp5UmXHwewu27eA"

## Source for folder locations/functions/db connections ----
## Jason wants us to connect to each database as required, rather than all of them, then disconnect
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))


#can't save on server yet, setting this up temporarily
#TomTest<-"C:/Users/harrisont/source/Workspaces/DataAnalytics/Election_Events/LG2101/02 Dashboard/03 Working pages"


## Load packages

library(jsonlite)
library(flexdashboard)
library(tidyr)
library(dplyr)
library(RODBC)
library(RODBCext)
library(summarywidget)
library(bsselectR)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(crosstalk)
library(rgdal)
library(DT)
library(stringr)
library(htmltools)
library(knitr)
library(highcharter)
library(geosphere)
library(rgeos)
library(ggplot2)

## Source the functions created just for the dashboard 
source(paste0(Sys.getenv("source_control_local_path"), 
              "Election_Events/LG2101/02 Dashboard/01 R scripts/Functions/dashboard_library.R"))



## Set directories for analysis
## Source the functions used only for the dashboard - I.E. graphs, etc.
root_dashboard   <- paste0(Sys.getenv('source_control_local_path'), 
                           'Election_Events/LG2101/02 Dashboard/')
script_files     <-paste0(root_dashboard, '01 R scripts/')
data_gen         <- paste0(root_dashboard, '01 R scripts/_Data generators/')
data_source      <- paste0(root_dashboard, '02 Source data/')
data_proc        <- paste0(root_dashboard, '01 R scripts/_Data processors/')
rmd_files        <- paste0(root_dashboard, '01 R scripts/_RMD files/')
data_files       <- paste0(root_dashboard, '02 Source data/')
image_files      <- paste0(root_dashboard, '01 R scripts/_RMD files/Dashboard_graphs')
sub_pages        <- paste0(root_dashboard, '01 R scripts/_RMD files/Sub pages')
noms_data        <- 'G:/Election Events/LGE 2020 Programme/AP02B Candidates & Parties/Candidates, Electoral Material, Ballot Papers/Dashboards/'

source(paste0(root_dashboard,              '01 R scripts/Functions/dashboard_plotting_library.R'))
source(paste0(root_dashboard,              '01 R scripts/Functions/dashboard_functions_library.R'))



## Set server locations
server_root_indexes      <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Indexes/'
server_root_subpages     <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Indexes/Sub pages/'
server_root_securedpages <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Secured pages/'

#local version for testing.
#TomTest<-paste0(Sys.getenv('source_control_local_path'),'Election_Events/LG2101/02 Dashboard/03 Working pages')



## Set global R options : 
## Stop R triggering scientific notation for the y-axis
## suppress summarise warnings
options(dplyr.summarise.inform = FALSE, scipen = 999)


## The actual data extraction and knitting starts from here
#open database here
setwd(database_connections)

from_staging       <- FALSE

source("Hyperion.R")
source("EMS.R")
source("EMA.R") 

## Create common data needed ----
source(paste0(data_gen, '00 Common data generator.R'))
#save.image(file = paste0(data_gen, "Dashboard_Common_data.RData"))



## 02 Staffing ---------------------------------------------------------------------------


## Start when staffing EOI start, review ALC mapping tool code

  
  if (FALSE) { ## This is only run upon request by Janet
    
    source(paste0(data_proc, '02_02 SEO mapping.R'))
    
    ## Change output_dir
    setwd(paste0(rmd_files, 'Secured pages/'))
    rmarkdown::render(paste0(rmd_files, 'Secured pages/', '02_02 SEO mapping.Rmd'), 
                      output_dir = server_root_securedpages)
    
  }
  
  ## TBC    
  dev_code <- FALSE # this creates small amount of calculation for quick testing when distance on hyperion had been cleared
  devnum <- 30
  
  source(paste0(data_gen,  '02 Staffing data generator.R'))
  
  source(paste0(data_proc, '02 Staffing data processor.R'))
 
  
  # individual ros
  
  for (i in 1:length(unique(RO_Centroids$RO_OFFICE))) {
    #  i = 68  
    source(paste0(data_proc,'02_03 EOI map.R'))
    
  }
  
  
  # bs select staffing index page
  
  ind_pages <- paste0(server_root_securedpages, list.files(path = server_root_securedpages, pattern= 'HTML|html',full.names = FALSE))
  to_replace <- paste0(server_root_securedpages, '|', ' RO Office EOI.html')
  names(ind_pages) <- str_replace_all(ind_pages, to_replace, "")
  
  
  summary_page <- paste0(server_root_subpages, list.files(path = server_root_subpages, pattern= 'Staffing EOI summary',full.names = FALSE))
  to_replace <- paste0(server_root_subpages, '|', '.html')
  names(summary_page) <- str_replace_all(summary_page, to_replace, "")
  
  ind_pages <- summary_page %>% append(ind_pages)
  
  
  setwd(rmd_files)
  rmarkdown::render(paste0(rmd_files, '02 Staffing.Rmd'), 
                    output_dir = server_root_indexes)
  


#################################################### TBC ###########################################################