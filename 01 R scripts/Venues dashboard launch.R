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
event_group_ID          <- 'LG2101'
api_key                 <- "AIzaSyB-ltOEpu8ZwF7H-ZOIIp5UmXHwewu27eA"
from_staging            <- FALSE
flex_headline_size      <- 24
check_ballot_track_data <- FALSE


## Source for folder locations/functions/db connections ----
## Jason wants us to connect to each database as required, rather than all of them, then disconnect
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))


#can't save on server yet, setting this up temporarily
#TomTest<-"C:/Users/harrisont/source/Workspaces/DataAnalytics/Election_Events/LG2101/02 Dashboard/03 Working pages"
# .libPaths('C:/Users/ChenJ/R Library')




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


#with this set as true, some datasets will call on LG17 data instead.
#Remove when no longer required.
TestWithOldData<-FALSE


## Set global R options : 
## Stop R triggering scientific notation for the y-axis
## suppress summarise warnings
options(dplyr.summarise.inform = FALSE, scipen = 999)


## The actual data extraction and knitting starts from here
#open database here
source(paste0(script_files,'Dashboard Opening Database Connections.R'))


## Create common data needed ----
source(paste0(data_gen, '00 Common data generator.R'))
#save.image(file = paste0(data_gen, "Dashboard_Common_data.RData"))





## 01 Venues and procurement --------------------------------------------------------------


  
  ## Create Venues data and objects
  #open database connections
  ## Create Venues data and objects
  #open database connections
#  source(paste0(script_files,'Dashboard Opening Database Connections.R'))
  source(paste0(data_gen,  '01 Venues data generator.R'))
  source(paste0(data_proc, '01 Venues data processor.R'))
  
  #Knit the sub-page to the sub-directory 
  setwd(paste0(sub_pages))
  rmarkdown::render('01 Venues and procurement Sub.Rmd', 
                    output_dir = server_root_subpages)
  #output_dir = TomTest)
  
  
  ## Knit the main page to the index directory
  
  setwd(rmd_files)
  rmarkdown::render('01 Venues and procurement.Rmd', 
                    output_dir = server_root_indexes)
  # output_dir = TomTest)
  




#################################################### TBC ###########################################################