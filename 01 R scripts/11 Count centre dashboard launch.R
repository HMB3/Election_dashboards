################################### ----- LG2101 COUNT CENTRE PIPELINE ---- ######################################



## This pipeline is used to create all the dashboards for the 2021  Local Government election  


## New  dashboard strucure has 4 main sections ----


# 1. Data extraction from Database is in a seperate file, so that when integration to hyperion is done it can be 
#    amended accordingly (initial thought is to add hyperion view code to process codes and maintain the raw data 
#    code for hyperion failure readiness)


# 2. Data processing mapping done in this phase


# 3. RMD files in indexes, and in sub pages (two separate RMD files for each dashboard)


# 4. HTML pages will be knitted straight to the server, and all within the launch script. 
#    The initial plan is to have 2 launch scripts: 1 for indexes, and 1 for subs (i.e. pages)



## Set parameters for analysis -----
# event_group_ID is harvested from the NOMs endpoint data set
# From staging is FALSE for the live event
api_key             <- "AIzaSyB-ltOEpu8ZwF7H-ZOIIp5UmXHwewu27eA"
from_staging        <- FALSE
flex_headline_size  <- 24
use_data_vault      <- FALSE
knit_rmd            <- TRUE
endpoint_skip       <- TRUE


## Source for folder locations/functions/db connections ----
## Jason wants us to connect to each database as required, rather than all of them, then disconnect
# source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
# source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))


## Load packages
library(MASS)
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
library(janitor)
library(forcats)
library(curl)



## Source the functions created just for the dashboard 
load('./01 R scripts/Dashboard_Noms_data.RData')
source('./01 R scripts/Functions/dashboard_library.R')


## Set directories for analysis
## Source the functions used only for the dashboard - I.E. graphs, etc.

script_files       <- './01 R scripts/'
data_gen           <- './01 R scripts/_Data generators/'

if(use_data_vault) {
  data_gen         <- './01 R scripts/_Data generators NSWECDV/'
}

data_source        <- './02 Source data/'
data_proc          <- './01 R scripts/_Data processors/'
rmd_files          <- './01 R scripts/_RMD files/'
data_files         <- './02 Source data/'
image_files        <- './01 R scripts/_RMD files/Dashboard_graphs'
sub_pages          <- './01 R scripts/_RMD files/Sub pages'



source('./01 R scripts/Functions/dashboard_plotting_library.R')
source('./01 R scripts/Functions/dashboard_functions_library.R')



## Set server locations
server_root_indexes      <- './08 Reports/Indexes/'
server_root_subpages     <- './08 Reports/Indexes/Sub pages/'
server_root_securedpages <- './08 Reports/Secured pages/'
server_messages          <- './08 Reports/Data/Messages/'
Noms_server_data         <- './08 Reports/Data/Noms/'



## Set global R options : 
## Stop R triggering scientific notation for the y-axis
## suppress summarise warnings
options(dplyr.summarise.inform = FALSE, scipen = 999)


## Set each dashboard to run or not
index         <- FALSE
Venues        <- FALSE
Staffing      <- FALSE
EarlyVoting   <- FALSE
DecVote       <- FALSE
Noms          <- FALSE
HTV           <- FALSE
BPP           <- FALSE
BallotTrack   <- FALSE
ROOps         <- FALSE


ResultsProc   <- FALSE
EEC           <- FALSE
RO            <- FALSE
CCMT          <- TRUE




## Set global R options : 
## Stop R triggering scientific notation for the y-axis
## suppress summarise warnings
options(dplyr.summarise.inform = FALSE, scipen = 999)



## 01 Venues and procurement --------------------------------------------------------------


## 
if (CCMT) {
  
  ## open database connections
  # setwd(database_connections)
  # source("Hyperion.R")
  # 
  # source(paste0(data_gen,  '11 Count centre data generator.R'))
  load('./01 R scripts/Dashboard_CC_data.RData')
  # source('./01 R scripts/_Data processors/11 Count centre data processor.R')
  
  
  if(knit_rmd) {
    
    #Knit the sub-page to the sub-directory 
    rmarkdown::render('./01 R scripts/_RMD files/Sub pages/11 Count centre Sub.Rmd', 
                      output_dir = './docs/')
    
    
    ## Knit the main page to the index directory
    rmarkdown::render('01 R scripts/_RMD files/11 Count centre.Rmd', 
                      output_dir = './docs/')
    
    }
}





#################################################### TBC ###########################################################