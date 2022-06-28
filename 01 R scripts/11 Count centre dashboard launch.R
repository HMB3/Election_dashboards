################################### ----- LG2101 COUNT CENTRE PIPELINE ---- ######################################



## This pipeline is used to create all the dashboards for the 2021  Local Government election  


## New  dashboard strucure has 4 main sections ----


# 1. Data extraction from Database is in a seperate file, so that when integration to hyperion is done it can be 
#    amended accordingly (initial thought is to add hyperion view code to process codes and maintain the raw data 
#    code for hyperion failure readiness)


# 2. Data processing mapping done in this phase


# 3. RMD files in indexes, and in sub pages (two separate RMD files for each dashboard)


# 4. HTML pages will be knitted straight to the server, and all within the launch script. 
#    The initial plan is to have 2 launch scripts: 1 for indexes, and 1 for subs (i.e. pages).


## Set parameters for analysis ----
event_group_ID  <- 'LG2101'
from_staging    <- FALSE
headline_size   <- 16
knit_rmd        <- TRUE
use_data_vault  <- TRUE

## Source for folder locations/functions/db connections ----
## Jason wants us to connect to each database as required, rather than all of them, then disconnect
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))


## 
library(flexdashboard)
library(flextable)
library(officer)
library(highcharter)
library(tidyr)
library(dplyr)
library(lubridate)
library(RODBC)
library(RODBCext)
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
script_files     <- paste0(root_dashboard, '01 R scripts/')
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
  setwd(database_connections)
  source("Hyperion.R")
  
  source(paste0(data_gen,  '11 Count centre data generator.R'))
  source(paste0(data_proc, '11 Count centre data processor.R'))
  
  
  if(knit_rmd) {
    
    #Knit the sub-page to the sub-directory 
    setwd(sub_pages)
    rmarkdown::render('11 Count centre Sub.Rmd', 
                      output_dir = server_root_subpages)
    
    
    ## Knit the main page to the index directory
    setwd(rmd_files)
    rmarkdown::render('11 Count centre.Rmd', 
                      output_dir = server_root_indexes)
    
    }
  
  
}





#################################################### TBC ###########################################################