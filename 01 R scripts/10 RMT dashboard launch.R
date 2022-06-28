################################### ----- LG2101 DASHBAORD PIPELINE ---- ######################################



## This pipeline is used to create all the dashboards for the 2021  Local Government election  

## Set parameters for analysis ----
event_group_ID          <- 'LG2103'
api_key                 <- "AIzaSyB-ltOEpu8ZwF7H-ZOIIp5UmXHwewu27eA"
from_staging            <- TRUE
not_load_prcc <- TRUE

## Source for folder locations/functions/db connections ----
## Jason wants us to connect to each database as required, rather than all of them, then disconnect
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))



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


## Stop R triggering scientific notation for the y-axis
options(dplyr.summarise.inform = FALSE, scipen = 999)


## The actual data extraction and knitting starts from here
#open database here
source(paste0(script_files,'Dashboard Opening Database Connections.R'))
## Analytics database connections ----
setwd(database_connections)
source('DataVault.R')
source("EMA.R") # potentially load everything from hyperion later this is temp

## Create common data needed ----
source(paste0(data_gen, '00 Common data generator.R'))



## 10 RMT --------------------------------------------------------------
if (FALSE) {  # this is temp off due to access to staging tables causing code to fail, mean while all else needed to run
  

## The actual data extraction and knitting starts from here

source(paste0(data_gen,  '10 RMT 01 Prepoll data generator.R'))
source(paste0(data_proc, '10 RMT 01 Prepoll data processor.R'))

  

  #Knit the sub-page to the sub-directory 
  setwd(paste0(sub_pages))
  rmarkdown::render('10 RMT 01 Prepoll.Rmd', 
                    output_dir = server_root_subpages)
  
}

# 01.5 Initial count -------------------------------------------------------------

source(paste0(data_gen,  '10 RMT 01.5 Initial Count data generator.R'))
source(paste0(data_proc, '10 RMT 01.5 Initial Count data processor.R'))

setwd(paste0(sub_pages))
rmarkdown::render('10 RMT 01.5 Initial Count.Rmd', 
                  output_dir = server_root_subpages)


# 02 Batching -------------------------------------------------------------

  source(paste0(data_gen,  '10 RMT 02 Batching data generator.R'))
  source(paste0(data_proc, '10 RMT 02 Batching data processor.R'))
  
  setwd(paste0(sub_pages))
  rmarkdown::render('10 RMT 02 Batching.Rmd', 
                    output_dir = server_root_subpages)
  
  
  

# index -------------------------------------------------------------------

  
  ## Knit the main page to the index directory
  # bs select
  
  ind_pages <- paste0(server_root_subpages, list.files(path = server_root_subpages, pattern= 'RMT',full.names = FALSE))
  to_replace <- paste0(server_root_subpages, '|', '.html')
  names(ind_pages) <- str_replace_all(ind_pages, to_replace, "")
  
  
  setwd(rmd_files)
  rmarkdown::render('10 RMT.Rmd', 
                    output_dir = server_root_indexes)
  
  



#################################################### TBC ###########################################################