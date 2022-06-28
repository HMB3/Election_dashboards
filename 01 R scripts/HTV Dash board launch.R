################################### ----- LG2101 DASHBOARD PIPELINE ---- ######################################



## This pipeline is used to create all the dashboards for the 2021  Local Government election  


## Dashboard strucure has 4 main sections ----


# 1. Data extraction from Database is in a seperate file, so that when integration to hyperion is done it can be 
#    amended accordingly (initial thought is to add hyperion view code to process codes and maintain the raw data 
#    code for hyperion failure readiness)dev


# 2. Data processing mapping done in this phase


# 3. RMD files in indexes, and in sub pages (two separate RMD files for each dashboard)


# 4. HTML pages will be knitted straight to the server, and all within the launch script. 
#    The initial plan is to have 2 launch scripts: 1 for indexes, and 1 for subs (i.e. pages)



## Set parameters for analysis ----
from_staging          <- FALSE
flex_headline_size    <- 20
use_data_vault        <- TRUE
HTV                   <- TRUE
knit_rmd              <- TRUE
test_data             <- FALSE
endpoint_skip         <- TRUE


## Source for folder locations/functions/db connections ----
## Jason wants us to connect to each database as required, rather than all of them, then disconnect
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))


## Load packages - we need them all for HTV!
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
library(scales)


## Source the functions created just for the dashboard 
source(paste0(Sys.getenv("source_control_local_path"), 
              "Election_Events/LG2101/02 Dashboard/01 R scripts/Functions/dashboard_library.R"))



## Set directories for analysis
## Source the functions used only for the dashboard - I.E. graphs, etc.
root_dashboard  <- paste0(Sys.getenv('source_control_local_path'), 
                          'Election_Events/LG2101/02 Dashboard/')

## 
script_files    <- paste0(root_dashboard, '01 R scripts/')
data_gen        <- paste0(root_dashboard, '01 R scripts/_Data generators/')

if(use_data_vault) {
  data_gen         <- paste0(root_dashboard, '01 R scripts/_Data generators NSWECDV/')
}



data_source     <- paste0(root_dashboard, '02 Source data/')
data_proc       <- paste0(root_dashboard, '01 R scripts/_Data processors/')


## 
rmd_files        <- paste0(root_dashboard, '01 R scripts/_RMD files/')
data_files       <- paste0(root_dashboard, '02 Source data/')
image_files      <- paste0(root_dashboard, '01 R scripts/_RMD files/Dashboard_graphs')
sub_pages        <- paste0(root_dashboard, '01 R scripts/_RMD files/Sub pages')
noms_data        <- '//seofp1/data/Election Events/LGE 2020 Programme/AP02B Candidates & Parties/Candidates, Electoral Material, Ballot Papers/Dashboards/'


## Source functions 
source(paste0(root_dashboard, '01 R scripts/Functions/dashboard_plotting_library.R'))
source(paste0(root_dashboard, '01 R scripts/Functions/dashboard_functions_library.R'))



## Set server locations
server_root_indexes      <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Indexes/'
server_root_subpages     <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Indexes/Sub pages/'
server_root_securedpages <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Secured pages/'
server_messages          <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Data/Messages/'
HTV_server_data          <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Data/HTV/'
Noms_server_data         <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Data/Noms/'


## Set global R options : 
## Stop R triggering scientific notation for the y-axis
## suppress summarise warnings
options(dplyr.summarise.inform = FALSE, scipen = 999)

# 
# ## Capture messages and errors to a file - this should work for all code
# zz <- file(paste0(server_messages, 'HTV_messages.txt'), open = "wt")
# sink(zz, type = "message")


## Open database connection
if(use_data_vault) {
  
  message('Source Hyperion data')
  source(paste0(Sys.getenv("source_control_local_path"),
                "Main/R Scripts - Production/Database connections/Hyperion.R"))
  
} else {
  message('Source EMA data')
  source(paste0(Sys.getenv("source_control_local_path"),
                "Main/R Scripts - Production/Database connections/EMA.R"))
}





## 06 How to Vote --------------------------------------------------------



## Create HTV data 
source(paste0(data_gen,  '06 How To Vote data generator.R'))
source(paste0(data_proc, '06 How To Vote data processor.R'))


## Knit the sub-page to the sub-directory 
#save.image(file = paste0(data_gen, "Dashboard_Noms_data.RData"))
if(knit_rmd) {
  
  setwd(sub_pages)
  rmarkdown::render('06 How to Vote Sub.Rmd', 
                    output_file = '06-How-to-Vote-Sub.html',
                    output_dir = server_root_subpages)
  
  ## Knit the main page to the index directory
  setwd(rmd_files)
  rmarkdown::render('06 How to Vote.Rmd', 
                    output_file = '06-How-to-Vote.html',
                    output_dir = server_root_indexes)
  
}





## reset message sink and close the file connection
# sink(type = "message")
# close(zz)





#################################################### TBC ###########################################################