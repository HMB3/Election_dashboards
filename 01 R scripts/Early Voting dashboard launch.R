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
event_group_ID        <- 'LG2101'
from_staging          <- FALSE
use_data_vault        <- TRUE
flex_headline_size    <- 22



#some venues were opened on the wrong days (i.e. not when EMS said they were open). Need a way to manually set those days to being 'open' 

#list pre-polls which have been open at incorrect times

Incorrect_Prepolls <- c("Jindera Pre-Poll")

#days which need to be set as open for pre-polls which were incorrectly opened

Incorrect_Prepoll_days<- c("Monday, 22 Nov 2021",
                             "Tuesday, 23 Nov 2021")



#with this set as true, some datasets will call on LG2103 data instead.
#Remove when no longer required.
TestWithOldData <- FALSE


## Source for folder locations/functions/db connections ----
## Jason wants us to connect to each database as required, rather than all of them, then disconnect
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))


#can't save on server yet, setting this up temporarily
#TomTest<-"C:/Users/harrisont/source/Workspaces/DataAnalytics/Election_Events/LG2101/02 Dashboard/03 Working pages"


## Load packages


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




## Source the functions created just for the dashboard 
source(paste0(Sys.getenv("source_control_local_path"), 
              "Election_Events/LG2101/02 Dashboard/01 R scripts/Functions/dashboard_library.R"))



## Set directories for analysis
## Source the functions used only for the dashboard - I.E. graphs, etc.
root_dashboard   <- paste0(Sys.getenv('source_control_local_path'), 
                           'Election_Events/LG2101/02 Dashboard/')

script_files     <-paste0(root_dashboard, '01 R scripts/')
data_gen         <- paste0(root_dashboard, '01 R scripts/_Data generators/')

if(use_data_vault) {
  data_gen       <- paste0(root_dashboard, '01 R scripts/_Data generators NSWECDV/')
}

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
server_data              <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Data/'
server_messages          <- '//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Data/Messages/'
server_root_subpage_files <- paste0(server_root_subpages, '03-Early-Voting-Sub_files/')

#local version for testing.
#TomTest<-paste0(Sys.getenv('source_control_local_path'),'Election_Events/LG2101/02 Dashboard/03 Working pages')


## Clean out sub-page program files (after RMD self_contained = TRUE has been set this can be deleted)
if(dir.exists(server_root_subpage_files)) {
  
  if(length(list.dirs(server_root_subpage_files, recursive = TRUE)) > 1){
    
    #do.call(file.remove, list(list.files(server_root_subpage_files, full.names = TRUE, recursive = TRUE)))
    
    directories_to_delete <- list.dirs(server_root_subpage_files, full.names = TRUE, recursive = FALSE)
    
    dir_delete(directories_to_delete)
    
  }
  
}



## Set global R options : 
## Stop R triggering scientific notation for the y-axis
## suppress summarise warnings
options(dplyr.summarise.inform = FALSE, scipen = 999)


## Capture messages and errors to a file - this should work for all code
# zz <- file(paste0(server_messages, 'Early_Voting_messages.txt'), open = "wt")
# sink(zz, type="message")


## The actual data extraction and knitting starts from here
#open database here
source(paste0(script_files,'Dashboard Opening Database Connections.R'))


## Create common data needed -----
source(paste0(data_gen, '00 Common data generator.R'))
#save.image(file = paste0(data_gen, "Dashboard_Common_data.RData"))





## 03 Early Voting --------------------------------------------------------


## Generate the EV dashboard data


if (TestWithOldData) {
  
  event_group_ID <- "LG2103"
  from_staging          <-  TRUE
}

## If the connection is slow, load the data manually
source(paste0(data_gen,  '03 Early Voting data generator.R'))
source(paste0(data_proc, '03 Early Voting data processor.R'))

## Knit the sub-page to the sub-directory 
setwd(paste0(sub_pages))
rmarkdown::render('03 Early Voting Sub.Rmd', 
                  output_dir = server_root_subpages)
# output_dir = TomTest)

#save.image(file = "Early_Voting_work_space.RData")

## Knit the main page to the index directory
setwd(rmd_files)
rmarkdown::render('03 Early Voting.Rmd', 
                  output_dir = server_root_indexes)
# output_dir = TomTest)

if (TestWithOldData) {
  event_group_ID<-"LG2003"
  from_staging          <- FALSE
}




## reset message sink and close the file connection
# sink(type="message")
# close(zz)





#################################################### TBC ###########################################################