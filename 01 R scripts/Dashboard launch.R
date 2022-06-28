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
from_staging       <- FALSE
flex_headline_size <- 24
check_ballot_track_data <- FALSE


## Source for folder locations/functions/db connections ----
## Jason wants us to connect to each database as required, rather than all of them, then disconnect
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))


#can't save on server yet, setting this up temporarily
#TomTest<-"C:/Users/harrisont/source/Workspaces/DataAnalytics/Election_Events/LG2101/02 Dashboard/03 Working pages"


## Load packages
dashboard.packages = dashboard.packages = c('MASS',
                                            'readxl',
                                            'bsselectR',
                                            'DT',
                                            'data.table',
                                            'RSQLite',
                                            'drake',
                                            'CodeDepends',
                                            'downloader',
                                            'janitor',
                                            'questionr',
                                            'purrr',
                                            'DataCombine',
                                            'colr',
                                            'tidyr',
                                            'readr',
                                            'survey',
                                            'stringr',
                                            'rmarkdown',
                                            'ggplot2',
                                            'ggpubr',
                                            'dplyr',
                                            'GGally',
                                            'lemon',
                                            'highcharter',
                                            'forcats',
                                            'sjPlot',
                                            'plotly',
                                            'gsubfn',
                                            'summarywidget',
                                            'kableExtra',
                                            'crosstalk',
                                            'gridExtra',
                                            'formattable',
                                            'flexdashboard',
                                            'RODBC',
                                            'RODBCext',
                                            'leaflet',
                                            'leaflet.extras',
                                            'leaflet.providers',
                                            'matrixStats',
                                            'Hmisc',
                                            'corrplot',
                                            'ggthemes',
                                            'cowplot',
                                            'lubridate',
                                            'fuzzyjoin',
                                            'stringdist',
                                            'htmltools',
                                            'apaTables',
                                            'nlme',
                                            'car',
                                            'Cairo',
                                            'cairoDevice',
                                            'tidyverse',
                                            'openxlsx',
                                            'knitr',
                                            'scales',
                                            'reshape2',
                                            'rstudioapi',
                                            'snakecase',
                                            'ggiraph',
                                            'geosphere',
                                            'jsonlite',
                                            'ellipsis',
                                            'fansi',
                                            'chron',
                                            'ggiraph',
                                            'Rgraphviz',
                                            'graph',
                                            'BiocGenerics',
                                            'visNetwork',
                                            'parallel',
                                            'RColorBrewer',
                                            'epiDisplay',
                                            'shiny', 
                                            'shinydashboard',
                                            'networkD3',
                                            'rjson',
                                            'httr',
                                            'compare',
                                            'ggrepel',
                                            'directlabels',
                                            'flextable',
                                            'officer',
                                            'jsonify',
                                            'rgdal',
                                            'mudata2',
                                            'rlang',
                                            'taskscheduleR',
                                            'summarywidget')


## Load the list of packages
ipak(dashboard.packages)


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


## Set each dashboard to run or not
index         <- TRUE
Venues        <- FALSE
Staffing      <- FALSE
EarlyVoting   <- FALSE
DecVote       <- FALSE
Noms          <- TRUE
HTV           <- FALSE
BPP           <- FALSE
BallotTrack   <- FALSE
ROOps         <- FALSE


ResultsProc   <- FALSE
EEC           <- FALSE
RO            <- FALSE

#with this set as true, some datasets will call on LG17 data instead.
#Remove when no longer required.
TestWithOldData<-TRUE


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


## 
if (Venues == TRUE) {
  
  if (TestWithOldData) {
    #using this as a flag for the new data
    event_group_ID <- "LG2101"
  }
  ## Create Venues data and objects
  #open database connections
  source(paste0(script_files, 'Dashboard Opening Database Connections.R'))
  source(paste0(data_gen,     '01 Venues data generator.R'))
  source(paste0(data_proc,    '01 Venues data processor.R'))
  
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
  
  if (TestWithOldData) {
    event_group_ID<-"LG2003)"
  }
}





## 02 Staffing ---------------------------------------------------------------------------


## Start when staffing EOI start, review ALC mapping tool code
if (Staffing == TRUE) {
  
  
  if (FALSE) { ## This is only run upon request by Janet
    
    source(paste0(data_proc, '02_02 SEO mapping.R'))
    
    ## Change output_dir
    setwd(paste0(rmd_files, 'Secured pages/'))
    rmarkdown::render(paste0(rmd_files, 'Secured pages/', '02_02 SEO mapping.Rmd'), 
                      output_dir = server_root_securedpages)
    
  }
  
  ## TBC    
  dev_code <- FALSE
  source(paste0(data_gen,  '02 Staffing data generator.R'))
  source(paste0(data_proc, '02 Staffing data processor.R'))
  
  setwd(rmd_files)
  rmarkdown::render(paste0(rmd_files, '02 Staffing.Rmd'), 
                    output_dir = server_root_indexes)
  
}





## 03 Early Voting --------------------------------------------------------


## Generate the EV dashboard data
if (EarlyVoting == TRUE) {
  
  
  if (TestWithOldData) {
    event_group_ID <- "LG1701"
  }
  
  ## If the connection is slow, load the data manually
  source(paste0(script_files,'Dashboard Opening Database Connections.R'))
  source(paste0(data_gen,  '03 Early Voting data generator.R'))
  source(paste0(data_proc, '03 Early Voting data processor.R'))
  source(paste0(data_proc, '03 Early Voting data plots.R'))
  
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
    event_group_ID<-"LG2003)"
  }
  
}





## 04 Dec Vote --------------------------------------------------------


## Generate the Dec Vote dashboard
if (DecVote == TRUE) {
  
  #adding some code so I can test with filled data
  #remove when finished
  #also remove reset at bottom. And update filter at top.
  
  
  if (TestWithOldData) {
    event_group_ID <- "LG1701"
  }
  
  ## Create Nominations data 
  source(paste0(data_gen,  '04 Declaration Voting data generator.R'))
  source(paste0(data_proc, '04 Declaration Voting data processor.R'))
  #source(paste0(data_proc, '04 Declaration Voting data plots.R'))
  
  ## Knit the sub-page to the sub-directory 
  setwd(paste0(sub_pages))
  rmarkdown::render('04 Declaration Voting Sub.Rmd', 
                    output_dir = server_root_subpages)
  #output_dir=TomTest)
  
  #save.image(file = "Declartion_Voting_work_space.RData")
  
  ## Knit the main page to the index directory
  setwd(rmd_files)
  rmarkdown::render('04 Declaration Voting.Rmd', 
                    output_dir = server_root_indexes)
  
  #testing
  # setwd(TomTest)
  # rmarkdown::render('04 Declaration Voting.Rmd',
  #                   output_dir = server_root_indexes)
  
  
  
  #adding some code so I can test with filled data
  #remove when finished
  #also remove setting in at top
  if (TestWithOldData) {
    event_group_ID<-"LG2003)"
  }
  
}





## 05 Nominations processing --------------------------------------------------------


## Generate the Noms dashboard data
if (Noms == TRUE) {
  
  ## Create Nominations data 
  source(paste0(data_gen,  '05 Candidate Nominations data generator.R'))
  source(paste0(data_proc, '05 Candidate Nominations data processor.R'))
  
  ## Knit the sub-page to the sub-directory 
  #save.image(file = paste0(data_gen, "Dashboard_Noms_data.RData"))
  setwd(sub_pages)
  rmarkdown::render('05 Candidate Nominations Sub.Rmd', 
                    output_dir = server_root_subpages)
  
  ## Knit the main page to the index directory
  setwd(rmd_files)
  rmarkdown::render('05 Candidate Nominations.Rmd', 
                    output_dir = server_root_indexes)
  
}





## 06 How to Vote --------------------------------------------------------


##
if (HTV == TRUE) {
  
  ## Create HTV data 
  source(paste0(data_gen,  '06 How To Vote data generator.R'))
  source(paste0(data_proc, '06 How To Vote data processor.R'))
  
  ## Knit the sub-page to the sub-directory 
  #save.image(file = paste0(data_gen, "Dashboard_Noms_data.RData"))
  setwd(sub_pages)
  rmarkdown::render('06 How to Vote Sub.Rmd', 
                    output_dir = server_root_subpages)
  
  ## Knit the main page to the index directory
  setwd(rmd_files)
  rmarkdown::render('06 How to Vote.Rmd', 
                    output_dir = server_root_indexes)
  
}



## 07 Ballot Paper Proofing ----------------------------------------------


## 
if (BPP == TRUE) {
  
  ## Create BPP data 
  source(paste0(data_gen,  '07 Ballot Paper Proofing data generator.R'))
  source(paste0(data_proc, '07 Ballot Paper Proofing data processor.R'))
  
  ## Knit the sub-page to the sub-directory 
  setwd(sub_pages)
  rmarkdown::render('07 BP Proofing Sub.Rmd', 
                    output_dir = server_root_subpages)
  
  ## Knit the main page to the index directory
  setwd(rmd_files)
  rmarkdown::render('07 BP Proofing.Rmd', 
                    output_dir = server_root_indexes)
  
}





## 08 Ballot Track --------------------------------------------------------


##
if (BallotTrack == TRUE) {
  
  ## Create BT data 
  source(paste0(data_gen,  '08 Ballot Track data generator.R'))
  source(paste0(data_proc, '08 Ballot Track data processor.R'))
  
  ## Knit the sub-page to the sub-directory 
  #save.image(file = paste0(data_gen, "Dashboard_Noms_data.RData"))
  setwd(sub_pages)
  rmarkdown::render('08 Ballot Track Sub.Rmd', 
                    output_dir = server_root_subpages)
  
  ## Knit the main page to the index directory
  setwd(rmd_files)
  rmarkdown::render('08 Ballot Track.Rmd', 
                    output_dir = server_root_indexes)
}





## 09 RO Operations --------------------------------------------------------


## 
if (RO_Ops == TRUE) {
  
  if (TestWithOldData) {
    event_group_ID <- "LG1701"
  }
  
  

    source(paste0(script_files,'Dashboard Opening Database Connections.R'))
    ## source data as needed, copy of existing code here.
    source(paste0(data_gen,  '09 RO data generator.R'))
  
    source(paste0(data_proc, '09 RO Ops data processor.R'))
    

  if (TestWithOldData) {
    event_group_ID<-"LG2003)"
  }
  
  
}





## 00 index ----------------------------------------------------------------


##
if(index == TRUE) {
  
  ## brewer.pal(10, 'Set3')
  ## brewer.pal(10, 'Paired')
  ## brewer.pal(10, 'Dark')
  setwd(rmd_files)
  rmarkdown::render('00 Index.Rmd', 
                    output_dir = server_root_indexes)
  
}





## Sub-page RMD files
## Knit only once        
## 99 Call sub page rmds ------------------------------------------------------
## Something like below
for (i in 1:length(Contests)) {
  
  ## 
  rmarkdown::render(paste0(rmd_files,'02 Staffing.Rmd'), 
                    output_dir = server_root_subpages)
  rename( '','')
  
}





#################################################### TBC ###########################################################