####################################################################################################################
###################################### CONFIGURE DASHBAORDS ---- ###################################################
####################################################################################################################


## Create the file paths, etc. for the dashboard.


## Make a map of the existing directories?
## https://gist.github.com/jennybc/2bf1dbe6eb1f261dfe60#file-twee-r
## directory_tree = twee(dashboard_folder, level = 1)
sys_path <- paste0(Sys.getenv('source_control_local_path'))


####################################################################################################################
## Import folder locations
source(paste0(sys_path, 'Main/R Scripts - Production/Folder_locations.R'))
source(paste0(sys_path, 'Main/R Scripts - Production/Functions.R'))


## Change all relevant paths to use sys_path
.libPaths(paste0(sys_path, 'Main/R Package Library'))


dashboard.packages = c('readxl',
                       'bsselectR',
                       'DT',
                       'data.table',
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
                       'rgdal',
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
                       'bsselectR',
                       'sp',
                       'DT',
                       'RODBC',
                       'RODBCext',
                       'openxlsx',
                       'rmarkdown',
                       'leaflet',
                       'leaflet.extras',
                       'rgdal',
                       'rgeos',
                       'knitr',
                       'scales',
                       'reshape2',
                       'rstudioapi',
                       'snakecase',
                       'highcharter',
                       'summarywidget',
                       'crosstalk',
                       'formattable',
                       'flexdashboard',
                       'ggiraph',
                       'geosphere',
                       'jsonlite',
                       'ellipsis',
                       'fansi')


#sapply(dashboard.packages, require, character.only = TRUE)
ipak(dashboard.packages)


#knitmode="dev"
knitmode = "prod"


#================Global config================
base_folder     <- paste0(sys_path, 'Election_Events/LG2001/07_Dashboard_RMT_and_EOG/')
report_folder   <- paste0(sys_path, 'Election_Events/LG2001/07_Dashboard_RMT_and_EOG/08 Reports/')
transfer_folder <- 'G:/Transfer/Data Analytics/LG2001 Dashboard'


relative_output        = '11 Website Children/'
relative_reference     = '01 Reference Files/'


dashboard_folder       <- paste0(base_folder, '04_R_scripts/')
helpers_folder         <- paste0(base_folder, '04_R_scripts/__Helpers/')
reference_folder       <- paste0(base_folder, '02 Source data/')


output_folder          <- paste0(base_folder, relative_output)
relative_output_folder <- paste0('../', relative_output)


production_base             <- '//SVRANALYTICS1/AnalyticsReports/Dashboard/'
production_folder           <- paste0('file://svranalytics1/AnalyticsReports/', relative_output)
production_reference_folder <- paste0('file://svranalytics1/AnalyticsReports/', relative_reference)
log_folder                  <- paste0(production_folder, 'Logs/')


minute <- 60
setwd(dashboard_folder)


#=============================================
source('G:/Elections Branch/Election Support/Data Analytics/R Scripts - Production/Database connections/AllServerLogins.R')
source('G:/Elections Branch/Election Support/Data Analytics/R Scripts - Production/SourceRepo.R')


#=============================================
render_interval <- minute
#=================Page config=================


## Create ballot track folders
ballot_track_input_folder             <- paste0(dashboard_folder,       '_Ballot Track/')
ballot_track_output_folder            <- paste0(output_folder,          'Ballot Track/')
relative_ballot_track_output_folder   <- paste0(relative_output_folder, 'Ballot Track/')
production_ballot_track_output_folder <- paste0(production_folder,      'Ballot Track/')
ballot_track_log_file                 <- paste0(log_folder,             'BallotTrack.txt')
ballot_track_render_interval          <- minute * 10


## Create candidate nominations folders
candidate_noms_input_folder              <- paste0(dashboard_folder,       '_Candidate Noms/')
candidate_noms_output_folder             <- paste0(output_folder,          'Candidate Noms/')
relative_candidate_noms_output_folder    <- paste0(relative_output_folder, 'Candidate Noms/')
production_candidate_noms_output_folder  <- paste0(production_folder,      'Candidate Noms/')
candidate_noms_log_file                  <- str_replace(paste0(log_folder, 'CandidateNoms.txt'), 'file:', '')
candidate_noms_render_interval           <- minute * 15


## Create districts folders
districts_input_folder              <- paste0(dashboard_folder,        '_Districts/')
districts_output_folder             <- paste0(output_folder,           'Districts/')
relative_districts_output_folder    <- paste0(relative_output_folder,  'Districts/')
production_districts_output_folder  <- paste0(production_folder,       'Districts/')
districts_log_file                  <- str_replace(paste0(log_folder,  'Districts.txt'), 'file:', '')
districts_render_interval           <- minute * 10


## Create early voting folders
early_voting_input_folder             <- paste0(dashboard_folder,       '_EarlyVoting/')
early_voting_output_folder            <- paste0(output_folder,          'EarlyVoting/')
relative_early_voting_output_folder   <- paste0(relative_output_folder, 'EarlyVoting/')
production_early_voting_output_folder <- paste0(production_folder,      'EarlyVoting/')
ealy_voting_log_file                  <- str_replace(paste0(log_folder, 'EarlyVoting.txt'), 'file:', '')
early_voting_render_interval          <- minute * 45


## Create eec folders
eec_input_folder             <- paste0(dashboard_folder,         '_EEC/')
eec_output_folder            <- paste0(output_folder,            'EEC/')
relative_eec_output_folder   <- paste0(relative_output_folder,   'EEC/')
production_eec_output_folder <- paste0(production_folder,        'EEC/')
eec_log_file                 <- str_replace(paste0(log_folder,   'EEC.txt'), 'file:', '')
eec_render_interval          <- minute * 10 ## UNUSED - no children - rendered in [Site Launcher.R]


## Create how_to_vote folders
how_to_vote_input_folder             <- paste0(dashboard_folder, '_How To Vote/')
how_to_vote_output_folder            <- paste0(output_folder,   'How To Vote/')


relative_how_to_vote_output_folder   <- paste0(relative_output_folder,   'How To Vote/')
production_how_to_vote_output_folder <- paste0(production_folder,        'How To Vote/')
how_to_vote_log_file                 <- str_replace(paste0(log_folder,   'HowToVote.txt'), 'file:', '')
how_to_vote_render_interval           = minute * 60


## Create i_vote folders
ivote_input_folder               <- paste0(dashboard_folder,        '_iVote/')
ivote_output_folder              <- paste0(output_folder,           'iVote/')
relative_ivote_output_folder     <- paste0(relative_output_folder,  'iVote/')
production_ivote_output_folder   <- paste0(production_folder,       'iVote/')
ivote_log_file                   <- str_replace(paste0(log_folder,  'iVote.txt'), 'file:', '')
ivote_render_interval            <- minute * 15


## Create results folders
results_audit_input_folder              <- paste0(dashboard_folder,       '_Results Audit/')
results_audit_output_folder             <- paste0(output_folder,          'Results Audit/')
relative_results_audit_output_folder    <- paste0(relative_output_folder, 'Results Audit/')
production_results_audit_output_folder  <- paste0(production_folder,      'Results Audit/')
results_audit_log_file                  <- str_replace(paste0(log_folder, 'ResultsAudit.txt'), 'file:', '')
results_audit_render_interval           <- minute * 3


## Create rmt folders
rmt_input_folder             <- paste0(dashboard_folder,       '_RMT/')
rmt_output_folder            <- paste0(output_folder,          'RMT/')
relative_rmt_output_folder   <- paste0(relative_output_folder, 'RMT/')
production_rmt_output_folder <- paste0(production_folder,      'RMT/')
rmt_log_file                 <- str_replace(paste0(log_folder, 'RMT.txt'), 'file:', '')
rmt_render_interval          <- minute * 10


## Create staffing folders
staffing_input_folder             <- paste0(dashboard_folder,        '_Staffing/')
staffing_output_folder            <- paste0(output_folder,           'Staffing/')
relative_staffing_output_folder   <- paste0(relative_output_folder,  'Staffing/')
production_staffing_output_folder <- paste0(production_folder,       'Staffing/')
staffing_log_file                 <- str_replace(paste0(log_folder,  'Staffing.txt'), 'file:', '')
staffing_render_interval          <- minute * 20


## Create staffing folders
venues_input_folder               <- paste0(dashboard_folder,        '_Venues/')
venues_output_folder              <- paste0(output_folder,           'Venues/')
relative_venues_output_folder     <- paste0(relative_output_folder,  'Venues/')
production_venues_output_folder   <- paste0(production_folder,       'Venues/')
venues_log_file                   <- str_replace(paste0(log_folder,  'Venues.txt'), 'file:', '')
venues_render_interval            <- minute * 10 ## UNUSED - no children - rendered in [Site Launcher.R]


#====================Other Paths==========================
maps_pdf_path <- "G:/Mapping/SGE2019/PPReview/Maps/SGE19_Final_Maps_IT"


#====================Environment=======================
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")


#================Highcharter Theme==================
newtheme <- hc_theme_merge(
  
  getOption("highcharter.theme"),  
  hc_theme(colors = c("#2980b9", "#ffc77f", "#ba0938", "#097726"))
  
)


options(highcharter.theme = newtheme)


#=====================Logging======================
logStatus <- function(logfile, message){
  
  if(!file.exists(logfile)){
    
    file.create(logfile, showWarnings = TRUE)
  }
  
  write(message, file = logfile, append = TRUE)
  
}

#===========================UI======================
if(knitmode == "dev"){
  
  black_icon = makeIcon(paste0(reference_folder, "UI/marker-icon-black.png"), 18, 18, iconAnchorX = 9, iconAnchorY = 18)
  red_icon   = makeIcon(paste0(reference_folder, "UI/marker-icon-red.png"),   18, 18, iconAnchorX = 9, iconAnchorY = 18)
  blue_icon  = makeIcon(paste0(reference_folder, "/UI/marker-icon-blue.png"), 18, 18, iconAnchorX = 9, iconAnchorY = 18)
  green_icon = makeIcon(paste0(reference_folder, "UI/marker-icon-green.png"), 18, 18, iconAnchorX = 9, iconAnchorY = 18)
  
} else
  
{
  black_icon = makeIcon(paste0(production_reference_folder, "UI/marker-icon-black.png"), 18, 18, iconAnchorX = 9, iconAnchorY = 18)
  red_icon   = makeIcon(paste0(production_reference_folder, "UI/marker-icon-red.png"),   18, 18, iconAnchorX = 9, iconAnchorY = 18)
  blue_icon  = makeIcon(paste0(production_reference_folder, "/UI/marker-icon-blue.png"), 18, 18, iconAnchorX = 9, iconAnchorY = 18)
  green_icon = makeIcon(paste0(production_reference_folder, "UI/marker-icon-green.png"), 18, 18, iconAnchorX = 9, iconAnchorY = 18)
}



## Set the directory back to the top
setwd(base_folder)

