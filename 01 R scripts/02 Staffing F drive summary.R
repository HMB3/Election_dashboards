
event_group_ID     <- 'LG2101'


dir_backup_time <- '2100'
AoBP_data <- FALSE
Form123Report <- FALSE

source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))

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



# source back up script
setwd(paste0(Sys.getenv("source_control_local_path"), 'Election_Events/LG2101/01 R launch scripts'))
source('LG2101 RO_Time_sheet_Roster_extraction.R')



# locate folder and remove bulk keeping 1 per day -------------------------


list.dirs(staffing_secured_backup_folder)

remove_folders <- data.frame(path = list.dirs(staffing_secured_backup_folder)) %>%
  mutate(subs = gsub(staffing_secured_backup_folder,'',path)) %>%
  mutate(item = word(gsub('/',' ',subs),1,4)) %>%
  mutate(date = substrRight(subs,15)
         ,time = substrRight(subs,4)) %>%
  mutate(date = substr(date,1,10)) %>%
  group_by(item,date) %>%
  filter(time != max(time))

# unlink(remove_folders$path, recursive = T)                      

lapply(as.character(remove_folders$path), FUN = fs::dir_delete)

# pull processing code

setwd(data_proc)
source('02_04 Staffing time sheet.R')

setwd(data_proc)
source('02_05 Staffing RO roster.R')

