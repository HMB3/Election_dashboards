
########## wasnt able to use sxconnect to on load 2 columns to increase efficacy need to try out latr


## Set parameters for analysis ----
event_group_ID     <- 'LG2101'

## Source for folder locations/functions/db connections ----
## Jason wants us to connect to each database as required, rather than all of them, then disconnect
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))


library(flexdashboard)
library(RODBC)
library(RODBCext)
library(summarywidget)
library(bsselectR)
library(rgdal)
library(DT)
library(htmltools)
library(knitr)
library(ggplot2)
library(XLConnect)


#local version for testing.
#TomTest<-paste0(Sys.getenv('source_control_local_path'),'Election_Events/LG2101/02 Dashboard/03 Working pages')



## Set global R options : 
## Stop R triggering scientific notation for the y-axis
## suppress summarise warnings
options(dplyr.summarise.inform = FALSE, scipen = 999)


# analyse the latest backed up work books ---------------------------------

backup_folder_name <- paste0(staffing_secured_backup_folder, 'RO Staff roster')


  dir_explore <- max(list.dirs(backup_folder_name))
  
  file_list <- list.files(dir_explore)
  
  setwd(dir_explore)


Roster_vaxstat <- data.frame()

for(f in 1:length(file_list)) {
  if(file.size(file_list[f]) >0) {
  print(file_list[f])
  ram1 <- readxl::read_xlsx(file_list[f], sheet = 'Staff Roster', skip=31)[,1:2] # works but slow
  
  Area <- ram1 %>% mutate(AreaCode = word(file_list[f],1)) #paste0(ram1[2,5], ram1[2,6]))
  
  
  
  
  Roster_vaxstat <-Roster_vaxstat %>% bind_rows(Area) %>% filter(!`Employee Name` %in% c('General Office'                
                                                                                       ,'Orientation & Training'        
                                                                                       ,'Voting Centre'                 
                                                                                       ,'Declared Facility Vote Issuing'
                                                                                       ,'Early Voting'                  
                                                                                       ,'Dress Rehearsal'               
                                                                                       ,'Initital Counts'               
                                                                                       ,'Return of Materials'           
                                                                                       ,'Election Night'                
                                                                                       ,'Post Election Night'           
                                                                                       ,'Batching & Data Entry'         
                                                                                       ,'Quality Assurance'
                                                                                       ,'0')) %>%
                                                          filter(!is.na(`Employee Name`)) %>%
    select(AreaCode,`Employee Name`,`Vaccination Cert Sighted`)
}
}

Roster_vaxstat_clean <- Roster_vaxstat %>% mutate(`Vaccination Cert Sighted` = ifelse(grepl('YES',toupper(`Vaccination Cert Sighted`)), 'Yes','No'))

Roster_summary <- CrossTab(Roster_vaxstat_clean, 'AreaCode','Vaccination Cert Sighted') %>% spread(`Vaccination Cert Sighted`,Count) %>%
  select(AreaCode, Yes, No)
  

setwd(paste0(rmd_files,'/Secured pages'))
rmarkdown::render('02_05 RO roster vaccine status.Rmd', 
                  output_dir = server_root_securedpages)
