

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

## Source for folder locations/functions/db connections ----
## Jason wants us to connect to each database as required, rather than all of them, then disconnect
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))

library(flexdashboard)
library(DT)
library(htmltools)
library(knitr)
library(ggplot2)


# analyse the latest backed up work books ---------------------------------

backup_folder_name <- paste0(staffing_secured_backup_folder, 'RO Time sheet')


  dir_explore <- max(list.dirs(backup_folder_name))
  
  file_list <- list.files(dir_explore)
  
  setwd(dir_explore)


timesheet_summary <- data.frame()

for(f in 1:length(file_list)) {
  print(file_list[f])
  if(file.size(file_list[f]) >0) {
  
  ram1 <- data.frame(read_excel(path = file_list[f]))
  ram1[is.na(ram1)] <- ""
  
  wb_summary <- data.frame(Name = paste0(ram1[2,1],ram1[2,2],ram1[2,3])
                          ,District = word(file_list[f],1)) #paste0(ram1[2,5], ram1[2,6]))
  
  
  
  sheet_names <- excel_sheets(file_list[f])
  sheet_names <- sheet_names[!grepl('Sheet',sheet_names)] # to deal with random sheets added by RO
  

  wb_sheets <- lapply(sheet_names, function(x) read_excel(path = file_list[f], sheet = x
                                                        ,range = cell_cols('B:N')
                                                       # ,skip=which(stringr::str_detect('Day',data.frame(read_excel(file_list[f], sheet=x))[,1]))[1]+1
                                                          ,col_types = c('text','date','date','date','date','date','date'
                                                                         ,'text','text','text','text','numeric','logical')))

  
  wb_details <- data.frame()
  
  for (i in 1:length(wb_sheets)) {
    
  temp <- data.frame(wb_sheets[i])
  
  colnames(temp) <- c('Day',	'Date',	'Start',	'Lunch Break',	'Dinner Break',	'Finish',	'Hours Worked',	'Notes',	'ROSO Reviewed', 'ROSO Note'
                           ,'Blank','Worked','Breach')
  
    wb_details <- wb_details %>% bind_rows(
                        temp
                            )
  }
  
  
  wb_details <- wb_details %>% filter(!is.na(Date))
  

# identify earliest and latest
  
  Earliest <- wb_details %>% filter(!is.na(Start)) %>% mutate(Earliest = order(Start)) %>% filter(Earliest == 1)
                 
  Latest <- wb_details %>% filter(!is.na(Finish)) %>% mutate(Latest = order(desc(Finish))) %>% filter(Latest == 1)
  
  Consec7 <- wb_details %>% mutate(non0 = ifelse(Worked == 0 | is.na(Worked), 0,1)) %>%
                            mutate(Consec = zoo::rollsumr(non0,k=7,fill=NA))
  
  Weekends <- wb_details %>% filter(Day %in% c('Saturday','Sunday') & Worked >0) 
    
 # format taking hour only
 tada <- function(x) {
    
  x <- format(x, format = "%H:%M:%S")
  
  }
  
  cols.num <- c("Lunch Break","Dinner Break",'Hours Worked','Start','Finish')
  
  wb_details[, which(colnames(wb_details) %in% cols.num)] <- sapply(wb_details[, which(colnames(wb_details) %in% cols.num)],tada)
  Earliest[, which(colnames(Earliest) %in% cols.num)] <- sapply(Earliest[, which(colnames(Earliest) %in% cols.num)],tada)
  Latest[, which(colnames(Latest) %in% cols.num)] <- sapply(Latest[, which(colnames(Latest) %in% cols.num)],tada)
  
# take numeric break  
  
  wb_details <- wb_details %>% mutate(LB = as.numeric(substr(`Lunch Break`,1,2))*60 + as.numeric(substr(`Lunch Break`,4,5))
                                      ,DB = as.numeric(substr(`Dinner Break`,1,2))*60 + as.numeric(substr(`Dinner Break`,4,5))
                                      ,Review = case_when(`ROSO Reviewed` == 'Yes' ~ 'N'
                                                          ,is.na(Start) & is.na(`Lunch Break`) & is.na(`Dinner Break`) & is.na(Finish) ~ 'N'
                                                          ,T ~ 'Y'))
  
  MoreThan10 <- wb_details %>% filter(ifelse(is.na(Worked),0,Worked) - ifelse(is.na(LB),0,LB) - ifelse(is.na(DB),0,DB) > 10)
  Rest <- wb_details %>% mutate(restneeded = case_when(Worked > 5 & Worked <10 ~ 30)) %>%
                         mutate(restbreach = ifelse(ifelse(is.na(restneeded),0,restneeded) - ifelse(is.na(LB),0,LB) - ifelse(is.na(DB),0,DB) > -0.1, 0, 1)) %>%
    filter(restbreach ==1)
  
 # summary
  
  wb_summary <- wb_summary %>% mutate(Earliest = ifelse(nrow(Earliest)==0, 'NA', paste(Earliest$Date,Earliest$Start, sep= ' '))
                        ,Latest = ifelse(nrow(Latest)==0, 'NA', paste(Latest$Date,Latest$Finish, sep= ' '))
                        ,Over7ConsecDays = nrow(Consec7 %>% filter(Consec > 6))
                        ,OtherBreaches = nrow(wb_details %>% filter(Date %in% c(MoreThan10$Date,Rest$Date)))
                        ,WeekendsWorked = nrow(Weekends)
                        ,`TotalHoursWorked(hrs)` = round(sum(wb_details$Worked,na.rm = T),2)
                        ,`TotalBreaks(hrs)` = (sum(wb_details$LB, na.rm = T) + sum(wb_details$DB, na.rm = T))/60
                        ,ROSOReviewNeeded = nrow(wb_details %>% filter(Review == 'Y'))) 
                        
                        
  } else (
    
    wb_summary <- data.frame(Name = 'Corrupt file'
                             ,District = word(file_list[f],1)) %>%
                        mutate(Earliest = NA
                             ,Latest = NA
                             ,Over7ConsecDays = NA
                             ,OtherBreaches = NA
                             ,WeekendsWorked = NA
                             ,`TotalHoursWorked(hrs)` = NA
                             ,`TotalBreaks(hrs)` = NA
                             ,ROSOReviewNeeded = NA)
    
  )
  
  timesheet_summary <-timesheet_summary %>% bind_rows(wb_summary)
}


setwd(paste0(rmd_files,'/Secured pages'))
rmarkdown::render('02_04 RO Time Sheet.Rmd', 
                  output_dir = server_root_securedpages)
