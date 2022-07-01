
####################################### ----- COUNT CENTRE DATA PROCESSOR ---- #############################################





## This script processes the data for the count centre management tool and the count schedule dashboard




# A dashboard for all Count Locations summarising the following for each Count Location

## To do list ---
## Create LUT of dates, etc


## 1). DEFINE VARIABLES ================================================================================


## Create all required date variables ----
## User needs to understand dates, not the days. Need target dates highlighted for communication of 
## the planned finish date KPI Start Date
overflow    <- 10 # extra days

counting_start_date_syd          <- '06/12/2021' %>% as_date(., format = "%d/%m/%Y", tz='Australia/Sydney')
counting_start_date_newc         <- '06/12/2021' %>% as_date(., format = "%d/%m/%Y", tz='Australia/Sydney')
counting_start_date_RO           <- '06/12/2021' %>% as_date(., format = "%d/%m/%Y", tz='Australia/Sydney')

scheduled_data_start_syd       <- '07/12/2021' %>% as_date(., format = "%d/%m/%Y", tz='Australia/Sydney')
scheduled_data_start_newc      <- '07/12/2021' %>% as_date(., format = "%d/%m/%Y", tz='Australia/Sydney')
scheduled_data_start_RO        <- '07/12/2021' %>% as_date(., format = "%d/%m/%Y", tz='Australia/Sydney')

scheduled_batch_finish_syd       <- '16/12/2021' %>% as_date(., format = "%d/%m/%Y", tz='Australia/Sydney')
scheduled_batch_finish_newc      <- '14/12/2021' %>% as_date(., format = "%d/%m/%Y", tz='Australia/Sydney')
scheduled_batch_finish_RO        <- '13/12/2021' %>% as_date(., format = "%d/%m/%Y", tz='Australia/Sydney')

scheduled_data_entry_finish_syd  <- '18/12/2021' %>% as_date(., format = "%d/%m/%Y", tz='Australia/Sydney') 
scheduled_data_entry_finish_newc <- '16/12/2021' %>% as_date(., format = "%d/%m/%Y", tz='Australia/Sydney')
scheduled_data_entry_finish_RO   <- '17/12/2021' %>% as_date(., format = "%d/%m/%Y", tz='Australia/Sydney')


## Set working dates for bathching and data entry


# Sydney
syd_startDate      <- counting_start_date_syd    ## This is fixed

syd_all_dates      <- seq(from = syd_startDate, to = scheduled_data_entry_finish_syd + overflow, by = "days")
syd_sundays        <- which(wday(syd_all_dates, label = TRUE) == "Sun") 
syd_working_dates  <- syd_all_dates[-syd_sundays]


syd_all_dates_batching      <- seq(from = syd_startDate, to = scheduled_batch_finish_syd, by = "days")
syd_sundays_batching        <- which(wday(syd_all_dates_batching, label = TRUE) == "Sun") 
syd_working_dates_batching  <- syd_all_dates_batching[-syd_sundays_batching]


syd_all_dates_data          <- seq(from = scheduled_data_start_syd, to = scheduled_data_entry_finish_syd, by = "days")
syd_sundays_data            <- which(wday(syd_all_dates_data, label = TRUE) == "Sun") 
syd_working_dates_data      <- syd_all_dates_data[-syd_sundays_data]


# Newcastle
newc_startDate      <- counting_start_date_newc    ## This is fixed

newc_all_dates      <- seq(from = newc_startDate, to = scheduled_data_entry_finish_newc, by = "days")
newc_sundays        <- which(wday(newc_all_dates, label = TRUE) == "Sun") 
newc_working_dates  <- newc_all_dates[-newc_sundays]


newc_all_dates_batching      <- seq(from = newc_startDate, to = scheduled_batch_finish_newc, by = "days")
newc_sundays_batching        <- which(wday(newc_all_dates_batching, label = TRUE) == "Sun") 
newc_working_dates_batching  <- newc_all_dates_batching[-newc_sundays_batching]


newc_all_dates_data          <- seq(from = scheduled_data_start_newc, to = scheduled_data_entry_finish_newc, by = "days")
newc_sundays_data            <- which(wday(newc_all_dates_data, label = TRUE) == "Sun") 
newc_working_dates_data      <- newc_all_dates_data[-newc_sundays_data]

# Returning Offices
RO_startDate      <- counting_start_date_RO    ## This is fixed

RO_all_dates      <- seq(from = RO_startDate, to = scheduled_data_entry_finish_RO, by = "days")
RO_sundays        <- which(wday(RO_all_dates, label = TRUE) == "Sun") 
RO_working_dates  <- RO_all_dates[-RO_sundays]


RO_all_dates_batching      <- seq(from = RO_startDate, to = scheduled_batch_finish_RO, by = "days")
RO_sundays_batching        <- which(wday(RO_all_dates_batching, label = TRUE) == "Sun") 
RO_working_dates_batching  <- RO_all_dates_batching[-RO_sundays_batching]


RO_all_dates_data          <- seq(from = scheduled_data_start_RO, to = scheduled_data_entry_finish_RO, by = "days")
RO_sundays_data            <- which(wday(RO_all_dates_data, label = TRUE) == "Sun") 
RO_working_dates_data      <- RO_all_dates_data[-RO_sundays_data]

## Use this to cacluate the time elapsed? Find the position of the start date in the sequence, 
## Divide the position by the length - double check this
current_date       <- Sys.Date()  ## Sys.Date()

## 
if(weekdays(current_date) == "Sunday") {
  
  message('Current date is a Sunday, substract day')
  current_date <- current_date - 1
  
}





## Create 'all required dates'elapsed time'  variables ----
## Now Crete the 'elapsed time' for batching and data entry
## These are global figures across the whole election
if(current_date > "2021-12-06") {
  
  message('Dashboard run after Election day, calculate elapsed time')
  
  
  # Sydney elapsed time
  syd_elapsed_time_batch       <- ifelse(current_date > max(syd_working_dates_batching)
                                         ,length(syd_working_dates_batching)
                                         ,which(syd_working_dates_batching == current_date))

  syd_elapsed_time_data        <- ifelse(current_date > max(syd_working_dates_data)
                                         ,length(syd_working_dates_data)
                                         ,which(syd_working_dates_data == current_date))
  
  syd_batching_elapsed_prop    <- round(syd_elapsed_time_batch/length(syd_working_dates_batching)*100)
  syd_data_entry_elapsed_prop  <- round(syd_elapsed_time_data/(length(syd_working_dates_data))*100)
  
  # Newcastle elapsed time
  newc_elapsed_time_batch       <- ifelse(current_date > max(newc_working_dates_batching)
                                         ,length(newc_working_dates_batching)
                                         ,which(newc_working_dates_batching == current_date))
  
  newc_elapsed_time_data        <- ifelse(current_date > max(newc_working_dates_data)
                                         ,length(newc_working_dates_data)
                                         ,which(newc_working_dates_data == current_date))
  
  newc_batching_elapsed_prop   <- round(newc_elapsed_time_batch/length(newc_working_dates_batching)*100)
  newc_data_entry_elapsed_prop <- round(newc_elapsed_time_data/(length(newc_working_dates_data))*100)
  
  # RO elapsed time
  RO_elapsed_time_batch       <- ifelse(current_date > max(RO_working_dates_batching)
                                         ,length(RO_working_dates_batching)
                                         ,which(RO_working_dates_batching == current_date))
  
  RO_elapsed_time_data        <- ifelse(current_date > max(RO_working_dates_data)
                                         ,length(RO_working_dates_data)
                                         ,which(RO_working_dates_data == current_date))
  
  RO_batching_elapsed_prop     <- round(RO_elapsed_time_batch/length(RO_working_dates_batching)*100)
  RO_data_entry_elapsed_prop   <- round(RO_elapsed_time_data/(length(RO_working_dates_data))*100) 
  
  ## Update
  
  
} else {
  
  message('Dashboard run before Election day, elapsed time = 0 ')
  syd_batching_elapsed_prop    <- 0
  syd_data_entry_elapsed_prop  <- 0
  newc_batching_elapsed_prop   <- 0 
  newc_data_entry_elapsed_prop <- 0 
  RO_batching_elapsed_prop     <- 0
  RO_data_entry_elapsed_prop   <- 0
  
}







## 2). COUNT CENTRE SUMMARIES ============================================================================



## % Complete for Sydney CC ----
Syd_percent_remaining <- forecastSummaryCountLocation %>%
  
  ## Just get Syd
  filter(CountLocation == "Sydney Count Centre")        %>% 
  
  dplyr::select(BatchingProgressPercent,
         DataEntryProgressPercent,
         CountLocation) %>% 
  
  ## Add the % remaining
  mutate(BatchingPercentRemain  = round((100 - `BatchingProgressPercent`),  digits = 3),
         DataEntryPercentRemain = round((100 - `DataEntryProgressPercent`), digits = 3)) %>%
  
  ## Wide to Long
  pivot_longer(., cols  = c("BatchingProgressPercent",
                            "DataEntryProgressPercent",
                            "BatchingPercentRemain",
                            "DataEntryPercentRemain"),
               
               names_to  = "Progress",
               values_to = "Proportion") %>% 
  
  mutate(Status   = ifelse(str_detect(Progress, 'Batching'), "Batching", "Data Entry"),
         Progress = gsub('BatchingProgressPercent',  'Complete', Progress),
         Progress = gsub('DataEntryProgressPercent', 'Complete', Progress),
         
         Progress = gsub('BatchingPercentRemain',  'Remaining', Progress),
         Progress = gsub('DataEntryPercentRemain', 'Remaining', Progress),
         
         Status   = factor(Status, 
                           levels = c('Batching', 'Data Entry')),
         
         Progress = factor(Progress, 
                           levels = c('Remaining', 'Complete'))) %>% 
  arrange(Status) 


## Split up the % remaining tables
Syd_percent_remaining_batch <- Syd_percent_remaining %>% filter(Status == 'Batching') 
Syd_percent_remaining_data  <- Syd_percent_remaining %>% filter(Status == 'Data Entry')



## % Complete for Newcastle CC ----
Newcastle_percent_remaining <- forecastSummaryCountLocation %>%
  
  ## Just get Syd
  filter(CountLocation == "Newcastle Count Centre") %>% 
  
  dplyr::select(`BatchingProgressPercent`,
         `DataEntryProgressPercent`,
         CountLocation) %>% 
  
  ## Add the % remaining
  mutate(BatchingPercentRemain  = round((100 - `BatchingProgressPercent`),  digits = 3),
         DataEntryPercentRemain = round((100 - `DataEntryProgressPercent`), digits = 3)) %>%
  
  ## Wide to Long
  pivot_longer(., cols = c("BatchingProgressPercent",
                           "DataEntryProgressPercent",
                           "BatchingPercentRemain",
                           "DataEntryPercentRemain"),
               
               names_to  = "Progress",
               values_to = "Proportion") %>%
  
  mutate(Status   = ifelse(str_detect(Progress, 'Batching'), "Batching", "Data Entry"),
         Progress = gsub('BatchingProgressPercent',  'Complete', Progress),
         Progress = gsub('DataEntryProgressPercent', 'Complete', Progress),
         
         Progress = gsub('BatchingPercentRemain',    'Remaining', Progress),
         Progress = gsub('DataEntryPercentRemain',   'Remaining', Progress),
         
         Status   = factor(Status,
                           levels = c('Batching', 'Data Entry')),
         
         Progress = factor(Progress,
                           levels = c('Remaining', 'Complete'))) %>%
  arrange(Status)


## Split up the % remaining tables
newc_percent_remaining_batch <- Newcastle_percent_remaining %>% filter(Status == 'Batching') 
newc_percent_remaining_data  <- Newcastle_percent_remaining %>% filter(Status == 'Data Entry')





## Syd Count Centre headline ----
Syd_Centre_HeadlineTable <- forecastSummaryCountLocation %>% 
  
  ## Just get Syd
  filter(CountLocation == "Sydney Count Centre") %>% 
  
  ## Remove columns
  dplyr::select(-`BatchingProgressPercent`,
         -`DataEntryProgressPercent`,
         -`EventGroupID`,
         -`CountLocation`) %>% 
  
  ## Format numbers
  mutate(BatchingWorkload        = prettyNum(BatchingWorkload,        big.mark = ',', scientific = FALSE),
         TotalBPsBatched         = prettyNum(TotalBPsBatched ,        big.mark = ',', scientific = FALSE),
         BPsRemainingForBatching = prettyNum(BPsRemainingForBatching, big.mark = ',', scientific = FALSE),
         
         DataEntryWorkload       = prettyNum(DataEntryWorkload,       big.mark = ',', scientific = FALSE),
         TotalDataEntered        = prettyNum(TotalDataEntered,        big.mark = ',', scientific = FALSE),
         TotalRemainingForDE     = prettyNum(TotalRemainingForDE,     big.mark = ',', scientific = FALSE)) %>% 
  
  ## Rename 
  rename(`Batching Workload`               = BatchingWorkload,
         `Total BPs Batched`               = TotalBPsBatched,
         `BPs Remaining For Batching`      = BPsRemainingForBatching,
         
         `Data Entry Workload (2 x Batching Workload)` = DataEntryWorkload,
         `Total Data Entered`              = TotalDataEntered,
         `Total Remaining For Data Entry`  = TotalRemainingForDE) %>%
  
  ## Create the finishing dates and days, excluding Sunday
  mutate(`Projected Batching Finish Day`   = syd_working_dates[BatchingFinishDay],
         `Projected Batching Finish Day`   = paste0(`Projected Batching Finish Day` %>% 
                                                      weekdays(., abbreviate = FALSE), 
                                                    ' ', format(`Projected Batching Finish Day`, "%d/%m/%Y")),
         
         `Projected Data Entry Finish Day` = syd_working_dates[DataEntryFinishDay], 
         `Projected Data Entry Finish Day` = paste0(`Projected Data Entry Finish Day` %>% 
                                                      weekdays(., abbreviate = FALSE), 
                                                    ' ', format(`Projected Data Entry Finish Day`, "%d/%m/%Y")),
         
         
         `Scheduled Batching Finish Day`    = paste0(scheduled_batch_finish_syd %>% 
                                                       weekdays(., abbreviate = FALSE), 
                                                     ' ', format(scheduled_batch_finish_syd, "%d/%m/%Y")),
         
         
         `Scheduled Data Entry Finish Day`  = paste0(scheduled_data_entry_finish_syd %>% 
                                                       weekdays(., abbreviate = FALSE), 
                                                     ' ', format(scheduled_data_entry_finish_syd, "%d/%m/%Y"))) %>%
  
  dplyr::select(-BatchingFinishDay, -DataEntryFinishDay) %>% 
  
  ## Wide to Long
  pivot_longer(., cols = everything(),
               
               names_to         = "Sydney",
               values_to        = "Value",
               values_transform = list(Value = as.character)) %>% 
  
  arrange(match(Sydney, c('Batching Workload', 
                          'Total BPs Batched',
                          'BPs Remaining For Batching',
                          'Scheduled Batching Finish Day',
                          'Projected Batching Finish Day',
                          'Data Entry Workload (2 x Batching Workload)',
                          'Total Data Entered',
                          'Total Remaining For Data Entry',
                          'Scheduled Data Entry Finish Day',
                          'Projected Data Entry Finish Day'))) %>% 
  
  ## Convert to flextable
  flextable() %>% 
  
  ## Set background formatting for flex table
  bg(bg = "coral",   i=~ str_detect(Sydney, "Batch"))                  %>%
  bg(bg = "#50C878", i=~ str_detect(Sydney, "Data")) %>%
  
  ## Set the Count Centres to black
  bg(bg = "black",       part = "header") %>%
  
  ## Fontsize, color and bold 
  fontsize(size = headline_size) %>% 
  fontsize(i = 1, j = NULL, size = headline_size, part = "header") %>%
  bold(i = 1,     j = NULL, bold = TRUE, part = "header") %>%
  
  ## Set the colour and width
  color(color = "white", i=~ Value > 0) %>%
  color(color = "white", part = "all")  %>% 
  
  width(j =~ Sydney,    width = 700)    %>%
  width(j =~ Value,     width = 200)    %>%
  border(border = fp_border(color = "white", width = 3)) %>% 
  height_all(., height = 10)





## Newcastle Count Centre headline ----
Newcastle_Count_HeadlineTable <- forecastSummaryCountLocation %>% 
  
  ## Just get Newcastle
  filter(CountLocation == "Newcastle Count Centre") %>% 
  
  ## Remove columns
  dplyr::select(-`BatchingProgressPercent`,
         -`DataEntryProgressPercent`,
         -`EventGroupID`,
         -`CountLocation`) %>% 
  
  ## Format numbers
  mutate(BatchingWorkload        = prettyNum(BatchingWorkload,        big.mark = ',', scientific = FALSE),
         TotalBPsBatched         = prettyNum(TotalBPsBatched ,        big.mark = ',', scientific = FALSE),
         BPsRemainingForBatching = prettyNum(BPsRemainingForBatching, big.mark = ',', scientific = FALSE),
         
         DataEntryWorkload       = prettyNum(DataEntryWorkload,       big.mark = ',', scientific = FALSE),
         TotalDataEntered        = prettyNum(TotalDataEntered,        big.mark = ',', scientific = FALSE),
         TotalRemainingForDE     = prettyNum(TotalRemainingForDE,     big.mark = ',', scientific = FALSE)) %>% 
  
  ## Rename 
  rename(`Batching Workload`               = BatchingWorkload,
         `Total BPs Batched`               = TotalBPsBatched,
         `BPs Remaining For Batching`      = BPsRemainingForBatching,
         
         `Data Entry Workload (2 x Batching Workload)` = DataEntryWorkload,
         `Total Data Entered`              = TotalDataEntered,
         `Total Remaining For Data Entry`  = TotalRemainingForDE) %>%
  
  ## Create the finishing dates and days, excluding Sunday
  mutate(`Projected Batching Finish Day`   = newc_working_dates[BatchingFinishDay],
         `Projected Batching Finish Day`   = paste0(`Projected Batching Finish Day` %>% 
                                                      weekdays(., abbreviate = FALSE), 
                                                    ' ', format(`Projected Batching Finish Day`, "%d/%m/%Y")),
         
         `Projected Data Entry Finish Day` = newc_working_dates[DataEntryFinishDay], 
         `Projected Data Entry Finish Day` = paste0(`Projected Data Entry Finish Day` %>% 
                                                      weekdays(., abbreviate = FALSE), 
                                                    ' ', format(`Projected Data Entry Finish Day`, "%d/%m/%Y")),
         
         `Scheduled Batching Finish Day`   = paste0(scheduled_batch_finish_newc %>% 
                                                      weekdays(., abbreviate = FALSE), 
                                                    ' ', format(scheduled_batch_finish_newc, "%d/%m/%Y")),
         
         
         `Scheduled Data Entry Finish Day` = paste0(scheduled_data_entry_finish_newc %>% 
                                                      weekdays(., abbreviate = FALSE), 
                                                    ' ', format(scheduled_data_entry_finish_newc, "%d/%m/%Y"))) %>%
  
  dplyr::select(-BatchingFinishDay, -DataEntryFinishDay) %>% 
  
  ## Wide to Long
  pivot_longer(., cols = everything(),
               
               names_to         = "Newcastle",
               values_to        = "Value",
               values_transform = list(Value = as.character)) %>%
  
  arrange(match(Newcastle, c('Batching Workload', 
                             'Total BPs Batched',
                             'BPs Remaining For Batching',
                             'Scheduled Batching Finish Day',
                             'Projected Batching Finish Day',
                             'Data Entry Workload (2 x Batching Workload)',
                             'Total Data Entered',
                             'Total Remaining For Data Entry',
                             'Scheduled Data Entry Finish Day',
                             'Projected Data Entry Finish Day'))) %>%
  
  ## Convert to flextable
  flextable() %>% 
  
  ## Set background formatting for flex table
  bg(bg = "coral",   i=~ str_detect(Newcastle, "Batch")) %>%
  bg(bg = "#50C878", i=~ str_detect(Newcastle, "Data"))  %>%
  
  ## Set the Count Centres to black
  bg(bg = "black",       part = "header") %>%
  
  ## Fontsize, color and bold 
  fontsize(size = headline_size) %>% 
  fontsize(i = 1, j = NULL, size = headline_size, part = "header") %>%
  bold(i = 1,     j = NULL, bold = TRUE, part = "header") %>%
  
  ## Set the colour and width
  color(color = "white", i=~ Value > 0) %>%
  color(color = "white", part = "all")  %>% 
  
  width(j =~ Newcastle,    width = 700)    %>%
  width(j =~ Value,     width = 200)    %>%
  border(border = fp_border(color = "white", width = 3)) %>% 
  height_all(., height = 10)





## 3). CONTEST SUMMARIES  ============================================================================



## % Syd Complete by contest ----
syd_contest_percent_remaining <- forecastSummaryByContest %>%
  
  filter(CountLocation == "Sydney Count Centre") %>%
  dplyr::select(ContestAreaCode,
         BatchingProgressPercent,
         DataEntryProgressPercent) %>% rename(Contest = ContestAreaCode) %>%
  
  ## Add the % remaining
  mutate(BatchingPercentRemain  = round((100 - BatchingProgressPercent),  digits = 3),
         DataEntryPercentRemain = round((100 - DataEntryProgressPercent), digits = 3)) %>%
  
  ## Wide to Long
  pivot_longer(., cols = c("BatchingProgressPercent",
                           "DataEntryProgressPercent",
                           "BatchingPercentRemain",
                           "DataEntryPercentRemain"),
               
               names_to  = "Progress",
               values_to = "Proportion") %>%
  
  mutate(Status   = ifelse(str_detect(Progress, 'Batching'), "Batching", "Data Entry"),
         Progress = gsub('BatchingProgressPercent',  'Complete', Progress),
         Progress = gsub('DataEntryProgressPercent', 'Complete', Progress),
         
         Progress = gsub('BatchingPercentRemain',    'Remaining', Progress),
         Progress = gsub('DataEntryPercentRemain',   'Remaining', Progress),
         
         Status   = factor(Status,
                           levels = c('Batching', 'Data Entry')),
         
         Progress = factor(Progress,
                           levels = c('Remaining', 'Complete'))) %>%
  arrange(Status)






## % Newcastle Complete by contest ----
newc_contest_percent_remaining <- forecastSummaryByContest %>%
  
  filter(CountLocation == "Newcastle Count Centre") %>%
  
  
  dplyr::select(ContestAreaCode,
         BatchingProgressPercent,
         DataEntryProgressPercent) %>% rename(Contest = ContestAreaCode) %>%
  
  ## Add the % remaining
  mutate(BatchingPercentRemain  = round((100 - BatchingProgressPercent),  digits = 3),
         DataEntryPercentRemain = round((100 - DataEntryProgressPercent), digits = 3)) %>%
  
  ## Wide to Long
  pivot_longer(., cols = c("BatchingProgressPercent",
                           "DataEntryProgressPercent",
                           "BatchingPercentRemain",
                           "DataEntryPercentRemain"),
               
               names_to  = "Progress",
               values_to = "Proportion") %>%
  
  mutate(Status   = ifelse(str_detect(Progress, 'Batching'), "Batching", "Data Entry"),
         Progress = gsub('BatchingProgressPercent',  'Complete', Progress),
         Progress = gsub('DataEntryProgressPercent', 'Complete', Progress),
         
         Progress = gsub('BatchingPercentRemain',    'Remaining', Progress),
         Progress = gsub('DataEntryPercentRemain',   'Remaining', Progress),
         
         Status   = factor(Status,
                           levels = c('Batching', 'Data Entry')),
         
         Progress = factor(Progress,
                           levels = c('Remaining', 'Complete'))) %>%
  arrange(Status)





## Split the contest % remaining graphs up by process and count location
syd_contest_batch_percent_remaining  <- syd_contest_percent_remaining  %>% filter(Status == 'Batching') %>% 
  dplyr::rename(Contest = ContestAreaCode)

syd_contest_data_percent_remaining   <- syd_contest_percent_remaining  %>% filter(Status == 'Data Entry') %>% 
  dplyr::rename(Contest = ContestAreaCode)   

newc_contest_batch_percent_remaining <- newc_contest_percent_remaining %>% filter(Status == 'Batching') %>% 
  dplyr::rename(Contest = ContestAreaCode)  

newc_contest_data_percent_remaining  <- newc_contest_percent_remaining %>% filter(Status == 'Data Entry') %>% 
  dplyr::rename(Contest = ContestAreaCode)   





## 4). RO SUMMARIES  ============================================================================


## Create a table to feed to the RO table
## % RO Complete ----
RO_count_centre_percent_remaining <- forecastSummaryCountLocation %>%
  
  filter(CountLocation != "Sydney Count Centre" & CountLocation != "Newcastle Count Centre") %>% 
  
  ## Just 
  dplyr::select(CountLocation,
         BatchingProgressPercent,
         DataEntryProgressPercent) %>% 
  
  ## Add the % remaining
  mutate(BatchingPercentRemain  = round((100 - BatchingProgressPercent),  digits = 3),
         DataEntryPercentRemain = round((100 - DataEntryProgressPercent), digits = 3)) %>%
  
  ## Wide to Long
  pivot_longer(., cols = c("BatchingProgressPercent",
                           "DataEntryProgressPercent",
                           "BatchingPercentRemain",
                           "DataEntryPercentRemain"),
               
               names_to  = "Progress",
               values_to = "Proportion") %>%
  
  mutate(Status   = ifelse(str_detect(Progress, 'Batching'), "Batching", "Data Entry"),
         Progress = gsub('BatchingProgressPercent',  'Complete', Progress),
         Progress = gsub('DataEntryProgressPercent', 'Complete', Progress),
         
         Progress = gsub('BatchingPercentRemain',  'Remaining',  Progress),
         Progress = gsub('DataEntryPercentRemain', 'Remaining',  Progress),
         
         Status   = factor(Status,
                           levels = c('Batching',  'Data Entry')),
         
         Progress = factor(Progress,
                           levels = c('Remaining', 'Complete'))) %>%
  arrange(Status)


## Split the contest % remaining graphs up by process and count location
RO_count_centre_batch_percent_remaining  <- RO_count_centre_percent_remaining  %>% filter(Status == 'Batching')
RO_count_centre_data_percent_remaining   <- RO_count_centre_percent_remaining  %>% filter(Status == 'Data Entry')





################################## ---- TBC ---- ########################################
