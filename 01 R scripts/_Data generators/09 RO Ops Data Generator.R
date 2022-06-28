#Ro Ops Data generator---------

#note - previous work saved in archive.
#data is generated in main dashboards, then the relevant files saved as csvs.


setwd(server_data)

# Pre-poll ----------------------------------------------------

SourceList<-list.files()

any(SourceList=="PrePoll_Data.csv")

if(any(SourceList=="PrePoll_Data.csv")){
  
  Pre_Poll_Data<-read_csv("PrePoll_Data.csv")
  
}

#dec voting----------------

if(any(SourceList=="DecVote_Data.csv")){
  Dec_Vote_Data <- read_csv("DecVote_Data.csv")
  
}



#ballot track-------------

setwd(BT_server_data)
SourceList <- list.files()


if(any(SourceList=="Ballot_track_RO_data.csv")){
  
  ## Read in the overall BT table
  Ballot_Track_Data <- read_csv("Ballot_track_RO_data.csv")
  
  ## Read in the individual stages
  RO_2.0_scan_in_from_printer <- read_csv("RO_2.0_scan_in_from_printer.csv")
  RO_3.0_scan_out_to_venue    <- read_csv("RO_3.0_scan_out_to_venue.csv")
  RO_4.0_scan_in_from_venue   <- read_csv("RO_4.0_scan_in_from_venue.csv")
  RO_5.10_scan_out_to_CC      <- read_csv("RO_5.10_scan_out_to_CC.csv")
  RO_5.11_scan_from_RO_to_CC  <- read_csv("RO_5.11_scan_from_RO_to_CC.csv")
  RO_5.0_scan_out_to_Ware     <- read_csv("RO_5.0_scan_out_to_Ware.csv")
  RO_5.14_CC_scan_in_to_ULD   <- read_csv("RO_5.14_CC_scan_in_to_ULD.csv")
  
  
}


#rmit files ----------------------------
setwd('//SVRANALYTICS1/AnalyticsData/LG2101/RMT')

Mayor_initial <- read.csv("Mayor initial count.csv", colClasses = c("NULL",rep(NA,16))) %>% as_tibble()
Councillor_initial <- read.csv("Counvillor initial count.csv", colClasses = c("NULL",rep(NA,15))) %>% as_tibble()
Ref_Poll_initial <- read.csv("Ref Poll initial count.csv", colClasses = c("NULL", rep(NA,17))) %>% as_tibble()
Mayor_check <- read.csv("Mayor check count.csv", colClasses = c("NULL",rep(NA,20))) %>% as_tibble()
Councillor_check <- read.csv("Counvillor check count.csv", colClasses = c("NULL",rep(NA,20))) %>% as_tibble()



#additional data required------------


#use base data to get RO List.
# 
# base_data_councils <- read.csv(paste0(base_data_working_folder, "/Base_data_councils.csv"),
#                                stringsAsFactors = FALSE, na.strings = "") %>%
#   filter(!is.na(ReturningOffice))%>%
#   as_tibble()

base_data_councils <- sqlExecute(hyperion_connection,
                                 "
                                  SELECT [ElectionStatus]
                                        ,[EventID]
                                        ,[ClientStatus]
                                        ,[VenueReviewComplete]
                                        ,[ElectoralAreaName]
                                        ,[LGAreaCode]
                                        ,[CountOfWards]
                                        ,[StaffingCategory]
                                        ,[Enrolment]
                                        ,[ReturningOffice]
                                        ,[CountLocation]
                                        ,[CouncillorPositions]
                                        ,[Mayoral]
                                        ,[Referendum]
                                        ,[Poll]
                                        ,[Count_of_polling_places]
                                        ,[Count_of_prepolling_places]
                                        ,[Count_of_DI_venues]
                                        ,[PollingPlace_Ordinary_Projected]
                                        ,[PollingPlace_Enrolment_Projected]
                                        ,[PollingPlace_Other_Projected]
                                        ,[PrePoll_Ordinary_Projected]
                                        ,[PrePoll_Enrolment_Projected]
                                        ,[PrePoll_Other_Projected]
                                        ,[DI_Ordinary_Projected]
                                        ,[DI_Enrolment_Projected]
                                        ,[DI_Other_Projected]
                                        ,[Postal_Projected]
                                        ,[iVote_Projected]
                                        ,[Total_Projected]
                                    FROM [ProcessingData].[base_data].[LG_Councils]",
                                 fetch=TRUE,
                                 stringsAsFactors = FALSE) %>%
                  filter(!is.na(ReturningOffice))


#get short ward names.

Ward_Names <- sqlExecute(EMS_connection,
                         "SELECT  
                       [Roll Area Code] AS WardAreaCode,
                       [Short Ward Name] AS ShortWardName
                      
                       FROM Events.EMS_VW_EventAreaAuthorityLGWardIan
                       WHERE EventGroupID = ?",
                         event_group_ID,
                         TRUE,
                         stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  
  #NAs are undivided councils.
  
  mutate(ShortWardName = ifelse(is.na(ShortWardName), "Undivided", ShortWardName))
