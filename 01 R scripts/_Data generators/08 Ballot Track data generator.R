################################# ---- BALLOT TRACK GENERATOR ---- #############################################




## This code pulls the data for the 'How to Vote' dashboard
message('Run R code to create Ballot Track data')


## Note that for the LG elections, the data won't come from a database, it will come from spreadsheets uploaded to
## the analytics server.


## TEST DATA ================================================================================



## Read in a list of files needed for the HTV dashboard
## Put all the excel files in a specific folder.
## Does the PPDM need aditional data that is below the RO level?
excelFiles             <- list.files(paste0(data_source, 'Ballot_track/'), pattern = "^[^~]", full.names = TRUE)
Barcode_Report_webport <- read_csv(paste0(data_source,   'Ballot_track/Barcode_Report_BallotTrack_webportal.csv'))





## 1). BALLOT TRACK FULL TABLES ================================================================================


## Pull Container table ----
## hyperion is svranalytics1 
## What does this table represent? 
Ballot_Track_containers <- sqlExecute(hyperion_connection,
                                      "SELECT 
                                       [ConsKey]
                                      ,[LineNo]
                                      ,[ContainerNo]
                                      ,[Barcode]
                                      ,[Deleted]
                                  FROM [DV_Staging_BallotTrack_2021].[dbo].[NSW_Container]",
                                      
                                      ## Add the event_group_ID
                                      fetch = TRUE,
                                      stringsAsFactors = FALSE)


## Pull Container Context table ----
## What does this table represent? 
Ballot_Track_connote <- sqlExecute(hyperion_connection,
                                   "SELECT 
                                     [ConsKey]
                                    ---,[ConsNo]
                                    ----,[EventId]
                                    ,[CDID]
                                    ,[ReturningOfficeName]
                                     ,[CountCentreCode]
                                    ,[CountCentreName]
                                    ----,[LGAId]
                                    ,[LGACode]
                                    ,[LGAName]
                                    ,[WardCode]
                                    ,[WardName]
                                    ,[VenueCode]
                                    ,[VenueName]
                                    ,[VenueLongName]
                                    ----,[VenueAddress]
                                    ,[VenueTypeCode]
                                    ,[VenueTypeName]
                                    ,[ContestCode]
                                    ,[ContestName]
                                    ,[ContainerCode]
                                    ,[ContainerName]
                                    ,[ContainerQuantity]
                                    ,[AdditionalContainerQuantity]
                                    ,[DeliveryNumber]
                                    ----,[PrinterJobId]
                                    
                                    ---,[Created]
                                    ---,[CreatedBy]
                                    ---,[Modified]
                                    ---,[ModifiedBy]
                                    ---,[Deleted]
                                    ---,[DeletedBy]
                                    ----,[DeletedReason]
                                    
                                FROM [DV_Staging_BallotTrack_2021].[dbo].[NSW_Connote]",
                                   
                                   ## Add the event_group_ID
                                   fetch = TRUE,
                                   stringsAsFactors = FALSE)



## Pull Container Status table ----
## What does this table represent? 
Ballot_Track_container_status <- sqlExecute(hyperion_connection,
                                            "SELECT
                                             [ConsKey]
                                            ,[LineNo]
                                            ,[StatusDate]
                                            ,[StatusTime]
                                            ,[StatusNotes]
                                            ,[StatusStage]
                                            ,[MovementDirection]
                                            ---,[CountCentreCode]
                                            ---,[ConsNo]
                                            ---,[ContainerNo]
                                            ----,[PrinterJobId]
                                            ----,[Barcode]
                                            ---,[Created]
                                            ---,[CreatedBy]
                                            ---,[Modified]
                                            ---,[ModifiedBy]
                                        FROM [DV_Staging_BallotTrack_2021].[dbo].[NSW_ContainerStatus]",
                                            
                                            ## Fetch
                                            fetch = TRUE,
                                            stringsAsFactors = FALSE)


## Now disconnect from all databases used in the script
# odbcCloseAll()





########################################### ---- TBC ---- #####################################################
