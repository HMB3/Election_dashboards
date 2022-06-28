################################# ---- HOW TO VOTE DATA GENERATOR ---- #############################################




## This code pulls the data for the 'How to Vote' dashboard
message('Run R code to create How to Vote data')


## Note that for the LG elections, the data won't come from a database, it will come from spreadsheets uploaded to
## the analytics server





## 1). EMA SYSTEM EXTRACTS ===================================================================


## Set variables
time_stamp    = gsub("-", "_", now()) %>% 
  gsub(" ", "_", .)                   %>% 
  gsub(":", "",  .)


## Read in Nominations test data as an extract from the system
## Need to setup an auto procedure to run this


## Read in a list of files needed for the HTV dashboard
## Remove the temporary excel files '^[^~]'
excelFiles <- list.files(paste0(noms_data, 'HTV_extracts/'), pattern = '.xlsx', full.names = TRUE) %>%
  
  ## Exclude the weird files
  grep(paste0("~$", collapse = "|"), ., invert = TRUE, value = TRUE)


##  Stop the processing here if there is no data
csvFiles   <- list.files(paste0(noms_data, 'HTV_extracts/'), pattern = ".csv",  full.names = TRUE)
file_list  <- length(excelFiles) + length(csvFiles)


if(file_list >= 5) {
  
  
  ## Get the sheet names for each workbook
  sheetNamesList <- lapply(seq_along(excelFiles), 
                           function (k) c(getSheetNames(excelFiles[k]))) %>%
    
    unlist() %>% as.character()  %>% 
    gsub(" ", "_", .)            %>% 
    gsub("in_minutes", "", .)    %>% 
    gsub("[()]", "", .)          %>% 
    gsub("Report_", "Report", .) 
  
  
  ## Read all the workbooks in using the sheetnames
  sheetData <- lapply(seq_along(excelFiles),
                      
                      function (k) {
                        message('open excel files for ', getSheetNames(excelFiles[k]))
                        tmpSheets <- openxlsx::getSheetNames(excelFiles[k])
                        tmpData   <- lapply(seq_along(tmpSheets), function (n) read_excel(excelFiles[k], sheet = tmpSheets[n]))
                        tmpData
                      })
  
  
  ## Now create the data frames from the list
  names(sheetData)         <- sheetNamesList
  Candidate_Summary_Report <- sheetData[["Candidate_Summary_Report"]][[1]] %>% row_to_names(row_number = 1)
  KPI_Detail_Report        <- sheetData[["KPI_Detail_Report"]][[1]]        %>% row_to_names(row_number = 1)
  KPI_Summary_Report       <- sheetData[["KPI_Summary_Report"]][[1]]       %>% row_to_names(row_number = 1)
  RPP_Summary_Report       <- sheetData[["RPP_Summary_Report"]][[1]]       %>% row_to_names(row_number = 1)
  Materials_Export         <- read_csv(csvFiles)
  
  
  ## Remove column names
  names(Candidate_Summary_Report) <- sub("# ", "", names(Candidate_Summary_Report))
  names(KPI_Detail_Report)        <- sub("# ", "", names(KPI_Detail_Report))
  names(KPI_Summary_Report)       <- sub("# ", "", names(KPI_Summary_Report))
  names(RPP_Summary_Report)       <- sub("# ", "", names(RPP_Summary_Report))
  
  
  ## The KPI detail report doesn't have enough detail inside the time stamp - are these minutes, seconds, etc?
  ## Can we ask Travers to create another endpoint for HTV, that is similar to the Noms status change endoint?
  ## EG Noms status change "dateAndTime" : 2021-03-02T23:12:42.4918046
  ## EG KPI_Detail_Report "Status Date" : 44049.5029939468
  # test <- test %>% mutate(X3 = as.POSIXct(test$X3,format="%d-%m-%Y%H:%M")
  
  
  ## If we include the date field, can we do what jason suggested, a histogram of time to approval
  
  
  
  
  
  ## 2). NOMINATIONS ENDPOINT DATA ==================================================================
  
  
  if(endpoint_skip) {
    
    setwd(HTV_server_data)
    message('Do not call the endpoint')
    noms_extract   <- readRDS(list.files(pattern = '.rds'))
    event_group_ID <- noms_extract$ELECTIONEVENTID[1]
    
  } else {
    
    ## UAT
    message('Call the endpoint')
    noms_endpoint  <- 'https://nomsprodfn02.cmsprodase.p.azurewebsites.net/api/GetDataAnalyticsNomsDataExport?code=qzEGGA05eUKIdMGZ0NIUyeYvQCLDwzJ1C7seuxH9uy6g2RN6e2WzWA=='
    noms_extract   <- jsonify::from_json(noms_endpoint, simplify = TRUE, fill_na = FALSE)
    event_group_ID <- noms_extract$ELECTIONEVENTID[1]
    
  }
  
  

  
  
  ## 2). EMA CANDIDATE/PARTY/CONTEST  DATA ================================================================================
  
  
  ## Note that the data for CANDIDATE/PARTY/CONTEST is coming from the NOMS database now, not EMA
  ## However, some of this data is probably still useful....
  
  
  ## Eligible Political Parties ----
  ## This should match Andrew's PDF
  EMA_Eligible_Parties <- sqlExecute(hyperion_connection,
                                     "SELECT
                                      a.electioneventid,
                                      a.partyid,
                                      a.includeddate,
                                      a.includedby,
                                      a.vtrresultcode,
                                      a.lastupdated,
                                      a.lastupdatedby,
                                      a.ucn,
                                      b.partyname,
                                      b.partyabbreviatedname
                                      FROM [DV_Staging_EMA].[PRD2008].[ES_PARTY] a
                                      left join [DV_Staging_EMA].[PRD2008].[PM_PARTY] b
                                      on a.partyid = b.partyid
                                      where a.electioneventid = ?
                                      and a.includeddate IS NOT NULL",
                                     
                                     event_group_ID,
                                     fetch            = TRUE,
                                     stringsAsFactors = FALSE) %>% 
    as_tibble() %>%
    
    dplyr::rename(PARTYNAME = partyname,
                  PARTYABBREVIATEDNAME = partyabbreviatedname) %>%
    
    ## Change the data
    mutate(PARTYABBREVIATEDNAME = str_to_upper(PARTYABBREVIATEDNAME),
           PARTYNAME            = str_to_upper(PARTYNAME),
           PARTYABBREVIATEDNAME = ifelse(is.na(PARTYABBREVIATEDNAME), PARTYNAME, PARTYABBREVIATEDNAME)) %>%
    select(PARTYABBREVIATEDNAME, PARTYNAME) %>%
    arrange(PARTYABBREVIATEDNAME)
  
  
  
  
  
  ## Contests ----
  EMA_contests <- sqlExecute(hyperion_connection, 
                             "SELECT
                              electioneventid,
                              areacode,
                              contestid,
                              contestareacode,
                              contesttypecode,
                              conteststatustypecode,
                              numberofpositionscontested
                          
                              FROM
                              [DV_Staging_EMA].[PRD2008].[ES_CONTEST]
                              WHERE electioneventid = ? 
                              AND conteststatustypecode not in ('Not Run by EC', 'Deferred', 'Under Administration')",
                             event_group_ID,
                             fetch            = TRUE,
                             stringsAsFactors = FALSE)
  
  
  ## Now disconnect from all databases used in the script
  odbcCloseAll()
  
  
} else {
  message('No HTV data, try again later')
  event_group_ID <- 'LG2101'
}


## Save the noms endpoint data out for future ref
# saveRDS(noms_extract, paste0(Noms_server_data, 'noms_extract_', time_stamp, '.rds'))





########################################### ---- TBC ---- #####################################################
