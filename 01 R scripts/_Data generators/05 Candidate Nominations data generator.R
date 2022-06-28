################################# ---- NOMINATIONS DATA GENERATOR ---- #############################################




## This code pulls the data for the 'Candidate nominations' dashboard
message('Run R code to create Candidate Nominations data')



## 1). NOMINATIONS ENDPOINT DATA ===================================================================


## Read in Nominations test data as an extract
## Need to setup an auto procedure to run this
setwd(root_dashboard)


## FROM TRAVERS ::
# We have had a request to add "Area" and "Ward" to the ZOHO extract in NOMS.  
# NOMS has a feature to extract the data that is provided to ZOHO via the API's 
# and put it into an Excel spreadsheet, that the operational team use for various reasons.  
# They have requested the two additional fields to help them identify which area/ward each 
# entry pertains to, currently, the ZOHO API only provides the contest code and no descriptive fields.


## Endpoint URL's
# The endpoints differ for each NOMS environment.  Comment and uncomment as required

if(endpoint_skip) {
  
  message('Do not call the endpoint')
  setwd(Noms_server_data)
  noms_extract   <- readRDS(list.files(pattern = '.rds'))
  event_group_ID <- noms_extract$ELECTIONEVENTID[1]
  
} else {
  
  ## Go-live endpoints
  message('Call the endpoint')
  noms_endpoint   <- 'https://nomsprodfn02.cmsprodase.p.azurewebsites.net/api/GetDataAnalyticsNomsDataExport?code=qzEGGA05eUKIdMGZ0NIUyeYvQCLDwzJ1C7seuxH9uy6g2RN6e2WzWA=='
  
  ## 
  options(timeout = 4000000)
  
  
  noms_extract <- data.frame()
  sleep_time   <- 15
  
  
  ## Import data from NOMs
  for(i in 1:10){
    
    if(nrow(noms_extract) == 0) {
      
      message('calling MR WRONG, calling me INSANE ')
      
      tryCatch(
        
        noms_extract <- jsonify::from_json(noms_endpoint,   simplify = TRUE, fill_na = FALSE),
        error = function(e) {message('Iteration ', i, ' timed out')})
      gc() }
    
    if(nrow(noms_extract) != 0) {
      Sys.sleep(sleep_time)
    }
  }
  
  gc()
  
  
  # json_data       <- rjson::fromJSON(file = paste0(Noms_server_data, "GetDataAnalyticsNomsDataExport.json"))
  # json_data_frame <- as.data.frame(json_data)
  
  ## Generate the event ID from the endpoint itself
  event_group_ID <- unique(noms_extract$ELECTIONEVENTID)
  
}





## 2). EMA CANDIDATE/PARTY/CONTEST  DATA ================================================================================


## Note that the data for CANDIDATE/PARTY/CONTEST is coming from the NOMS database now, not EMA
## However, some of this data is probably still useful...


## Eligible Political Parties ----
## This should match Andrew's PDF.
if(class(event_group_ID) != "NULL") {
  
  EMA_Eligible_Parties <- sqlExecute(EMA_connection,
                                     "SELECT
                                      a.electioneventid,
                                      a.partyid,
                                      a.includeddate,
                                      a.includedby,
                                      a.vtrresultcode,
                                      a.lastupdated,
                                      a.lastupdatedby,
                                      a.ucn,
                                      b.partyname
                                  FROM
                                      prd2008.es_party a
                                      left join prd2008.pm_party b
                                      on a.partyid = b.partyid
                                      where a.electioneventid = ?
                                      and a.includeddate IS NOT NULL",
                                     
                                     event_group_ID,
                                     fetch            = TRUE,
                                     stringsAsFactors = FALSE) %>%
    
    mutate(PARTYNAME = str_to_upper(PARTYNAME)) %>%
    dplyr::rename(RPPNAME = PARTYNAME) %>% dplyr::select(RPPNAME) %>% arrange(RPPNAME)
  
  
  
  
  
  ## Contests ----
  EMA_contests <- sqlExecute(EMA_connection, 
                             "SELECT
                              electioneventid,
                              areacode,
                              contestid,
                              contestareacode,
                              contesttypecode,
                              conteststatustypecode,
                              numberofpositionscontested
                          
                              FROM
                              prd2008.es_contest
                              
                              WHERE electioneventid = ? 
                              AND conteststatustypecode not in ('Not Run by EC', 'Deferred', 'Under Administration')",
                             event_group_ID,
                             fetch            = TRUE,
                             stringsAsFactors = FALSE)
  
} else {
  print('Nominations database is empty, use test data')
  event_group_ID <- 'LG2101'
  EMA_Eligible_Parties <- sqlExecute(EMA_connection,
                                     "SELECT
                                      a.electioneventid,
                                      a.partyid,
                                      a.includeddate,
                                      a.includedby,
                                      a.vtrresultcode,
                                      a.lastupdated,
                                      a.lastupdatedby,
                                      a.ucn,
                                      b.partyname
                                  FROM
                                      prd2008.es_party a
                                      left join prd2008.pm_party b
                                      on a.partyid = b.partyid
                                      where a.electioneventid = ?
                                      and a.includeddate IS NOT NULL",
                                     
                                     event_group_ID,
                                     fetch            = TRUE,
                                     stringsAsFactors = FALSE) %>%
    
    mutate(PARTYNAME = str_to_upper(PARTYNAME)) %>%
    dplyr::rename(RPPNAME   = PARTYNAME) %>% dplyr::select(RPPNAME) %>% arrange(RPPNAME)
  
  
  
  
  
  ## Contests ----
  EMA_contests <- sqlExecute(EMA_connection, 
                             "SELECT
                              electioneventid,
                              areacode,
                              contestid,
                              contestareacode,
                              contesttypecode,
                              conteststatustypecode,
                              numberofpositionscontested
                          
                              FROM
                              prd2008.es_contest
                              
                              WHERE electioneventid = ? 
                              AND conteststatustypecode not in ('Not Run by EC', 'Deferred', 'Under Administration')",
                             event_group_ID,
                             fetch            = TRUE,
                             stringsAsFactors = FALSE)
  
}


## Now disconnect from all databases used in the script
odbcCloseAll()



## Save data
## write_csv(noms_extract, paste0('noms_extract_', time_stamp, '.csv'))




#################################################### TBC ###########################################################