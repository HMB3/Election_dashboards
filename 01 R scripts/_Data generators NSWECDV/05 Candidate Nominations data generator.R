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


## Go-live endpoints
noms_endpoint   <- 'https://nomsprodfn02.cmsprodase.p.azurewebsites.net/api/GetDataAnalyticsNomsDataExport?code=qzEGGA05eUKIdMGZ0NIUyeYvQCLDwzJ1C7seuxH9uy6g2RN6e2WzWA=='
party_endpoint  <- 'https://nomsprodfn02.cmsprodase.p.azurewebsites.net/api/GetDataAnalyticsPartyData?code=Va/4pE8NMPlPbfrYzNx6tJXBZPInra/RXngV4xziFSWQtorak4n2TQ=='
status_endpoint <- "https://nomsprodfn02.cmsprodase.p.azurewebsites.net/api/GetDataAnalyticsStatusLog?code=dXTgcnDtlO2LkaJHGSiwLFlz8GAApD9GmFbeqOdUG6FzKaFVgnqHHA=="



## Import data from NOMs
noms_extract   <- jsonify::from_json(noms_endpoint,   simplify = TRUE, fill_na = FALSE)
noms_party     <- jsonify::from_json(party_endpoint,  simplify = TRUE, fill_na = FALSE)
noms_status    <- jsonify::from_json(status_endpoint, simplify = TRUE, fill_na = FALSE)


## Extract individual tables from status endpoint
# candidate_noms_status_log <- noms_status[[1]]
# groups_noms_status_log    <- noms_status[[2]]
# 


## Generate the event ID from the endpoint itself
event_group_ID <- unique(noms_extract$ELECTIONEVENTID)





## 2). EMA CANDIDATE/PARTY/CONTEST  DATA ================================================================================


## Note that the data for CANDIDATE/PARTY/CONTEST is coming from the NOMS database now, not EMA
## However, some of this data is probably still useful...


## Eligible Political Parties ----
## This should match Andrew's PDF.
if(class(event_group_ID) != "NULL") {
  
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
                                      b.partyname
                                      FROM [DV_Staging_EMA].[PRD2008].[ES_PARTY] a
                                      left join [DV_Staging_EMA].[PRD2008].[PM_PARTY] b
                                      on a.partyid = b.partyid
                                      where a.electioneventid = ?
                                      and a.includeddate IS NOT NULL",
                                     
                                     event_group_ID,
                                     fetch            = TRUE,
                                     stringsAsFactors = FALSE) %>%
    
    mutate(PARTYNAME = str_to_upper(PARTYNAME)) %>%
    dplyr::rename(RPPNAME   = PARTYNAME) %>% select(RPPNAME) %>% arrange(RPPNAME)
  
  
  
  
  
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
  
} else {
  print('Nominations database is empty, try again later :]')
}


## Now disconnect from all databases used in the script
odbcCloseAll()



## Save data
## write_csv(noms_extract, paste0('noms_extract_', time_stamp, '.csv'))




#################################################### TBC ###########################################################