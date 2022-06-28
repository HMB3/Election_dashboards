####################################################################################################################
###################################### EARLY VOTING DATA GENERATOR  ---- ###########################################
####################################################################################################################


## To do :
## Get every SQL statement working ---- 
## Check table fields, etc against requirements and graphs ---

## Projection from EMS, Markoff from EMA, AOBP from EMA
## Find where the AOBP is in EMMA ----





# The audience for the dashboard is:
#   .	Attendance voting team
# .	Logistics team
# .	EOG
# 
# 2.2. Project Objectives
# .	Monitor pre-poll voting and markoff (including declaration votes)
# .	Highlight pre-poll venues that may be of concern
# 
# 2.3. Project Scope
# In scope;
# .	Tracking the number of votes issued in pre-poll venues versus the number of markoffs
# .	Tracking ballot paper stock levels
# .	Sydney Town Hall
# 
# Out of scope;
# .	Tracking of declaration votes


## This code pulls the data for the early-voting dashboard from the databases
message('Run R code to create the RO Data')



## When are we going to get the data from the nominations team?


## Analytics database connections ----
## Production is the real life stuff, staging is the testing
hyperion_connection <- odbcDriverConnect('driver={SQL Server};server=SVRANALYTICS1;trusted_connection=true',
                                         readOnlyOptimize = TRUE)


EMS_connection      <- odbcDriverConnect('driver={SQL Server};server=svrrollSQL1;database=EMS2009;trusted_connection=true',
                                         readOnlyOptimize = TRUE)





## 1). PULL EARLY VOTING OPENING DATA FROM DATABASES ===================================================================


## Pull pre-poll projection data ----
## Check this query
## FIND in EMS, relevant tables
Pre_poll_projections <- sqlExecute(EMS_connection,
                                   "SELECT EventID,
                                   
                          AreaCode,
                          VenueName,
                          VotingChannelCode,
                          LocationTypeCode,
                          
                          ProjectedVoters
                          FROM Resources.EMS_VW_EventRollChannelPotential
                          WHERE EventID LIKE (?) 
                                   AND VotingChannelCode = 'Pre-poll Ordinary'",
                                   paste(event_group_ID,"%", sep = ""),
                                   TRUE,
                                   stringsAsFactors = FALSE) %>%
  as_tibble()



opening_hour <- sqlExecute(EMS_connection, "SELECT  
                           evp.EventID,
                           ea.AreaCode,
                           evp.VenueName,
                           el.[LocationStatusCode],
                           datename(dw, date) + ', ' + CONVERT(varchar, date, 106) OpeningDate, 
                           Date,
                           IsOpen,
                           
                           loc.StreetAddressID,
                           
                           evp.OpeningTime,
                           evp.ClosingTime
                           from Events.EMS_EventLocationOpeningHours evp
                           left join [EMS2009].[Resources].[EMS_Location] loc on evp.VenueName = loc.VenueName
                           left join [EMS2009].[Events].EMS_Contest ea on EVP.EventID= substring(ea.ContestID,1,10)
                           left join [EMS2009].[Events].[EMS_EventLocation] el on loc.VenueName = el.VenueName
                           and el.EventID = evp.EventID
                           where evp.EventID like ?
                           AND ea.ContestStatusTypeCode = 'Contested'
                           AND el.LocationStatusCode NOT IN ('Away Cancelled',
                           'Cancellation Notified',
                           'Cancelled',
                           'Deferred',
                           'Hire Agreement Rejected',
                           'Initial',
                           'Managed by 3rd party',
                           'Print Cancelled letter',
                           'Print Deferred letter',
                           'Processed',
                           'Unacceptable',
                           'Unavailable',
                           'Unavailable Indefinitely',
                           'Uncontested',
                           'Visit Not Required')
                           order by EventID, VenueName, Date", 
                           paste(event_group_ID, "%", sep = ""), TRUE,
                           stringsAsFactors = FALSE)%>%
  as_tibble()


## Check if this is needed


##  If EMA is down
if(EMA_down == TRUE) {
  
  ## Pull candidate data ----
  ## Check where does this come from?
  Candidates <- read.csv('G:/Election Events/SGE 2019 Programme/AP9 Data Management/12 Deliverables/07 Dashboard, RMT and EOG/02 Source data/Candidates.csv') %>%
    select(-X)
  
  
  ## Pull enrollment data 
  enrolment <- read.csv('G:/Election Events/SGE 2019 Programme/AP9 Data Management/12 Deliverables/07 Dashboard, RMT and EOG/02 Source data/enrolment.csv') %>%
    select(-X)
}





## 2). PULL EARLY VOTING CANDIDATE DATA FROM DATABASES ================================================================================


## If EMA is NOT down
if(EMA_down == FALSE) {
  
  ## Pull candidate data from EMA ----
  ## Maybe add columns: CONTESTID is unique to ward
  Candidates <- sqlExecute(EMA_connection,
                           "SELECT can.ELECTIONEVENTID,
                             can.AREACODE,
                             can.CONTESTID,
                             can.CANDIDATEBALLOTNAME,
                             pt.PARTYNAME,
                             can.GROUPNUMBER,
                             grp.grouplabel,
                             can.DRAWNUMBER

                             FROM PRD2008.ES_CANDIDATE can
                             left join PRD2008.PM_PARTY pt on can.ENDORSEDBYPARTYID = pt.PARTYID
                             left JOIN PRD2008.ES_GROUP grp on can.GROUPNUMBER = grp.GROUPNUMBER -- or (can.GROUPNUMBER is null)
                             where can.ELECTIONEVENTID = ?
                             and grp.ELECTIONEVENTID = ?
                             and can.validatedbyHO is not null
                             and can.AREACODE = 'NSW'

                             -- order by GroupNumber, CandidateBallotName

                             union

                             SELECT can.ELECTIONEVENTID,
                             can.AREACODE,
                             can.CONTESTID,
                             can.CANDIDATEBALLOTNAME,
                             pt.PARTYNAME,
                             can.GROUPNUMBER,
                             null as grouplabel,
                             can.DRAWNUMBER
    
                             FROM PRD2008.ES_CANDIDATE can
                             left join PRD2008.PM_PARTY pt on can.ENDORSEDBYPARTYID = pt.PARTYID
                             where can.ELECTIONEVENTID = ?
                             and can.validatedbyHO is not null
                             and AREACODE != 'NSW'
    
                             union
    
                             SELECT can.ELECTIONEVENTID,
                             can.AREACODE,
                             can.CONTESTID,
                             can.CANDIDATEBALLOTNAME,
                             pt.PARTYNAME,
                             can.GROUPNUMBER,
                             null as grouplabel,
                             can.DRAWNUMBER
    
                             FROM PRD2008.ES_CANDIDATE can
                             left join PRD2008.PM_PARTY pt on can.ENDORSEDBYPARTYID = pt.PARTYID
                             where can.ELECTIONEVENTID = ?
                             and can.validatedbyHO is not null
                             and can.AREACODE = 'NSW'
                             and can.GROUPNUMBER is null
                         ",
                           as.list(rep(event_group_ID, 4)),
                           fetch = TRUE,
                           stringsAsFactors = FALSE) %>%
    rename(AreaCode = AREACODE)%>%
    as_tibble()
  
  
  ## Pull Enrollment data from EMA ----
  ## Jerry says this stays the same :: how does 'district' become LGA?
  enrolment <- sqlExecute(EMA_connection,
                          "SELECT INITCAP(ELEC.NSWECSTATEDISTRICT) as AREACODE,
                            COUNT(1) AS ENROLMENT
                            FROM PRD2008.ES_ONTHEROLL OTR
                            LEFT JOIN PRD2008.ES_ELECTOR ELEC
                            ON OTR.ELECTORID = ELEC.ELECTORID
                            WHERE OTR.ELECTIONEVENTID = ?
                            GROUP BY NSWECSTATEDISTRICT
                            ORDER BY NSWECSTATEDISTRICT",
                          event_group_ID,
                          fetch = TRUE,
                          stringsAsFactors = FALSE) %>%
    
    ## Is the Ku-Ring-Gai change still needed? 
    group_by(AREACODE) %>%
    summarise(Enrolment = sum(ENROLMENT)) %>%
    mutate(AreaCode = ifelse(AREACODE=='Ku-Ring-Gai', 'Ku-ring-gai', AREACODE))
  
}





## 3). PULL EARLY VOTING MARKOFF DATA FROM DATABASES ================================================================================


## Pull markoff data from EMA ----
## Might need extra fields :: eg Venue
Markoff <- sqlExecute(EMA_connection,    
                      "SELECT
                      A.electioneventid,
                      A.electorid,
                      A.declarationexcusevotetypecode,
                      A.applicationnumber,
                      A.votestatuscode,
                      A.voteprocesseddate,
                      A.livesinelectorate,
                      A.livedinelectoraterecently,
                      A.votersmarkwitnessed,
                      A.voteprocessedby,
                      A.locationid,
                      A.votingcentretypecode,
                      A.locationflagid,
                      A.scrutinizedby,
                      A.issuingdistredistributioncode,
                      A.issuingdistareacode,
                      A.electorelectionareaflag,
                      A.electorelectionwardflag,
                      A.count_number,
                      A.votingareacode,
                      B.GAZETTEDNAME,
                      B.LOCATIONID
    
                      FROM
                      prd2008.es_decexcusevote A
                      
                      LEFT JOIN prd2008.ES_LOCATIONS B
    
                      ON A.LOCATIONID = B.LOCATIONID
                      AND A.ELECTIONEVENTID = B.ELECTIONEVENTID
                      AND A.VOTINGCENTRETYPECODE = B.VOTINGCENTRETYPECODE
    
                      WHERE A.ELECTIONEVENTID = ?
                      AND A.VOTINGCENTRETYPECODE = 'Pre-Poll'
                      AND A.DECLARATIONEXCUSEVOTETYPECODE = 'Pre-poll Ordinary'
                              ",
                      event_group_ID,
                      fetch            = TRUE,
                      stringsAsFactors = FALSE) %>%
  
  ## Change the VOTE PROCESSED DATE field
  mutate(VOTEPROCESSEDDATE = as.character(as.Date(VOTEPROCESSEDDATE, tz = 'Australia/Sydney')),
         StreetAddressID   = as.integer(substring(LOCATIONID, 15, 25)))%>%
  as_tibble()


## Account of Ballot Paper ----
## Check this code :: This will be from hyperion staging (Election Data)
## UAT for AOBP needs to be completed, talk to Deepak
## Projection from EMS, Markoff from EMA, AOBP from EMA...? Find out Who knows
AoBP_read <- sqlExecute(hyperion_connection,
                        "
                        /****** Script for SelectTopNRows command from SSMS  ******/
              SELECT [ReturningOffice]
                                      ,[SGAreaCode]
                                      ,[ContestTypeCode]
                                      ,[LocationTypeCode]
                                      ,[VenueName]
                                      ,[Date]
                                      ,[PrintQuantity]
                                      ,[Allocated_BPs]
                                      ,[Daily_start_quantity]
                                      ,[Spoilt]
                                      ,[Discarded]
                                      ,[Prepoll_Ordinary]
                                      ,[Enrolment]
                                      ,[Absent]
                                      ,[NAMAV]
                                      ,[Unused]
                                      ,[LocationType]
                                      ,[Variance]
                                      FROM [ElectionData].[Staging].[AoBP_Prepoll_daily_account]
                        ",
                        fetch            = TRUE,
                        stringsAsFactors = FALSE)


## Get Sydney Town Hall location? ---
## Not important, just taken for the map
STH <- sqlExecute(hyperion_connection,
                  "SELECT 
                  [AddressName]
                  ,[Latitude]
                  ,[Longitude]
                  FROM [ElectionData].[Staging].[EMS_Address]
                  where AddressName like 'Sydney Town Hall'",
                  fetch            = TRUE,
                  stringsAsFactors = FALSE) %>% 
  distinct(Latitude, Longitude)


## Now disconnect from all databases used in the script






####################################################################################################################
################################################# TBC ---- #########################################################
####################################################################################################################