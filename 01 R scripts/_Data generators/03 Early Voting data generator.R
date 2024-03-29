
################################# ---- EARLY VOTING DATA GENERATOR  ---- ###########################################


## To do ----
## Get every SQL statement working
## Check table fields, etc. against requirements and graphs, remove uneeded stuff


## This code pulls the data for the early-voting dashboard from the databases
message('Run R code to generate the early voting data')





## 1). PULL EARLY VOTING OPENING DATA FROM DATABASES ===============================================================


## Remove from here any tables that are not relevant


## Pull pre-poll projection data ----
## LG2021 Projection data from EMS (i.e. input by Tom/Jade/Branislav), 
Pre_poll_projections <- sqlExecute(EMS_connection,
                                   "SELECT EventID,
                                   
                                    AreaCode,
                                    VenueName,
                                    VotingChannelCode,
                                    LocationTypeCode,
                                    
                                    ----PrePollDay,
                                    
                                    ProjectedVoters
                                    FROM Resources.EMS_VW_EventRollChannelPotential
                                    WHERE EventID LIKE (?) 
                                             AND VotingChannelCode = 'Pre-poll Ordinary'",
                                   
                                   ## Add the event_group_ID
                                   paste(event_group_ID, "%", sep = ""),
                                   TRUE,
                                   stringsAsFactors = FALSE)


## Pull opening hour data ----
## Get everything for all election events
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
                           left join [Resources].[EMS_Location] loc on evp.VenueName = loc.VenueName
                           left join [Events].EMS_Contest ea on EVP.EventID= substring(ea.ContestID,1,10)
                           left join [Events].[EMS_EventLocation] el on loc.VenueName = el.VenueName
                           and el.EventID = evp.EventID
                           
                           WHERE evp.EventID like ?
                           
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
                           paste(event_group_ID, "%", sep = ""),
                           fetch = TRUE,
                           stringsAsFactors = FALSE)





# 2) Pre-poll markoffs ----------------------------------------------------



## Note: non-councillor contest markoff totals are aggregated from councillor markoff counts for whole area


## Pull pre-poll markoffs by day and contests from EMA ----
## Check NAs in this query 
Markoffs_day_contests <- sqlExecute(EMA_connection,
                                    "WITH

                                    -- Extract contested election details
                                    contests AS (

                                    SELECT distinct areacode,
                                    		contestareacode,
                                    		CASE
                                    			WHEN CONTESTTYPECODE = 'By LGA'
                                    			  THEN 'Councillor'
                                    			WHEN CONTESTTYPECODE = 'General LGA'
                                    			  THEN 'Councillor'
                                    			WHEN CONTESTTYPECODE = 'By LGA Referendum'
                                    			  THEN 'Referendum'
                                    			WHEN CONTESTTYPECODE = 'General LGA Ref'
                                    			  THEN 'Referendum'
                                    			WHEN CONTESTTYPECODE = 'By LGA Poll'
                                    			  THEN 'Poll'
                                    			WHEN CONTESTTYPECODE = 'General LGA Poll'
                                    			  THEN 'Poll'
                                    			WHEN CONTESTTYPECODE = 'By Ward'
                                    			  THEN 'Councillor'
                                    			WHEN CONTESTTYPECODE = 'General Ward'
                                    			  THEN 'Councillor'
                                    			WHEN CONTESTTYPECODE = 'General Mayoralty'
                                    			  THEN 'Mayor'
                                    			WHEN CONTESTTYPECODE = 'By Mayoralty'
                                    			  THEN 'Mayor'
                                    			ELSE CONTESTTYPECODE
                                    		 END AS contesttypecode

                                    FROM PRD2008.ES_CONTEST
                                    WHERE  ELECTIONEVENTID = ?
                                    AND CONTESTSTATUSTYPECODE = 'Contested'
                                    ),

                                    -- Extract councillor contests
                                    contests_non_councillor AS (

                                    SELECT areacode,
                                    		contestareacode,
                                    		contesttypecode
                                    FROM contests
                                    WHERE contesttypecode != 'Councillor'
                                    ),

                                    -- Extract non-councillor contests
                                    contests_councillor AS (

                                    SELECT areacode,
                                    		contestareacode,
                                    		contesttypecode
                                    FROM contests
                                    WHERE contesttypecode = 'Councillor'
                                    ),

                                    -- Apply councillor contest areas to all 
                                    -- non-councillor contest types for relevent areas
                                    contests_mixed AS (

                                    SELECT A.areacode,
                                    		B.contestareacode,
                                    		A.contesttypecode
                                    FROM contests_non_councillor A

                                    LEFT JOIN contests_councillor B
                                    ON a.areacode = b.areacode
                                    ),

                                    -- Extract venue names
                                    venuenames AS (

                                    SELECT DISTINCT GAZETTEDNAME,
                                            LOCATIONID

                                    FROM PRD2008.ES_LOCATIONS
                                    WHERE VOTINGCENTRETYPECODE <> 'RO'
                                    AND ELECTIONEVENTID = ?
                                    ),

                                    -- Extract pre-poll markoff counts by day
                                    markoffs AS (

                                    SELECT  ISSUINGDISTAREACODE,
                                    		LOCATIONID,
                                            TO_CHAR(VOTEPROCESSEDDATE, 'YYYY-MM-DD') AS VOTEPROCESSEDDATE,
                                            COUNT(1) AS PRE_POLL_MARKOFFS

                                    FROM PRD2008.ES_DECEXCUSEVOTE

                                    WHERE ELECTIONEVENTID = ?
                                      AND DECLARATIONEXCUSEVOTETYPECODE = 'Pre-poll Ordinary'

                                    GROUP BY ISSUINGDISTAREACODE, LOCATIONID, TO_CHAR(VOTEPROCESSEDDATE, 'YYYY-MM-DD')
                                    ),

                                    ---- Add contest types and aggregate markoffs to IssuingArea for non-councillor contests
                                    markoffs_non_councillor AS (

                                    SELECT B.AREACODE AS ISSUINGAREA,
                                            B.AREACODE AS CONTESTAREA,
                                            B.CONTESTTYPECODE,
                                            A.LOCATIONID,
                                            A.VOTEPROCESSEDDATE,
                                            SUM(A.PRE_POLL_MARKOFFS) AS PRE_POLL_MARKOFFS

                                    FROM markoffs A

                                    ---- Right join to exclude markoffs with councillor-only areas (no ref/may/poll)
                                    RIGHT JOIN contests_mixed B
                                        ON A.ISSUINGDISTAREACODE = B.CONTESTAREACODE
                                        
                                    WHERE A.LOCATIONID IS NOT NULL

                                    GROUP BY B.AREACODE,
                                            B.AREACODE,
                                            B.CONTESTTYPECODE,
                                            A.LOCATIONID,
                                            A.VOTEPROCESSEDDATE
                                    ),

                                    ---- Add contest types for councillor contests
                                    markoffs_councillor AS (

                                    SELECT B.AREACODE AS ISSUINGAREA,
                                            A.ISSUINGDISTAREACODE AS CONTESTAREA,
                                            B.CONTESTTYPECODE,
                                            A.LOCATIONID,
                                            VOTEPROCESSEDDATE,
                                            PRE_POLL_MARKOFFS

                                    FROM markoffs A

                                    LEFT JOIN contests_councillor B
                                        ON A.ISSUINGDISTAREACODE = B.contestareacode
                                    ),

                                    -- Combine councillor and non-councillor markoffs
                                    markoffs_all AS (

                                    SELECT * FROM markoffs_non_councillor
                                    UNION
                                    SELECT * FROM markoffs_councillor
                                    )

                                    -- Join venue names and select results

                                    SELECT A.ISSUINGAREA,
                                            A.CONTESTAREA,
                                    		B.GAZETTEDNAME AS VENUENAME,
                                            A.CONTESTTYPECODE,
                                            VOTEPROCESSEDDATE,
                                            PRE_POLL_MARKOFFS

                                    FROM markoffs_all A
                                    LEFT JOIN venuenames B
                                    ON A.LOCATIONID = B.LOCATIONID

                                    ORDER BY A.ISSUINGAREA,
                                    A.CONTESTAREA,
                                    		B.GAZETTEDNAME,
                                            A.CONTESTTYPECODE,
                                            A.VOTEPROCESSEDDATE
                                                ",
                                    
                                    ## Attach event_group_ID
                                    as.list(rep(event_group_ID, 3)),
                                    fetch            = TRUE,
                                    stringsAsFactors = FALSE)


#get event names
Contests_council <- sqlExecute(EMA_connection,    
                               "SELECT
                               
                                electioneventid,
                                areacode,
                                contestid,
                                contestareacode   
                                FROM
                                prd2008.es_contest
                                WHERE ELECTIONEVENTID = ?
                                and CONTESTTYPECODE IN ('By LGA', 'General LGA', 'By Ward', 'General Ward')
                                ORDER BY AREACODE, CONTESTAREACODE",
                               event_group_ID,
                               fetch            = TRUE,
                               stringsAsFactors = FALSE) %>%
  as_tibble()


## Now disconnect from all databases used in the script
odbcCloseAll()





####################################################################################################################
################################################# TBC ---- #########################################################
####################################################################################################################