################################# ---- DECLARATION VOTING DATA GENERATOR  ---- #####################################


## This code extracts the data for the declartion voting dashboard
message('Run R code to create the Declaration Voting data')





## 1). PULL MARKOFF DATA ============================================================



## Provisional markoffs issued at pre-poll and Sydney Town Hall
EMA_markoffs <- sqlExecute(channel = EMA_connection,
                           query   = "
                           
                            WITH
                            
                            -- Get list of provisional markoff elector ID's
                            provisional_IDs AS (
                            
                            SELECT  distinct a.electorid, a.declarationexcusevotetypecode
                            FROM PRD2008.es_votestatushistory A
                            WHERE A.ELECTIONEVENTID = ?
                            AND a.votestatustypecode = 'Markoff'
                            
                            ),
                            
                            
                            -- Count number of provisional markoffs
                            provisional AS (
                            
                            SELECT  
                            A.ISSUINGDISTAREACODE,
                            A.DECLARATIONEXCUSEVOTETYPECODE,
                            a.locationid,
                            a.VOTINGCENTRETYPECODE,
                            TO_CHAR(A.voteprocesseddate, 'YYYY-MM-DD') AS VOTEPROCESSDATE,
                            
                            COUNT(1) AS PROVISIONAL_MARKOFFS
                            FROM PRD2008.ES_DECEXCUSEVOTE A
                            INNER JOIN provisional_IDs B
                                ON A.ELECTORID = B.ELECTORID
                                    AND A.declarationexcusevotetypecode = B.declarationexcusevotetypecode
                            WHERE A.ELECTIONEVENTID = ?
                                --AND A.VOTINGCENTRETYPECODE = 'Pre-Poll'
                                AND (A.DECLARATIONEXCUSEVOTETYPECODE = 'NAMAV' OR A.DECLARATIONEXCUSEVOTETYPECODE = 'Enrolment')
                            GROUP BY A.ISSUINGDISTAREACODE, a.locationid, a.VOTINGCENTRETYPECODE, 
                            A.DECLARATIONEXCUSEVOTETYPECODE, TO_CHAR(A.voteprocesseddate, 'YYYY-MM-DD')
                            
                            ),
                            
                            
                            -- Count number of scrutinised dec votes
                            scrutinised AS (
                            
                            SELECT  
                            A.ISSUINGDISTAREACODE,
                            A.DECLARATIONEXCUSEVOTETYPECODE,
                            a.locationid,
                            a.VOTINGCENTRETYPECODE,
                            TO_CHAR(A.voteprocesseddate, 'YYYY-MM-DD') AS VOTEPROCESSDATE,
                            COUNT(1) AS TOTAL_MARKOFFS, 
                            COUNT(CASE A.VOTESTATUSCODE WHEN 'Accepted' THEN 1 ELSE NULL END) AS ACCEPTED_MARKOFFS,
                            COUNT(CASE A.VOTESTATUSCODE WHEN 'Rejected' THEN 1 ELSE NULL END) AS REJECTED_MARKOFFS
                            
                            FROM PRD2008.ES_DECEXCUSEVOTE A
                            WHERE A.ELECTIONEVENTID = ?
                            AND (A.DECLARATIONEXCUSEVOTETYPECODE = 'NAMAV' OR A.DECLARATIONEXCUSEVOTETYPECODE = 'Enrolment')
                            GROUP BY A.ISSUINGDISTAREACODE, a.locationid, a.VOTINGCENTRETYPECODE, 
                            A.DECLARATIONEXCUSEVOTETYPECODE, TO_CHAR(A.voteprocesseddate, 'YYYY-MM-DD')
                            
                            ),
                            
                            -- Combine the tables
                            combined AS (
                            
                            select
                                ISSUINGDISTAREACODE,
                                DECLARATIONEXCUSEVOTETYPECODE,
                                locationid,
                                VOTINGCENTRETYPECODE,
                                VOTEPROCESSDATE,
                                NULL AS TOTAL_MARKOFFS,
                                PROVISIONAL_MARKOFFS,
                                NULL AS ACCEPTED_MARKOFFS,
                                NULL AS REJECTED_MARKOFFS
                            
                            from provisional 
                            
                            union all
                            
                            select
                                ISSUINGDISTAREACODE,
                                DECLARATIONEXCUSEVOTETYPECODE,
                                locationid,
                                VOTINGCENTRETYPECODE,
                                VOTEPROCESSDATE,
                                TOTAL_MARKOFFS,
                                NULL AS PROVISIONAL_MARKOFFS,
                                ACCEPTED_MARKOFFS,
                                REJECTED_MARKOFFS
                                
                            from scrutinised
                            
                            ),
                            
                            -- Aggregate again
                            condensed AS (
                            
                            select 
                                ISSUINGDISTAREACODE,
                                DECLARATIONEXCUSEVOTETYPECODE,
                                locationid,
                                VOTINGCENTRETYPECODE,
                                VOTEPROCESSDATE,
                                
                                --- These numbers are not matching the EMA readout
                                sum(TOTAL_MARKOFFS) AS TOTAL_MARKOFFS,
                                sum(PROVISIONAL_MARKOFFS) AS PROVISIONAL_MARKOFFS,
                                
                                ---- But these numners are
                                sum(ACCEPTED_MARKOFFS) AS ACCEPTED_MARKOFFS,
                                sum(REJECTED_MARKOFFS) AS REJECTED_MARKOFFS
                                
                                from combined
                                group by ISSUINGDISTAREACODE,
                                DECLARATIONEXCUSEVOTETYPECODE,
                                locationid,
                                VOTINGCENTRETYPECODE,
                                VOTEPROCESSDATE
                            )
                            
                            
                            -- Join to location table and clean up columns
                            SELECT  
                                A.ISSUINGDISTAREACODE,
                                A.VOTINGCENTRETYPECODE,
                                B.GAZETTEDNAME,
                                A.DECLARATIONEXCUSEVOTETYPECODE AS DEC_VOTE_TYPE,
                                A.VOTEPROCESSDATE,
                                TOTAL_MARKOFFS,
                                PROVISIONAL_MARKOFFS,
                                ACCEPTED_MARKOFFS,
                                REJECTED_MARKOFFS
                            
                            FROM condensed A
                            
                            LEFT JOIN (
                                SELECT
                                    GAZETTEDNAME,
                                    LOCATIONID,
                                    VOTINGCENTRETYPECODE,
                                    ELECTIONEVENTID
                                FROM PRD2008.ES_LOCATIONS
                                WHERE VOTINGCENTRETYPECODE <> 'RO'
                                AND ELECTIONEVENTID = ?) B
                            ON A.LOCATIONID = B.LOCATIONID
                            AND A.VOTINGCENTRETYPECODE = B.VOTINGCENTRETYPECODE
                            
                            ORDER BY A.ISSUINGDISTAREACODE, A.VOTINGCENTRETYPECODE, 
                            B.GAZETTEDNAME, A.DECLARATIONEXCUSEVOTETYPECODE, A.VOTEPROCESSDATE                           
                                                          ",
                           as.list(rep(event_group_ID, 4)),
                           TRUE,
                           stringsAsFactors = FALSE) %>%
  as_tibble()





## 3). PULL AOBP DATA ============================================================


## DecVote AOBP table 2). ----
## Data sourced from polling place AoBP 
## Make sure the NAs are real, then add exception
EMA_AoBP <- sqlExecute(channel = EMA_connection,
                       query   = "SELECT  
                                  A.AREACODE,
                                  C.CONTESTAREACODE,
                                  CASE
                                  		WHEN C.CONTESTTYPECODE = 'By LGA'
                                  		   THEN 'Councillor'
                                  		WHEN C.CONTESTTYPECODE = 'General LGA'
                                  		   THEN 'Councillor'
                                  		WHEN C.CONTESTTYPECODE = 'By LGA Referendum'
                                  		   THEN 'Referendum'
                                  		WHEN C.CONTESTTYPECODE = 'General LGA Ref'
                                  		   THEN 'Referendum'
                                  		WHEN C.CONTESTTYPECODE = 'By LGA Poll'
                                  		   THEN 'Poll'
                                  		WHEN C.CONTESTTYPECODE = 'General LGA Poll'
                                  		   THEN 'Poll'
                                  		WHEN C.CONTESTTYPECODE = 'By Ward'
                                  		   THEN 'Councillor'
                                  		WHEN C.CONTESTTYPECODE = 'General Ward'
                                  		   THEN 'Councillor'
                                  		WHEN C.CONTESTTYPECODE = 'General Mayoralty'
                                  		   THEN 'Mayor'
                                  		WHEN C.CONTESTTYPECODE = 'By Mayoralty'
                                  		   THEN 'Mayor'
                                  		ELSE C.CONTESTTYPECODE
                                  	  END AS CONTESTTYPECODE,
                                  	  B.GAZETTEDNAME AS VENUE,
                                  	  B.VOTINGCENTRETYPECODE,
                                  	  A.ENROL_ENVELOPES,
                                  	  A.NAMAV_ENVELOPES
                                  	  
                                  ---- This is results, it should be empty until election night
                                  ---- NULL are fine 
                                  FROM PRD2008.ES_VOTECOUNTINGPOINT A
                                  JOIN (SELECT  GAZETTEDNAME,
                                  			  VOTINGCENTRETYPECODE,
                                  			  LOCATIONID,
                                  			  ELECTIONEVENTID
                                  	  FROM PRD2008.ES_LOCATIONS
                                  	  WHERE VOTINGCENTRETYPECODE != 'RO') B
                                    ON    A.LOCATIONID = B.LOCATIONID
                                  JOIN PRD2008.ES_CONTEST C
                                    ON    A.CONTESTID = C.CONTESTID
                                  WHERE A.ELECTIONEVENTID = ?
                                  AND B.ELECTIONEVENTID = ?
                                  AND C.ELECTIONEVENTID = ?
                                  AND C.CONTESTSTATUSTYPECODE = 'Contested'
                                  -- AND C.CONTESTTYPECODE IN ('By LGA', 'General LGA', 'By Ward', 'General Ward')
                                  ORDER BY A.AREACODE, CONTESTTYPECODE, C.CONTESTAREACODE, VENUE",
                       
                       as.list(rep(event_group_ID, 3)),
                       TRUE,
                       stringsAsFactors = FALSE) %>%
  as_tibble()






## 4). PULL POSTAL DATA ============================================================




## Postal Votes
Postal_accepted_rejected <- sqlExecute(EMA_connection,    
                                       "select count(*) total,
                                        sum(decode(pv.rejected, null, 1, 0)) accepted,
                                        sum(decode(pv.rejected, null, 0, 1))  rejected_application
                                        
                                        from prd2008.es_postalvoteapplication pv
                                        
                                        where electioneventid = ?",
                                       event_group_ID,
                                       fetch            = TRUE,
                                       stringsAsFactors = FALSE) %>%
  as_tibble()




## Postal Votes ----
## View(Postal_votes_council[duplicated(Postal_votes_council$ELECTORID),])
Postal_votes_council <- sqlExecute(EMA_connection,    
                                   "SELECT
                                   
                                    electioneventid,
                                    declarationexcusevotetypecode,
                                    electorid,
                                    applicationnumber,
                                    processeddate,
                                    postalvotereturneddate,
                                    certificatedateprinted,
                                    certificateaccepted,
                                    processeddateby,
                                    certificatedateprintedby,
                                    certificateacceptedby,
                                    typecode,
                                    fulfillmenttype,
                                    rejectextractiondate,
                                    resendfulfillmentdate,
                                    sendfulfillmentdate,
                                    batchid,
                                    sendfulfillmentfilename,
                                    resendfulfillmentfilename
                                    
                                    ----
                                    FROM
                                    prd2008.es_postalvote
                                    WHERE electioneventid = ?
                                        ",
                                   event_group_ID,
                                   fetch            = TRUE,
                                   stringsAsFactors = FALSE) %>%
  as_tibble()






## Postal Applications ----

Postal_applications_council <- sqlExecute(EMA_connection,    
                                          "SELECT
                                          
                                            electioneventid,
                                            electorid,
                                            declarationexcusevotetypecode,
                                            applicationnumber,
                                            datereceived,
                                            accepted,
                                            acceptedby,
                                            typecode,
                                            acceptedreason,
                                            rejected,
                                            rejectedreason
                                
                                            ---- 
                                            FROM
                                            prd2008.es_postalvoteapplication
                                            WHERE electioneventid = ?
                                            ",
                                          event_group_ID,
                                          fetch            = TRUE,
                                          stringsAsFactors = FALSE) %>%
  as_tibble()


#Not all applicants are captured in Postal_applications_council - working theory is this is because of the connection
#to an elector.
#all applicantions rejected due to reasons not attached to an elector appear to be here.
#definitely matches the application number on PV Application report given by Nikki.

Postal_Applications_not_attached <-  sqlExecute(EMA_connection,    
                                                "SELECT
                                          
                                          electioneventid,
    created,
    rejectedby,
    rejectedreason,
    applicationnumber,
    rejected
                                
                                            ---- 
                                            FROM
                                            prd2008.es_pvatempdocs
                                            WHERE electioneventid = ?
                                            AND rejectedreason IS NOT null
                                            ",
                                                event_group_ID,
                                                fetch            = TRUE,
                                                stringsAsFactors = FALSE) %>%
  as_tibble()




## Dec excuse votes ----

Dec_excuse_postal_council <- sqlExecute(EMA_connection,    
                                        "SELECT
                                        
                                          electioneventid,
                                          electorid,
                                          applicationnumber,
                                          declarationexcusevotetypecode,
                                          votestatuscode,
                                          voteprocesseddate,
                                          issuingdistareacode,
                                          votingareacode
                                         FROM
                                          prd2008.es_decexcusevote
                                          WHERE electioneventid = ?
                                          AND declarationexcusevotetypecode = 'Postal'",
                                        event_group_ID,
                                        fetch            = TRUE,
                                        stringsAsFactors = FALSE) %>%
  as_tibble()







## Vote status ----




#previous code did not account for voters who change from rejected to accepted.
Votestatus_council_Capture_Changes <- sqlExecute(EMA_connection,    
                                                 "SELECT
                                 
                                                  DEV.issuingdistareacode,
                                                  VSH.votestatusinfo,
                                                  VSH.electorid,
                                                  VSH.votestatustypecode,
                                                  VSH.votestatusrecorddate
                                                  FROM
                                                  prd2008.es_votestatushistory VSH 
                                                  LEFT JOIN prd2008.es_decexcusevote DEV
                                                  ON VSH.ELECTORID = DEV.ELECTORID
                                                  AND VSH.DECLARATIONEXCUSEVOTETYPECODE = DEV.DECLARATIONEXCUSEVOTETYPECODE
                                                  
                                                  WHERE VSH.electioneventid = ?
                                                  AND DEV.electioneventid = ?
                                                  AND VSH.declarationexcusevotetypecode = 'Postal'",
                                                 
                                                 as.list(rep(event_group_ID, 2)),
                                                 TRUE,
                                                 stringsAsFactors = FALSE) %>%
  as_tibble()
                                                 




## Council and Ward ----

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


#create a version of contests_council which also includes an entry for the whole council area in an undivided case.

if (nrow(Contests_council) > 0) {

Contests_council_including_undivided <- Contests_council %>%
  dplyr::select(AREACODE) %>%
  distinct() %>%
  mutate(CONTESTAREACODE=AREACODE) %>%
  add_row(Contests_council %>% 
            dplyr::select(AREACODE,CONTESTAREACODE)) %>%
  distinct()

} else {
  Contests_council_including_undivided <- Contests_council
}

#list of reject scrutiny and application reasons.

Reject_Reasons <- sqlExecute(EMA_connection,    
                               "SELECT    electioneventid,
    typecode,
    valueentered,
    groupcode,
    description,
    sortorder
FROM
    prd2008.ec_appconfiguration
    where electioneventid = ?
    and typecode like 'PV%'",
                               event_group_ID,
                               fetch            = TRUE,
                               stringsAsFactors = FALSE) %>%
  as_tibble()




## Close database connections
# odbcCloseAll()




#################################################### TBC ###########################################################