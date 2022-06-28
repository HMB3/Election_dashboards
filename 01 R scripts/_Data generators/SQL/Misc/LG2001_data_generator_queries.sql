---- ### LG2001 Dashboard SQL Queries ####
---- Record all the queries needed for all the LG2001 dashboars here
---- Queries will change between elections, but having a record of them is good
---- USe ORAPROD, LG1701


---- ### EARLY VOTING AND DECLARATION VOTING DASHBOARD QUERIES ###


--- * Pull mark-off data from EMA 
----  WHERE A.ELECTIONEVENTID = 'SG1901'
----  Not all columns are needed, clean out the extraneous
SELECT

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
                                  AND A.ELECTIONEVENTID      = B.ELECTIONEVENTID
                                  AND A.VOTINGCENTRETYPECODE = B.VOTINGCENTRETYPECODE
    
    ----WHERE A.ELECTIONEVENTID = 'SG1901'
                                  AND A.VOTINGCENTRETYPECODE          = 'Pre-Poll'
                                  AND A.DECLARATIONEXCUSEVOTETYPECODE = 'Pre-poll Ordinary'
                                  
                                  
--- What are the unique election event IDs in each table?
SELECT DISTINCT ELECTIONEVENTID 
FROM prd2008.es_decexcusevote
ORDER BY ELECTIONEVENTID

    
--- * Pull candidate data from EMA 
---   where can.ELECTIONEVENTID = 'SG1901'
SELECT can.ELECTIONEVENTID,
                         can.AREACODE,
                         can.CONTESTID,
                         can.CANDIDATEBALLOTNAME,
                         pt.PARTYNAME,
                         can.GROUPNUMBER,
                         grp.grouplabel,
                         can.DRAWNUMBER
                         
                         --- Lots of stuff not relevant for LG elections
                         FROM PRD2008.ES_CANDIDATE can
                         left join PRD2008.PM_PARTY pt  on can.ENDORSEDBYPARTYID = pt.PARTYID
                         left JOIN PRD2008.ES_GROUP grp on can.GROUPNUMBER = grp.GROUPNUMBER -- or (can.GROUPNUMBER is null)
                         where can.ELECTIONEVENTID = 'SG1701'
                         and grp.ELECTIONEVENTID = 'SG1701'
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
                         where can.ELECTIONEVENTID = 'SG1701'
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
                         where can.ELECTIONEVENTID = 'SG1701'
                         and can.validatedbyHO is not null
                         and can.AREACODE = 'NSW'
                         and can.GROUPNUMBER is null;
                         
                         
--- * Pull postal vote application data from EMA 
---  electioneventid
SELECT
    electioneventid,
    electorid,
    declarationexcusevotetypecode,
    applicationnumber,
    partyid,
    country,
    datereceived,
    accepted,
    addressline1,
    addressline2,
    suburb,
    state,
    postcode,
    posttoenrolledaddress,
    ucn,
    imagematching,
    acceptedby,
    typecode,
    daytimephone,
    mobilenumber,
    email,
    secretquestion,
    secretanswer,
    addressline3,
    acceptedreason,
    rejected,
    rejectedreason,
    rejectedby,
    lastcontact,
    lastcontactstatus,
    lastcontactedby
    
FROM
    prd2008.es_postalvoteapplication;
    ----where electioneventid = 'SG1701';


       
--- * Pull postal vote data from EMA    
SELECT
    electioneventid,
    declarationexcusevotetypecode,
    electorid,
    applicationnumber,
    processeddate,
    postalvotereturneddate,
    certificatedateprinted,
    certificateaccepted,
    ucn,
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
FROM
    prd2008.es_postalvote;
    
    
---- Pull Account of Ballot Paper     
--- * This will be from hyperion staging (Election Data)
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

    
---- ### NOMINATIONS DASH QUERIES ###


---- * Pull the list of Representative Political Parties (RPPs) from EMA to compare with the
----   Table provide by Nominations
----   The RPP data is not in EMA yet...
SELECT
    partyid,
    partyname,
    partyabbreviatedname,
    streetaddressid,
    postaladdressid,
    deliveryaddressid,
    dateregistered,
    dateregisteredacksent,
    datederegistered,
    partytype,
    seofilenumber,
    emailaddress,
    website,
    efaregistered,
    results_abbrev_party_name,
    ucn,
    partystatuscode,
    updated,
    updated_by,
    party_short_name,
    updateexpire
FROM
    prd2008.pm_party;


                         