--- * Pull mark-off data from EMA 
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
                                  AND A.ELECTIONEVENTID = B.ELECTIONEVENTID
                                  AND A.VOTINGCENTRETYPECODE = B.VOTINGCENTRETYPECODE
    
    WHERE A.ELECTIONEVENTID = 'SG1901'
                                  AND A.VOTINGCENTRETYPECODE = 'Pre-Poll'
                                  AND A.DECLARATIONEXCUSEVOTETYPECODE = 'Pre-poll Ordinary'

    

    
--- * Pull candidate data from EMA     
SELECT can.ELECTIONEVENTID,
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
                         where can.ELECTIONEVENTID = 'SG901'
                         and grp.ELECTIONEVENTID = 'SG901'
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
                         where can.ELECTIONEVENTID = 'SG901'
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
                         where can.ELECTIONEVENTID = 'SG1901'
                         and can.validatedbyHO is not null
                         and can.AREACODE = 'NSW'
                         and can.GROUPNUMBER is null
                         
--- * Pull candidate data from EMA                          
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
                         