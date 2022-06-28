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
    prd2008.es_postalvoteapplication
    WHERE electioneventid = ('LG2002');
        
    
    
    
    
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
    prd2008.es_postalvote
    WHERE electioneventid = ('LG2002');
    
    

SELECT
    electioneventid,
    electorid,
    declarationexcusevotetypecode,
    votestatuscode,
    voteprocesseddate,
    issuingdistareacode,
    votingareacode
FROM
    prd2008.es_decexcusevote
    WHERE electioneventid = ('LG2002')
    AND declarationexcusevotetypecode = 'Postal';

    
SELECT
    electioneventid,
    electorid,
    votestatustypecode,
    votestatusrecorddate,
    votestatusinfo,
    ucn,
    votestatusrecordby,
    declarationexcusevotetypecode,
    applicationnumber
FROM
    prd2008.es_votestatushistory
    WHERE electioneventid = ('LG2002');
    
SELECT
DISTINCT votestatusinfo
FROM    prd2008.es_votestatushistory
    
WHERE electioneventid = 'LG2002';
    
    
    