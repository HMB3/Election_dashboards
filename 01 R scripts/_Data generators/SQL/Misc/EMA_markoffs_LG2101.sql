SELECT  
                                      A.ISSUINGDISTAREACODE,
                                      B.VOTINGCENTRETYPECODE,
                                      B.GAZETTEDNAME,
                                      A.DECLARATIONEXCUSEVOTETYPECODE AS DEC_VOTE_TYPE,
                                      TO_CHAR(A.voteprocesseddate, 'YYYY-MM-DD') AS VOTEPROCESSDATE,
                                      COUNT(1) AS TOTAL_MARKOFFS, 
                                      
                                      COUNT(CASE B.VOTINGCENTRETYPECODE WHEN 'Pre-Poll' THEN 1 ELSE NULL END) AS PROVISIONAL_MARKOFFS,
                                      COUNT(CASE A.VOTESTATUSCODE WHEN 'Accepted' THEN 1 ELSE NULL END) AS ACCEPTED_MARKOFFS,
                                      COUNT(CASE A.VOTESTATUSCODE WHEN 'Rejected' THEN 1 ELSE NULL END) AS REJECTED_MARKOFFS
                                      
                                      FROM PRD2008.ES_DECEXCUSEVOTE A
                                      LEFT JOIN (SELECT GAZETTEDNAME,
                                      LOCATIONID,
                                      VOTINGCENTRETYPECODE,
                                      ELECTIONEVENTID
                                      FROM PRD2008.ES_LOCATIONS
                                      WHERE VOTINGCENTRETYPECODE <> 'RO') B
                                      ON A.LOCATIONID = B.LOCATIONID
                                      AND A.VOTINGCENTRETYPECODE = B.VOTINGCENTRETYPECODE
                                      WHERE A.ELECTIONEVENTID = 'LG2002'
                                      AND B.ELECTIONEVENTID = 'LG2002'
                                      AND (A.DECLARATIONEXCUSEVOTETYPECODE = 'NAMAV' OR A.DECLARATIONEXCUSEVOTETYPECODE = 'Enrolment')
                                      GROUP BY A.ISSUINGDISTAREACODE, B.GAZETTEDNAME, B.VOTINGCENTRETYPECODE, A.DECLARATIONEXCUSEVOTETYPECODE, TO_CHAR(A.voteprocesseddate, 'YYYY-MM-DD')
                                      ORDER BY A.ISSUINGDISTAREACODE, B.GAZETTEDNAME;
                                      
                                      
                                      
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

                      ----WHERE A.ELECTIONEVENTID = ?
                      WHERE A.VOTINGCENTRETYPECODE = 'Pre-Poll'
                      AND A.DECLARATIONEXCUSEVOTETYPECODE = 'Pre-poll Ordinary';
                      
                      
                      
SELECT DISTINCT ELECTIONEVENTID 
FROM prd2008.es_decexcusevote
ORDER BY ELECTIONEVENTID; 



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
WHERE ELECTIONEVENTID = 'LG2002';



                      