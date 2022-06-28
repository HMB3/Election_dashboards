--- What are the unique election event IDs in each table?
SELECT DISTINCT ELECTIONEVENTID 
FROM prd2008.es_decexcusevote
ORDER BY ELECTIONEVENTID ;




-- ACCEPTED DEC VOTE MARKOFF COUNTS FROM ES_DECEXCUSEVOTE
-- Has the vote been issued?

SELECT  A.ISSUINGDISTAREACODE,
		B.GAZETTEDNAME,                                   ---  venue name
		A.DECLARATIONEXCUSEVOTETYPECODE AS DEC_VOTE_TYPE, ---- Type: Absent, etc.
		COUNT(CASE A.VOTESTATUSCODE WHEN 'Accepted' THEN 1 ELSE NULL END) AS ACCEPTED_DEC_VOTE_MARKOFFS, -- Count rows
		COUNT(CASE A.VOTESTATUSCODE WHEN 'Rejected' THEN 1 ELSE NULL END) AS REJECTED_DEC_VOTE_MARKOFFS  -- Count rows

--- For every venue name, count the number of rejected and accepted dec votes
--- Multiple rows relevant, WHERE clauses are trying to make the data atomic.
--- Modify with caution
FROM PRD2008.ES_DECEXCUSEVOTE A
LEFT JOIN (SELECT GAZETTEDNAME,
				  LOCATIONID,
				  ELECTIONEVENTID
			FROM PRD2008.ES_LOCATIONS
			WHERE VOTINGCENTRETYPECODE <> 'RO') B
  ON A.LOCATIONID = B.LOCATIONID

--- Getting rid of things
WHERE A.ELECTIONEVENTID = 'LG1701'
  AND B.ELECTIONEVENTID = 'LG1701'
  AND (A.DECLARATIONEXCUSEVOTETYPECODE <> 'Pre-poll Ordinary'
  AND A.DECLARATIONEXCUSEVOTETYPECODE <> 'DI Ordinary'
  AND A.DECLARATIONEXCUSEVOTETYPECODE <> 'Ordinary')

GROUP BY A.ISSUINGDISTAREACODE, B.GAZETTEDNAME, A.DECLARATIONEXCUSEVOTETYPECODE

ORDER BY A.ISSUINGDISTAREACODE, B.GAZETTEDNAME


SELECT
    electioneventid,
    electorid,
    declarationexcusevotetypecode,
    applicationnumber,
    votestatuscode,
    voteprocesseddate,
    livesinelectorate,
    livedinelectoraterecently,
    votersmarkwitnessed,
    ucn,
    voteprocessedby,
    locationid,
    votingcentretypecode,
    locationflagid,
    scrutinizedby,
    issuingdistredistributioncode,
    issuingdistareacode,
    electorelectionareaflag,
    electorelectionwardflag,
    count_number,
    votingareacode
    
FROM
    prd2008.es_decexcusevote
    
WHERE electioneventid = 'LG1701'


