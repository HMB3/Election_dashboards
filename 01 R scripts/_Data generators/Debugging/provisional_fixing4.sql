select *
from PRD2008.ES_DECEXCUSEVOTE A
left JOIN (

SELECT  a.electorid, A.VOTESTATUSTYPECODE, a.votestatusrecorddate
FROM PRD2008.es_votestatushistory A
WHERE A.ELECTIONEVENTID = 'LG2003'
--AND a.votestatustypecode = 'Markoff Deleted'
and a.declarationexcusevotetypecode = 'Enrolment'

) B

ON A.ELECTORID = B.ELECTORID


WHERE A.ELECTIONEVENTID = 'LG2003'
AND A.DECLARATIONEXCUSEVOTETYPECODE = 'Enrolment'
AND A.ISSUINGDISTAREACODE = 'North Sydney Cammeraygal'
--AND A.ELECTORID = 'EI0000012'

ORDER BY A.ELECTORID, b.votestatusrecorddate
--ORDER BY B.VOTESTATUSTYPECODE
--ORDER BY A.ELECTORELECTIONAREAFLAG