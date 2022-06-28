SELECT  
                            A.ISSUINGDISTAREACODE,
                            A.DECLARATIONEXCUSEVOTETYPECODE,
                            a.locationid,
                            a.VOTINGCENTRETYPECODE,
                            TO_CHAR(A.voteprocesseddate, 'YYYY-MM-DD') AS VOTEPROCESSDATE,                            
                            COUNT(1) AS PROVISIONAL_MARKOFFS
                            
                            FROM PRD2008.ES_DECEXCUSEVOTE A
                            INNER JOIN (
                            SELECT  distinct a.electorid, a.declarationexcusevotetypecode
                            FROM PRD2008.es_votestatushistory A
                            WHERE A.ELECTIONEVENTID = 'LG2003'
                            AND a.votestatustypecode = 'Markoff'
                            and a.declarationexcusevotetypecode = 'Enrolment'
                            ) B
                               ON A.ELECTORID = B.ELECTORID
                                    AND A.declarationexcusevotetypecode = B.declarationexcusevotetypecode
                            WHERE A.ELECTIONEVENTID = 'LG2003'
                                --AND A.VOTINGCENTRETYPECODE = 'Pre-Poll'
                                AND A.DECLARATIONEXCUSEVOTETYPECODE = 'Enrolment'
                                AND ISSUINGDISTAREACODE = 'North Sydney Cammeraygal'
                            GROUP BY A.ISSUINGDISTAREACODE, a.locationid, a.VOTINGCENTRETYPECODE, 
                            A.DECLARATIONEXCUSEVOTETYPECODE, TO_CHAR(A.voteprocesseddate, 'YYYY-MM-DD')