SELECT  
A.ISSUINGDISTAREACODE,
            B.VOTINGCENTRETYPECODE,
            B.GAZETTEDNAME,
            A.DECLARATIONEXCUSEVOTETYPECODE AS DEC_VOTE_TYPE,
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
                                
                                
                                --- Change to DECLARATIONEXCUSEVOTETYPECODE
                                WHERE A.ELECTIONEVENTID = 'LG1701'
                                  AND B.ELECTIONEVENTID = 'LG1701'
                                  AND (A.DECLARATIONEXCUSEVOTETYPECODE = 'NAMAV' OR A.DECLARATIONEXCUSEVOTETYPECODE = 'Enrolment')
                                  AND B.VOTINGCENTRETYPECODE = 'Pre-Poll'
                                
                                GROUP BY A.ISSUINGDISTAREACODE, B.GAZETTEDNAME, B.VOTINGCENTRETYPECODE, A.DECLARATIONEXCUSEVOTETYPECODE
                                
                                ORDER BY A.ISSUINGDISTAREACODE, B.GAZETTEDNAME;
                                
                                
                                