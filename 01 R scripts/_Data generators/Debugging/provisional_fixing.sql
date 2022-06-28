WITH
                            
                            -- Get list of provisional markoff elector ID's
                            provisional_IDs AS (
                            
                            SELECT  distinct a.electorid, a.declarationexcusevotetypecode
                            FROM PRD2008.es_votestatushistory A
                            WHERE A.ELECTIONEVENTID = 'LG2003'
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
                            WHERE A.ELECTIONEVENTID = 'LG2003'
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
                            WHERE A.ELECTIONEVENTID = 'LG2003'
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
                            ),
                            
                            final_table AS (
                            
                            
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
                                AND ELECTIONEVENTID = 'LG2003') B
                            ON A.LOCATIONID = B.LOCATIONID
                            AND A.VOTINGCENTRETYPECODE = B.VOTINGCENTRETYPECODE
                            
                            ORDER BY A.ISSUINGDISTAREACODE, A.VOTINGCENTRETYPECODE, 
                            B.GAZETTEDNAME, A.DECLARATIONEXCUSEVOTETYPECODE, A.VOTEPROCESSDATE),
                            
                            aggregated_table AS (
                            SELECT  
                                ISSUINGDISTAREACODE,
                                DEC_VOTE_TYPE,
                                SUM(PROVISIONAL_MARKOFFS) AS PROVISIONAL_MARKOFFS,
                                SUM(ACCEPTED_MARKOFFS + REJECTED_MARKOFFS) AS TotalScrutinised
                                
                                FROM final_table
                                WHERE DEC_VOTE_TYPE = 'Enrolment'
                                
                                GROUP BY ISSUINGDISTAREACODE,
                                DEC_VOTE_TYPE
                                
                                ORDER BY ISSUINGDISTAREACODE),
                                
                            totals AS (
                            
                            SELECT SUM(PROVISIONAL_MARKOFFS) AS PROVISIONAL_MARKOFFS,
                                SUM(TotalScrutinised)
                                
                                FROM aggregated_table)
                                
                            SELECT * FROM aggregated_table

                                