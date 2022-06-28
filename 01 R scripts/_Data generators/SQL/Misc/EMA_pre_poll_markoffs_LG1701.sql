WITH

-- Extract contested election details
contests AS (

                                    SELECT distinct areacode,
                                    		contestareacode,
                                    		CASE
                                    			WHEN CONTESTTYPECODE = 'By LGA'
                                    			  THEN 'Councillor'
                                    			WHEN CONTESTTYPECODE = 'General LGA'
                                    			  THEN 'Councillor'
                                    			WHEN CONTESTTYPECODE = 'By LGA Referendum'
                                    			  THEN 'Referendum'
                                    			WHEN CONTESTTYPECODE = 'General LGA Ref'
                                    			  THEN 'Referendum'
                                    			WHEN CONTESTTYPECODE = 'By LGA Poll'
                                    			  THEN 'Poll'
                                    			WHEN CONTESTTYPECODE = 'General LGA Poll'
                                    			  THEN 'Poll'
                                    			WHEN CONTESTTYPECODE = 'By Ward'
                                    			  THEN 'Councillor'
                                    			WHEN CONTESTTYPECODE = 'General Ward'
                                    			  THEN 'Councillor'
                                    			WHEN CONTESTTYPECODE = 'General Mayoralty'
                                    			  THEN 'Mayor'
                                    			WHEN CONTESTTYPECODE = 'By Mayoralty'
                                    			  THEN 'Mayor'
                                    			ELSE CONTESTTYPECODE
                                    		 END AS contesttypecode

                                    FROM PRD2008.ES_CONTEST
                                    WHERE  ELECTIONEVENTID = 'LG1701'
                                    AND CONTESTSTATUSTYPECODE = 'Contested'
                                    ),

                                    -- Extract councillor contests
                                    contests_non_councillor AS (

                                    SELECT areacode,
                                    		contestareacode,
                                    		contesttypecode
                                    FROM contests
                                    WHERE contesttypecode != 'Councillor'
                                    ),

                                    -- Extract non-councillor contests
                                    contests_councillor AS (

                                    SELECT areacode,
                                    		contestareacode,
                                    		contesttypecode
                                    FROM contests
                                    WHERE contesttypecode = 'Councillor'
                                    ),

                                    -- Apply councillor contest areas to all 
                                    -- non-councillor contest types for relevent areas
                                    contests_mixed AS (

                                    SELECT A.areacode,
                                    		B.contestareacode,
                                    		A.contesttypecode
                                    FROM contests_non_councillor A

                                    LEFT JOIN contests_councillor B
                                    ON a.areacode = b.areacode
                                    ),

                                    -- Extract venue names
                                    venuenames AS (

                                    SELECT GAZETTEDNAME,
                                            LOCATIONID

                                    FROM PRD2008.ES_LOCATIONS
                                    WHERE VOTINGCENTRETYPECODE <> 'RO'
                                    AND ELECTIONEVENTID = 'LG1701'
                                    ),

                                    -- Extract pre-poll markoff counts by day
                                    markoffs AS (

                                    SELECT  ISSUINGDISTAREACODE,
                                    		LOCATIONID,
                                            TO_CHAR(VOTEPROCESSEDDATE, 'YYYY-MM-DD') AS VOTEPROCESSEDDATE,
                                            COUNT(1) AS PRE_POLL_MARKOFFS

                                    FROM PRD2008.ES_DECEXCUSEVOTE

                                    WHERE ELECTIONEVENTID = 'LG1701'
                                      AND DECLARATIONEXCUSEVOTETYPECODE = 'Pre-poll Ordinary'

                                    GROUP BY ISSUINGDISTAREACODE, LOCATIONID, TO_CHAR(VOTEPROCESSEDDATE, 'YYYY-MM-DD')
                                    ),

                                    -- Add contest types and aggregate markoffs to IssuingArea for non-councillor contests
                                    markoffs_non_councillor AS (

                                    SELECT B.AREACODE AS ISSUINGAREA,
                                            B.AREACODE AS CONTESTAREA,
                                            B.CONTESTTYPECODE,
                                            A.LOCATIONID,
                                            A.VOTEPROCESSEDDATE,
                                            SUM(A.PRE_POLL_MARKOFFS) AS PRE_POLL_MARKOFFS

                                    FROM markoffs A

                                    LEFT JOIN contests_mixed B
                                        ON A.ISSUINGDISTAREACODE = B.CONTESTAREACODE

                                    GROUP BY B.AREACODE,
                                            B.AREACODE,
                                            B.CONTESTTYPECODE,
                                            A.LOCATIONID,
                                            A.VOTEPROCESSEDDATE
                                    ),

                                    -- Add contest types for councillor contests
                                    markoffs_councillor AS (

                                    SELECT B.AREACODE AS ISSUINGAREA,
                                            A.ISSUINGDISTAREACODE AS CONTESTAREA,
                                            B.CONTESTTYPECODE,
                                            A.LOCATIONID,
                                            VOTEPROCESSEDDATE,
                                            PRE_POLL_MARKOFFS

                                    FROM markoffs A

                                    LEFT JOIN contests_councillor B
                                        ON A.ISSUINGDISTAREACODE = B.contestareacode
                                    ),

                                    -- Combine councillor and non-councillor markoffs
                                    markoffs_all AS (

                                    SELECT * FROM markoffs_non_councillor
                                    UNION
                                    SELECT * FROM markoffs_councillor
                                    )

                                    -- Join venue names and select results

                                    SELECT A.ISSUINGAREA,
                                            A.CONTESTAREA,
                                    		B.GAZETTEDNAME AS VENUENAME,
                                            A.CONTESTTYPECODE,
                                            VOTEPROCESSEDDATE,
                                            PRE_POLL_MARKOFFS

                                    FROM markoffs_all A
                                    LEFT JOIN venuenames B
                                    ON A.LOCATIONID = B.LOCATIONID

                                    -----WHERE  B.GAZETTEDNAME != 'Sydney Town Hall'

                                    ORDER BY A.ISSUINGAREA,
                                    A.CONTESTAREA,
                                    		B.GAZETTEDNAME,
                                            A.CONTESTTYPECODE,
                                            A.VOTEPROCESSEDDATE;
                                      
                                      