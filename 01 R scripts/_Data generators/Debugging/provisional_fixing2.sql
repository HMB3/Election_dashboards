                            SELECT  *
                            FROM PRD2008.es_votestatushistory A
                            WHERE A.ELECTIONEVENTID = 'LG2003'
                            --AND a.votestatustypecode = 'Markoff'
                            and a.declarationexcusevotetypecode = 'Enrolment'
                            AND ELECTORID = 'EI0000026'
                            ORDER BY a.votestatusrecorddate