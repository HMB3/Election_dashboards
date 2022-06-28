
## 1). EMA first count ===================================================================


 # EMA_postal <- sqlExecute(EMA_connection,
 #                          "
 #                      SELECT
 #   --  electioneventid,
 #   --  areacode,
 #   --  wardcode,
 #     contestid,
 #     count,
 #     sum(accepted) as accepted
 # 
 # FROM
 #     prd2008.es_postalvoteextraction
 #     where electioneventid = ?
 #     group by contestid,
 #     count
 #     order by contestid, count
 #     ;
 #                           
 #                         ",
 #                          event_group_ID,
 #                          fetch=TRUE,
 #                          stringsAsFactors = FALSE) 





EMA_comments <- sqlExecute(hyperion_connection,"
                         with vc as (SELECT
    contestid,
    areacode,
    votingcentretypecode,
    locationid,
    commentdate,
    comments,
    commentsby,
    commenttype,
    null as count_number
FROM
    [DV_Staging_EMA].prd2008.es_votecountingpointcomments
        where electioneventid like ?
),

dv as (

SELECT
    contestid,
    areacode,
    decc.declarationexcusevotetypecode as  votingcentretypecode,
    case when loc.locationid is null then decc.declarationexcusevotetypecode
            else loc.locationid end as locationid, 
    commentdate,
    comments,
    commentsby,
    dvtype AS commenttype,
    count_number
FROM
    [DV_Staging_EMA].prd2008.es_decvotecomments decc
    full outer join (select * from [DV_Staging_EMA].prd2008.es_locations where votingcentretypecode != 'RO') loc
     on loc.electioneventid = decc.electioneventid and loc.gazettedname = decc.declarationexcusevotetypecode
   where decc.electioneventid like ?

)

SELECT * FROM VC
UNION
SELECT * FROM DV


                                 ",
as.list(rep(paste0(event_group_ID, '%'),2)),
fetch=TRUE,
                            stringsAsFactors = FALSE) 




EMA_allocated <- sqlExecute(hyperion_connection,
                               "
                      SELECT
   
vc.contestid,
    vc.areacode,
    vc.votingcentretypecode,
    vc.locationid,
    loc.GAZETTEDNAME,
    vc.ballotpaperissued as Allocated
    
 FROM
    [DV_Staging_EMA].prd2008.es_votecountingpoint vc
        
    LEFT JOIN (SELECT GAZETTEDNAME,
                    LOCATIONID,
                    ELECTIONEVENTID
             FROM [DV_Staging_EMA].PRD2008.ES_LOCATIONS
             WHERE VOTINGCENTRETYPECODE = 'PP') loc
    ON vc.LOCATIONID = loc.LOCATIONID
    and vc.electioneventid = loc.electioneventid
    
    where vc.electioneventid = ?
                        ",
                               event_group_ID,
                               fetch=TRUE,
                               stringsAsFactors = FALSE) 

####################################################################################################################

names(EMA_comments) <- toupper(names(EMA_comments))
names(EMA_allocated) <- toupper(names(EMA_allocated))






















