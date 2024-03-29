################################# ----- COMMON DATA GENERATOR  ---- #####################################

# EMS missing etl
# ema ord missing candidate table

## This script extracts the common data needed to run all the dashboard code



## Contest_details ----
Contest_details <- sqlExecute(hyperion_connection,"
                       with con as (
                        SELECT 
                        					   ContestID,
                                               AreaCode,
                                               ContestTypeCode,
                                               ContestStatusTypeCode
                                               FROM [DV_Staging_EMS].Events.EMS_Contest
                                               WHERE ContestID LIKE ?
                                             --  AND ContestStatusTypeCode = 'Contested'
                        			),
                        area as (SELECT [RedistCode]
                              ,[AreaCode]
                        	  ,isnull(ParentAreaCode,AreaCode) as LGArea
                              ,[EstimatedElectors]
                              ,[AreaInSquareKilometers]
                          FROM [DV_Staging_EMS].[dbo].[area]
                          where RedistCode = 'LG2019'
                        			) 
                        
                        select  con.ContestID,
                                               con.AreaCode,
                                               con.ContestTypeCode,
                                               con.ContestStatusTypeCode
                        	  ,area.LGArea
                              ,area.[EstimatedElectors]
                              ,area.[AreaInSquareKilometers]
                        
                        from con left join area on con.areacode = area.AreaCode",
                              
                              paste0(event_group_ID,'%'),
                              fetch = TRUE,
                              stringsAsFactors = FALSE)



Contest_raw <-  sqlExecute(hyperion_connection,
                       "
                    SELECT
                          contestid,
                          areacode,
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
                       END AS CONTESTTYPECODE,
                          conteststatustypecode,
                          numberofpositionscontested,
                          questionlabel,
                          unformattedquestion
                        
                      FROM
                          [DV_Staging_EMA].prd2008.es_contest
                          where electioneventid = ?
                        
                        ",
                       event_group_ID,
                       fetch=TRUE,
                       stringsAsFactors = FALSE)

names(Contest_raw) <- toupper(names(Contest_raw))

## Potential_Venues ----
Potential_Venues_raw <- sqlExecute(hyperion_connection,
                                   "/****** Script for SelectTopNRows command from SSMS  ******/
SELECT [EventID]
      ,evl.[VenueName]
      ,[LocationTypeCode]
      ,[LocationStatusCode]
	  ,loc.StreetAddressID
	  ,addr.Latitude
	  ,addr.Longitude
	  ,addr.AddressName
	  ,addr.AddressLine1
	  ,addr.AddressLine2
	  ,addr.LocalityName
	  ,addr.PostCode
	  ,[NumberOfRooms]
      ,[NumberOfHalls]
      ,[NumberOfOther]
     
  FROM DV_Staging_EMS.[Events].[EMS_EventLocation] evl
  left join DV_Staging_EMS.[Resources].[EMS_Location] loc on loc.VenueName = evl.VenueName
  left join DV_Staging_EMS.[Resources].[EMS_Address] addr on loc.StreetAddressID = addr.AddressID

where [EventID] like ?
 AND evl.LocationStatusCode NOT IN ('Away Cancelled',
                                                                    'Cancellation Notified',
                                                                    'Cancelled',
                                                                    'Deferred',
                                                                    'Hire Agreement Rejected',
                                                                    'Initial',
                                                                    'Managed by 3rd party',
                                                                    'Print Cancelled letter',
                                                                    'Print Deferred letter',
                                                                    'Processed',
                                                                    'Unacceptable',
                                                                    'Unavailable',
                                                                    'Unavailable Indefinitely',
                                                                    'Uncontested',
                                                                    'Visit Not Required')",
                                   paste0(event_group_ID,'%'),
                                   fetch = TRUE,
                                   stringsAsFactors = FALSE)





EMA_ord <- sqlExecute(hyperion_connection,
                      "
                     with group_results as (                   
                      SELECT
                          gr.contestid,
    gr.areacode,
    gr.votingcentretypecode,
    gr.locationid,
    loc.gazettedname,
    gr.count_number,
    sum(gr.en_ratl) as firstcount
    ,'gr' as source
FROM
    [DV_Staging_EMA].prd2008.es_groupvcresults gr
left join [DV_Staging_EMA].prd2008.es_locations loc on gr.locationid = loc.locationid and gr.votingcentretypecode = loc.votingcentretypecode
full join [DV_Staging_EMA].prd2008.es_group g on gr.contestid = g.contestid and gr.groupnumber = g.groupnumber
    where gr.electioneventid like ?
    and loc.electioneventid = gr.electioneventid
 group by  gr.contestid,
    gr.areacode,
    gr.votingcentretypecode,
    gr.locationid,
    loc.gazettedname,
            gr.count_number

),

cand_results as (
                        SELECT
                            cr.contestid,
                            cr.areacode,
                            cr.votingcentretypecode,
                            cr.locationid,
                            loc.gazettedname,
                            cr.count_number,
                            sum(cr.firstpreferencevotecount) as firstcount
                            ,'cr' as source
                           
                        FROM
                            [DV_Staging_EMA].prd2008.es_candidateresults cr
                            left join [DV_Staging_EMA].prd2008.es_locations loc on cr.locationid = loc.locationid and cr.votingcentretypecode = loc.votingcentretypecode
                            left join [DV_Staging_EMA].prd2008.es_candidate cand on cand.contestid = cr.contestid and cand.candidateballotname = cr.candidateballotname
                            full join [DV_Staging_EMA].prd2008.es_group g on cand.contestid = g.contestid and cand.groupnumber = g.groupnumber
                            
                            where cr.electioneventid like ?
                            and loc.electioneventid = cr.electioneventid
                            
                            group by  cr.contestid,
                            cr.areacode,
                            cr.votingcentretypecode,
                            cr.locationid,
                            loc.gazettedname,
                            cr.count_number
                ),
informal as (
                    SELECT
                          vcp.contestid,
                          vcp.areacode,
                          vcp.votingcentretypecode,
                          vcp.locationid,
                          loc.gazettedname,
                          vcp.count_number,
                          case when sum(vcp.informalsvotecount) is null then sum(vcp.othervotes)
                               when sum(vcp.othervotes) is null then sum(vcp.informalsvotecount)
                               end as firstcount
                       --   sum(vcp.informalsvotecount) as informal,
                       --   sum(vcp.othervotes) as other
                         ,'informal' as source

                      FROM
                          [DV_Staging_EMA].prd2008.es_votecountingpoint vcp
                          left join [DV_Staging_EMA].prd2008.es_locations loc on vcp.locationid = loc.locationid and vcp.votingcentretypecode = loc.votingcentretypecode
                          where vcp.electioneventid like ?
                          and loc.electioneventid = vcp.electioneventid
                          
                          group by  vcp.contestid,
                          vcp.areacode,
                          vcp.votingcentretypecode,
                          vcp.locationid,
                          loc.gazettedname,
                          vcp.count_number
)

select contestid,
                          areacode,
                          votingcentretypecode,
                          locationid,
                          gazettedname,
                          count_number,
                          sum(firstcount) as InitialCount
                  --        firstcount,
                  --        source
                          
 from (
 
select * from group_results
union 
select * from cand_results
union
select * from informal
) tot


 group by contestid,
 areacode,
 votingcentretypecode,
 locationid,
 gazettedname,
 count_number

                          
                        ",
as.list(rep(paste0(event_group_ID,'%'),3)),
fetch=TRUE,
stringsAsFactors = FALSE) 


EMA_dec <- sqlExecute(hyperion_connection,
                      "
                 with contestloc as (
                   SELECT
                  contestid,
                  areacode,
                  declarationexcusevotetypecode,
                  sum(informalsvotecount) as informalsvotecount,
               --   accepteddecenvelopes,
                  locationid,
                  count_number

              FROM
                  [DV_Staging_EMA].prd2008.es_decvotesbycontest
            where electioneventid like ?

                  group by  contestid,
                  areacode,
                  declarationexcusevotetypecode,
                  locationid,
                  count_number
              ),
              
              candidate as (
              SELECT
              
                  contestid,
                  areacode,
                  declarationexcusevotetypecode,
                  sum(ISNULL(ratl,0) + ISNULL(satl,0)) as RATL,
                  locationid,
                  count_number
              FROM
                       [DV_Staging_EMA].prd2008.es_decvoteresultsgroup
                  where electioneventid like ?
                  group by contestid,
                  areacode,
                  declarationexcusevotetypecode,
                  locationid,
                  count_number
              ),
              
              groupbulk as (
                   SELECT
                  contestid,
                  areacode,
                  declarationexcusevotetypecode,
                  sum(firstpreferencevotecount) as firstpreferencevotecount,
                  locationid,
                  count_number
              FROM
                  [DV_Staging_EMA].prd2008.es_declarationvotesresults
                  where electioneventid like ?
              group by  contestid,
                  areacode,
                  declarationexcusevotetypecode,
                  locationid,
                  count_number
              )
              
              select  
                  contestloc.contestid,
                  contestloc.areacode,
                  contestloc.declarationexcusevotetypecode as votingcentretypecode,
                  case when contestloc.locationid = 'n/a' then contestloc.declarationexcusevotetypecode
                  else contestloc.locationid
                  end as locationid,
                  ISNULL(loc.gazettedname,contestloc.declarationexcusevotetypecode) as gazettedname,
                  contestloc.count_number,

              --    contestloc.informalsvotecount,
              --    contestloc.accepteddecenvelopes,
              --    candidate.ratl,
              --    groupbulk.firstpreferencevotecount,
          --        ISNULL(contestloc.informalsvotecount,0) + ISNULL(candidate.ratl,0) + ISNULL(groupbulk.firstpreferencevotecount,0) as initialcount
          --        contestloc.informalsvotecount + candidate.ratl + groupbulk.firstpreferencevotecount  as initialcount
              case when contestloc.informalsvotecount is null and candidate.ratl is null and groupbulk.firstpreferencevotecount is null then null
                   else ISNULL(contestloc.informalsvotecount,0) + ISNULL(candidate.ratl,0) + ISNULL(groupbulk.firstpreferencevotecount,0) 
                   end as initialcount

              
              
              from contestloc 
              left join candidate on contestloc.contestid = candidate.contestid and contestloc.areacode = candidate.areacode and contestloc.declarationexcusevotetypecode = candidate.declarationexcusevotetypecode
                                      and contestloc.locationid = candidate.locationid and contestloc.count_number = candidate.count_number
              left join groupbulk on contestloc.contestid = groupbulk.contestid and contestloc.areacode = groupbulk.areacode and contestloc.declarationexcusevotetypecode = groupbulk.declarationexcusevotetypecode
                                      and contestloc.locationid = groupbulk.locationid and contestloc.count_number = groupbulk.count_number
              left join DV_Staging_EMA.prd2008.es_locations loc on contestloc.locationid = loc.locationid and loc.electioneventid like ? and loc.votingcentretypecode = 'Pre-Poll' 
              
              order by contestloc.contestid, contestloc.declarationexcusevotetypecode, contestloc.count_number
                           ",
                      as.list(rep(paste0(event_group_ID,'%'),4)),
                      fetch=TRUE,
                      stringsAsFactors = FALSE) 



EMA_postal_raw <- sqlExecute(hyperion_connection,
                             "
                                      SELECT
    electioneventid,
    areacode,
    wardcode,
    contestid,
    count,
    accepted

FROM
    DV_Staging_EMA.prd2008.es_postalvoteextraction
    where electioneventid = ?
    order by contestid, count
    ;
                          
                        ",
                             event_group_ID,
                             fetch=TRUE,
                             stringsAsFactors = FALSE) 

# EMA querrie is too long therefore split into 3


EMA_ord_expected_raw <- sqlExecute(hyperion_connection,
                               "
                      SELECT
   
vc.contestid,
    vc.areacode,
    vc.votingcentretypecode,
    vc.locationid,
    loc.GAZETTEDNAME,
    vc.expected_ballots,
    vc.accounted_ballots_en
 FROM
    DV_Staging_EMA.prd2008.es_votecountingpoint vc
        
    LEFT JOIN (SELECT GAZETTEDNAME,
                    LOCATIONID,
                    ELECTIONEVENTID
             FROM DV_Staging_EMA.PRD2008.ES_LOCATIONS
             WHERE VOTINGCENTRETYPECODE = 'PP') loc
    ON vc.LOCATIONID = loc.LOCATIONID
    and vc.electioneventid = loc.electioneventid
    
    where vc.electioneventid = ?
                        ",
                               event_group_ID,
                               fetch=TRUE,
                               stringsAsFactors = FALSE) 


EMA_dec_markoff <- sqlExecute(hyperion_connection,
                              "
                  
with prepoll as(
SELECT  A.ISSUINGDISTAREACODE,
a.votingcentretypecode,
        CASE
          WHEN B.GAZETTEDNAME  = 'Sydney Town Hall'
            THEN 'Sydney Town Hall Pre-Poll'
          ELSE B.GAZETTEDNAME
        END AS VENUE_VOTE_TYPE,
        a.locationid
        ,COUNT(DISTINCT A.ELECTORID) AS DEC_VOTE_MARKOFFS

FROM DV_Staging_EMA.PRD2008.ES_DECEXCUSEVOTE A
  LEFT JOIN (SELECT GAZETTEDNAME,
                    LOCATIONID,
                    ELECTIONEVENTID
             FROM DV_Staging_EMA.PRD2008.ES_LOCATIONS
             WHERE VOTINGCENTRETYPECODE <> 'RO') B
    ON A.LOCATIONID = B.LOCATIONID and A.ELECTIONEVENTID = B.ELECTIONEVENTID


WHERE A.ELECTIONEVENTID like ?
  AND A.VOTESTATUSCODE in ('Accepted')
  AND (A.DECLARATIONEXCUSEVOTETYPECODE = 'Pre-poll Ordinary')
GROUP BY A.ISSUINGDISTAREACODE, B.GAZETTEDNAME, A.votingcentretypecode, A.locationid
),

decv as (
SELECT  A.ISSUINGDISTAREACODE,
a.DECLARATIONEXCUSEVOTETYPECODE,

        A.DECLARATIONEXCUSEVOTETYPECODE AS VENUE_VOTE_TYPE,
        A.locationid
        ,COUNT(DISTINCT A.ELECTORID) AS DEC_VOTE_MARKOFFS

FROM DV_Staging_EMA.PRD2008.ES_DECEXCUSEVOTE A
  LEFT JOIN (SELECT GAZETTEDNAME,
                    LOCATIONID,
                    ELECTIONEVENTID
             FROM DV_Staging_EMA.PRD2008.ES_LOCATIONS
             WHERE VOTINGCENTRETYPECODE <> 'RO') B
    ON A.LOCATIONID = B.LOCATIONID and A.ELECTIONEVENTID = B.ELECTIONEVENTID


WHERE A.ELECTIONEVENTID like ?
  AND A.VOTESTATUSCODE in ('Accepted')
  AND A.DECLARATIONEXCUSEVOTETYPECODE not in ('Pre-poll Ordinary', 'Postal')
 -- EXCLUDE THE ONLY ORDINARY VOTE MARKOFFS WHICH ARE FOR SYDNEY TOWN HALL 



GROUP BY A.ISSUINGDISTAREACODE, A.DECLARATIONEXCUSEVOTETYPECODE,A.locationid

)

select * from prepoll
union 
select * from decv    
    
    
    
                        ",
as.list(rep(paste0(event_group_ID,'%'),2)),
fetch=TRUE,
stringsAsFactors = FALSE) 


names(EMA_ord) <- toupper(names(EMA_ord))
names(EMA_dec) <- toupper(names(EMA_dec))
names(EMA_postal_raw) <- toupper(names(EMA_postal_raw))
names(EMA_ord_expected_raw) <- toupper(names(EMA_ord_expected_raw))
names(EMA_dec_markoff) <- toupper(names(EMA_dec_markoff))



DV_load_time_raw <- sqlExecute(hyperion_connection,
                           "
                      SELECT 
      max([LoadDate])
  FROM [DV_Staging_EMA].[PRD2008].[ES_LOCATIONS]
  where electioneventid = ?
                        ",
                           event_group_ID,
                           fetch=TRUE,
                           stringsAsFactors = FALSE)[1,1]

DV_load_time_raw <- as.POSIXct(DV_load_time_raw)

# DV_load_time <- substr(DV_load_time_raw,12,19)
# DV_load_time_nx <- substr(DV_load_time_raw + 1800,12,19)

## End ------------------------------------------------------------------------------

