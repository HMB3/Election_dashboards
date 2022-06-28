####################################################################################################################
###################################### rmt DATA GENERATOR  ---- ###########################################
####################################################################################################################


library(RODBC)
library(RODBCext)
library(flextable)
library(flexdashboard)
library(kableExtra)
library(DT)
library(knitr)
library(formattable)
library(openxlsx)
library(tidyr)
library(dplyr)

options(scipen=999)

# test event LG2003 did not cover votecountingpoint table, using LG1701 instead

#event_group_ID= 'LG2101'

# 1. set parameters  ---------------------------------------------------------
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))


## Analytics database connections ----
setwd(database_connections)
source('DataVault.R')
source("EMA.R") # potentially load everything from hyperion later this is temp

if (from_staging) {
  

PRCC_connection <- odbcDriverConnect(paste0("driver={SQL Server};server=svrelecusql1;",
                                            "database=PRCC_",
                                            paste0(event_group_ID,'_UAT'),
                                            ";",
                                            "trusted_connection=true"),
                                     readOnlyOptimize = TRUE)

} else {
  
 PRCC_connection <- odbcDriverConnect(paste0("driver={SQL Server};server=svrelecpsql1;",
                                              "database=PRCC_",
                                              paste0(event_group_ID,'_PRT'),
                                              ";",
                                              "trusted_connection=true"),
                                       readOnlyOptimize = TRUE)
}


## 1). EMA first count ===================================================================
# EMA querrie is too long therefore split into 3
# ema first count queries have been moved to common data gen


if (FALSE) {

EMA_comments <- sqlExecute(EMA_connection,"
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
    prd2008.es_votecountingpointcomments
        where electioneventid = ?
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
    prd2008.es_decvotecomments decc
    full outer join (select * from prd2008.es_locations where votingcentretypecode != 'RO') loc
     on loc.electioneventid = decc.electioneventid and loc.gazettedname = decc.declarationexcusevotetypecode
   where decc.electioneventid = ?

)

SELECT * FROM VC
UNION
SELECT * FROM DV


                                 ",
as.list(rep(event_group_ID,2)),
fetch=TRUE,
stringsAsFactors = FALSE) 
 # ref pol no longer required for rmt
  

# Ref_poll second count missing dec

Ref_poll_checkC <-  sqlExecute(EMA_connection,"
                         with contestinf as (
                    SELECT
                          contest.contestid,
                          contest.areacode,
                          contest.contestareacode,
                          contest.CONTESTTYPECODE,
                          vcp.votingcentretypecode,
                          vcp.locationid,
                          vcp.informalscheckcount as CheckInformal
                          	                   
                      FROM
                          prd2008.es_contest contest
                          left join prd2008.es_votecountingpoint vcp on vcp.contestid = contest.contestid 
                          where contest.electioneventid = ?
                          and contest.CONTESTTYPECODE in ('By LGA Referendum','General LGA Ref','By LGA Poll','General LGA Poll')
                 ),

cand as (

SELECT
    contest.contestid,
    contest.areacode,
    contest.contestareacode,
    contest.CONTESTTYPECODE,
        cr.votingcentretypecode,

    cr.locationid,
    sum(cr.checkfpvotecount) as CheckFormal
 FROM
    prd2008.es_contest contest
  left join prd2008.es_candidateresults cr on contest.contestid = cr.contestid
    where contest.electioneventid = ?
    and contest.CONTESTTYPECODE in ('By LGA Referendum','General LGA Ref','By LGA Poll','General LGA Poll')

    group by   contest.contestid,
    contest.areacode,
    contest.contestareacode,
    contest.CONTESTTYPECODE,
        cr.votingcentretypecode,
    cr.locationid
        )
    
select 
 contestinf.contestid,
 contestinf.areacode,
 contestinf.contestareacode,
 contestinf.CONTESTTYPECODE,
 contestinf.votingcentretypecode as VENUEVOTETYPE,
 contestinf.locationid,
loc.gazettedname,
 contestinf.CheckInformal +  cand.CheckFormal as CheckCount

from contestinf
full join cand on contestinf.contestid = cand.contestid and cand.locationid = contestinf.locationid
left join prd2008.es_locations loc on loc.votingcentretypecode = contestinf.votingcentretypecode and loc.locationid = contestinf.locationid
                and loc.electioneventid = ?


                           
                           ",
                               as.list(rep(event_group_ID,3)),
                               fetch=TRUE,
                               stringsAsFactors = FALSE) 
}
# mayoral <- candidate + informal
# councilor <- group + btl(candidate) + other


## 2). PRCC batched (2nd count) ================================================================================
# the idea of election code here in prcc is election area only it combines mayoral and councilor together, this is really just pp level data, need to combine with contest to split out
if (load_PRCC) {
  
  PRCC_raw <- sqlExecute(PRCC_connection,
                         "
                        with bulktotal as
                    (
                    SELECT 
                           [POLLINGPLACE_ID]
                    	  ,sum([BULK_TOTAL]) as BULKTOTAL
                          ,sum([RATL_TOTAL]) as RATL
                      FROM [dbo].[GROUPBULKTOTAL]
                    group by POLLINGPLACE_ID
                    
                    ),
                    
                    polling_place as
                    (
                    SELECT 
                          [POLLINGPLACE_ID]
						  ,pp.[ELECTION_AREA_ID]
						  ,pp.[AREAWARD_ID]
						  ,[VOTINGCHANNEL_ID]
						  ,[PROGRESSIVECOUNT_ID]
                    	  ,ea.ELECTION_CODE
                          ,aw.AREA_CODE
                          ,[POLLING_PLACE_TYPE]
                          ,[POLLING_PLACE_CODE]
                          ,[POLLING_PLACE_NAME]
                          ,[OTHER_TOTAL]
                          ,[BTL_TOTAL]
                          ,[MAYOR_CHECK_COUNT]
                          ,COUNCILLOR_CHECK_COUNT
                          ,COUNCILLOR_EN_COUNT
                          ,REGISTER_APPROVED
                    
                      FROM [dbo].[POLLINGPLACE] pp
                      left join [dbo].[AREAWARD] aw on pp.AREAWARD_ID = aw.AREAWARD_ID
                      left join [dbo].[ELECTIONAREA] ea on pp.ELECTION_AREA_ID = ea.ELECTION_AREA_ID and pp.AREAWARD_ID = ea.AREAWARD_ID
                      
                    )
                    
                    select 
						   polling_place.[POLLINGPLACE_ID]
						  ,polling_place.[ELECTION_AREA_ID]
						  ,polling_place.[AREAWARD_ID]
						  ,polling_place.[VOTINGCHANNEL_ID]
						  ,polling_place.[PROGRESSIVECOUNT_ID]
                    	   ,ELECTION_CODE
                          ,AREA_CODE as CONTESTAREACODE
                          ,[POLLING_PLACE_TYPE]
                          ,[POLLING_PLACE_CODE]
                          ,[POLLING_PLACE_NAME]
                          ,[MAYOR_CHECK_COUNT]
                          ,COUNCILLOR_CHECK_COUNT
                          ,COUNCILLOR_EN_COUNT
                       --   ,ISNULL([OTHER_TOTAL],0) + ISNULL([BTL_TOTAL],0) + ISNULL(BULKTOTAL,0) + ISNULL(RATL,0) as OTHER_TOTAL
                    ,OTHER_TOTAL
                    ,polling_place.REGISTER_APPROVED

                    from polling_place
                    left join bulktotal on bulktotal.POLLINGPLACE_ID = polling_place.POLLINGPLACE_ID

                        ",
                         fetch=TRUE,
                         stringsAsFactors = FALSE) 
  
  
  
  
  ####################################################################################################################
  
  
  ## 1). PRCC data entry figures ===================================================================
  
  
  PRCC_BPs <- sqlExecute(PRCC_connection,
                         "
    /****** Script for SelectTopNRows command from SSMS  ******/
with batch as (
SELECT 
	   [POLLINGPLACE_ID]
       ,sum([TOTAL_PAPERS_REGISTERED]) as Registered
       ,sum([TOTAL_PAPERS_ENTERED]) as Entered
--  ,TOTAL_PAPERS_REGISTERED
--  ,TOTAL_PAPERS_ENTERED
  FROM [dbo].[BATCH] 
  group by [POLLINGPLACE_ID]
 
 -- order by POLLINGPLACE_ID

  ),
  inc_batch as (
  
SELECT 
	   [POLLINGPLACE_ID]
       ,count(STATUS) as incomplete_Batch
--  ,TOTAL_PAPERS_REGISTERED
--  ,TOTAL_PAPERS_ENTERED
  FROM [dbo].[BATCH] 
  where STATUS != 100
  group by [POLLINGPLACE_ID]

  
  ),

  contest_venues as (

  SELECT 
		ea.[ELECTION_CODE]
		,aw.AREA_CODE
	  ,pp.[POLLINGPLACE_ID]
      ,pp.[POLLING_PLACE_NAME]
      ,pp.[POLLING_PLACE_CODE]
      ,pp.[POLLING_PLACE_TYPE]
      ,pp.[PROGRESSIVECOUNT_ID]
      ,pp.[COUNCILLOR_EN_COUNT]
      ,pp.[ACCEPTED_DECLARATIONS]
      ,pp.[ACCEPTED_BALLOT_PAPERS]
      ,pp.[EN_BLANK_TOTAL]
      ,pp.[EN_OTHER_TOTAL]
      ,pp.[EN_RATL_TOTAL]
      ,pp.[EN_BTL_TOTAL]
      ,pp.[REF_EN_COUNT]
      ,pp.[MAYOR_EN_COUNT]
      ,pp.[REF_CHECK_COUNT]
      ,pp.[MAYOR_CHECK_COUNT]
      ,pp.[COUNCILLOR_CHECK_COUNT]
      ,pp.[COUNCILLOR_BP_ISSUED]
      ,pp.[MAYOR_BP_ISSUED]
      ,pp.[REF_BP_ISSUED]
      ,pp.[DV_BP_ISSUED]
      ,pp.[BP_ISSUED]
      ,pp.[BLANK_TOTAL]
      ,pp.[OTHER_TOTAL]
      ,pp.[RATL_TOTAL]
      ,pp.[BTL_TOTAL]
      ,pp.[POLLING_PLACE_COMMENT]
      ,pp.[DV_GROUP_COUNT]
      ,pp.[TOTAL_FORMAL_VOTES]
      ,pp.[SORT_ORDER]
      ,pp.[MULTI_WARD]
  FROM [dbo].[POLLINGPLACE] pp
  left join [dbo].[ELECTIONAREA] ea on pp.[ELECTION_AREA_ID] = ea.[ELECTION_AREA_ID]
  left join [dbo].[AREAWARD] aw on pp.AREAWARD_ID = aw.AREAWARD_ID
  -- a group is required here, currently missing due to unsure of what columns to sum for. require more testing data
  )

  select 
		contest_venues.ELECTION_CODE
		,contest_venues.AREA_CODE
  ,contest_venues.[POLLINGPLACE_ID]
  ,contest_venues.[PROGRESSIVECOUNT_ID]
  ,contest_venues.[POLLING_PLACE_CODE]
--	  ,batch.[POLLINGPLACE_ID]
	  ,contest_venues.POLLING_PLACE_NAME
	  ,contest_venues.[POLLING_PLACE_TYPE]
      ,batch.Registered
      ,batch.Entered 
--	  ,contest_venues.MAYOR_EN_COUNT
--	  ,contest_venues.MAYOR_CHECK_COUNT
	  ,isnull(inc_batch.incomplete_Batch,0) as incomplete_Batch
	  
from batch
  left join contest_venues on batch.POLLINGPLACE_ID = contest_venues.POLLINGPLACE_ID
    left join inc_batch on batch.POLLINGPLACE_ID = inc_batch.POLLINGPLACE_ID

                        ",
                         fetch = T,
                         stringsAsFactors = FALSE)
  
  
  
  PRCC_comments <- sqlExecute(PRCC_connection,
                         "
 SELECT 
ea.ELECTION_CODE
,aw.AREA_CODE
	  ,pp.POLLING_PLACE_TYPE
	  ,pp.POLLING_PLACE_CODE
	  ,pp.POLLING_PLACE_NAME
      ,comt.[POLLINGPLACE_ID]
	  ,comt.[COMMENTARY_ID]
      ,comt.[POLLING_PLACE_COMMENT]
      ,comt.[COMMENT_BY_ID]
      ,comt.[DATE_COMMENT]
      	  ,pp.PROGRESSIVECOUNT_ID

  FROM [dbo].[COMMENTARY] comt
  left join [dbo].[POLLINGPLACE] pp on comt.[POLLINGPLACE_ID] = pp.POLLINGPLACE_ID
  left join [dbo].[AREAWARD] aw on pp.AREAWARD_ID = aw.AREAWARD_ID
  left join [dbo].[ELECTIONAREA] ea on pp.ELECTION_AREA_ID = ea.ELECTION_AREA_ID and pp.AREAWARD_ID = ea.AREAWARD_ID



                         ",
                         fetch = T,
                         stringsAsFactors = FALSE)
    
} else {
  
  PRCC_raw <- data.frame()
  PRCC_BPs <- data.frame()
  PRCC_comments <- data.frame()
  
}

####################################################################################################################























