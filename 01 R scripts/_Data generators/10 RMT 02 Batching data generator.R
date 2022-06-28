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

#event_group_ID= 'LG1701'

# 1. set parameters  ---------------------------------------------------------
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))


## Analytics database connections ----
setwd(database_connections)
source('DataVault.R')
source("PRCC.R")
source("EMA.R") # potentially load everything from hyperion later this is temp

## 1). EMA first count ===================================================================
# EMA querrie is too long therefore split into 3
# ema first count queries have been moved to common data gen



EMA_comments <- sqlExecute(EMA_connection,"
                         SELECT
    contestid,
    areacode,
    votingcentretypecode,
    locationid,
    commentdate,
    comments,
    commentsby,
    commenttype,
    ucn
FROM
    prd2008.es_votecountingpointcomments
    where electioneventid = ?
                           
                           ",
                           event_group_ID,
                            fetch=TRUE,
                            stringsAsFactors = FALSE) 


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

# mayoral <- candidate + informal
# councilor <- group + btl(candidate) + other


## 2). PRCC batched (2nd count) ================================================================================
# the idea of election code here in prcc is election area only it combines mayoral and councilor together, this is really just pp level data, need to combine with contest to split out
if (not_load_prcc) {
  
  PRCC_raw <- data.frame()
  
} else {
  
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
                    	  ,ea.ELECTION_CODE
                          ,aw.AREA_CODE
                          ,[POLLING_PLACE_TYPE]
                          ,[POLLING_PLACE_CODE]
                          ,[POLLING_PLACE_NAME]
                          ,[OTHER_TOTAL]
                          ,[BTL_TOTAL]
                          ,[MAYOR_CHECK_COUNT]
                    
                      FROM [dbo].[POLLINGPLACE] pp
                      left join [dbo].[AREAWARD] aw on pp.AREAWARD_ID = aw.AREAWARD_ID
                      left join [dbo].[ELECTIONAREA] ea on pp.ELECTION_AREA_ID = ea.ELECTION_AREA_ID and pp.AREAWARD_ID = ea.AREAWARD_ID
                      
                    )
                    
                    select 
                    
                    	   ELECTION_CODE
                          ,AREA_CODE as CONTESTAREACODE
                          ,[POLLING_PLACE_TYPE]
                          ,[POLLING_PLACE_CODE]
                          ,[POLLING_PLACE_NAME]
                          ,[MAYOR_CHECK_COUNT]
                          ,ISNULL([OTHER_TOTAL],0) + ISNULL([BTL_TOTAL],0) + ISNULL(BULKTOTAL,0) + ISNULL(RATL,0) as CouncillorCheckCount
                    
                    from polling_place
                    left join bulktotal on bulktotal.POLLINGPLACE_ID = polling_place.POLLINGPLACE_ID



                        ",
                            fetch=TRUE,
                            stringsAsFactors = FALSE) 

}


####################################################################################################################























