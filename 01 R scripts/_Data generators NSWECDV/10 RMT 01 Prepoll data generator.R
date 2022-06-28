####################################################################################################################
###################################### rmt DATA GENERATOR  ---- ###########################################
####################################################################################################################


library(RODBC)
library(RODBCext)
library(kableExtra)
library(DT)
library(knitr)
library(formattable)
library(openxlsx)
library(tidyr)
library(dplyr)

options(scipen=999)


# event_group_ID= 'LG2003'

# 1. set parameters  ---------------------------------------------------------
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Functions.R"))




## 1). Hyperion for old AoBP resource ===================================================================

AoBP <- sqlExecute(datavault_connection,
                                   "/****** Script for SelectTopNRows command from SSMS  ******/
SELECT 
--cl.ContestLocationHashKey,
       cl.[ContestID]
	  ,cl.[AreaCode]
      ,cl.[LocationID]
      ,cl.[LocationType]
 --  ,alloc.[Quantity]
	 ,convert(date,rec.ReconciliationDate) as ReconciliationDate
	 ,rec.[Spoilt]
	 ,rec.[Discarded]
	 ,rec.[Unused]
	 ,sgl.gazettedname as VenueName
,cs.ContestName

  FROM [NSWECDataVault].[RawDataVault].[Hub_ContestLocation] cl
--  left join [RawDataVault].[Sat_BP_Allocations] alloc on cl.ContestLocationHashKey = alloc.ContestLocationHashkey
  left join [RawDataVault].[Sat_BP_Reconciliation] rec on cl.ContestLocationHashKey = rec.ContestLocationHashkey
  left join [RawDataVault].[Hub_EventGroupLocation] gl on cl.LocationID = gl.LocationID and gl.LocationType = cl.LocationType and gl.EventGroupID = substring(cl.ContestID,1,6)
  left join [RawDataVault].[Sat_EventGroupLocation] sgl on gl.EventGroupLocationHashKey = sgl.EventGroupLocationHashKey

left join [RawDataVault].[Hub_Contest] co on cl.ContestID = co.ContestID
left join [RawDataVault].[Sat_Contest_EMS] cs on co.ContestHashKey = cs.ContestHashKey

  where SUBSTRING(cl.ContestID,1,6) = 'LG2003'
  and rec.ReconciliationDate is not null
  order by rec.ReconciliationDate

                        ",
 #                  event_group_ID,
                 fetch=TRUE,
                                   stringsAsFactors = FALSE) 



Allocations <- sqlExecute(datavault_connection,
                     "
                    /****** Script for SelectTopNRows command from SSMS  ******/
SELECT 
cl.ContestID
,cs.ContestName
,cl.AreaCode
	  ,cl.LocationID
	  ,sgl.gazettedname as VenueName
	  ,cl.LocationType
,convert(date,al.DateAllocated) as DateAllocated
      ,[Quantity]
  FROM [NSWECDataVault].[RawDataVault].[Sat_BP_Allocations] al
  left join [RawDataVault].[Hub_ContestLocation] cl on al.[ContestLocationHashkey] = cl.[ContestLocationHashkey]
  left join [RawDataVault].[Hub_EventGroupLocation] gl on cl.LocationID = gl.LocationID and gl.LocationType = cl.LocationType and gl.EventGroupID = substring(cl.ContestID,1,6)
  left join [RawDataVault].[Sat_EventGroupLocation] sgl on gl.EventGroupLocationHashKey = sgl.EventGroupLocationHashKey
  left join [RawDataVault].[Hub_Contest] co on cl.ContestID = co.ContestID
left join [RawDataVault].[Sat_Contest_EMS] cs on co.ContestHashKey = cs.ContestHashKey

  where cl.ContestID like ?
                        ",
                  paste0(event_group_ID,'%'),
                     fetch=TRUE,
                     stringsAsFactors = FALSE) 


## 2). markOff ================================================================================

PrePoll_Markoff <- sqlExecute(EMA_connection,"
                       SELECT
dec.electioneventid,
    dec.declarationexcusevotetypecode,
     dec.votestatuscode,
     SUBSTR(dec.voteprocesseddate,1,9) as Day,
     dec.locationid,
     loc.gazettedname as VenueName,
     dec.votingcentretypecode,
     dec.issuingdistareacode,
    count(1) as Total

FROM
    prd2008.es_decexcusevote dec
    left join prd2008.es_locations loc on dec.locationid = loc.locationid and dec.votingcentretypecode = loc.votingcentretypecode
    and loc.electioneventid = dec.electioneventid

where  dec.electioneventid = ?
and  dec.votingcentretypecode = 'Pre-Poll'
    group by  
    dec.electioneventid,
      dec.declarationexcusevotetypecode,
      dec.votestatuscode,
     SUBSTR( dec.voteprocesseddate,1,9),
  dec.locationid, 
       loc.gazettedname,

  dec.votingcentretypecode,
      dec.issuingdistareacode
    
order by       loc.gazettedname,
 Day


                       
                              ",
                              
                              event_group_ID,
                              fetch = TRUE,
                              stringsAsFactors = FALSE)


####################################################################################################################























