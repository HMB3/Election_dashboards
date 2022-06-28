



if (FALSE) { # reseed everything
  
  # reset application table to nulls
  sqlExecute(hyperion_connection,
             "DELETE [ProcessingData].[dashboard].[staffing_EOI_applicants]
                          where electioneventid like ?

           DBCC CHECKIDENT ('[ProcessingData].[dbo].[staffing_EOI_applicants]', RESEED, 0)",
             paste0(event_group_ID,'%'),
             
             fetch = FALSE)
  
  
  # reset application extra lookup table to nulls
  
  sqlExecute(hyperion_connection,
             "DELETE [ProcessingData].[dashboard].[staffing_geo_lookups]
                          where electioneventid like ?

           DBCC CHECKIDENT ('[ProcessingData].[dbo].[staffing_geo_lookups]', RESEED, 0)",
             paste0(event_group_ID,'%'),
             
             fetch = FALSE)
  
  
  # reset alldist 3nearst
  
  
  sqlExecute(hyperion_connection,
             "DELETE [ProcessingData].[dashboard].[staffing_distance]
                          where electioneventid like ?

           DBCC CHECKIDENT ('[ProcessingData].[dbo].[staffing_distance]', RESEED, 0)",
             paste0(event_group_ID,'%'),
             
             fetch = FALSE)
  
  
  sqlExecute(hyperion_connection,
             "DELETE [ProcessingData].[dashboard].[staffing_already_hired]
                          where electioneventid like ?

           DBCC CHECKIDENT ('[ProcessingData].[dbo].[staffing_already_hired]', RESEED, 0)",
             paste0(event_group_ID,'%'),
             
             fetch = FALSE)
  
  
  
  sqlExecute(hyperion_connection,
             "DELETE [ProcessingData].[dashboard].[staffing_RO_centroids]
           DBCC CHECKIDENT ('[ProcessingData].[dbo].[staffing_RO_centroids]', RESEED, 0)",
             fetch = FALSE)
  
  
  
  
}

# EMA source queries ------------------------------------------------------
if(FALSE) {
  
EMA_staffing_raw <- sqlExecute(hyperion_connection,
                           "SELECT stf.ELECTIONEVENTID,
stf.HOMEPHONE,
stf.WORKPHONE,
                           stf.MOBILEPHONE,
                                                      stf.emailaddress,

                           stf.LOGINID,
                           stf.ELECTORID,
                           stf.SURNAME,
                           stf.GIVENNAMES,
                           stf.PREFERREDFIRSTNAME,
                           stf.DATEOFBIRTH,
                           stf.PREVIOUSPPPOSITION as PREVIOUSPOSITION,
                           stf.SECONDLANGUAGE,
                           stf.ROCOMMENTS,
                           stf.REGISTRATIONCONFIRMED as CREATIONDATE,
                           stf.INTERESTEDASEOPP,
                           stf.INTERESTEDASOA,
                           stf.STAFFSTATUS,
                           stf.HOMEADDRESS,
                           stf.HOMEADDRESSLINE2,
                           stf.HOMELOCALITYNAME,
                           stf.HOMEPOSTCODE,
                           stf.HOMESTATE,
                           stfp.stafftypecode,
                           stfp.positiontypecode,
                           ap.areacode,
                           lprf.locationid,
                           lprf.preferenceorder,
                           stc.VALUEENTERED as ATSI,
                           elt.addressid,
                           elt.catenatedelectoraladdress,
                           elt.localityname,
                           elt.postcode
                           FROM STAFFING.ES_STAFF stf
                           left join STAFFING.ES_STAFFCOMPLIANCE stc
                           on stf.LOGINID = stc.LOGINID and stf.ELECTIONEVENTID = stc.ELECTIONEVENTID
                           
                           left join staffing.es_staffoapositionpreferences stfp
                           on stfp.electioneventid = stf.electioneventid and stfp.loginid = stf.loginid
                           
                           left join staffing.es_staffareapreferences ap 
                           on ap.loginid = stf.loginid and ap.electioneventid = stf.electioneventid and stfp.stafftypecode = ap.stafftypecode
                            
                           left join staffing.es_stafflocationpreference lprf
                           on stf.loginid = lprf.loginid and stf.electioneventid = lprf.electioneventid and ap.areacode = lprf.areacode and lprf.stafftypecode = stfp.stafftypecode
                           
                           left join prd2008.es_elector elt
                           on stf.electorid = elt.electorid
                           
                           where stf.ELECTIONEVENTID = ?
                           and stc.TYPECODE = 'HasAboriginalTSIBackground'
                           and stf.STAFFSTATUS != 'INITIAL'
                           and stf.REGISTRATIONCONFIRMED is not null

                           ",
                           event_group_ID,
                           fetch = TRUE,
                           stringsAsFactors = FALSE) %>%
  mutate(CATENATEDELECTORALADDRESS = ifelse(is.na(CATENATEDELECTORALADDRESS),paste(HOMEADDRESS,HOMEADDRESSLINE2,sep=','),CATENATEDELECTORALADDRESS)
         ,LOCALITYNAME = ifelse(is.na(CATENATEDELECTORALADDRESS),HOMELOCALITY,LOCALITYNAME)
         ,POSTCODE = ifelse(is.na(CATENATEDELECTORALADDRESS),HOMEPOSTCODE,POSTCODE))%>%
  as_tibble()
}


EMA_staffing_raw <- sqlExecute(hyperion_connection,
                               "SELECT stf.ELECTIONEVENTID,
stf.HOMEPHONE,
stf.WORKPHONE,
                           stf.MOBILEPHONE,
                                                      stf.emailaddress,

                           stf.LOGINID,
                           stf.ELECTORID,
                           stf.SURNAME,
                           stf.GIVENNAMES,
                           stf.PREFERREDFIRSTNAME,
                           stf.DATEOFBIRTH,
                           stf.PREVIOUSPPPOSITION as PREVIOUSPOSITION,
                           stf.SECONDLANGUAGE,
                           stf.ROCOMMENTS,
                           stf.REGISTRATIONCONFIRMED as CREATIONDATE,
                           stf.INTERESTEDASEOPP,
                           stf.INTERESTEDASOA,
                           stf.STAFFSTATUS,
                           stf.HOMEADDRESS,
                           stf.HOMEADDRESSLINE2,
                           stf.HOMELOCALITYNAME,
                           stf.HOMEPOSTCODE,
                           stf.HOMESTATE,
                           stfp.stafftypecode,
                           stfp.positiontypecode,
                           ap.areacode,
                           lprf.locationid,
                           lprf.preferenceorder,
                           stc.VALUEENTERED as ATSI
                        
                           FROM STAFFING.ES_STAFF stf
                           left join STAFFING.ES_STAFFCOMPLIANCE stc
                           on stf.LOGINID = stc.LOGINID and stf.ELECTIONEVENTID = stc.ELECTIONEVENTID
                           
                           left join staffing.es_staffoapositionpreferences stfp
                           on stfp.electioneventid = stf.electioneventid and stfp.loginid = stf.loginid
                           
                           left join staffing.es_staffareapreferences ap 
                           on ap.loginid = stf.loginid and ap.electioneventid = stf.electioneventid and stfp.stafftypecode = ap.stafftypecode
                            
                           left join staffing.es_stafflocationpreference lprf
                           on stf.loginid = lprf.loginid and stf.electioneventid = lprf.electioneventid and ap.areacode = lprf.areacode and lprf.stafftypecode = stfp.stafftypecode

                           where stf.ELECTIONEVENTID = ?
                           and stc.TYPECODE = 'HasAboriginalTSIBackground'
                           and stf.STAFFSTATUS != 'INITIAL'
                           and stf.REGISTRATIONCONFIRMED is not null

                           ",
                               event_group_ID,
                               fetch = TRUE,
                               stringsAsFactors = FALSE) %>%
  mutate(CATENATEDELECTORALADDRESS = paste(HOMEADDRESS,HOMEADDRESSLINE2,sep=',')
         ,LOCALITYNAME = HOMELOCALITYNAME
         ,POSTCODE = HOMEPOSTCODE
         ,ADDRESSID = NA) %>%
  as_tibble()

# spread out the preferences

EMA_staffing <- EMA_staffing_raw %>%
  mutate(AgeAtElection = floor((20200915 - DATEOFBIRTH)/10000)
         ,LOGINID = as.character(LOGINID)) %>%
  dplyr::select(-DATEOFBIRTH)# %>%
#  mutate(PREFERENCEORDER = ifelse(is.na(PREFERENCEORDER),'PREFERENCE_1',paste0('PREFERENCE_',PREFERENCEORDER))) %>% distinct() %>%
#  spread(PREFERENCEORDER,value=LOCATIONID)


staffing_distance <- sqlExecute(hyperion_connection,
                                "/****** Script for SelectTopNRows command from SSMS  ******/
                SELECT [ELECTIONEVENTID]
      ,[LOGINID]
      ,[ELECTORID]
      ,[AREACODE]
      ,[STREETADDRESSID]
      ,[NEAREST]
      ,[DRIVINGDISTMETERS]
      ,[TIMESEC]
                  FROM [ProcessingData].[dashboard].[staffing_distance]
                  where [ELECTIONEVENTID] like ?
                             ",
                                 paste0(event_group_ID,'%'),
                                fetch = TRUE,
                                stringsAsFactors = FALSE) %>%
  mutate(LOGINID = as.character(LOGINID))%>%
  as_tibble()

### new applicants upload to hyperion to merge with address geos in view

new_valid_applicants <- EMA_staffing %>% anti_join(staffing_distance,by='LOGINID') %>% 
                        filter(!is.na(CATENATEDELECTORALADDRESS)) %>%
                        mutate(LOGINID = str_trim(LOGINID, side = c("both", "left", "right"))
                               ,ELECTORID = str_trim(ELECTORID, side = c("both", "left", "right"))) 

if (dev_code) {
  
  new_valid_applicants <- head(new_valid_applicants,devnum)
  
}


if(nrow(new_valid_applicants) > 0) {
  

sqlExecute(hyperion_connection,
           "INSERT INTO [ProcessingData].[dashboard].[staffing_EOI_applicants]
           (
       [ELECTIONEVENTID]
      ,[HOMEPHONE]
      ,[WORKPHONE]
      ,[MOBILEPHONE]              
      ,[EMAILADDRESS]
      ,[LOGINID]
      ,[ELECTORID]
      ,[SURNAME]                  
      ,[GIVENNAMES]
      ,[PREFERREDFIRSTNAME]
      ,[PREVIOUSPOSITION]
      ,[SECONDLANGUAGE]           
      ,[ROCOMMENTS]
      ,[CREATIONDATE]
      ,[INTERESTEDASEOPP]
      ,[INTERESTEDASOA]           
      ,[STAFFSTATUS]
      ,[HOMEADDRESS]
      ,[HOMEADDRESSLINE2]
      ,[HOMELOCALITYNAME]         
      ,[HOMEPOSTCODE]
      ,[HOMESTATE]
      ,[STAFFTYPECODE]
      ,[POSITIONTYPECODE]         
      ,[AREACODE]
      ,[LOCATIONID]
      ,[PREFERENCEORDER]
      ,[ATSI]                     
      ,[ADDRESSID]
      ,[CATENATEDELECTORALADDRESS]
      ,[LOCALITYNAME]
      ,[POSTCODE]                 
      ,[AGEATELECTION]        
           ) VALUES (?,?,?,?,?,?
                    ,?,?,?,?,?,?
                    ,?,?,?,?,?,?
                    ,?,?,?,?,?,?
                    ,?,?,?,?,?,?
                    ,?,?,?)",
           data = new_valid_applicants)

}
# load hyperion view for the already geocoded stuff

staffing_EOI_applicants_addresses <- sqlExecute(hyperion_connection,
                                                "SELECT [ELECTIONEVENTID]
      ,[LOGINID]
      ,[ELECTORID]
      ,[AREACODE]
      ,[LATITUDE]
      ,[LONGITUDE]
  FROM [ProcessingData].[dashboard].[staffing_geo_lookups]
      where ELECTIONEVENTID = ?
                             ",
                                                event_group_ID,
                                                fetch = TRUE,
                                                stringsAsFactors = FALSE) %>%
  mutate(LOGINID = as.character(LOGINID))



Venue_staffing <- sqlExecute(hyperion_connection,
                              "SELECT ELECTIONEVENTID,
                             REDISTRIBUTIONCODE,
                             AREACODE,
                             STAFFTYPECODE,
                             LOCATIONID,
                             POSITIONID,
                             LOGINID,
                             OFFERMETHOD,
                             POSITIONSTATUS,
                             POSITIONTYPECODE,
                             OFFERDATE,
                             OFFEREDBY,
                             ACCEPTEDDATE,
                             ACCEPTEDBY,
                             RECORDTYPE,
                             DATEREQUIREDONDUTY,
                             LASTDATEOFDUTY,
                             UCN,
                             EMPLOYEDBY,
                             EMPLOYEDDATE,
                             UPDATED
                             FROM STAFFING.ES_STAFFPOSITIONS
                             where ELECTIONEVENTID = ?
                              ",
                              event_group_ID,
                              fetch = TRUE,
                              stringsAsFactors = FALSE) %>%
      mutate(LOGINID = as.character(LOGINID))



stored_hired <- sqlExecute(hyperion_connection, "SELECT *
                              FROM [ProcessingData].[dashboard].[staffing_already_hired]
                              where electioneventid = ?
                              and [LOGINID] is not null
                                
                                ",event_group_ID,
                           fetch = TRUE,
                           stringsAsFactors = FALSE) %>%
  mutate(LOGINID = as.character(LOGINID))


# Training status ---------------------------------------------------------



training_status <- sqlExecute(hyperion_connection,
                             "SELECT
    electioneventid,
    loginid,
    typecode,
    valueentered,
    ucn
FROM
    staffing.es_staffcompliance
    where typecode = 'COM020_OLT'
                             and ELECTIONEVENTID = ?
                              ",
                             event_group_ID,
                             fetch = TRUE,
                             stringsAsFactors = FALSE) %>%
  mutate(LOGINID = as.character(LOGINID))



# exp and lan -------------------------------------------------------------




exp_lan <- sqlExecute(hyperion_connection,
                              "
SELECT
electioneventid,
loginid,
typecode,
valueentered
FROM
staffing.es_staffcompliance


where electioneventid = ?
and TYPECODE in ('PrevPosition1','SecondLanguage1')
                              ",
                              event_group_ID,
                              fetch = TRUE,
                              stringsAsFactors = FALSE) %>%
  mutate(LOGINID = as.character(LOGINID)) %>%
  spread(TYPECODE,VALUEENTERED) %>%
  dplyr::select(PREVIOUSPOSITION = PrevPosition1
                ,SECONDLANGUAGE = SecondLanguage1
                ,LOGINID)
  


# Venue geo


# find ro dist to prepoll


RO_Centroids <- sqlExecute(hyperion_connection,
                           "SELECT [LONGITUDE]
      ,[LATITUDE]
      ,[RO_OFFICE]
      ,[SIZE_WEIGHT]
      ,[SIZE_MAP]
      
  FROM [ProcessingData].[dashboard].[staffing_RO_centroids]
  where [EVENTGROUPID] = ?
                             ",
                           event_group_ID,
                           fetch = TRUE,
                           stringsAsFactors = FALSE) 




Vennus_RO_UPLOAD <- sqlExecute(hyperion_connection,
                               "SELECT [EVENTID]
      ,[STREETADDRESSID]
      ,[TimeSec]
  FROM [ProcessingData].[dashboard].[staffing_VC_to_RO]
    where [EVENTID] like ?

                             ",
                               paste0(event_group_ID,'%'),
                               fetch = TRUE,
                               stringsAsFactors = FALSE) %>%
  mutate(Time = paste0(ceiling(TimeSec/60),' Mins')) %>% dplyr::select(-TimeSec)



