staffing_EOI_applicants_addresses <- staffing_EOI_applicants_addresses %>% group_by(LOGINID) %>% mutate(rank = order(LOGINID)) %>%
  filter(rank==1) %>% select(-rank)


Contest <- Contest_raw  
  
# SOA OAs need to be binded to employed list as currently they are left out due to the left join

EMA_staffing_raw <- EMA_staffing_raw %>% dplyr::select(-PREVIOUSPOSITION,-SECONDLANGUAGE) %>%
                                  left_join(exp_lan,by='LOGINID')



# create eventid at lga level for merge to other information
Contests <- Contest_details %>% mutate(EventID = substr(ContestID,1,10))



# Data cleansing and analysis---------------------------------------------------------
Hiring_status <- Venue_staffing %>% #filter(POSITIONSTATUS %in% c('OFFERED','EMPLOYED')) %>%
                dplyr::select(ELECTIONEVENTID,AREACODE,STAFFTYPECODE,LOCATIONID,
                       POSITIONID,LOGINID,POSITIONSTATUS,POSITIONTYPECODE) %>%
                mutate(StreetAddressID = as.numeric(gsub('[^0-9]','',LOCATIONID))) %>%
                mutate(POSITIONSTATUS = ifelse(POSITIONSTATUS =='ACCEPTED','EMPLOYED',POSITIONSTATUS)) %>%
                left_join(Contests %>% select(LGArea,EventID) %>% distinct(), by=c('AREACODE'='LGArea'))



RO_Offices <- Potential_Venues_raw %>% 
  filter(LocationTypeCode == 'Returning Office') %>% filter(!VenueName %in% c('Bankstown RO Office 2011','Bankstown RO Office 2012')) %>%
  left_join(Contests, by='EventID') %>%
  dplyr::select(EventID,RO_Office = VenueName,LATITUDE_RO = Latitude,LONGITUDE_RO = Longitude,LGArea) %>%
  distinct()



# joining venues to contest for RO area
EMS_Venues <- Potential_Venues_raw %>% 
  left_join(RO_Offices %>% dplyr::select(RO_Office, 
                                  RO_lat = LATITUDE_RO,
                                  RO_long = LONGITUDE_RO,
                                  EventID,
                                  LGArea)
            ,by='EventID')







Location_positions <- Potential_Venues_raw %>% left_join(Hiring_status, by = c("EventID", "StreetAddressID")) %>% 
  
#  group_by(EventID,StreetAddressID,POSITIONSTATUS) %>%
#  mutate(EmployeeCount = n()) %>%
#  mutate(EmployeeCount = ifelse(is.na(POSITIONSTATUS),0,EmployeeCount)) %>%
  left_join(EMA_staffing %>% dplyr::select(LOGINID,SURNAME,PREFERREDFIRSTNAME), by='LOGINID') %>% distinct() #%>%
#  ungroup() %>%
#  left_join(Hiring_status) #%>% filter(POSITIONSTATUS == 'EMPLOYED') %>% 
#              group_by(StreetAddressID) %>%
#              mutate(Employed = n()) ) %>%
#  mutate(Employed = ifelse(is.na(Employed),0,Employed))

 
Unique_Venues <- EMS_Venues %>% filter(LocationTypeCode == 'Polling Place') %>%
  dplyr::select(VenueName
                                       ,StreetAddressID
                                       ,Latitude
                                       ,Longitude
                                       ,AddressName
                                       ,AddressLine1
                                       ,AddressLine2
                                       ,LocalityName
                                       ,PostCode
                                       ,LGArea
                                       ,RO_Office
                                       ,RO_lat
                                       ,RO_long
                                       ) %>% 
                      distinct()

# redo
# EMA_Unique_applications <- EMA_staffing %>% select(ELECTIONEVENTID,ELECTORID,LOGINID,AREACODE,PREFERENCE_1,PREFERENCE_2,PREFERENCE_3) %>%
#                                          distinct() %>% 
#   mutate(Preferences = paste(PREFERENCE_1,PREFERENCE_2,PREFERENCE_3,sep=' ,')) %>%
#                                         group_by(ELECTIONEVENTID,ELECTORID,LOGINID,LOGINID) %>%
#                    summarise(AOI = as.character(list(AREACODE))
#                             ,Preferences = as.character(list(Preferences)))

# Geolocation of staff

# Address lookup ----------------------------------------------------------
# since ggmap is blocked by firewall, below pings ggmap and saves locally before reading data----can addopt new ggmap pakg later-------------------------------------------
if (nrow(new_valid_applicants) >0) {

  new_valid_applicants <- new_valid_applicants %>% filter(!is.na(AREACODE))
additional_lookup_needed <- new_valid_applicants %>% anti_join(staffing_EOI_applicants_addresses, by=c('LOGINID','AREACODE')) %>%# this is generated in the data generator
  dplyr::select(ELECTIONEVENTID, LOGINID, ELECTORID,AREACODE,HOMEADDRESS,HOMEADDRESSLINE2,HOMELOCALITYNAME,HOMESTATE,HOMEPOSTCODE) %>%
                            distinct()

if(nrow(additional_lookup_needed) >0) { # if there are new applications we need to geocode them
  newgeo_lookup <- additional_lookup_needed %>%
        mutate(Address = paste(HOMEADDRESS,HOMEADDRESSLINE2,HOMELOCALITYNAME,HOMESTATE,HOMEPOSTCODE,'Australia',sep=','))
  
  baseurl <- paste0('https://maps.googleapis.com/maps/api/geocode/json?address=',newgeo_lookup$Address,'&key=',api_key)
  baseurl <- gsub('#','',baseurl)
  
  setwd(rmd_files)
  
  
  library('jsonlite')
  options(download.file.method="wininet")
  
  GeoCoded <- data.frame()
  
  for (i in 1:length(baseurl)) {
    print(i/length(baseurl))
    download.file(baseurl[i], destfile = "ram.html")
    
    if (read_json("ram.html")$status == 'OK') {
      
      
   #   GeoCode_temp <- as.data.frame(read_json("ram.html")) %>%
   #     dplyr::select(LATITUDE = results.geometry.location.lat,
   #            LONGITUDE = results.geometry.location.lng) %>%
   #     mutate(Address = newgeo_lookup$Address[i])
      
      GeoCode_temp <- data.frame(
                    LATITUDE = read_json("ram.html")$results[[1]]$geometry$location$lat,
                    LONGITUDE =read_json("ram.html")$results[[1]]$geometry$location$lng) %>%
             mutate(Address = newgeo_lookup$Address[i])
      
      
    } else {
      
      GeoCode_temp <- data.frame(LATITUDE=NA,
                                 LONGITUDE=NA,
                                 Address = newgeo_lookup$Address[i])
      
    }
    GeoCoded <- GeoCoded %>% rbind(GeoCode_temp)
  }
  
  Staff_Geod_temp <- newgeo_lookup %>% dplyr::select(ELECTIONEVENTID, LOGINID, ELECTORID,AREACODE) %>% cbind(GeoCoded %>% dplyr::select(-Address))
 
  
  # upload results to lookup table
  
  sqlExecute(hyperion_connection,
             "INSERT INTO [ProcessingData].[dashboard].[staffing_geo_lookups]
           (
         [ELECTIONEVENTID]
      ,[LOGINID]
      ,[ELECTORID]
      ,[AREACODE]
      ,[LATITUDE]
      ,[LONGITUDE]
           ) VALUES (?,?,?,?,?,?)",
             data = Staff_Geod_temp)
  
}
  
  
# antijoin the whole view here

  # distance cLG2001ulations
  

  distance_cLG2001 <- staffing_EOI_applicants_addresses %>% anti_join(staffing_distance,by=c('LOGINID','AREACODE')) %>%
                      left_join(RO_Offices, by=c('AREACODE'='LGArea')) %>%
    filter(AREACODE %in% Contest[Contest$CONTESTSTATUSTYPECODE == 'Contested',]$AREACODE)
  

  Staff_Geod_dired <- data.frame()
  if(nrow(distance_cLG2001)>0) {
    
    # direct distance
    All_Direct_3NEAREST_area <- data.frame()
        for(j in 1:length(unique(distance_cLG2001$RO_Office))) {
      
    Area_Venues <- Unique_Venues %>% filter(RO_Office == unique(distance_cLG2001$RO_Office)[j]) %>% 
      dplyr::select(-LGArea) %>%
                                distinct()
    
    Area_distance_cLG2001 <- distance_cLG2001 %>% filter(RO_Office == unique(distance_cLG2001$RO_Office)[j]) 
    
    Direct_dis <- data.frame(distance=distm(Area_distance_cLG2001[which(colnames(Area_distance_cLG2001) == "LONGITUDE"):which(colnames(Area_distance_cLG2001) == "LATITUDE")],
                                            Area_Venues[which(colnames(Area_Venues) == "Longitude"):which(colnames(Area_Venues) == "Latitude")],fun = distHaversine))
    
    colnames(Direct_dis) <- Area_Venues[,2]
    
    All_Direct_3NEAREST <- data.frame()
    for (i in 1:nrow(Direct_dis)) {
      Direct_3NEAREST <- slice(Direct_dis,i) %>% gather(StreetAddressID,DISTANCE) %>%
        arrange(DISTANCE) %>% 
        filter(StreetAddressID != 1) %>% # filter out head office
        head(3) %>%
        mutate(LOGINID = Area_distance_cLG2001$LOGINID[i]
               ,RO_Office = Area_distance_cLG2001$RO_Office[i]
               ,NEAREST = order(DISTANCE))
      # bind all NEAREST_3 together
      All_Direct_3NEAREST <- All_Direct_3NEAREST %>% bind_rows(Direct_3NEAREST)
    }
    All_Direct_3NEAREST_area <- All_Direct_3NEAREST_area %>% bind_rows(All_Direct_3NEAREST)
    
    }
    
    # Bind direct dist with addr geo and venue geo
    
    Staff_Geod_dired <- distance_cLG2001 %>% #filter(LOGINID %in% All_Direct_3NEAREST$ELECTORID) %>%
      # select(-NEAREST,-Latitude, -Longitude, -DistKM, -Time, -VenueName,-StreetAddressID) %>%
      right_join(All_Direct_3NEAREST_area, by = c("LOGINID",'RO_Office')) %>% 
      dplyr::select(-DISTANCE) %>% 
      left_join(Potential_Venues_raw %>%
                  mutate(StreetAddressID = as.character(StreetAddressID)) %>%
                  dplyr::select(StreetAddressID,LATITUDE_VENUE = Latitude, LONGITUDE_VENUE = Longitude), by=c('StreetAddressID')) %>%
    distinct()
    
    
    
  }
  
  
  # ggmap distance
  
  
  if(nrow(Staff_Geod_dired) >0) {
    
    #.x is home geo and .y is venue geo
    from <- data.frame(Lon = Staff_Geod_dired$LONGITUDE,
                       Lat = Staff_Geod_dired$LATITUDE)
    
    to <- data.frame(Lon = Staff_Geod_dired$LONGITUDE_VENUE,
                     Lat = Staff_Geod_dired$LATITUDE_VENUE)
    
    baseurl <- paste0("https://maps.googleapis.com/maps/api/distancematrix/json?&origins=",from$Lat,
                      ",",from$Lon,"&destinations=",to$Lat,",",to$Lon,
                      "&alternatives=false&units=metric&mode=driving&key=",api_key)
    
    
    individual_distances <- data.frame() 
    for (i in 1:length(baseurl)) {
      
      print(paste0(i,'/',length(baseurl)))
      download.file(baseurl[i], destfile = "ram.html")
      from_to <- as.data.frame(read_json("ram.html"))
      
      if(from_to$status == 'OK') {
        
        colnames(from_to) <- c("From","To","DISTKM","DRIVINGDISTMETERS","TIMEMINS","TIMESEC","Status","Status2")
      } else {
        
        from_to = data.frame(From = NA,
                             To = NA,
                             DISTKM = NA,
                             DRIVINGDISTMETERS = NA,
                             TIMEMINS = NA,
                             TIMESEC = NA,
                             Status = NA,
                             Status2 = NA)
      }
      
      individual_distances <- individual_distances %>% bind_rows(from_to)
    }
    
    All_Applicant_3NEAREST <- Staff_Geod_dired %>% bind_cols(individual_distances %>% dplyr::select(DRIVINGDISTMETERS,TIMESEC)) 
    
   #%>%
   #  left_join(Unique_Venues %>% 
   #              mutate(StreetAddressID = as.character(StreetAddressID)) %>%
   #              select(StreetAddressID),by='StreetAddressID') %>%
   #  mutate(LOGINID = str_trim(LOGINID, side = c("both", "left", "right")))
    
 #   
 #   %>%
 #                 mutate(HOMEPHONE = as.character(HOMEPHONE)
 #                        ,WORKPHONE = as.character(WORKPHONE)
 #                        ,MOBILEPHONE = as.character(MOBILEPHONE)
 #                        ,AgeAtElection = as.character(AgeAtElection)
 #                        ,lat = as.character(lat)
 #                        ,long = as.character(long)
 #                        ,NEAREST = as.character(NEAREST)
 #                        ,Latitude = as.character(Latitude)
 #                        ,Longitude = as.character(Longitude)
 #                       # ,
 #                       # PREFERENCEORDER = as.character(PREFERENCEORDER)
 #                       )
 #   
 #   Staff_Geod <- Staff_Geod %>% mutate(StreetAddressID = as.character(StreetAddressID),
 #                                       CREATIONDATE = as.Date(CREATIONDATE)
 #                                       ) %>% 
 #     bind_rows(All_Applicant_3NEAREST) %>% distinct()
    
    
    # save to csv so that we dont over ping google

    sqlExecute(hyperion_connection,
               "INSERT INTO [ProcessingData].[dashboard].[staffing_distance]
           (
           [ELECTIONEVENTID]
      ,[LOGINID]
      ,[ELECTORID]
      ,[AREACODE]
      ,[STREETADDRESSID]
      ,[NEAREST]
      ,[DRIVINGDISTMETERS]
      ,[TIMESEC]
           ) VALUES (?,?,?,?,?,?
                    ,?,?)",
               data = All_Applicant_3NEAREST %>% dplyr::select(ELECTIONEVENTID
                                                        ,LOGINID
                                                        ,ELECTORID
                                                        ,AREACODE
                                                        ,StreetAddressID
                                                        ,NEAREST
                                                        ,DRIVINGDISTMETERS
                                                        ,TIMESEC))
    
  }
}

# 3 NEAREST venues --------------------------------------------------------
# join to all the txts eg venuenames etc.

if(nrow(staffing_distance) >0) {   #if there is data then carry on
  
Staff_Geod_NEAREST_CLG2001ed <- staffing_distance %>% mutate(NEAREST = paste0('NEAREST_',NEAREST)) %>%
              left_join(Unique_Venues %>% dplyr::select(VenueName, LGArea,STREETADDRESSID = StreetAddressID)
                                                , by=c('STREETADDRESSID','AREACODE'='LGArea')) %>%
              mutate(Hiring_Venues = paste0(VenueName,' - ',round(DRIVINGDISTMETERS/1000,2),' Kms - ','TravelTime: ',ceiling(TIMESEC/60),'Mins')) %>%
  dplyr::select(ELECTIONEVENTID,LOGINID,ELECTORID,NEAREST,Hiring_Venues,AREACODE) %>% 
                            distinct() %>% # new line
              pivot_wider(names_from =NEAREST,values_from =Hiring_Venues) %>%
          left_join(staffing_EOI_applicants_addresses, by=c('LOGINID','ELECTIONEVENTID','ELECTORID','AREACODE'))
  




# Mapping -----------------------------------------------------------------


# Mapping attributes below -----------------------------------------------------------



setwd(data_files)
LG2001 <- rgdal::readOGR(dsn="Compact_LGA", layer="Compact_LGA")


# data split adding a new binary var classifying staff experience

Staff_split <- Staff_Geod_NEAREST_CLG2001ed %>% 
                            left_join(EMA_staffing %>% dplyr::select(ELECTIONEVENTID,
                                                              LOGINID,
                                                              ELECTORID,
                                                              AREACODE,
                                                              STAFFSTATUS,
                                                              CREATIONDATE) %>%
                                      distinct(), by=c('LOGINID','ELECTIONEVENTID','ELECTORID','AREACODE')) %>%
                            filter(!is.na(STAFFSTATUS))

# Geo dots are added separately as different colors

# cLG2001ulate hired staff home to rodistance

# treat with lgas that has less than 3 venues

if ("NEAREST_3" %in% colnames(Staff_split)) {
  

hired_staff <- Staff_split %>% filter(STAFFSTATUS == 'ACTIVE') %>% dplyr::select(-STAFFSTATUS) %>% # remove poposed areacode
                          distinct() %>%
  left_join(Hiring_status %>% mutate(LOCATIONID = as.numeric(gsub('\\D',' ',LOCATIONID)))
            , by=c('ELECTIONEVENTID','LOGINID','AREACODE')) %>% 
  filter(!is.na(STAFFTYPECODE)) %>%
  
  ## Join where they work to the return office location
  left_join(Potential_Venues_raw %>% filter(LocationStatusCode != "It's Away") %>% dplyr::select(StreetAddressID,VenueName),
            by=c('LOCATIONID'='StreetAddressID')) %>%
  
  mutate(POSITION = paste0(VenueName,'-',POSITIONTYPECODE),
         
         LOGINID = as.character(LOGINID),
         STAFFSTATUS = POSITIONSTATUS) %>%
  
  group_by(ELECTIONEVENTID,ELECTORID,LOGINID,LATITUDE,
           LONGITUDE,NEAREST_1,NEAREST_2,NEAREST_3,AREACODE
           ,STAFFSTATUS,CREATIONDATE
           ) %>%
  summarise(POSITION = list(POSITION)) %>% ungroup() %>%
  mutate(POSITION = as.character(POSITION)) %>%
  mutate(STAFFSTATUS = ifelse(is.na(STAFFSTATUS),'EOI',STAFFSTATUS))

} else if ("NEAREST_2" %in% colnames(Staff_split)) {
  
  
  hired_staff <- Staff_split %>% filter(STAFFSTATUS == 'ACTIVE') %>% dplyr::select(-STAFFSTATUS) %>% # remove poposed areacode
    distinct() %>%
    left_join(Hiring_status %>% mutate(LOCATIONID = as.numeric(gsub('\\D',' ',LOCATIONID)))
              , by=c('ELECTIONEVENTID','LOGINID','AREACODE')) %>% 
    filter(!is.na(STAFFTYPECODE)) %>%
    
    ## Join where they work to the return office location
    left_join(Potential_Venues_raw %>% filter(LocationStatusCode != "It's Away") %>% dplyr::select(StreetAddressID,VenueName),
              by=c('LOCATIONID'='StreetAddressID')) %>%
    
    mutate(POSITION = paste0(VenueName,'-',POSITIONTYPECODE),
           
           LOGINID = as.character(LOGINID),
           STAFFSTATUS = POSITIONSTATUS) %>%
    
    group_by(ELECTIONEVENTID,ELECTORID,LOGINID,LATITUDE,
             LONGITUDE,NEAREST_1,NEAREST_2,AREACODE
             ,STAFFSTATUS,CREATIONDATE
    ) %>%
    summarise(POSITION = list(POSITION)) %>% ungroup() %>%
    mutate(POSITION = as.character(POSITION)) %>%
    mutate(STAFFSTATUS = ifelse(is.na(STAFFSTATUS),'EOI',STAFFSTATUS))
  
} else {
  
hired_staff <- Staff_split %>% filter(STAFFSTATUS == 'ACTIVE') %>% dplyr::select(-STAFFSTATUS) %>% # remove poposed areacode
    distinct() %>%
    left_join(Hiring_status %>% mutate(LOCATIONID = as.numeric(gsub('\\D',' ',LOCATIONID)))
              , by=c('ELECTIONEVENTID','LOGINID','AREACODE')) %>% 
    filter(!is.na(STAFFTYPECODE)) %>%
    
    ## Join where they work to the return office location
    left_join(Potential_Venues_raw %>% filter(LocationStatusCode != "It's Away") %>% dplyr::select(StreetAddressID,VenueName),
              by=c('LOCATIONID'='StreetAddressID')) %>%
    
    mutate(POSITION = paste0(VenueName,'-',POSITIONTYPECODE),
           
           LOGINID = as.character(LOGINID),
           STAFFSTATUS = POSITIONSTATUS) %>%
    
    group_by(ELECTIONEVENTID,ELECTORID,LOGINID,LATITUDE,
             LONGITUDE,NEAREST_1,AREACODE
             ,STAFFSTATUS,CREATIONDATE
    ) %>%
    summarise(POSITION = list(POSITION)) %>% ungroup() %>%
    mutate(POSITION = as.character(POSITION)) %>%
    mutate(STAFFSTATUS = ifelse(is.na(STAFFSTATUS),'EOI',STAFFSTATUS))
  
  
}









################# what if one is hired in multiple areas
# rejoin offered hired and eoi  
Staff_split_RefreshedHiringStatus <- Staff_split %>% dplyr::select(ELECTIONEVENTID,ELECTORID,LOGINID,ProposedArea=AREACODE) %>% # remove poposed areacode
                               distinct() %>%
               left_join(hired_staff %>% dplyr::select(ELECTORID,LOGINID,STAFFSTATUS,AREACODE)) %>%
                  mutate(STAFFSTATUS = ifelse(is.na(STAFFSTATUS),'EOI',STAFFSTATUS)
                         ,AREACODE = ifelse(is.na(AREACODE),ProposedArea,AREACODE)) %>%
  dplyr::select(-ProposedArea) %>% distinct()


# here upload hired staff and antijoin 

hired_staff_lookup <- hired_staff %>% anti_join(stored_hired,by=c('LOGINID','POSITION','AREACODE')) %>%
                                  filter(STAFFSTATUS =='EMPLOYED') %>%
                                  left_join(RO_Offices %>% distinct(LGArea,RO_Office,LATITUDE_RO,LONGITUDE_RO),by=c('AREACODE'='LGArea'))


if (nrow(hired_staff_lookup) > 0 ) {

## CLG2001ualte the distance between the staff address and the return office 
#for (i in 1:nrow(hired_staff_lookup)) {
  
  from <- data.frame(Lon = hired_staff_lookup$LONGITUDE,
                     Lat = hired_staff_lookup$LATITUDE)
  
  to <- data.frame(Lon = hired_staff_lookup$LONGITUDE_RO,
                   Lat = hired_staff_lookup$LATITUDE_RO)
  
  ## Bind the adresses into a link and ping google
  baseurl <- paste0("https://maps.googleapis.com/maps/api/distancematrix/json?&origins=",from$Lat,
                    "+",from$Lon,"&destinations=",to$Lat,"+",to$Lon,
                    "&alternatives=false&units=metric&mode=driving&key=",api_key)
  
  
  ## Storing the distances
  individual_distances <- data.frame() 
  for (i in 1:length(baseurl)) {
    print(paste0(i,'/',length(baseurl)))
        download.file(baseurl[i], destfile = "ram.html")
    from_to <- as.data.frame(read_json("ram.html"))
    
    if(from_to$status == 'OK') {
      
      colnames(from_to) <- c("From","To","DISTKM","DRIVINGDISTMETERS","TIMEMINS","TIMESEC","Status","Status2")
    } else {
      
      from_to = data.frame(From = NA,
                           To = NA,
                           DISTKM = NA,
                           DRIVINGDISTMETERS = NA,
                           TIMEMINS = NA,
                           TIMESEC = NA,
                           Status = NA,
                           Status2 = NA)
      }
    
    individual_distances <- individual_distances %>% rbind(from_to)
  }
  



## Now binding on the distances 
newly_hired_staff <- hired_staff_lookup %>% cbind(individual_distances %>% dplyr::select(DRIVINGDISTMETERS, TIMESEC)) %>%
                                       mutate(POSITION = as.character(POSITION)) 
  


# filter out the change in employment status
# hired_staff <- EMA_staffing %>% filter(STAFFSTATUS == 'ACTIVE') %>% select(ELECTORID) %>% left_join(hired_staff) %>%
#                                   filter(STAFFSTATUS =='EMPLOYED')

# here join back hired staff and write to hired_staff and make sure it's the same with the dataframe



sqlExecute(hyperion_connection,
           "INSERT INTO [ProcessingData].[dashboard].[staffing_already_hired]
           (
           [ELECTIONEVENTID]
      ,[ELECTORID]
      ,[LOGINID]
      ,[AREACODE]
      ,[STAFFSTATUS]
      ,[CREATIONDATE]
      ,[POSITION]
      ,[RO_OFFICE]
      ,[LATITUDE_RO]
      ,[LONGITUDE_RO]
      ,[DRIVINGDISTMETERS]
      ,[TIMESEC]
           ) VALUES (?,?,?,?,
           ?,?,?,?,
           ?,?,?,?
          )",
           data = newly_hired_staff %>% dplyr::select(ELECTIONEVENTID
                                                      ,ELECTORID
                                                      ,LOGINID
                                                      ,AREACODE
                                                      ,STAFFSTATUS
                                                      ,CREATIONDATE
                                                      ,POSITION
                                                      ,RO_Office
                                                      ,LATITUDE_RO
                                                      ,LONGITUDE_RO
                                                      ,DRIVINGDISTMETERS
                                                      ,TIMESEC))
}

# all database stuff sorted above, below join back to demographics and start mapping

  
Final_Staffing_data <- Staff_split_RefreshedHiringStatus %>% 
  left_join(stored_hired, by = c("ELECTIONEVENTID", "LOGINID", "ELECTORID", "AREACODE",'STAFFSTATUS')) %>%
            rename(RO_Office = RO_OFFICE)


if (FALSE) {
  # manual override
  Final_Staffing_data <- Final_Staffing_data %>% mutate(STAFFSTATUS = ifelse(LOGINID == 'G0097362','EOI',STAFFSTATUS))
  
}

Final_Staffing_data <- bind_rows(
       
       Final_Staffing_data %>% filter(STAFFSTATUS == 'EOI') %>% dplyr::select(-CREATIONDATE
                                                                       ,-LATITUDE_RO
                                                                       ,-LONGITUDE_RO
                                                                       ,-RO_Office) %>%
         left_join(RO_Offices, by=c('AREACODE'='LGArea')) %>%
         left_join(EMA_staffing %>% dplyr::select(-PREFERENCEORDER,-LOCATIONID,-PREFERENCEORDER
                                           ,-POSITIONTYPECODE
                                           ,-CREATIONDATE,-STAFFSTATUS
                                           ,-STAFFTYPECODE) %>% 
                     distinct(), by = c("ELECTIONEVENTID", "ELECTORID", "LOGINID", "AREACODE"
                     )) 
        ,
       Final_Staffing_data %>% filter(STAFFSTATUS != 'EOI') %>%
         left_join(EMA_staffing %>% dplyr::select(-PREFERENCEORDER,-LOCATIONID,-PREFERENCEORDER
                                                   ,-POSITIONTYPECODE
                                                  ,-CREATIONDATE,-STAFFSTATUS
                                                   ,-STAFFTYPECODE) %>% 
                             distinct(), by = c("ELECTIONEVENTID", "ELECTORID", "LOGINID", "AREACODE"
                              )) %>% mutate(LATITUDE_RO = as.double(LATITUDE_RO)
                                           ,LONGITUDE_RO = as.double(LONGITUDE_RO))
                  ) 


# we tried to fix people's hiring status from location position an -------- this is a patch here
Final_Staffing_data <- Final_Staffing_data %>% left_join(Location_positions %>% select(LOGINID, POSITIONSTATUS, AREACODE) %>%
                                            left_join(RO_Offices, by=c('AREACODE' = 'LGArea')) %>% 
                                            select(LOGINID
                                                  ,POSITIONSTATUS
                                                  ,AREACODE_loc = AREACODE
                                                  ,RO_Office_loc = RO_Office
                                                  ,LATITUDE_RO_loc = LATITUDE_RO
                                                  ,LONGITUDE_RO_loc = LONGITUDE_RO)
                                          ,by = 'LOGINID') %>% 
       #     mutate(POSITIONSTATUS = ifelse(is.na(POSITIONSTATUS),' ',POSITIONSTATUS)
       #            ,AREACODE_loc = ifelse(is.na(AREACODE_loc),' ',AREACODE_loc)
       #            ,RO_Office_loc = ifelse(is.na(RO_Office_loc),' ',RO_Office_loc)
       #            ,LATITUDE_RO_loc = ifelse(is.na(LATITUDE_RO_loc),' ',LATITUDE_RO_loc)
       #            ,LONGITUDE_RO_loc = ifelse(is.na(LONGITUDE_RO_loc),' ',LONGITUDE_RO_loc)) %>%
                                 
  # override status and areacode
            mutate(STAFFSTATUS = ifelse(!is.na(POSITIONSTATUS), POSITIONSTATUS, STAFFSTATUS)
                   ,AREACODE = ifelse(!is.na(POSITIONSTATUS), AREACODE_loc, AREACODE)
                   ,RO_Office = ifelse(!is.na(POSITIONSTATUS), RO_Office_loc, AREACODE)
                   ,LATITUDE_RO = ifelse(!is.na(POSITIONSTATUS), LATITUDE_RO_loc, AREACODE)
                   ,LONGITUDE_RO = ifelse(!is.na(POSITIONSTATUS), LONGITUDE_RO_loc, AREACODE)
                  )

EOI <- Final_Staffing_data %>% filter(STAFFSTATUS != 'EMPLOYED') %>% dplyr::select(-DRIVINGDISTMETERS,-TIMESEC) %>%
         left_join(staffing_distance, by = c('LOGINID','ELECTORID','AREACODE','ELECTIONEVENTID')) %>%
         left_join(Unique_Venues %>% dplyr::select(StreetAddressID,VenueName) %>% distinct()
                                                                        , by=c('STREETADDRESSID'='StreetAddressID'
                                                                                #    ,'AREACODE'='LGArea'  #fix na in applicant problem, also removed lga in unique venues and distincted could create other problems
                                                                                        )) %>% distinct() %>%
    left_join(staffing_EOI_applicants_addresses,by=c('ELECTIONEVENTID','LOGINID','AREACODE','ELECTORID')) %>%
            filter(!is.na(VenueName)) %>%
            mutate(NEAREST = paste0('NEAREST_',NEAREST),
                   Venue = paste0(VenueName,' - ', round(DRIVINGDISTMETERS/1000,1),'KM - ',ceiling(TIMESEC/60),' mins')) %>%
  dplyr::select(-DRIVINGDISTMETERS, -TIMESEC,-VenueName,-STREETADDRESSID) %>% filter(!is.na(LATITUDE)) %>% 
                                                  distinct() %>% group_by(LOGINID,AREACODE,NEAREST) %>% mutate(rank = order(Venue)) %>% 
                                                filter(rank==1) %>% select(-rank) %>%
            spread(NEAREST,Venue) %>%
  rename(lat=LATITUDE,long = LONGITUDE) %>%
  dplyr::select(-PREVIOUSPOSITION,-SECONDLANGUAGE) %>%
  left_join(exp_lan,by='LOGINID')



# EMPLOYED <- Final_Staffing_data %>% filter(STAFFSTATUS == 'EMPLOYED') %>%
#                     left_join(staffing_EOI_applicants_addresses,by=c('ELECTIONEVENTID','LOGINID','AREACODE','ELECTORID'))  %>%
#   rename(lat=LATITUDE,long = LONGITUDE) %>% distinct() %>%
#   left_join(training_status %>% dplyr::select(LOGINID,Trained = VALUEENTERED)) %>%
#   mutate(Trained = ifelse(is.na(Trained),'N','Y')) %>%
#   dplyr::select(-PREVIOUSPOSITION,-SECONDLANGUAGE) %>%
#   left_join(exp_lan,by='LOGINID') 


EMPLOYED <- Location_positions %>% filter(POSITIONSTATUS == 'EMPLOYED') %>%
  left_join(staffing_EOI_applicants_addresses %>%
              select(LOGINID
                     ,LATITUDE
                     ,LONGITUDE),by= 'LOGINID')  %>%
  rename(lat=LATITUDE,long = LONGITUDE) %>% distinct() %>%
  left_join(training_status %>% dplyr::select(LOGINID,Trained = VALUEENTERED)) %>%
  mutate(Trained = ifelse(is.na(Trained),'N','Y')) %>%
  left_join(exp_lan,by='LOGINID') %>%
  left_join(EMA_staffing %>% dplyr::select(LOGINID, ATSI, AgeAtElection) %>% distinct()) 



## split emo and vc from venue hire status table yet to be created
EMOs <- Potential_Venues_raw %>% filter(LocationTypeCode == 'Returning Office') %>% 
  dplyr::select(EventID,VenueName,StreetAddressID,LocationTypeCode,LocationStatusCode
                                           ,Latitude ,Longitude)




Location_positions <- Location_positions %>% left_join(Vennus_RO_UPLOAD, by=c('StreetAddressID'='STREETADDRESSID'
                                                             ,'EventID'='EVENTID'))


EVCs <- Potential_Venues_raw %>% filter(LocationTypeCode == 'Pre-polling Place') %>% 
  filter(LocationStatusCode != "It's Away") %>%
  left_join(EMOs %>% dplyr::select(-StreetAddressID
                            ,-VenueName
                            ,-LocationTypeCode
                            ,-LocationStatusCode
                            ,RO_Office = VenueName 
                            ,RO_lat = Latitude,RO_long = Longitude),by=c('EventID'))  %>% 
  left_join(Vennus_RO_UPLOAD, by  =c('StreetAddressID'='STREETADDRESSID'
                                     ,'EventID'='EVENTID'))


Opened_Positions <- Location_positions %>% filter(LocationTypeCode == 'Polling Place' & LocationStatusCode != "It's Away") %>%
  group_by(EventID,AREACODE,VenueName,StreetAddressID,POSITIONSTATUS) %>% summarise(NumberOfPositions = n()) %>%
  bind_rows(Location_positions %>% filter(LocationTypeCode == 'Polling Place' & LocationStatusCode == "It's Away") %>%
  group_by(EventID,AREACODE,VenueName,StreetAddressID,POSITIONSTATUS) %>% summarise(NumberOfPositions = n()) 
)

Opened_Positions$POSITIONSTATUS[is.na(Opened_Positions$POSITIONSTATUS)] <- 'OPEN'


toc <- unique(factor(Opened_Positions$POSITIONSTATUS))


Opened_Positions <- Opened_Positions %>%
  spread(POSITIONSTATUS,NumberOfPositions, fill =0) 


  if('EMPLOYED' %in% toc) {
    
    emp <- Opened_Positions$EMPLOYED
  } else {emp <- 0}

  if('OPEN' %in% toc) {
    
    op <- Opened_Positions$OPEN
  } else {op <- 0}
  if('OFFERED' %in% toc) {
    
    of <- Opened_Positions$OFFERED
  } else {of <- 0}
 



Opened_Positions$TotalPositions <- emp + op + of


Opened_Positions <- Opened_Positions %>%
              left_join(EMOs %>% dplyr::select(RO_Office = VenueName
                                ,EventID), by='EventID') %>% rename(LGAreaCode = AREACODE)
        
DIs <- Potential_Venues_raw %>% filter(LocationTypeCode == 'Declared Institution') %>%
 # filter(LocationStatusCode != "It's Away") %>%
  left_join(Vennus_RO_UPLOAD, by  =c('StreetAddressID'='STREETADDRESSID'
                                     ,'EventID'='EVENTID')) %>%
  left_join(EMOs %>% dplyr::select(EventID, RO_Office = VenueName)) 

VCs <- Potential_Venues_raw %>% filter(LocationTypeCode == 'Polling Place') %>%
  filter(LocationStatusCode != "It's Away") %>%
  left_join(Vennus_RO_UPLOAD, by  =c('StreetAddressID'='STREETADDRESSID'
                                     ,'EventID'='EVENTID')) %>%
  left_join(Opened_Positions) %>%
  filter(OPEN > 0)

Completed_VCs <- Potential_Venues_raw %>% filter(LocationTypeCode == 'Polling Place') %>%
  filter(LocationStatusCode != "It's Away") %>%
  left_join(Vennus_RO_UPLOAD, by  =c('StreetAddressID'='STREETADDRESSID'
                                     ,'EventID'='EVENTID')) %>%
  left_join(Opened_Positions) %>%
  filter(OPEN == 0)

Away_venues <- Potential_Venues_raw %>% filter(LocationTypeCode == 'Polling Place') %>%
  filter(LocationStatusCode == "It's Away") %>%
  left_join(Vennus_RO_UPLOAD, by  =c('StreetAddressID'='STREETADDRESSID')) %>%
  left_join(Opened_Positions) %>%
  filter(OPEN > 0)


Completed_Away_venues <- Potential_Venues_raw %>% filter(LocationTypeCode == 'Polling Place') %>%
  filter(LocationStatusCode == "It's Away") %>%
  left_join(Vennus_RO_UPLOAD, by  =c('StreetAddressID'='STREETADDRESSID')) %>%
  left_join(Opened_Positions) %>%
  filter(OPEN == 0)


## Mapping code
circle_color <- c('#7bc7c6','#de2a93','#37b324')
circle_label <- c('EO application','SOA appilcation','Hired/Active')



# End database connections ------------------------------------------------
   

}


# summary page 

Past_exp <- bind_rows(EOI %>% ungroup() %>% dplyr::select(PREVIOUSPOSITION) %>% mutate(Status = 'EOI')
                      ,EMPLOYED %>% dplyr::select(PREVIOUSPOSITION) %>% mutate(Status = 'Hired')
)
 
gauge_exp <- round(nrow(Past_exp %>% filter(!is.na(PREVIOUSPOSITION)))/nrow(Past_exp),2)*100
gauge_exp_hired <- round(nrow(Past_exp %>% filter(!is.na(PREVIOUSPOSITION), Status == 'Hired'))/nrow(Past_exp),2)*100

election_day_staff <- Location_positions %>% filter(STAFFTYPECODE == 'PP') %>% left_join(training_status) 

gauge_ed_hired <- 100 * round(nrow(election_day_staff %>% filter(POSITIONSTATUS == 'EMPLOYED'))/nrow(election_day_staff),2)
gauge_ed_trained <- 100 * round(nrow(election_day_staff %>% filter(POSITIONSTATUS == 'EMPLOYED',!is.na(VALUEENTERED)))/nrow(election_day_staff %>% filter(POSITIONSTATUS == 'EMPLOYED')),2)


ATSI <- bind_rows(EOI %>% dplyr::select(ATSI) %>% mutate(Status = 'EOI')
                  ,EMPLOYED %>% dplyr::select(ATSI) %>% mutate(Status = 'Hired')
)

gauge_ATSI_hired <- nrow(ATSI %>% filter(Status =='Hired',ATSI == 'Y'))

age_dist <- bind_rows(EOI %>% dplyr::select(AgeAtElection) %>% mutate(Status = 'EOI')
                      ,EMPLOYED %>% dplyr::select(AgeAtElection) %>% mutate(Status = 'Hired')
)

if (nrow(age_dist) >0) {
  
    agehist <- 
               hist(age_dist$AgeAtElection)
    
  # density(age_dist$AgeAtElection[!is.na(age_dist$AgeAtElection)])
  
} else {
  
  agehist <- data.frame()
}



Lan <- bind_rows(EOI %>% dplyr::select(SECONDLANGUAGE) %>% mutate(Status = 'EOI')
                 ,EMPLOYED %>% dplyr::select(SECONDLANGUAGE) %>% mutate(Status = 'Hired')
)
if (nrow(Lan) > 0) {
  
  table_lan <- CrossTab(Lan, 'SECONDLANGUAGE','Status')
  
  Lan <- Lan %>% mutate(SECONDLANGUAGE = ifelse(is.na(SECONDLANGUAGE),'NONE',SECONDLANGUAGE))
  
  table_lan <- as.data.frame(table(Lan$SECONDLANGUAGE,Lan$Status)) %>% select(SECONDLANGUAGE = Var1,Status = Var2,Count = Freq)
  
  lan_stack <- ggplot(table_lan, aes(fill=Status, y=Count, x=SECONDLANGUAGE)) + 
    geom_bar(position="stack", stat="identity")
  
  
} else {
  
  table_lan <- data.frame()



}


# training status----------------------------------------------------------------
# where its position based in calculating summary we turn to the location_position table to minimise discrepency and this shall be investigated further.
# 
# offered <- Location_positions %>% filter(POSITIONSTATUS %in% c('OFFERED')) %>%
#   left_join(staffing_EOI_applicants_addresses,by=c('ELECTIONEVENTID','LOGINID','AREACODE'))  %>%
#   rename(lat=LATITUDE,long = LONGITUDE) %>% distinct() %>%
#   left_join(training_status %>% dplyr::select(LOGINID,Trained = VALUEENTERED)) %>%
#   mutate(Trained = as.character(ifelse(is.na(Trained),'N','Y'))) %>%
# #  dplyr::select(-PREVIOUSPOSITION,-SECONDLANGUAGE) %>%
#   left_join(exp_lan,by='LOGINID')


# 
# 
# offered <- Final_Staffing_data %>% filter(STAFFSTATUS %in% c('OFFERED')) %>%
#                           left_join(staffing_EOI_applicants_addresses,by=c('ELECTIONEVENTID','LOGINID','AREACODE','ELECTORID'))  %>%
#                           rename(lat=LATITUDE,long = LONGITUDE) %>% distinct() %>%
#                           left_join(training_status %>% dplyr::select(LOGINID,Trained = VALUEENTERED)) %>%
#                           mutate(Trained = as.character(ifelse(is.na(Trained),'N','Y'))) %>%
#                           dplyr::select(-PREVIOUSPOSITION,-SECONDLANGUAGE) %>%
#                           left_join(exp_lan,by='LOGINID') %>%
#   dplyr::select(-POSITION) %>% 
#   left_join(Location_positions %>% filter(POSITIONSTATUS == 'OFFERED') %>% mutate(POSITION = paste0(VenueName,'-',POSITIONTYPECODE)) %>% dplyr::select(LOGINID,POSITION) %>% distinct())
# 


# as staff are so offerred multiple positions positions employed now does a count of position held
# need to amend to filter list style later

Location_simp_positions <- Hiring_status %>% mutate(POSITIONTYPECODE = gsub("[^[:alnum:] ]", "", POSITIONTYPECODE)) %>%
  mutate(POSITIONTYPECODE = gsub('[[:digit:]]+', '', POSITIONTYPECODE)) %>%
  mutate(POSITIONSTATUS = ifelse(POSITIONSTATUS == 'ACCEPTED', 'EMPLOYED', POSITIONSTATUS)) %>%
  left_join(training_status %>% select(LOGINID, Trained = VALUEENTERED), by = 'LOGINID') %>%
  mutate(Trained = ifelse(is.na(Trained),'N', 'TRAINED')) # %>% 
#  mutate(POSITIONSTATUS = ifelse((Trained == 'Y' & POSITIONSTATUS == 'EMPLOYED'), 'TRAINED', POSITIONSTATUS))

# position_types <- c('DPPM','DVIO','EO','OA','OACC','PPM','SOACC','SOACR', 'SOAEM', 'SOAPP', 'SOASV')

offered <- CrossTab(Location_simp_positions %>% filter(POSITIONSTATUS == 'OFFERED'), 'POSITIONTYPECODE', 'POSITIONSTATUS')
  
employed <- CrossTab(Location_simp_positions %>% filter(POSITIONSTATUS == 'EMPLOYED',Trained == 'N'), 'POSITIONTYPECODE', 'POSITIONSTATUS')
  
trained <- CrossTab(Location_simp_positions %>% filter(POSITIONSTATUS == 'EMPLOYED',Trained == 'TRAINED') %>% mutate(POSITIONSTATUS ='TRAINED'), 'POSITIONTYPECODE', 'POSITIONSTATUS')

open <- CrossTab(Location_simp_positions %>% filter(POSITIONSTATUS == 'OPEN'), 'POSITIONTYPECODE', 'POSITIONSTATUS')

training_graph_data <- bind_rows(open,offered,employed,trained)


# training_stack <- ggplot(training_graph_data, aes(fill=status, y=count, x=Type)) + 
#   geom_bar(position="stack", stat="identity")

training_stack_ed_data <- training_graph_data %>% filter(POSITIONTYPECODE %in% c('DVIO','EO','PPM ','DPPM'))

training_stack_else_data <- training_graph_data %>% filter(POSITIONTYPECODE %!in% c('DVIO','EO','PPM ','DPPM'))


training_ed_stack <- hchart(training_stack_ed_data, "column", color = c('red','lightblue','black','lightgreen'), hcaes(x = "POSITIONTYPECODE", y = "Count", group = "POSITIONSTATUS")) %>%
                  hc_plotOptions(column = list(stacking = "normal"))

training_else_stack <- hchart(training_stack_else_data, "column", color = c('red','lightblue','black','lightgreen'), hcaes(x = "POSITIONTYPECODE", y = "Count", group = "POSITIONSTATUS")) %>%
  hc_plotOptions(column = list(stacking = "normal"))



# EOG figures -------------------------------------------------------------

 
 Summary_status <- CrossTab(Location_simp_positions, 'POSITIONTYPECODE','POSITIONSTATUS') %>% arrange(POSITIONTYPECODE) %>%
                              spread(POSITIONSTATUS, Count) %>% 
                           mutate(Total = EMPLOYED + OFFERED + OPEN) %>%
                            mutate(Completion = formattable::percent(round((EMPLOYED)/Total,4))) %>%
                           left_join(CrossTab(Location_simp_positions, 'POSITIONTYPECODE', 'Trained') %>% filter(Trained != 'N') %>%
                                          select(POSITIONTYPECODE, TRAINED = Count)) %>%
                          select(POSITIONTYPECODE, OPEN, OFFERED, EMPLOYED, Total, TRAINED, everything())
 
 
 
 
 
 
 Summary_status <- Summary_status %>% bind_rows(data.frame(POSITIONTYPECODE = 'Total'
                                        ,OPEN = sum(Summary_status$OPEN)
                                        ,OFFERED = sum(Summary_status$OFFERED)
                                        ,EMPLOYED = sum(Summary_status$EMPLOYED)
                                        ,Total = sum(Summary_status$Total)
                                        ,TRAINED = sum(Summary_status$TRAINED)
                                        ,Completion =  formattable::percent(round(sum(Summary_status$EMPLOYED)/sum(Summary_status$Total),4))))

 
 library(formattable)
 
 
 boldlastrow <- formatter("span", 
                                     style = x ~ style(
                                       font.weight = "bold"
                                     ))
 
 Summary_status_table <- formattable::formattable(Summary_status, 
                                 list(`Completion` = formattable::formatter("span", 
                                                                            style = x ~ style(color = ifelse(x == 1, 'green' 
                                                                                                             ,ifelse(x > 0.9, 'lightgreen' 
                                                                                                             ,ifelse(x < 0.50, "red", "blue")))
                                                                                              )
                                                                            )
                                      ,TRAINED = color_tile('lightgreen','lightgreen')
                                                                                          

                                      ,area(row = nrow(Summary_status), col = 1) ~ boldlastrow))
                                              

 
 
 RO_Summary <- CrossTab(Location_simp_positions %>% left_join(RO_Offices, by=c('AREACODE'='LGArea')), 'RO_Office','POSITIONSTATUS') %>% 
                              spread(POSITIONSTATUS, Count) %>% select(RO_Office, OPEN, OFFERED, everything()) %>%
                          left_join(CrossTab(Location_simp_positions %>% left_join(RO_Offices, by=c('AREACODE'='LGArea')), 'RO_Office', 'Trained') %>% filter(Trained != 'N') %>%
                                      select(RO_Office, TRAINED = Count)) %>%
                           mutate(Total = EMPLOYED + OFFERED + OPEN) %>%
   
                          mutate(Completion = paste0((round((EMPLOYED)/Total,4))*100,'%')) %>%
                      # mutate(Completion = formattable::percent(round((EMPLOYED)/(EMPLOYED + OFFERED + OPEN),4))) %>%
                              arrange(RO_Office)
 
 
 available_EOI_RO <- EMA_staffing %>% filter(STAFFSTATUS == 'EOI') %>% 
                           left_join(RO_Offices, by=c('AREACODE'='LGArea')) %>% select(LOGINID, RO_Office) %>% distinct() %>%
                           group_by(RO_Office) %>%
                           summarise(`Available Applicants` = n())
 
 available_EOI_LGA <- EMA_staffing %>% filter(STAFFSTATUS == 'EOI') %>% 
                            left_join(RO_Offices, by=c('AREACODE'='LGArea')) %>% select(LOGINID, RO_Office, AREACODE) %>% distinct() %>%
                            group_by(RO_Office, AREACODE) %>%
                            summarise(`Available Applicants` = n())
 
 
 RO_Summary <- RO_Summary %>% left_join(available_EOI_RO) %>% 
    # remove a word to save space
                    mutate(RO_Office = gsub('RO Office','',RO_Office)) %>%
                                                      select(RO_Office
                                                                     ,OPEN 
                                                                     ,OFFERED 
                                                                     ,EMPLOYED 
                                                                     ,Total
                                                                     ,everything()
                                                                    )
                                                                 
 

 
 LGA_Summary <- CrossTab(Location_simp_positions, 'AREACODE','POSITIONSTATUS') %>% 
                             spread(POSITIONSTATUS, Count) %>% select(AREACODE, OPEN, OFFERED, everything()) %>%
                             left_join(CrossTab(Location_simp_positions, 'AREACODE', 'Trained') %>% filter(Trained != 'N') %>%
                                         select(AREACODE, TRAINED = Count)) %>%
                            mutate(Total = EMPLOYED + OFFERED + OPEN) %>%
                            mutate(Completion = paste0((round((EMPLOYED)/Total,4))*100,'%')) %>%
                             # mutate(Completion = formattable::percent(round((EMPLOYED)/(EMPLOYED + OFFERED + OPEN),4))) %>%
   
                        left_join(available_EOI_LGA) %>% 
                             mutate(RO_Office = gsub('RO Office','',RO_Office)) %>%
   
                                                            select(RO_Office
                                                                ,LGArea = AREACODE
                                                               ,OPEN 
                                                               ,OFFERED 
                                                               ,EMPLOYED 
                                                               ,Total
                                                               ,everything()
                                                               ) %>%
                             arrange(RO_Office)
 
 
 shortageAnalysis <- FALSE # used for staffing shortage analysis
 
 if (shortageAnalysis) {
 
 all_venues <- Opened_Positions %>% mutate(KPI = (TotalPositions - OPEN)/TotalPositions)
 
 group_up <- all_venues %>% mutate(under_60 = KPI < 0.6,
                                   under_40 = KPI < 0.4) %>%
                        group_by(RO_Office) %>%
                       summarise(TotalVenues = n()
                                 ,under_60 = sum(under_60)
                                 ,under_40 = sum(under_40)
                                 ,AverageHiringStatus = round(mean(KPI),2)
                                 ,TotalPositions = sum(TotalPositions)
                                 ,TotalHired = sum(TotalPositions) - sum(OPEN))
   
 
 
 
 
 
 }
 
 
 

# Additional reports ------------------------------------------------------


 # Open PPM DPPM positions
 ppm_dppm <- Hiring_status %>%
   dplyr::select(ELECTIONEVENTID
                 ,AREACODE
                 ,STAFFTYPECODE
                 ,LOCATIONID
                 ,POSITIONSTATUS
                 ,POSITIONTYPECODE) %>%
   filter(grepl('PPM', POSITIONTYPECODE)) %>%
   
   mutate(POSITIONTYPECODE = word(POSITIONTYPECODE,1)) %>% distinct() %>%
   spread(POSITIONTYPECODE, POSITIONSTATUS) %>%
   filter(!DPPM %in% c('EMPLOYED','OFFERED') | !PPM %in% c('EMPLOYED','OFFERED')) %>%
   mutate(LOCATIONID = str_sub(LOCATIONID, 15, 21)
          ,LOCATIONID = as.numeric(LOCATIONID)) %>%
   dplyr::select(LGAreaCode = AREACODE
                 ,StreetAddressID = LOCATIONID
                 ,PPM
                 ,DPPM)
   
 
 # Open positions report
 open_positions_summary <- Opened_Positions %>% 
   ungroup() %>%
   left_join(ppm_dppm,
             by = c('LGAreaCode', 'StreetAddressID')) %>%
   mutate(PPM = replace_na(PPM, 'EMPLOYED')
          ,DPPM = replace_na(DPPM, 'EMPLOYED')) %>%
   
   dplyr::select(RO_Office, everything(),-EventID, -StreetAddressID)
 
 
 # Recent applicants
 # Staffing have requested a report that lists only new applicants since a given date
 applicants_since_date <- as.Date('2021-11-27')
 
 applicants_since_date_data <- EMA_staffing %>%
   mutate(CREATIONDATE = as.Date(CREATIONDATE)) %>%
   filter(CREATIONDATE >= applicants_since_date) %>%
   filter(STAFFSTATUS == 'EOI') %>%
   left_join(RO_Offices %>%
               select(RETURNINGOFFICE = RO_Office
                      ,AREACODE = LGArea)
             ,by = 'AREACODE') %>%
   select(
     ELECTIONEVENTID
     ,RETURNINGOFFICE
     ,AREACODE
     ,CREATIONDATE
     ,STAFFSTATUS
     ,LOGINID
     ,SURNAME
     ,GIVENNAMES
     ,PREFERREDFIRSTNAME
     ,HOMEPHONE
     ,WORKPHONE
     ,MOBILEPHONE
   ) %>%
   distinct() %>%
   
   group_by(across(c(-AREACODE))) %>%
   
   summarise(AREACODE = paste0(AREACODE, collapse = '; '))
 
 
 

# Write to file -----------------------------------------------------------

 
 
 setwd('//SVRANALYTICS1/AnalyticsReports/LG2001 Dashboard/Data/')
 write.csv(open_positions_summary
           ,'Staffing Venue hiring status.csv'
           ,na = ""
           ,row.names = FALSE)
 
 setwd(staffing_secured_backup_folder)
 write.csv(applicants_since_date_data
           ,'Recent_applicants.csv'
           ,na = ""
           ,row.names = FALSE)
 
 
# knitting and etc --------------------------------------------------------


setwd(root_dashboard)
rmarkdown::render(paste0(rmd_files,'Sub pages/','02_03 EOI summary.Rmd'),
                  output_file = paste0('-Staffing EOI summary.HTML'),
                  output_dir = paste0(server_root_subpages))




CONTESTED_ROS <- EMS_Venues %>% select(RO_Office,LGArea) %>%
  filter(LGArea %in% Contest[Contest$CONTESTSTATUSTYPECODE == 'Contested',]$AREACODE)

RO_Centroids <- RO_Centroids %>% 
  filter(RO_OFFICE %in% CONTESTED_ROS$RO_Office)


