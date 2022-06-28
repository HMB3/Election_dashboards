# this file is ran manually once when theres a venue change etc

Contests <- Contest_details %>% mutate(EventID = substr(ContestID,1,10))

## split emo and vc from venue hire status table yet to be created
EMOs <- Potential_Venues_raw %>% filter(LocationTypeCode == 'Returning Office') %>% 
  dplyr::select(EventID,VenueName,StreetAddressID,LocationTypeCode,LocationStatusCode
                ,Latitude ,Longitude)


RO_Offices <- Potential_Venues_raw %>% 
  filter(LocationTypeCode == 'Returning Office') %>%
  left_join(Contests, by='EventID') %>% filter(ContestStatusTypeCode == 'Contested') %>% 
  dplyr::select(EventID,RO_Office = VenueName,LATITUDE_RO = Latitude,LONGITUDE_RO = Longitude,LGArea) %>%
  distinct()


  Vennus_RO <- Potential_Venues_raw %>% #filter(LocationTypeCode == 'Pre-polling Place') %>% 
    filter(LocationStatusCode != "It's Away") %>%
    left_join(EMOs %>% dplyr::select(-StreetAddressID
                                     ,-VenueName
                                     ,-LocationTypeCode
                                     ,-LocationStatusCode
                                     ,RO_lat = Latitude,RO_long = Longitude),by=c('EventID')) 
  
  baseurl <- paste0("https://maps.googleapis.com/maps/api/distancematrix/json?&origins=",Vennus_RO$Latitude,
                    "+",Vennus_RO$Longitude,"&destinations=",Vennus_RO$RO_lat,"+",Vennus_RO$RO_long,
                    "&alternatives=false&units=metric&mode=driving&key=",api_key)
  
  
  prp_distances <- data.frame() 
  for (i in 1:length(baseurl)) {
    print(paste0(i,'/',length(baseurl)))
    download.file(baseurl[i], destfile = "ram.html")
    from_to <- as.data.frame(read_json("ram.html"))
    
    if(from_to$status == 'OK') {
      
      colnames(from_to) <- c("From","To","DistKM","DrivingDist","Time","TimeSec","Status","Status2")
    } else {
      
      from_to = data.frame(From = NA,
                           To = NA,
                           DistKM = NA,
                           DrivingDist = NA,
                           Time = NA,
                           TimeSec = NA,
                           Status = NA,
                           Status2 = NA)
    }
    
    prp_distances <- prp_distances %>% rbind(from_to)
    
    
  }
  
  Vennus_RO <- Vennus_RO %>% bind_cols(prp_distances %>% dplyr::select(TimeSec))
  
  Vennus_RO_UPLOAD <- Vennus_RO %>% dplyr::select(EventID,StreetAddressID,TimeSec)
  
  
  sqlExecute(hyperion_connection,
             "DELETE [ProcessingData].[dashboard].[staffing_VC_to_RO]
             where EVENTID like ?
           DBCC CHECKIDENT ('[ProcessingData].[dashboard].[staffing_VC_to_RO]', RESEED, 0)",
             paste0(event_group_ID,'%'),
             fetch = FALSE)
  
  sqlExecute(hyperion_connection,
             "INSERT INTO [ProcessingData].[dashboard].[staffing_VC_to_RO]
           (
       [EVENTID]
      ,[STREETADDRESSID]
      ,[TimeSec]
           ) VALUES (?,?,?
           )",
             data = Vennus_RO_UPLOAD)
  
  # centroid and regeion
  
  
  setwd(data_files)
  LG2001 <- rgdal::readOGR(dsn="Compact_LGA", layer="Compact_LGA")
#  LG2001 <- rgdal::readOGR(dsn="NSW_LGA_POLYGON_shp", layer="NSW_LGA_POLYGON_shp")
#  LG2001@data <- LG2001@data %>% rename(LGAreaCode = NSW_LGA__3)
#  ram1 <- LG2001@data
  
  centeroids <- data.frame()
  for(i in 1:length(unique(RO_Offices$RO_Office))) {
    
    
    centroidcalc <- RO_Offices %>% filter(RO_Office == unique(RO_Offices$RO_Office)[i])
    
    ram1 <- subset(LG2001, LGAreaCode %in% centroidcalc$LGArea)
   # ram1 <- subset(LG2001, LGAreaCode %in% toupper(centroidcalc$LGArea))
    
    centeroids_ind <- gCentroid(ram1, byid=FALSE)@coords %>% cbind(data.frame(RO_Offices = unique(RO_Offices$RO_Office)[i]
                                                                              ,size_weight =  sum(raster::area(ram1))/1000000)) %>%
      mutate(size_of_map = case_when(size_weight >250000 ~ 6,
                                     size_weight >100000 ~ 7,
                                     size_weight >10000 ~ 8,
                                     size_weight > 5000 ~ 9,
                                     size_weight > 1000 ~ 10,
                                     size_weight > 100 ~ 11,
                                     T ~ 12))
    
    centeroids <- centeroids %>% bind_rows(centeroids_ind)
    
  }
  centeroids <- centeroids %>% mutate(EVENTGROUPID = event_group_ID)
  # upoad centroids
  sqlExecute(hyperion_connection,
             "DELETE [ProcessingData].[dashboard].[staffing_RO_centroids]
             where EVENTGROUPID = ?
           DBCC CHECKIDENT ('[ProcessingData].[dashboard].[staffing_RO_centroids]', RESEED, 0)",
             event_group_ID,
             fetch = FALSE)
  
  
  
  sqlExecute(hyperion_connection,
             "INSERT INTO [ProcessingData].[dashboard].[staffing_RO_centroids]
           (
       [LONGITUDE]
      ,[LATITUDE]
      ,[RO_OFFICE]
      ,[SIZE_WEIGHT]
      ,[SIZE_MAP]
      ,EVENTGROUPID
           ) VALUES (?,?,?,?,?,?
           )",
             data = centeroids)
  
  
  
  
  
  
  