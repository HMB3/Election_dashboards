####################################################################################################################
###################################### CANDIDATE NOMINATIONS DASHBAORD ---- ########################################
####################################################################################################################


## This code creates the 'Candidate nominations' dashboard
## Note that this code refers to EC databases which change over time - so the output will be different 
## before and after the election

## Split this in two

## Is this dashboard still needed? If so, how do we need to change it....
message('Run R code to create Candidate nominations dashboard')





## 1). PULL EARLY DATA FROM DATABASES ================================================================================


## Shared with venues, duplicated so that each file can run independently if needed
Pre_poll_projections <- sqlExecute(hyperion_connection,
                                   "SELECT * FROM [ElectionData].[Views].[VenueProjectionsPrepoll]",
                                   fetch = TRUE,
                                   stringsAsFactors = FALSE)


## 
DI_Visits <- sqlExecute(hyperion_connection,
                        "SELECT * FROM [ElectionData].[Views].[VenueDI]",
                        fetch = TRUE,
                        stringsAsFactors = FALSE) %>%
  filter(LocationStatusCode == 'Visit Required')


## Run SQL on EMS connection
opening_hour <- sqlExecute(EMS_connection, "SELECT  
                           evp.EventID,
                           ea.AreaCode,
                           evp.VenueName,
                           el.[LocationStatusCode],
                           datename(dw, date) + ', ' + CONVERT(varchar, date, 106) OpeningDate, 
                           Date,
                           IsOpen,
                           loc.StreetAddressID,
                           evp.OpeningTime,
                           evp.ClosingTime
                           from Events.EMS_EventLocationOpeningHours evp
                           left join [EMS2009].[Resources].[EMS_Location] loc on evp.VenueName = loc.VenueName
                           left join [EMS2009].[Events].EMS_Contest ea on EVP.EventID= substring(ea.ContestID,1,10)
                           left join [EMS2009].[Events].[EMS_EventLocation] el on loc.VenueName = el.VenueName
                           and el.EventID = evp.EventID
                           where evp.EventID like ?
                           AND ea.ContestStatusTypeCode = 'Contested'
                           AND el.LocationStatusCode NOT IN ('Away Cancelled',
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
                           'Visit Not Required')
                           order by EventID, VenueName,Date", 
                           paste(event_group_ID,"%", sep = ""), TRUE,
                           stringsAsFactors = FALSE)


##  When EMA is down
if(TRUE) {
  
  Candidates <- read.csv('G:/Election Events/SGE 2019 Programme/AP9 Data Management/12 Deliverables/07 Dashboard, RMT and EOG/02 Source data/Candidates.csv') %>%
    select(-X)
  
  enrolment <- read.csv('G:/Election Events/SGE 2019 Programme/AP9 Data Management/12 Deliverables/07 Dashboard, RMT and EOG/02 Source data/enrolment.csv') %>%
    select(-X)
}


# if(FALSE) {
#   
# 
# Candidates <- sqlExecute(EMA_connection,
#                          "SELECT can.ELECTIONEVENTID,
#                          can.AREACODE,
#                          can.CONTESTID,
#                          can.CANDIDATEBALLOTNAME,
#                          pt.PARTYNAME,
#                          can.GROUPNUMBER,
#                          grp.grouplabel,
#                          can.DRAWNUMBER
#                          
#                          FROM PRD2008.ES_CANDIDATE can
#                          left join PRD2008.PM_PARTY pt on can.ENDORSEDBYPARTYID = pt.PARTYID
#                          left JOIN PRD2008.ES_GROUP grp on can.GROUPNUMBER = grp.GROUPNUMBER -- or (can.GROUPNUMBER is null)
#                          where can.ELECTIONEVENTID = ?
#                          and grp.ELECTIONEVENTID = ?
#                          and can.validatedbyHO is not null
#                          and can.AREACODE = 'NSW'
#                          
#                          -- order by GroupNumber, CandidateBallotName
#                          
#                          union
#                          
#                          SELECT can.ELECTIONEVENTID,
#                          can.AREACODE,
#                          can.CONTESTID,
#                          can.CANDIDATEBALLOTNAME,
#                          pt.PARTYNAME,
#                          can.GROUPNUMBER,
#                          null as grouplabel,
#                          can.DRAWNUMBER
#                          
#                          FROM PRD2008.ES_CANDIDATE can
#                          left join PRD2008.PM_PARTY pt on can.ENDORSEDBYPARTYID = pt.PARTYID
#                          where can.ELECTIONEVENTID = ?
#                          and can.validatedbyHO is not null
#                          and AREACODE != 'NSW'
#                          
#                          union
#                          
#                          SELECT can.ELECTIONEVENTID,
#                          can.AREACODE,
#                          can.CONTESTID,
#                          can.CANDIDATEBALLOTNAME,
#                          pt.PARTYNAME,
#                          can.GROUPNUMBER,
#                          null as grouplabel,
#                          can.DRAWNUMBER
#                          
#                          FROM PRD2008.ES_CANDIDATE can
#                          left join PRD2008.PM_PARTY pt on can.ENDORSEDBYPARTYID = pt.PARTYID
#                          where can.ELECTIONEVENTID = ?
#                          and can.validatedbyHO is not null
#                          and can.AREACODE = 'NSW'
#                          and can.GROUPNUMBER is null
#                          ",
#                          as.list(rep(event_group_ID,4)),
#                          fetch = TRUE,
#                          stringsAsFactors = FALSE) %>%
#   rename(AreaCode = AREACODE)
# 
# 
# enrolment <- sqlExecute(EMA_connection,
#                         "SELECT INITCAP(ELEC.NSWECSTATEDISTRICT) as AREACODE,
#                         COUNT(1) AS ENROLMENT
#                         FROM PRD2008.ES_ONTHEROLL OTR
#                         LEFT JOIN PRD2008.ES_ELECTOR ELEC
#                         ON OTR.ELECTORID = ELEC.ELECTORID
#                         WHERE OTR.ELECTIONEVENTID = ?
#                         GROUP BY NSWECSTATEDISTRICT
#                         ORDER BY NSWECSTATEDISTRICT",
#                         event_group_ID,
#                         fetch = TRUE,
#                         stringsAsFactors = FALSE) %>%
#   group_by(AREACODE) %>%
#   summarise(Enrolment = sum(ENROLMENT)) %>%
#   mutate(AreaCode = ifelse(AREACODE=='Ku-Ring-Gai','Ku-ring-gai',AREACODE))
# 
# }



# save enrolment in a csv when roll closes

# setwd()
# enrolment <- read.csv('')


## Pull markoff data from hyperion ----
Markoff <- sqlExecute(hyperion_connection, # change Postal into Pre-poll Ordinary when launch
                      "SELECT A.ELECTORID, 
                              A.ELECTIONEVENTID,
                              A.DECLARATIONEXCUSEVOTETYPECODE,
                              A.VOTEPROCESSEDDATE,
                              A.ISSUINGDISTAREACODE,
                              B.GAZETTEDNAME,
                              B.LOCATIONID
                              FROM [ElectionData].[Staging].[EMA_ES_DECEXCUSEVOTE] A
                              LEFT JOIN [ElectionData].[Staging].[EMA_ES_LOCATIONS] B
                                ON A.LOCATIONID = B.LOCATIONID
                                  AND A.ELECTIONEVENTID = B.ELECTIONEVENTID
                                  AND A.VOTINGCENTRETYPECODE = B.VOTINGCENTRETYPECODE
                            
                                WHERE A.ELECTIONEVENTID = ?
                                  AND A.VOTINGCENTRETYPECODE = 'Pre-Poll'
                                    AND A.DECLARATIONEXCUSEVOTETYPECODE = 'Pre-poll Ordinary'
                              ",
                      event_group_ID,
                      fetch = TRUE,
                      stringsAsFactors = FALSE) %>%
  mutate(VOTEPROCESSEDDATE = as.character(as.Date(VOTEPROCESSEDDATE, tz =  'Australia/Sydney')),
         StreetAddressID = as.integer(substring(LOCATIONID,15,25)))


## AoBP stuff ----
AoBP_read <- sqlExecute(hyperion_connection,
                        "
                        /****** Script for SelectTopNRows command from SSMS  ******/
              SELECT [ReturningOffice]
                                      ,[SGAreaCode]
                                      ,[ContestTypeCode]
                                      ,[LocationTypeCode]
                                      ,[VenueName]
                                      ,[Date]
                                      ,[PrintQuantity]
                                      ,[Allocated_BPs]
                                      ,[Daily_start_quantity]
                                      ,[Spoilt]
                                      ,[Discarded]
                                      ,[Prepoll_Ordinary]
                                      ,[Enrolment]
                                      ,[Absent]
                                      ,[NAMAV]
                                      ,[Unused]
                                      ,[LocationType]
                                      ,[Variance]
                                      FROM [ElectionData].[Staging].[AoBP_Prepoll_daily_account]
                        ",
                        fetch = TRUE,
                        stringsAsFactors = FALSE)



## STH
STH <- sqlExecute(hyperion_connection,
                  "SELECT 
                  [AddressName]
                  ,[Latitude]
                  ,[Longitude]
                  FROM [ElectionData].[Staging].[EMS_Address]
                  where AddressName like 'Sydney Town Hall'",
                  fetch = TRUE,
                  stringsAsFactors = FALSE) %>% distinct(Latitude, Longitude)


## Data grouping
setwd('G:/Elections Branch/Election Support/Data Analytics/Ad-hoc data requests/19014 - Prepoll report resurrection/01 Reference files')
daily_loading_matrix <- read.xlsx('DailyPrePollWeights.xlsx', sheet=1) %>% select(-Dates)


## Data processor

## Update
open_days <- opening_hour %>% filter(IsOpen == 'Y') %>% mutate(Date = as.character(Date))
ndays     <- open_days %>% group_by(EventID,AreaCode,VenueName) %>% mutate(key=row_number())


## 
All_EVCs <- Pre_poll_projections %>% 
  mutate(LocationTypeCode = case_when(LocationTypeCode == 'Pre-polling Place' ~ 'EVC',
                                      LocationTypeCode == 'Returning Office' ~ 'EMO',
                                      LocationTypeCode == 'Polling Place' ~ 'VC'))


## Create projection days data
Projection_days <- All_EVCs %>% arrange(EventID,AreaCode,VenueName) %>% 
  filter(LocationTypeCode =='EVC' | LocationTypeCode == 'EMO') %>%
  left_join(open_days, by = c('AreaCode','EventID','VenueName','StreetAddressID')) %>%
  
  group_by(EventID,AreaCode,VenueName,StreetAddressID,
           ProjectedVoters,Latitude,Longitude,LocationStatusCode,
           LocationTypeCode) %>%
  
  summarise(Days = n()) %>% 
  left_join(daily_loading_matrix,by=c('Days'='Days_Open')) %>%
  
  gather(key,value,-EventID,-AreaCode, -VenueName, -StreetAddressID, -Latitude, -Longitude,
         -LocationStatusCode, -LocationTypeCode,-Days,-ProjectedVoters) %>%
  mutate(Projection = round(ProjectedVoters*value,0)) %>%
  filter(Projection > 0) %>%
  
  group_by(EventID,AreaCode,VenueName,StreetAddressID,
           Latitude,Longitude,LocationStatusCode,
           LocationTypeCode) %>%
  mutate(key = row_number()) %>%
  
  left_join(ndays)


## Create mark-off
Markoff_days <- Markoff  %>%
  group_by(ELECTIONEVENTID,DECLARATIONEXCUSEVOTETYPECODE, ISSUINGDISTAREACODE, GAZETTEDNAME,StreetAddressID,VOTEPROCESSEDDATE) %>%
  summarise(`Early Voting` = n()) %>% filter(DECLARATIONEXCUSEVOTETYPECODE == 'Pre-poll Ordinary') %>% ungroup()


## Sydney town hall not in below as it has no projections
Votes_projection_day <- Projection_days %>% left_join(Markoff_days, by = c('AreaCode'='ISSUINGDISTAREACODE',
                                                                           'StreetAddressID',
                                                                           'Date'='VOTEPROCESSEDDATE'))





## 1). MAPPING =======================================================================================================


## Need polygon shape file for wards
if(loopdistricts) {
  
  ## 
  centroidcalc <- readOGR(paste0(reference_folder, 'DeterminedBoundaries2013.mif'))
  centeroids   <- gCentroid(centroidcalc, byid=TRUE)@coords %>% cbind(data.frame(AreaCode = centroidcalc$Name))
  
  rm(centroidcalc)
  
  for (i in 1:nrow(centeroids)) {
    
    ## SG
    if(grepl('SG',event_group_ID)) {
      
      map_polygons  <- SpatialPolygons(list())
      
      ##
      Polygon_Shape      <- readOGR(dsn=paste0(reference_folder, "NSW_compact_boundary"), layer="NSW_compact")
      Polygon_Shape$Size <- raster::area(Polygon_Shape)/1000000
      
      ## 
      Polygon_Shape@data <- Polygon_Shape@data %>% left_join(enrolment, by=c('Name'='AreaCode')) %>%
        
        ## 
        left_join(Pre_poll_projections %>% 
                    group_by(EventID,AreaCode) %>%
                    summarise(EalyVoting = sum(ProjectedVoters)), 
                  by=c('Name'='AreaCode')) %>%
        rename(AreaCode=Name) %>% 
        
        ## 
        mutate(size_of_map = case_when(Size >250000 ~ 6,
                                       Size >100000 ~ 7,
                                       Size >10000 ~ 8,
                                       Size > 5000 ~ 9,
                                       Size > 1000 ~ 10,
                                       Size > 100 ~ 11,
                                       T ~ 12))
      
    }
    
    ## 
    ram1         <- subset(Polygon_Shape, AreaCode==centeroids$AreaCode[i])
    map_polygons <- raster::bind(map_polygons, ram1)
    
    ##
    centeroid    <- centeroids %>% filter(AreaCode == map_polygons$AreaCode)
    centLong     <- centeroid[,1]
    centLat      <- centeroid[,2]
    
    ## EVC mark-off data
    All_EVCs_markoff <- All_EVCs %>% left_join(Markoff %>% group_by(ELECTIONEVENTID,
                                                                    DECLARATIONEXCUSEVOTETYPECODE,
                                                                    ISSUINGDISTAREACODE,
                                                                    GAZETTEDNAME,
                                                                    StreetAddressID) %>% summarise(Voted = n()),
                                               by=c('AreaCode' = 'ISSUINGDISTAREACODE',
                                                    'VenueName' = 'GAZETTEDNAME',
                                                    'StreetAddressID'))
    
    ## 
    Sized_venues <- All_EVCs_markoff %>%
      mutate(size = case_when(ProjectedVoters < 500 ~ 1,
                              ProjectedVoters < 1000 ~ 1.2,
                              ProjectedVoters < 1500 ~ 1.5,
                              ProjectedVoters < 2000 ~ 2,
                              ProjectedVoters < 5000 ~ 2.5,
                              ProjectedVoters < 10000 ~ 3,
                              T ~ 4)) 
    
    ## 
    District_EMO <- Sized_venues %>% filter(LocationTypeCode == 'EMO') %>% filter(AreaCode == centeroids[i,]$AreaCode)
    District_EVC <- Sized_venues %>% filter(LocationTypeCode == 'EVC') %>% filter(AreaCode == centeroids[i,]$AreaCode)
    District_DI  <- DI_Visits %>% filter(AreaCode ==  map_polygons$AreaCode)
    
    ##
    District_Candidates <- Candidates %>% filter(AreaCode ==  as.character(centeroids[i,]$AreaCode))
    District_OP         <- open_days  %>% filter(AreaCode == centeroids[i,]$AreaCode & !grepl('EM Office',VenueName)) %>%
      arrange(VenueName,Date) %>% select(VenueName,OpeningDate,OpeningTime, ClosingTime)
    
    ## Counts
    Counts <- data.frame(
      EVCs  = nrow(Sized_venues %>% filter(AreaCode == centeroids[i,]$AreaCode) %>% filter(LocationTypeCode != 'VC')),
      Aways = nrow(Sized_venues %>% filter(AreaCode == centeroids[i,]$AreaCode) %>% filter(LocationStatusCode == "It's Away")),
      Candidates = nrow(District_Candidates))
    
    
    ## Labels   
    evc_popuplabels <- sprintf(
      "<strong>%s</strong><br>Projected: %s<br> Voted: %s<br>LocationTypeCode: %s <br>Hiring Status: %s" ,
      District_EVC$VenueName
      ,District_EVC$ProjectedVoters
      ,District_EVC$Voted
      ,District_EVC$LocationTypeCode
      ,District_EVC$LocationStatusCode) %>% lapply(htmltools::HTML) 
    
    ## 
    ram  <- District_OP %>% distinct(VenueName)
    ram1 <- list() 
    if(nrow(ram) > 0) {
      
      for (j in 1:nrow(ram)) {
        ram1[[j]] <- htmlTable::htmlTable(District_OP %>% filter(VenueName == ram[j,1]) %>% select(-VenueName),
                                          rnames = F, css.cell = "padding-left: .5em; padding-right: .2em;",
                                          align = "r") 
      }
    }
    
    evc_popuplabel_2 <- sprintf("%s",
                                ram1) %>% lapply(htmltools::HTML) 
    
    ## 
    emo_popuplabels <- sprintf(
      "<strong>%s</strong><br>Projected: %s<br> Voted: %s<br> LocationTypeCode: %s <br>Hiring Status: %s" ,
      District_EMO$VenueName
      ,District_EMO$ProjectedVoters
      ,District_EMO$Voted
      ,District_EMO$LocationTypeCode
      ,District_EMO$LocationStatusCode) %>% lapply(htmltools::HTML) 
    
    ## 
    di_popuplabels <- sprintf(
      "<strong>%s</strong><br>Projected: %s<br/> LocationTypeCode: %s <br>Status: %s" ,
      District_DI$VenueName
      ,District_DI$ProjectedVoters
      ,District_DI$LocationTypeCode
      ,District_DI$LocationStatusCode) %>% lapply(htmltools::HTML) 
    
    ## 
    circle_color <- c('#4e7a09','#b50e90','#070460','#ed8c42')
    circle_label <- c('VC','EVC','EMO','DI')
    
    ## Create a district map
    District_Summary_Map <- leaflet(map_polygons,
                                    height  = 830, 
                                    width   = 800, 
                                    padding = 0, 
                                    options = leafletOptions(zoomControl = FALSE, minZoom = 6)) %>%
      
      ## Update
      setView(centLong, centLat, zoom = map_polygons$size_of_map) %>%
      setMaxBounds(lng1 = 131.000333332,
                   lat1 = -18.992662696,
                   lng2 = 175.612793,
                   lat2 = -43.840233) %>% 
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
      addResetMapButton() 
    
    
    ## Polygon   
    poly_popup <- sprintf(
      "<strong>%s</strong>",
      map_polygons$AreaCode) %>% lapply(htmltools::HTML)
    
    
    District_Summary_Map <- District_Summary_Map %>% addPolygons(
      
      color     = "#000000", 
      weight    = 5,
      highlight = highlightOptions(
        color   = "#155e58",
        fillOpacity = 0.1,
        bringToFront = FALSE),
      
      label        = poly_popup,
      labelOptions = labelOptions(
        noHide     = T,
        textOnly   = TRUE,
        style      = list("font-weight" = "bold", padding = "3px 8px"),
        
        textsize   = "25px",
        direction  = "auto"),
      group = 'District') 
    
    
    ## 
    if(nrow(District_EVC) > 0) {
      
      District_Summary_Map <- District_Summary_Map %>%
        
        addCircleMarkers(District_EVC, lat = District_EVC$Latitude, lng = District_EVC$Longitude,
                         label = paste0(District_EVC$ProjectedVoters),
                         
                         labelOptions = labelOptions(textsize  = 15,
                                                     textOnly  = TRUE,
                                                     noHide    = T,
                                                     direction = 'center',
                                                     offset    = c(0,0),
                                                     style     = list(
                                                       "color" = "white",
                                                       "font-family" = "serif",
                                                       "font-size" = "12px"
                                                     )),
                         
                         popup  = paste0(evc_popuplabels,'<br>',
                                         evc_popuplabel_2),
                         radius = ~(District_EVC$size*10),
                         weight = 0,
                         fillColor = circle_color[2],
                         fillOpacity = 0.7,
                         group = 'EVC')
    }
    
    
    if(nrow(District_DI) > 0) {
      
      District_Summary_Map <- District_Summary_Map %>% 
        
        addCircleMarkers(District_DI, 
                         lat    = District_DI$Latitude, 
                         lng    = District_DI$Longitude,
                         label  = paste0(District_DI$VenueName),
                         popup  = di_popuplabels,
                         radius = 5,
                         weight = 0,
                         fillColor = circle_color[4],
                         fillOpacity = 0.7,
                         group = 'DI')
    }
    
    District_Summary_Map <- District_Summary_Map %>% 
      
      addCircleMarkers(District_EMO, lat = District_EMO$Latitude, lng = District_EMO$Longitude,
                       label = paste0(District_EMO$ProjectedVoters),
                       labelOptions = labelOptions(textsize = 15,
                                                   textOnly = TRUE,
                                                   noHide = T,
                                                   direction = 'center',
                                                   offset = c(0,0),
                                                   style  = list(
                                                     "color" = "white",
                                                     "font-family" = "serif",
                                                     "font-size" = "12px"
                                                   )),
                       
                       popup = paste0(emo_popuplabels,'<br>',
                                      htmlTable::htmlTable(open_days %>% filter(VenueName == District_EMO$VenueName  &
                                                                                  AreaCode == District_EMO$AreaCode) %>%
                                                             arrange(Date) %>% select(OpeningDate,OpeningTime, ClosingTime),
                                                           rnames = F, css.cell = "padding-left: .5em; padding-right: .2em;",
                                                           align = "r")),
                       radius = ~(District_EMO$size*10),
                       weight = 0,
                       fillColor = circle_color[3],
                       fillOpacity = 0.7,
                       group = 'EVC') %>%
      addLegend(
        position = 'bottomright',
        colors = circle_color[2:4],
        labels = circle_label[2:4],
        opacity = 0.7,
        title = map_polygons$AreaCode
        
      ) %>%
      
      addMarkers(Sized_venues,lat = Sized_venues$Latitude,lng = Sized_venues$Longitude,
                 label = Sized_venues$VenueName,
                 group = 'ALL') %>%
      
      addSearchFeatures(
        targetGroups = "ALL",
        options = searchFeaturesOptions(
          zoom=18, openPopup = TRUE, firstTipSubmit = TRUE,
          autoCollapse = TRUE, hideMarkerOnCollapse = TRUE )) %>%
      hideGroup('ALL') 
    
    
    ## Charts and Data extraction AoBP ---------------------------------------------------------
    ## Aobp ------------------------------------------------------------------------------------
    ElectionArea <- All_EVCs %>% filter(EventID == map_polygons@data$EventID) %>% distinct(AreaCode)
    AoBP_area    <- AoBP_read %>% filter(SGAreaCode == as.character(ElectionArea) &
                                           ContestTypeCode == 'LA' &
                                           ReturningOffice != 'Sydney Town Hall')
    
    ## 
    if (nrow(AoBP_area) > 0) {
      
      UsedBP <- AoBP_area
      sum.UsedBP <- AoBP_area %>% group_by(SGAreaCode,Date) %>% summarise(AoBP = sum(Prepoll_Ordinary))
      
    } else {
      
      UsedBP <- data.frame()
      sum.UsedBP <- data.frame()
      
    }
    
    ##  
    districtMarkoff <- Votes_projection_day %>% 
      filter(AreaCode == map_polygons@data$AreaCode) %>% 
      ungroup() %>%
      arrange(VenueName,Date)
    
    area_markoff <- districtMarkoff
    
    ## 
    if (length(UsedBP) > 0) {
      
      ## 
      chart1_data_beta <- districtMarkoff %>% 
        mutate(AreaCode = paste0('Total ',map_polygons@data$AreaCode)) %>% 
        group_by(AreaCode,OpeningDate,Date) %>% summarise(Projection = sum(Projection),
                                                          `Early Voting` = sum(`Early Voting`,na.rm = TRUE)) %>%
        
        ## UsedBP instead of TotalBP_exhausted is used in charting as unsure of aobp structure
        left_join(sum.UsedBP) %>% arrange(Date) %>% 
        mutate(`Early Voting` = ifelse(`Early Voting` == 0,NA,`Early Voting`))
      
    } else {
      
      ## 
      chart1_data_beta <- districtMarkoff %>% 
        mutate(AreaCode = paste0('Total ',map_polygons@data$AreaCode)) %>% 
        group_by(AreaCode,OpeningDate,Date) %>% 
        summarise(Projection = sum(Projection),
                  `Early Voting` = sum(`Early Voting`,na.rm = TRUE)) %>%
        
        mutate(AoBP = NA,
               name = 'AoBP not found') %>% arrange(Date) %>% 
        mutate(`Early Voting` = ifelse(`Early Voting` == 0,NA,`Early Voting`))
      
    }
    
    ## Hi-chart code, replace with GGPLOT
    chart1 <- hchart(chart1_data_beta, 'line', hcaes(x = OpeningDate, 
                                                     y =`Early Voting`, 
                                                     group = AreaCode)) %>%
      
      hc_add_series(chart1_data_beta, 
                    type = "line", 
                    dashStyle = "longdash",hcaes(x=OpeningDate,y=Projection),
                    color = 'pink',name='Total Projected') %>%
      
      hc_title(
        text = paste(map_polygons@data$EventID,map_polygons@data$AreaCode,'District Total Votes by date', sep = ' '),
        useHTML = TRUE) %>%
      
      ## 
      hc_yAxis(title = list(text='Counts')) %>%
      hc_tooltip(table = TRUE, sort = FALSE) %>%
      hc_legend(enabled = F) %>%
      hc_tooltip(crosshairs=c(TRUE,TRUE)) %>%
      
      ## 
      hc_add_series(chart1_data_beta, type = "line", 
                    hcaes(x = OpeningDate,y = AoBP), 
                    color = 'purple', name = 'AoBP') 
    
    if (nrow(UsedBP) >0) {
      
      chart2_data <- districtMarkoff %>% left_join(UsedBP, by=c("VenueName", "Date"))
      
    } else {
      
      chart2_data <- districtMarkoff %>% mutate(Prepoll_Ordinary = NA)
      
    }
    
    ## 
    chart2_data <- chart2_data %>% group_by(OpeningDate, Date) %>%
      summarise(TotalProjected = sum(Projection),
                TotalMarkoff = sum(`Early Voting`,na.rm=TRUE),
                TotalAoBP = sum(Prepoll_Ordinary,na.rm=TRUE)) %>%
      
      ungroup() %>% 
      arrange(Date) %>%
      
      ## 
      mutate(TotalProjected = ifelse(TotalProjected==0,NA,TotalProjected),
             TotalMarkoff = ifelse(TotalMarkoff==0,NA,TotalMarkoff),
             TotalAoBP = ifelse(TotalAoBP==0,NA,TotalAoBP)) %>%
      
      ## 
      mutate(TotalProjected = cumsum(TotalProjected),
             TotalMarkoff = cumsum(TotalMarkoff),
             TotalAoBP = cumsum(TotalAoBP)) %>%
      
      ## 
      gather(Type,Count,
             -OpeningDate,
             -Date) %>%
      arrange(Type,Date)
    
    ##
    chart2 <- hchart(chart2_data, 'line',hcaes(x=OpeningDate,y=Count, group=Type),color=c("purple", "#2980b9", "pink")) %>%
      
      ## 
      hc_title(
        text = paste(map_polygons@data$EventID,map_polygons@data$AreaCode,'Total Cumulative Votes by date', sep = ' '),
        useHTML = TRUE) %>%
      
      ## 
      hc_yAxis(title = list(text = 'Counts')) %>%
      hc_tooltip(table = TRUE, sort = FALSE) %>%
      hc_legend(enabled = F)
    
    ##
    Area_EMB <- districtMarkoff %>%
      select(VenueName,LocationTypeCode,OpeningDate,Projection,`Early Voting`,Date) 
    
    ## 
    if (length(UsedBP) > 0) {
      
      Area_EMB <- Area_EMB %>% left_join(UsedBP %>% 
                                           select(AoBP = Prepoll_Ordinary, VenueName,Date), 
                                         by = c('VenueName','Date'))
      
    }
    
    ## 
    setwd(early_voting_input_folder)
    
    ## 
    rmarkdown::render('03 Bar Charts.Rmd', 
                      output_file = paste0(map_polygons$EventID, ' ', 
                                           map_polygons$AreaCode,' Daily Bar Charts 03.html'))
    
    rmarkdown::render('03 Line Charts.Rmd', 
                      output_file = paste0(map_polygons$EventID, ' ', 
                                           map_polygons$AreaCode,' Cumulative Line Charts 03.html'))
    
    rmarkdown::render('03 DataSet Charts.Rmd', 
                      output_file = paste0(map_polygons$EventID, ' ', 
                                           map_polygons$AreaCode,' DataSet 03.html'))
    
    ## 
    chart.list       <- paste0(early_voting_input_folder,'/', 
                               list.files(early_voting_input_folder, 
                                          pattern = paste0(map_polygons$EventID, ' ', 
                                                           map_polygons$AreaCode), full.names = FALSE))
    
    
    ## 
    rootfolder.charts <- paste0(early_voting_output_folder,'/', 
                                list.files(early_voting_input_folder, 
                                           pattern=paste0(map_polygons$EventID, ' ', 
                                                          map_polygons$AreaCode), full.names = FALSE))
    
    file.copy(chart.list, rootfolder.charts, overwrite = TRUE)
    
    
    
    ## Knit district -----------------------------------------------------------
    rmarkdown::render(paste0(early_voting_input_folder, '02 EarlyVotingDistricts.Rmd'), 
                      output_file = paste(early_voting_output_folder, map_polygons@data$EventID, 
                                          map_polygons$AreaCode,'Early Voting.html',sep=' ')
    )
    
    file.remove(chart.list)
    
  }
  
}

## Summary -----------------------------------------------------------------


evc_open   <- Votes_projection_day %>% filter(Date == Sys.Date() & LocationStatusCode != "It's Away")
evc_closed <- Votes_projection_day %>% filter(Date != Sys.Date() & LocationStatusCode != "It's Away") %>%
  distinct(AreaCode,VenueName,Latitude,Longitude) %>% anti_join(evc_open)


## STH stuff
STH_daily_markoff_dist <- Markoff %>% filter(GAZETTEDNAME == 'Sydney Town Hall') %>%
  group_by(ELECTIONEVENTID,
           DECLARATIONEXCUSEVOTETYPECODE,
           VOTEPROCESSEDDATE,
           ISSUINGDISTAREACODE,
           GAZETTEDNAME) %>%
  summarise(markoffs = n())


## District level stuff 
District_markoffs <- Votes_projection_day %>% group_by(AreaCode,Date) %>%
  summarise(ProjectedVoters = sum(Projection),
            `Early Voting` = sum(`Early Voting`,na.rm = TRUE)) %>%
  filter(`Early Voting` > 0) %>%
  
  group_by(AreaCode) %>%
  summarise(ProjectedVoters = sum(ProjectedVoters),
            `Early Voting` = sum(`Early Voting`,na.rm = TRUE)) %>%
  arrange(desc(`Early Voting`))


## 
chart1_data <- District_markoffs %>% head(31)
chart3_data <- District_markoffs %>% tail(31)
chart2_data <- District_markoffs %>% anti_join(chart1_data) %>% anti_join(chart3_data)


## AoBP stuff
Venue_Markoffs_vs_AoBP <- Votes_projection_day %>% 
  filter(!is.na(`Early Voting`)) %>%
  rename(MarkOff = `Early Voting`) %>%
  
  left_join(AoBP_read %>% filter(ReturningOffice != 'Sydney Town Hall' &
                                   ContestTypeCode == 'LA') %>% 
              rename(AoBP=Prepoll_Ordinary),
            by=c('AreaCode'='SGAreaCode',
                 'VenueName',
                 'Date')) %>%
  
  mutate(AoBPEntered = ifelse(is.na(AoBP),FALSE,TRUE),
         MarkOff_vs_AoBP = MarkOff - AoBP) %>% ungroup() %>%
  select(AreaCode, VenueName,Date,AoBPEntered,MarkOff,AoBP,MarkOff_vs_AoBP)


## Render RMD ----
rmarkdown::render(paste0(early_voting_input_folder, '02 EVC summary.Rmd'), 
                  output_file = paste0(early_voting_output_folder, '- EVC summary Early Voting.html'))



## EOG Processing -----------------------------------------------------
EOG_EVcumchart <- Votes_projection_day %>% group_by(OpeningDate,Date) %>%
  
  
  ## 
  summarise(`Early Voting` = sum(`Early Voting`,na.rm = TRUE),
            Projection = sum(Projection)) %>% ungroup() %>% arrange(Date) %>%
  
  
  ##
  mutate(`Cumulative Early Voting` = cumsum(`Early Voting`),
         `Cumulative Projection` = cumsum(Projection)) %>% 
  
  ## 
  mutate(`Cumulative Early Voting` = ifelse(`Early Voting`==0,NA,`Cumulative Early Voting`)) %>% 
  select(-`Early Voting`, -Projection) %>%
  
  
  ## 
  gather(key=Status, value=Count,-OpeningDate,-Date) %>% arrange(Date) %>%
  mutate(Count = ifelse(Count==0,NA,Count))


## 
if (as.POSIXlt(updatedon)$hour < 12) {
  
  DistDiff <- Votes_projection_day %>% filter(Date != Sys.Date())
  
} else {
  
  DistDiff <- Votes_projection_day
}


##  Calculate difference in distance
DistDiff <- DistDiff %>%            
  group_by(AreaCode,Date) %>%
  
  summarise(ProjectedVoters = sum(Projection),
            `Early Voting` = sum(`Early Voting`,na.rm = TRUE)) %>%
  filter(`Early Voting` > 0) %>%
  group_by(AreaCode) %>%
  
  summarise(ProjectedVoters = sum(ProjectedVoters),
            `Early Voting` = sum(`Early Voting`,na.rm = TRUE)) %>%
  mutate(Diff = `Early Voting` - ProjectedVoters) %>%
  arrange(desc(Diff))


## Update this
top10  <- DistDiff %>% filter(Diff > 0) %>% arrange(desc(Diff)) %>% head(10)
tail10 <- DistDiff %>% filter(Diff < 0) %>% arrange(desc(Diff)) %>% tail(10)


rmarkdown::render(paste0(early_voting_input_folder, 'EOG_EV_graphs.Rmd'), 
                  output_file = paste0('//SVRANALYTICS1/AnalyticsReports/Dashboard/', 'EOG_EV_graphs.html'))





print(Sys.time())
