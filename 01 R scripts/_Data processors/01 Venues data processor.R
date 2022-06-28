


# Opening hours -----------------------------------------------------------



opening_hours <- opening_hour %>% filter(IsOpen == 'Y')
opening_days  <- opening_hours %>% group_by(AreaCode,VenueName,StreetAddressID) %>%
  summarise(Days = n()) %>% ungroup() %>%
  mutate(Days=ifelse((Days>11 | is.na(Days)),11,Days))







# raw venue projection is extracted at ward level
Venue_Projections <- Venue_Projections %>% left_join(Contest_details %>% 
                                          filter(ContestTypeCode =='Councillor') %>%
                                          select(AreaCode, LGArea)
                                        ,by = "AreaCode") %>%
                                  left_join(Potential_Venues_raw)




LGA_level_projections <- Venue_Projections %>% mutate(ProjectedVoters = OrdinaryVotes + AllDecVotes) %>%
                                               group_by(EventGroupId
                                                        ,EventID
                                                        ,LocationTypeCode
                                                        ,VenueName
                                                        ,StreetAddressID
                                                        ,VotingChannelCode
                                                        ,RedistributionCode
                                                        ,LGArea
                                                        ,LocationStatusCode
                                                        ,Latitude
                                                        ,Longitude) %>%
                                               summarise(ProjectedVoters = sum(ProjectedVoters)) %>% ungroup()



Pre_poll_projection_districts <- LGA_level_projections %>% 
  filter(LocationTypeCode %in% c('Pre-poll','Returning Office')) %>%
  left_join(opening_days, by = c('LGArea'='AreaCode'
                                 ,'VenueName'='VenueName'
                                 ,'StreetAddressID'='StreetAddressID'))


Other_Venues <- LGA_level_projections %>% 
  filter(LocationTypeCode %in% c('Polling Place'))





## Retreive Areacode
Potential_Venues <- Potential_Venues_raw %>% 
  left_join(LGA_level_projections %>% 
              select(-LocationStatusCode
                     ,-Latitude
                     ,-Longitude),by = c("EventID", "VenueName", "LocationTypeCode", "StreetAddressID")) %>%
  filter(!is.na(ProjectedVoters))


## Procurement summary
Procurement_report <- CrossTab(Potential_Venues,'LocationTypeCode','LocationStatusCode') %>%
  select(LocationStatusCode,LocationTypeCode,Count)

# procurement summary by area
Procurement_report_by_area <- Potential_Venues%>%
  as_tibble() %>%
  select(LGArea,VenueName,LocationStatusCode,LocationTypeCode) %>%
  
  #change for factor
  mutate(LocationTypeCode=as.factor(LocationTypeCode)) %>%
  
  #remove it's aways
  filter(LocationStatusCode!="It's Away") %>%
  group_by(LGArea,LocationStatusCode,LocationTypeCode) %>%
  summarise(VenueNumber=n()) %>%
  pivot_wider(names_from = LocationStatusCode,
              values_from=VenueNumber)


## Metrics Widgets -------------------------------------------------------
procurement_sd <- Potential_Venues %>% 
    mutate(GeoStatus = ifelse(is.na(Latitude),'Missing','OK')) %>%
    dplyr::select(LGArea, VenueName, ProjectedVoters, LocationTypeCode, LocationStatusCode, GeoStatus)

procurement_sd = SharedData$new(procurement_sd %>%
                                  dplyr::select(LGArea, VenueName, ProjectedVoters, LocationTypeCode, LocationStatusCode
                                                ,GeoStatus
                                  ))

## Filters
filter_VenueType  <- filter_checkbox("LocationType", "LocationType",      procurement_sd, ~ LocationTypeCode, inline = FALSE)
filter_Area       <- filter_select("Area", "Select LGArea",               procurement_sd, ~ LGArea)
filter_projection <- filter_slider("ProjectedVoters", "Projection Range", procurement_sd, ~ ProjectedVoters, width = "80%")


filter_Geo        <- filter_select("Geoed", "Location set", procurement_sd, ~ ifelse(GeoStatus == 'Missing', "Location not set", "Geo-ed"))
filter_Status     <- filter_checkbox("Status", "Status",    procurement_sd, ~ LocationStatusCode, inline = FALSE)





## Mapping -----------------------------------------------------------------

red_icon = makeIcon(paste0('//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI',"/marker-icon-red.png"), 18, 18, iconAnchorX = 9, iconAnchorY = 18)
black_icon = makeIcon(paste0('//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI',"/marker-icon-black.png"), 18, 18, iconAnchorX = 9, iconAnchorY = 18)

  
## Import shape files

Polygon_Shape      <- readOGR(dsn = paste0(root_dashboard,'02 Source data/', "Compact_LGA"), layer = "Compact_LGA")
Polygon_Shape@data <- Polygon_Shape@data %>% 
  left_join(LGA_level_projections %>%
              
              group_by(EventID,LGArea) %>%
              summarise(Projected = sum(ProjectedVoters)), 
            by = c('LGAreaCode'='LGArea')) %>%
  rename(AreaCode = LGAreaCode)

map_polygons <- Polygon_Shape


## Labels and parameters
labels <- sprintf(
  "<strong>%s</strong><br>",
  map_polygons$AreaCode) %>% 
  lapply(htmltools::HTML)


html_legend <- "<img src='//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI/marker-icon-black.png'> Polling place<br/>
<img src='//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI/marker-icon-red.png'> Pre-poll"


# need to distinguish evc and vc from here

Unique_evc <- Pre_poll_projection_districts %>% filter(LocationStatusCode != "It's Away") %>% filter(!is.na(Latitude))
Unique_vc <- Other_Venues %>% filter(LocationStatusCode != "It's Away") %>% filter(!is.na(Latitude))


evc_popuplabels <- sprintf(
  "<strong>%s</strong><br>Projected: %s<br/>Hiring Status: %s" ,
  Unique_evc$VenueName
  ,Unique_evc$ProjectedVoters
  ,Unique_evc$LocationStatusCode) %>% 
  lapply(htmltools::HTML) 

vc_popuplabels <- sprintf(
  "<strong>%s</strong><br>Projected: %s<br/>Hiring Status: %s" ,
  Unique_vc$VenueName
  ,Unique_vc$ProjectedVoters
  ,Unique_vc$LocationStatusCode) %>% 
  lapply(htmltools::HTML) 





## Draw map
VenueMap <- leaflet(map_polygons,height = 500, width=1600, padding = 0, options = leafletOptions(zoomControl = FALSE,
                                                                                                 minZoom = 6, maxZoom = 20)) %>% 
  setView(151.0000486, -33.8027653, zoom = 6) %>%
  setMaxBounds(lng1 = 131.000333332,
               lat1 = -18.992662696,
               lng2 = 175.612793,
               lat2 = -43.840233) %>% 
  addTiles() %>%
  
  addResetMapButton() %>%
  
  # polygons
  addPolygons(
    color= "#000000", 
    fillColor = "#ffffff",
    weight = 3,
    fillOpacity = 0.05,
    highlight = highlightOptions(
      color = "#514d4d",
      fillOpacity = 0.05,
      bringToFront = FALSE),
    popup = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"),
    group = 'District')  

  

# EVC
  if(nrow(Unique_evc) > 0) {
    
    VenueMap <- VenueMap %>% addMarkers(data=Unique_evc,
             lat = Unique_evc$Latitude, lng = Unique_evc$Longitude,
             label = Unique_evc$VenueName,
             labelOptions = labelOptions(textsize = 15),
             popup = evc_popuplabels,
             icon = red_icon,
             clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                   maxClusterRadius = 70),
             group = 'Pre-poll')
   
  }


# VC
VenueMap <- VenueMap %>%
  
  addMarkers(data=Unique_vc
             , lat = Unique_vc$Latitude, lng = Unique_vc$Longitude,
             label = Unique_vc$VenueName,
             popup = vc_popuplabels,
             icon = black_icon,
             clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                   maxClusterRadius = 70),
             group = 'Polling place') %>%
  
  # Add bottom legend
  
  addControl(html = html_legend, position = "bottomleft") %>% 
  
  # Add top legend
  
  addControl("<P><B>Hint!</B> Search for a venue name and select from the list.</P>",
             position = "topright") %>%
  
  # Add features
  
  addSearchFeatures(
    targetGroups = c('Polling place','Pre-poll'),
    options = searchFeaturesOptions(
      zoom=16, openPopup = TRUE, firstTipSubmit = TRUE,
      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE )) %>%
  
  addLayersControl(
    overlayGroups = c("Pre-poll", "Polling place"),
    options = layersControlOptions(collapsed = FALSE))


