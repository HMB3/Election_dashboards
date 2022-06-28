source(paste0(helpers_folder, 'DistrictVenueData.R'))
#------------------------------------------------------------------------------
setwd(venues_input_folder)
#------------------------------------------------------------------------------

Polygon_Shape <- readOGR(dsn=paste0(reference_folder, "NSW_compact_boundary"), layer="NSW_compact")

Polygon_Shape@data <- Polygon_Shape@data %>% left_join(Pre_poll_projections %>% 
                                                         group_by(EventID,AreaCode) %>%
                                                         summarise(Projected = sum(ProjectedVoters)), 
                                                       by=c('Name'='AreaCode')) %>%
  rename(AreaCode=Name)
map_polygons <- Polygon_Shape

PrepollReportMap <- leaflet(map_polygons,height = 500, width = 1600, padding = 0, options = leafletOptions(zoomControl = FALSE,
                                                                                                           minZoom = 6)) %>% 
  setView(151.0000486, -33.8027653, zoom = 10) %>%
  setMaxBounds(lng1 = 131.000333332,
               lat1 = -18.992662696,
               lng2 = 175.612793,
               lat2 = -43.840233) %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% addResetMapButton()

popuplabels <- sprintf(
  "<strong>%s</strong><br>Projected: %s<br/> Voted: %s <br>Open %s days<br>Hiring Status: %s" ,
  Pre_poll_projection_districts$VenueName
  ,Pre_poll_projection_districts$ProjectedVoters
  ,Pre_poll_projection_districts$Count
  ,Pre_poll_projection_districts$Days
  ,Pre_poll_projection_districts$LocationStatusCode) %>% lapply(htmltools::HTML) 


# EVC
PrepollReportMap <- PrepollReportMap %>% addMarkers(Pre_poll_projection_districts
                                                    , lat = Pre_poll_projection_districts$Latitude, lng = Pre_poll_projection_districts$Longitude,
                                                    label = Pre_poll_projection_districts$VenueName,
                                                    labelOptions = labelOptions(textsize = 15),
                                                    popup = popuplabels,
                                                    icon = red_icon,
                                                    group = 'EVC') 

# VC
PrepollReportMap <- PrepollReportMap %>% addMarkers(Other_Venues
                                                    , lat = Other_Venues$Latitude, lng = Other_Venues$Longitude,
                                                    label = Other_Venues$VenueName,
                                                    popup = Other_Venues$LocationStatusCode,
                                                    icon = black_icon,
                                                    group = 'VC') %>%
  addControl(html = html_legend, position = "bottomleft")

# polygons

# number of votes by district (summing prepoll)
map_polygons@data <- map_polygons@data %>% left_join(Pre_poll_projection_districts %>% group_by(AreaCode) %>%
                                                       summarise(Votes = sum(ifelse(is.na(Count),0,Count)))
                                                     ,by = 'AreaCode')

labels <- sprintf(
  "<strong>%s</strong><br>Prepoll Votes: %s<br/>",
  map_polygons$AreaCode,map_polygons$Votes) %>% lapply(htmltools::HTML)

PrepollReportMap <- PrepollReportMap %>% addPolygons(
  color= "#000000", 
  fillColor = pal(map_polygons$Votes),
  weight = 2,
  fillOpacity = 0.5,
  highlight = highlightOptions(
    color = "#514d4d",
    fillOpacity = 0.005,
    bringToFront = FALSE),
  popup = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"),
  group = 'District') %>%
  addLegend('bottomright',pal = pal, values=map_polygons$Projected,
            title = 'Prepoll Votes taken',
            opacity = 0.5)

#Map
PrepollReportMap <- PrepollReportMap %>% 
  addSearchFeatures(
    targetGroups = c('EVC','VC'),
    options = searchFeaturesOptions(
      zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE )) %>%
  addLayersControl(
    overlayGroups = c("EVC", "VC"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup("VC")

#Metric Widgets -------------------------------------------------------
EVC_diag <- Pre_poll_projection_districts %>%
  mutate(VenueType = 'EVC') %>%
  bind_rows(Other_Venues %>% mutate(VenueType = 'VC')) %>% 
  dplyr::select(-Days) %>% # rename(DaysAmended = Days) %>%
  left_join(opening_hours %>% group_by(AreaCode,VenueName,StreetAddressID) %>%
              summarise(Days = n()), by = c('AreaCode'
                                            ,'VenueName'
                                            ,'StreetAddressID')) %>%
  mutate(OpeningDays = ifelse(is.na(Days),1,Days))

EVC_diag = SharedData$new(EVC_diag %>% dplyr::select(AreaCode, VenueName, VenueType, ProjectedVoters, Latitude, Longitude, LocationStatusCode, OpeningDays))

#Filters
filter_VenueType <- filter_checkbox("VenueType", "VenueType", EVC_diag, ~VenueType, inline = FALSE)
filter_Area <- filter_select("Area", "AreaCode", EVC_diag, ~AreaCode)
filter_projection <- filter_slider("ProjectedVoters", "Projection", EVC_diag, ~ProjectedVoters, width = "80%")
filter_OpeningDays <- filter_slider("Days", "Opening Days", EVC_diag, ~OpeningDays, width = "80%")
filter_Geo <- filter_select("Geoed", "Location set", EVC_diag, ~ifelse(is.na(Latitude), "Location not set", "Geo-ed"))
filter_Status <- filter_checkbox("LocationStatusCode", "Location Status", EVC_diag, ~LocationStatusCode, inline = FALSE)
