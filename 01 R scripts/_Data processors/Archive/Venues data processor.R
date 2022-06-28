####################################################################################################################
########################################### VENUES DATA GENERATOR ---- #############################################
####################################################################################################################





## This code pulls the data for the 'Candidate nominations' dashboard
message('Run R code to process Venues data')


## The code below should run off the event ID - E.G. now the event has been created, Liam enters things into the 
## database which are captured by the event ID


## Note the administritive hierarchy :: Council - Venue - Ward


## Procurement summary ----
Procurement <- Pre_poll_projection_districts %>% 
  dplyr::select(-Days) %>% rbind(Other_Venues) %>%
  filter(LocationStatusCode != "It's Away") %>%
  
  mutate(Status = ifelse(LocationStatusCode == 'Hire Agreement Signed','Hire Agreement Signed','Other'),
         LocationType = case_when(LocationTypeCode == 'Polling Place' ~ 'Voting Centre',
                                  LocationTypeCode == 'Pre-polling Place' ~ 'Early Voting Centre',
                                  LocationTypeCode == 'Returning Office' ~ 'EM Office'))


## Create procurement report
Procurement_report <- table(Procurement$LocationType, Procurement$Status)
TotalProcurement   <- as.data.frame(Procurement_report) %>% group_by(Var2) %>% 
  
  summarise(Completion = sum(Freq)) %>%
  mutate(CompletionRate = round(Completion/sum(Completion),3)*100) %>%
  rename(Status = Var2)


## Procurement status bar ----
Procurement_statusBar <- ggplot(TotalProcurement, 
                                aes(x = as.numeric(1), y = CompletionRate, fill = Status)) + 
  
  geom_bar_interactive(stat='identity', width = 0.75, tooltip = c('Other','Hire Agreement Signed')) +
  coord_flip() +
  
  scale_fill_manual(values = c("#12e6c8", "#a287f4")) + 
  geom_text(aes(group = Status, label = paste0(CompletionRate,'%'), colour = Status), 
            position = position_stack(vjust = 0.5), 
            fontface = "bold", size = 6) +
  
  scale_colour_manual(values = c("black", "red")) +
  theme_minimal() +
  
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0, linetype = "solid"),
        text = element_text(family = "Source Sans Pro"),
        
        panel.grid      = element_blank(),
        axis.title      = element_blank(),
        axis.text       = element_blank(),
        plot.margin     = grid::unit(c(0,0,0,0), "mm"),
        panel.spacing   = unit(c(0,0,0,0), "cm"),
        legend.position = "none") + 
  
  scale_x_continuous(expand = c(0,1)) #+ 
#scale_y_continuous(expand = c(0,5))

#ggiraph(code = print(Procurement_statusBar))





## Add metric Widgets ----
procurement_sd <- Procurement %>% 
  
  dplyr::select(AreaCode, VenueName, ProjectedVoters, LocationType, Status, Latitude, Longitude) %>%
  left_join(opening_hours %>% group_by(AreaCode,VenueName,StreetAddressID) %>%
              
              summarise(Days = n()), by = c('AreaCode'
                                            ,'VenueName')) %>%
  mutate(Days = ifelse(is.na(Days), 1, Days))

procurement_sd = SharedData$new(procurement_sd %>% 
                                  dplyr::select(AreaCode, VenueName, ProjectedVoters, LocationType, Status, Latitude))

## Apply Filters
filter_VenueType  <- filter_checkbox("LocationType", "LocationType",      procurement_sd, ~ LocationType, inline = FALSE)
filter_Area       <- filter_select("Area", "Select AreaCode",             procurement_sd, ~ AreaCode)
filter_projection <- filter_slider("ProjectedVoters", "Projection Range", procurement_sd, ~ ProjectedVoters, width = "80%")
# filter_OpeningDays <- filter_slider("Days", "Opening Days", procurement_sd, ~Days, width = "80%")


filter_Geo        <- filter_select("Geoed", "Location set", procurement_sd, ~ ifelse(is.na(Latitude), "Location not set", "Geo-ed"))
filter_Status     <- filter_checkbox("Status", "Status",    procurement_sd, ~ Status, inline = FALSE)





## Mapping -----------------------------------------------------------------


## Read in the polygon data
Polygon_Shape      <- readOGR(dsn = paste0(data_files, "NSW_compact_boundary"), layer = "NSW_compact")
Polygon_Shape@data <- Polygon_Shape@data %>% left_join(Pre_poll_projections %>% 
                                                         group_by(EventID, AreaCode) %>%
                                                         summarise(Projected = sum(ProjectedVoters)), 
                                                       by = c('Name'='AreaCode')) %>% rename(AreaCode = Name)
map_polygons <- Polygon_Shape


## Labels and parameters
labels <- sprintf(
  "<strong>%s</strong><br>",
  map_polygons$AreaCode) %>% lapply(htmltools::HTML)


## Change this to the current folder
## If they need to be used?
html_legend <- "<img src = './03 Reference files/UI/marker-icon-black.png'> VC<br/>
<img src = './03 Reference files/UI/marker-icon-red.png'> EVC"
# polygoncolors <- rev(heat.colors(11))
# bins <- c(0, 10, 1000, 2000, 4000, 8000, 16000, 32000, 64000, 120000)
# polygoncolors[1] <- "#ffffff00"
# pal <- colorBin(polygoncolors, domain = 0:120000, bins = bins)

Unique_evc <- Pre_poll_projection_districts %>% filter(LocationStatusCode != "It's Away")
Unique_vc  <- Other_Venues %>% filter(LocationStatusCode != "It's Away")


evc_popuplabels <- sprintf(
  "<strong>%s</strong><br>Projected: %s<br/>Hiring Status: %s" ,
  Unique_evc$VenueName
  ,Unique_evc$ProjectedVoters
  #,Pre_poll_projection_districts$Count
  #,Pre_poll_projection_districts$Days
  ,Unique_evc$LocationStatusCode) %>% lapply(htmltools::HTML) 

vc_popuplabels <- sprintf(
  "<strong>%s</strong><br>Projected: %s<br/>Hiring Status: %s" ,
  Unique_vc$VenueName
  ,Unique_vc$ProjectedVoters
  #,Pre_poll_projection_districts$Count
  #,Pre_poll_projection_districts$Days
  ,Unique_vc$LocationStatusCode) %>% lapply(htmltools::HTML) 



## Draw maps ----
VenueMap <- leaflet(map_polygons, 
                    height = 500, 
                    width = 1600, 
                    padding = 0, 
                    options = leafletOptions(zoomControl = FALSE, 
                                             minZoom = 6, 
                                             maxZoom = 20)) %>% 
  
  setView(151.0000486, -33.8027653, zoom = 6) %>%
  setMaxBounds(lng1 = 131.000333332,
               lat1 = -18.992662696,
               lng2 = 175.612793,
               lat2 = -43.840233) %>% 
  
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite,
                   options = providerTileOptions(minZoom = 6, maxZoom = 20)) %>% 
  addResetMapButton() %>%
  
  
  ## Polygons
  addPolygons(
    color= "#000000", 
    fillColor = "#ffffff",
    weight = 2,
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
    group = 'District')  %>%
  # addLegend('bottomright',pal = pal, values=map_polygons$Projected,
  #           title = 'Prepoll Votes taken',
  #           opacity = 0.5)
  
  
  addMarkers(data=Unique_evc,
             lat = Unique_evc$Latitude, lng = Unique_evc$Longitude,
             label = Unique_evc$VenueName,
             labelOptions = labelOptions(textsize = 15),
             popup = evc_popuplabels,
             icon = red_icon,
             clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                   maxClusterRadius = 70),
             group = 'EVC') %>% 
  
  
## Add sydney town hall ----------------------------------------------------
addMarkers(data=STH,
           lat = STH$Latitude, lng = STH$Longitude,
           label = 'Sydney Town Hall',
           labelOptions = labelOptions(textsize = 15),
           popup = 'Sydney Town Hall',
           icon = red_icon,
           clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                 maxClusterRadius = 70),
           group = 'EVC') %>% 
  
  
  ## Add VC
  addMarkers(data=Unique_vc
             , lat = Unique_vc$Latitude, lng = Unique_vc$Longitude,
             label = Unique_vc$VenueName,
             popup = vc_popuplabels,
             icon = black_icon,
             clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                   maxClusterRadius = 70),
             group = 'VC') %>%
  addControl(html = html_legend, position = "bottomleft") %>% 
  #      addMarkers(data=Procurement,
  #                 lat = Procurement$Latitude, lng = Procurement$Longitude,
  #               #  label = Procurement$VenueName,
  #                 popup = proc_popuplabels,
  #                 group = 'hidden') %>%
  #
  
  
## Add  Features ----
addSearchFeatures(
  targetGroups = c('VC','EVC'),
  options = searchFeaturesOptions(
    zoom=16, openPopup = TRUE, firstTipSubmit = TRUE,
    autoCollapse = TRUE, hideMarkerOnCollapse = TRUE )) %>%
  
  addControl("<P><B>Hint!</B> Search for a venue name and select from the list.</P>",
             position = "topright") %>%
  
  addLayersControl(
    overlayGroups = c("EVC", "VC"),
    options = layersControlOptions(collapsed = FALSE)) 
#       hideGroup("hidden")


