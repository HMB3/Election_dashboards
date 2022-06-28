


#rm(LG2001__staffing_map)
  
  RO_region <- RO_Offices %>% filter(RO_Office == unique(RO_Centroids$RO_OFFICE)[i])
  
  region <- subset(LG2001, LGAreaCode %in% RO_region$LGArea)
  
  EVCs_region <- EVCs %>% filter(EventID %in% RO_region$EventID)
  
  Opened_Positions_region <- Opened_Positions %>% filter(EventID %in% RO_region$EventID) %>% select(-LGAreaCode)
 
  DIs_region <- DIs %>% filter(EventID %in% RO_region$EventID)
  
  Away_venues_region <- Away_venues %>% filter(EventID %in% RO_region$EventID)
  
  Completed_Away_venues_region <- Completed_Away_venues  %>% filter(EventID %in% RO_region$EventID)
  
  VCs_region <- VCs %>% filter(EventID %in% RO_region$EventID) %>%
    bind_rows(Away_venues_region)
  
  # completed does not look at aways atm
  Completed_VCs_region <-Completed_VCs %>% filter(EventID %in% RO_region$EventID) %>%
                              bind_rows(Completed_Away_venues_region)
  
  
  EMOs_region <- EMOs %>% filter(VenueName == RO_region$RO_Office) %>% filter(LocationStatusCode != "It's Away")
    
  hired_staff_region <- EMPLOYED %>% filter(AREACODE %in% RO_region$LGArea) %>% mutate(POSITION = paste0(VenueName,'-',POSITIONTYPECODE)) %>% 
                                   left_join(VCs %>% select(VenueName,Time))
  
  EOI_EO_region <- EOI %>% filter(INTERESTEDASEOPP == 'Y', AREACODE %in% RO_region$LGArea) 
  EOI_SOA_region <- EOI %>% filter(INTERESTEDASOA == 'Y', AREACODE %in% RO_region$LGArea) 
  
  # 
  # Staff_searchable <- Final_Staffing_data %>% filter(AREACODE %in% RO_region$LGArea) %>% 
  #                     left_join(staffing_EOI_applicants_addresses %>%
  #                                 select(LOGINID
  #                                        ,LATITUDE
  #                                        ,LONGITUDE),by= 'LOGINID') %>% # c('ELECTIONEVENTID','LOGINID','AREACODE','ELECTORID'))  %>%
  #                     rename(lat=LATITUDE,long = LONGITUDE)
  #   
  Staff_searchable <- EOI %>% filter(AREACODE %in% RO_region$LGArea) %>% select(AREACODE,LOGINID,lat,long, PREFERREDFIRSTNAME, SURNAME) %>%
                       bind_rows(hired_staff_region %>% select(AREACODE,LOGINID,lat, long, PREFERREDFIRSTNAME,SURNAME))
  
  
  
  
  # above need to find out positions open in staffing
  

# training chunk ----------------------------------------------------------

  # training status----------------------------------------------------------------
  # need to amend to filter list style later
  
  Location_simp_positions_region <- Hiring_status %>% filter(AREACODE %in% RO_region$LGArea) %>%
    
    mutate(POSITIONTYPECODE = gsub("[^[:alnum:] ]", "", POSITIONTYPECODE)) %>%
    mutate(POSITIONTYPECODE = gsub('[[:digit:]]+', '', POSITIONTYPECODE)) %>%
    mutate(POSITIONSTATUS = ifelse(POSITIONSTATUS == 'ACCEPTED', 'EMPLOYED', POSITIONSTATUS)) %>%
    left_join(training_status %>% select(LOGINID, Trained = VALUEENTERED), by = 'LOGINID') %>%
    mutate(Trained = ifelse(is.na(Trained),'N', 'TRAINED')) # %>% 
  #  mutate(POSITIONSTATUS = ifelse((Trained == 'Y' & POSITIONSTATUS == 'EMPLOYED'), 'TRAINED', POSITIONSTATUS))
  
  # position_types <- c('DPPM','DVIO','EO','OA','OACC','PPM','SOACC','SOACR', 'SOAEM', 'SOAPP', 'SOASV')
  
  if(nrow(Location_simp_positions_region %>% filter(POSITIONSTATUS == 'OFFERED')) >0) {
    
  
  offered <- CrossTab(Location_simp_positions_region %>% filter(POSITIONSTATUS == 'OFFERED'), 'POSITIONTYPECODE', 'POSITIONSTATUS')
  
  } else {
    
    offered <- data.frame()
  }
  
  
  if(nrow(Location_simp_positions_region %>% filter(POSITIONSTATUS == 'OFFERED')) >0) {
    
  employed <- CrossTab(Location_simp_positions_region %>% filter(POSITIONSTATUS == 'EMPLOYED',Trained == 'N'), 'POSITIONTYPECODE', 'POSITIONSTATUS')
  
  } else {
    
    employed <- data.frame()
  }
  
  
  if(nrow(Location_simp_positions_region %>% filter(POSITIONSTATUS == 'EMPLOYED',Trained == 'TRAINED')) >0) {
  
  trained <- CrossTab(Location_simp_positions_region %>% filter(POSITIONSTATUS == 'EMPLOYED',Trained == 'TRAINED') %>% mutate(POSITIONSTATUS ='TRAINED'), 'POSITIONTYPECODE', 'POSITIONSTATUS')
  } else {
    
    trained <- data.frame()
    
  }
  
  if(nrow(Location_simp_positions_region %>% filter(POSITIONSTATUS == 'OPEN')) >0) {
    
    open <- CrossTab(Location_simp_positions_region %>% filter(POSITIONSTATUS == 'OPEN'), 'POSITIONTYPECODE', 'POSITIONSTATUS')
    
  } else {
    
    open <- data.frame()
    
  }
  
  training_graph_data <- bind_rows(open,
                                   offered
                                   ,employed
                                   ,trained)
  
  
  # training_stack <- ggplot(training_graph_data, aes(fill=status, y=count, x=Type)) + 
  #   geom_bar(position="stack", stat="identity")
  
  if(nrow(training_graph_data) >0) {
    
  
  training_stack_ed_data <- training_graph_data %>% filter(POSITIONTYPECODE %in% c('DVIO','EO','PPM ','DPPM'))
  
  training_stack_else_data <- training_graph_data %>% filter(POSITIONTYPECODE %!in% c('DVIO','EO','PPM ','DPPM'))
  
  
  
  
  
  
  
  training_ed_stack <- hchart(training_stack_ed_data, "column"
                              ,color = rev(c('lightgreen','black','lightblue','red')[1:length(unique(training_stack_ed_data$POSITIONSTATUS))])
                              , hcaes(x = "POSITIONTYPECODE", y = "Count", group = "POSITIONSTATUS")) %>%
    hc_plotOptions(column = list(stacking = "normal"))
  
  training_else_stack <- hchart(training_stack_else_data, "column"
                              ,color = rev(c('lightgreen','black','lightblue','red')[1:length(unique(training_stack_else_data$POSITIONSTATUS))])
                                , hcaes(x = "POSITIONTYPECODE", y = "Count", group = "POSITIONSTATUS")) %>%
    hc_plotOptions(column = list(stacking = "normal"))
  
  
  
  } else {
    
    training_stack_ed_data <- data.frame()
    training_stack_ed_data <- data.frame()
    
    training_ed_stack <- data.frame()
    training_else_stack <- data.frame()
  }
  
   
    
    
  ## Create labels   
  
  
  html_legend <- "<img src='//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI/marker-icon-red.png'> RO<br/>
<img src='//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI/marker-icon-blue.png'> Pre-poll<br/>
<img src='//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI/marker-icon-black.png'> PP<br/>
<img src='//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI/house.png' height=33px width=33px> Fully employed<br/>"
  
  
  
  
  
  EMO_Venue_details <- list()
  for (j in 1:nrow(EMOs_region)) {
    
    EMO_Venue_details[[j]] <- htmlTable::htmlTable(Location_positions %>% filter(StreetAddressID == EMOs_region[j,'StreetAddressID']) %>% 
                                                     filter(LocationStatusCode != "It's Away") %>% arrange(POSITIONSTATUS,desc(POSITIONID)) %>%
                                                     dplyr::select(Position = POSITIONTYPECODE,Status = POSITIONSTATUS,
                                                            FirstName = PREFERREDFIRSTNAME,SurName = SURNAME),
                                                   rnames = F, css.cell = "padding-left: .5em; padding-right: .2em;",
                                                   align = "r") 
  }
  
  EMO_popuplabel_1 <- sprintf(
    "<strong>%s</strong>" ,
    EMOs_region$VenueName
  ) %>% lapply(htmltools::HTML) 
  
  EMO_popuplabel_2 <- sprintf("%s",
                              EMO_Venue_details) %>% lapply(htmltools::HTML) 
  
  
 
  
  
  ## polygon   
  poly_popup <- sprintf(
    "<strong>%s</strong>",
    region$LGAreaCode) %>% lapply(htmltools::HTML)
  ##################
  
  
  
  
LG2001__staffing_map <- leaflet(region,height = 1230, width=1230, padding = 0, options = leafletOptions(zoomControl = FALSE,
                                                                                                        minZoom = 6)) %>% 
  setView(RO_Centroids$LONGITUDE[i], RO_Centroids$LATITUDE[i], zoom = RO_Centroids$SIZE_MAP[i]) %>%
 # setMaxBounds(lng1 = 131.000333332,
 #              lat1 = -18.992662696,
 #              lng2 = 175.612793,
 #              lat2 = -43.840233) %>% 
  addProviderTiles("CartoDB") %>% 
  addResetMapButton() 

## 
LG2001__staffing_map <- LG2001__staffing_map %>% addPolygons(
  color= "#000000", 
  # fillColor = '#FFFFFF',
  weight = 5,
  # fillOpacity = 0.5,
  highlight = highlightOptions(
    color = "#155e58",
    fillOpacity = 0.01,
    bringToFront = FALSE),
  label = poly_popup,
  labelOptions = labelOptions(
    noHide = F,
    textOnly = TRUE,
    style = list("font-weight" = "bold", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"),
  group = 'Area') 



## Add the tags as different sections
if(nrow(EVCs_region) > 0) {
  
  
  EVC_Venue_details <- list()
  for (j in 1:nrow(EVCs_region)) {
    
    EVC_Venue_details[[j]] <- htmlTable::htmlTable(Location_positions %>% filter(StreetAddressID == EVCs_region[j,'StreetAddressID']) %>% 
                                                     filter(LocationStatusCode != "It's Away") %>%
                                                     dplyr::select(Position = POSITIONTYPECODE,Status = POSITIONSTATUS,
                                                            FirstName = PREFERREDFIRSTNAME,SurName = SURNAME,TimeToRO = Time),
                                                   rnames = F, css.cell = "padding-left: .5em; padding-right: .2em;",
                                                   align = "r") 
  }
  
  EVC_popuplabel_1 <- sprintf(
    "<strong>%s</strong>" ,
    EVCs_region$VenueName
  ) %>% lapply(htmltools::HTML) 
  
  EVC_popuplabel_2 <- sprintf("%s",
                              EVC_Venue_details) %>% lapply(htmltools::HTML) 
  
  
  ## Add early voting centres 
  LG2001__staffing_map <- LG2001__staffing_map %>% addMarkers(EVCs_region, lat = EVCs_region$Latitude, lng = EVCs_region$Longitude,
                                                              icon = list(
                                                                iconUrl = '//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI/marker-icon-blue.png',
                                                                iconSize = c(18, 30),
                                                                iconAnchorX = 12, iconAnchorY = 45),
                                                              
                                                              label = EVCs_region$VenueName,
                                                              labelOptions = labelOptions(textsize = 12,
                                                                                          textOnly = TRUE,
                                                                                          noHide = F,
                                                                                          direction = 'center',
                                                                                          offset=c(0,0),
                                                                                          style = list(
                                                                                            "font-weight" = "bold",
                                                                                            "color" = "black",
                                                                                            "font-family" = "serif",
                                                                                            "font-size" = "15px"
                                                                                          )),
                                                              popup = paste0(EVCs_region$Time,' driving to RO'),
                                                              group = 'Venues')
}
# 
# if(nrow(DIs_region) > 0) {
#   
#   
# 
#   ## Add DI
#   LG2001__staffing_map <- LG2001__staffing_map %>% addCircleMarkers(DIs_region, lat = DIs_region$Latitude, lng = DIs_region$Longitude,
#                                                               radius = 5,
#                                                               color = 'orange',
#                                                               stroke = FALSE, fillOpacity = 0.9,
#                                                               
#                                                               label = DIs_region$VenueName,
#                                                               labelOptions = labelOptions(textsize = 12,
#                                                                                           textOnly = TRUE,
#                                                                                           noHide = F,
#                                                                                           direction = 'center',
#                                                                                           offset=c(0,0),
#                                                                                           style = list(
#                                                                                             "font-weight" = "bold",
#                                                                                             "color" = "black",
#                                                                                             "font-family" = "serif",
#                                                                                             "font-size" = "15px"
#                                                                                           )),
#                                                               popup = paste0(DIs_region$VenueName,' <br>',DIs_region$Time,' driving to RO'),
#                                                               group = 'Venues')
# }
## If there are any voting centres, add them in
if(nrow(VCs_region) > 0) {
  
  
  
  
  VC_Venue_details <- list()
  for (j in 1:nrow(VCs_region)) {
    
    VC_Venue_details[[j]] <- htmlTable::htmlTable(Location_positions %>% filter(StreetAddressID == VCs_region[j,'StreetAddressID']) %>% 
                                                  #  filter(LocationStatusCode != "It's Away") %>%
                                                    dplyr::select(Position = POSITIONTYPECODE,Status = POSITIONSTATUS,
                                                           FirstName = PREFERREDFIRSTNAME, SurName = SURNAME),
                                                  rnames = F, css.cell = "padding-left: .5em; padding-right: .2em;",
                                                  align = "r") 
  }
  
  VC_popuplabel_1 <- sprintf(
    "<strong>%s</strong> Time to RO: %s"
    ,VCs_region$VenueName
    ,VCs_region$Time 
  ) %>% lapply(htmltools::HTML) 
  
  VC_popuplabel_2 <- sprintf("%s",
                             VC_Venue_details) %>% lapply(htmltools::HTML) 
  
  
  # VC 
  LG2001__staffing_map <- LG2001__staffing_map %>% addMarkers(VCs_region, lat = VCs_region$Latitude, lng = VCs_region$Longitude,
                                                              icon = list(
                                                                iconUrl = '//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI/marker-icon-black.png',
                                                                iconSize = c(15, 25),
                                                                iconAnchorX = 7, iconAnchorY = 25),
                                                              
                                                              label = VCs_region$VenueName,
                                                              labelOptions = labelOptions(textsize = 12,
                                                                                          textOnly = TRUE,
                                                                                          noHide = F,
                                                                                          direction = 'center',
                                                                                          offset=c(0,0),
                                                                                          style = list(
                                                                                            "font-weight" = "bold",
                                                                                            "color" = "black",
                                                                                            "font-family" = "serif",
                                                                                            "font-size" = "15px"
                                                                                          )),
                                                              popup = paste0(VC_popuplabel_1, '<br>',VC_popuplabel_2),
                                                              group = 'Venues')
}



## If there are any completed voting centres, add them in
if(nrow(Completed_VCs_region) > 0) {
  
  
  
  Completed_VCs_Venue_details <- list()
  for (j in 1:nrow(Completed_VCs_region)) {
    
    Completed_VCs_Venue_details[[j]] <- htmlTable::htmlTable(Location_positions %>% filter(StreetAddressID == Completed_VCs_region[j,'StreetAddressID']) %>% 
                                                               dplyr::select(Position = POSITIONTYPECODE,Status = POSITIONSTATUS,
                                                                      FirstName = PREFERREDFIRSTNAME,SurName = SURNAME), #,TimeToRO = Time),  
                                                             rnames = F, css.cell = "padding-left: .5em; padding-right: .2em;",
                                                             align = "r") 
  }
  
  Completed_VCs_Venue_details <- gsub('<td', '<td nowrap="nowrap"; ', Completed_VCs_Venue_details)
  
  Completed_VCs_popuplabel_1 <- sprintf(
    "<strong>%s</strong> Time to RO: %s"
    ,Completed_VCs_region$VenueName
    ,Completed_VCs_region$Time
    
  ) %>% lapply(htmltools::HTML) 
  
  Completed_VCs_popuplabel_2 <- sprintf("%s",
                                        Completed_VCs_Venue_details) %>% lapply(htmltools::HTML) 
  
  
  
  
  # VC 
  LG2001__staffing_map <- LG2001__staffing_map %>% addMarkers(Completed_VCs_region, lat = Completed_VCs_region$Latitude, lng = Completed_VCs_region$Longitude,
                                                              icon = list(
                                                                iconUrl = '//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI/house.png',
                                                                iconSize = c(30, 30),
                                                                iconAnchorX = 12, iconAnchorY = 30),
                                                              
                                                              label = Completed_VCs_region$VenueName,
                                                              labelOptions = labelOptions(textsize = 12,
                                                                                          textOnly = TRUE,
                                                                                          noHide = F,
                                                                                          direction = 'center',
                                                                                          offset=c(0,0),
                                                                                          style = list(
                                                                                            "font-weight" = "bold",
                                                                                            "color" = "black",
                                                                                            "font-family" = "serif",
                                                                                            "font-size" = "15px"
                                                                                          )),
                                                              popup = paste0(Completed_VCs_popuplabel_1, '<br>' ,Completed_VCs_popuplabel_2),
                                                              group = 'Completed Venues')
}




## There are any election managers offices, add them in 
if(nrow(EMOs_region) > 0) {
  
  ## EMO
  LG2001__staffing_map <- LG2001__staffing_map %>% addMarkers(EMOs_region, lat = EMOs_region$Latitude, lng = EMOs_region$Longitude,
                                                              icon = list(
                                                                iconUrl = '//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI/marker-icon-red.png',
                                                                iconSize = c(18, 30),
                                                                iconAnchorX = 12, iconAnchorY = 45),
                                                              
                                                              label = EMOs_region$VenueName,
                                                              labelOptions = labelOptions(textsize = 12,
                                                                                          textOnly = TRUE,
                                                                                          noHide = F,
                                                                                          direction = 'center',
                                                                                          offset=c(0,0),
                                                                                          style = list(
                                                                                            "font-weight" = "bold",
                                                                                            "color" = "black",
                                                                                            "font-family" = "serif",
                                                                                            "font-size" = "15px"
                                                                                          )),
                                                              popup = paste0(EMO_popuplabel_1,'<br>',EMO_popuplabel_2),
                                                              group = 'Venues')
}


## Now add in the staff
## People added as circle, add marker vs add circle marker
if(nrow(hired_staff_region) > 0) {
  
  
  hired_staff_region_popuplabels <- sprintf(
    "%s <strong>%s</strong><br>Other Lan: %s <br>Aboriginal: %s<br>Position: %s <br>Status: %s
    <br>
    " ,
    hired_staff_region$PREFERREDFIRSTNAME
    ,hired_staff_region$SURNAME
    ,hired_staff_region$SECONDLANGUAGE
    ,hired_staff_region$ATSI
    ,hired_staff_region$POSITION
    ,hired_staff_region$POSITIONSTATUS
#    ,paste0(round(hired_staff_region$DRIVINGDISTMETERS/1000,1),' Km')
#    ,paste0(hired_staff_region$Time,' Mins')
  ) %>% lapply(htmltools::HTML) 
  
  
  # hired_staff 
  LG2001__staffing_map <- LG2001__staffing_map %>% addCircleMarkers(hired_staff_region, lat = hired_staff_region$lat, lng = hired_staff_region$long,
                                                                    label = paste0(hired_staff_region$PREFERREDFIRSTNAME,' ',hired_staff_region$SURNAME),
                                                                    labelOptions = labelOptions(
                                                                      noHide = F,
                                                                      textOnly = TRUE,
                                                                      style = list("font-weight" = "bold", padding = "3px 8px"),
                                                                      textsize = "12px",
                                                                      direction = "auto"),
                                                                    popup = hired_staff_region_popuplabels,
                                                                    radius = 5,
                                                                    weight = 0,
                                                                    fillColor = circle_color[3],
                                                                    fillOpacity = 0.7,
                                                                    group = 'Hired/Active')
}


if(nrow(EOI_SOA_region) > 0) {
  
  EOI_SOA_region_popuplabels <- sprintf(
    "%s <strong>%s</strong><br>Other Lan: %s <br>Aboriginal: %s<br>Previous Role: %s <br>Status: %s
    <br>1: %s 
    <br>2: %s 
    <br>3: %s" ,
    EOI_SOA_region$PREFERREDFIRSTNAME
    ,EOI_SOA_region$SURNAME
    ,EOI_SOA_region$SECONDLANGUAGE
    ,EOI_SOA_region$ATSI
    ,EOI_SOA_region$PREVIOUSPOSITION
    ,EOI_SOA_region$STAFFSTATUS
    ,EOI_SOA_region$`NEAREST_1`
    ,EOI_SOA_region$`NEAREST_2`
    ,EOI_SOA_region$`NEAREST_3`
    ) %>% lapply(htmltools::HTML) 
 
  
    # ATSI_staff 
    LG2001__staffing_map <- LG2001__staffing_map %>% addCircleMarkers(EOI_SOA_region, lat = EOI_SOA_region$lat, lng = EOI_SOA_region$long,
                                                                      label = paste0(EOI_SOA_region$PREFERREDFIRSTNAME,' ',EOI_SOA_region$SURNAME),
                                                                      labelOptions = labelOptions(
                                                                        noHide = F,
                                                                        textOnly = TRUE,
                                                                        style = list("font-weight" = "bold", padding = "3px 8px"),
                                                                        textsize = "12px",
                                                                        direction = "auto"),
                                                                      popup = EOI_SOA_region_popuplabels,
                                                                      radius = 4,
                                                                      weight = 0,
                                                                      fillColor = circle_color[2],
                                                                      fillOpacity = 0.7,
                                                                      group = 'SOA')
    
  }
  



if(nrow(EOI_EO_region) > 0) {
  
  EOI_EO_region_popuplabels <- sprintf(
    "%s <strong>%s</strong><br>Other Lan: %s <br>Aboriginal: %s<br>Previous Role: %s <br>Status: %s
    <br>1: %s 
    <br>2: %s 
    <br>3: %s" ,
    EOI_EO_region$PREFERREDFIRSTNAME
    ,EOI_EO_region$SURNAME
    ,EOI_EO_region$SECONDLANGUAGE
    ,EOI_EO_region$ATSI
    ,EOI_EO_region$PREVIOUSPOSITION
    ,EOI_EO_region$STAFFSTATUS
    ,EOI_EO_region$`NEAREST_1`
    ,EOI_EO_region$`NEAREST_2`
    ,EOI_EO_region$`NEAREST_3`
  ) %>% lapply(htmltools::HTML) 
  
  
  # ATSI_staff 
  LG2001__staffing_map <- LG2001__staffing_map %>% addCircleMarkers(EOI_EO_region, lat = EOI_EO_region$lat, lng = EOI_EO_region$long,
                                                                    label = paste0(EOI_EO_region$PREFERREDFIRSTNAME,' ',EOI_EO_region$SURNAME),
                                                                    labelOptions = labelOptions(
                                                                      noHide = F,
                                                                      textOnly = TRUE,
                                                                      style = list("font-weight" = "bold", padding = "3px 8px"),
                                                                      textsize = "12px",
                                                                      direction = "auto"),
                                                                    popup = EOI_EO_region_popuplabels,
                                                                    radius = 4,
                                                                    weight = 0,
                                                                    fillColor = circle_color[1],
                                                                    fillOpacity = 0.7,
                                                                    group = 'EO')
  
}

Venue_searchable  <- Potential_Venues_raw %>% 
  filter(LocationStatusCode != "It's Away") %>%
  left_join(RO_region) %>%
  filter(LGArea %in% RO_region$LGArea)
  
  
  searchables <- bind_rows(Staff_searchable %>% dplyr::select(long,lat,A1=PREFERREDFIRSTNAME,A2=SURNAME),
                           Venue_searchable %>% dplyr::select(long = Longitude,lat = Latitude, A1 = VenueName,A2=RO_Office))
  
  ## Add legend 
  LG2001__staffing_map <- LG2001__staffing_map %>%  addLegend(
    position = 'bottomright',
    colors = circle_color,
    labels = circle_label,
    opacity = 0.7,
    title = 'Applicants') %>%
    
    ## Important
    addMarkers(searchables,
               lat = searchables$lat,lng = searchables$long,
               label = paste0(searchables$A1,' ',searchables$A2),
               group = 'searchable') %>%
    
    ## features 
    addSearchFeatures(
      targetGroups = "searchable",
      options = searchFeaturesOptions(
        zoom=18, openPopup = TRUE, firstTipSubmit = TRUE,
        autoCollapse = TRUE, hideMarkerOnCollapse = TRUE )) %>%
    hideGroup('searchable') %>%
    
    #  addLegend(position = 'bottomright',
    #            color = "yellow", 
    #            labels = 'Multilingual') %>%
    #addControl("<P>Search by applicant name with the search icon</P>",
    #           position='bottomright') %>%
    
    addControl(html = html_legend, position = "bottomleft") %>%
    addLayersControl(
      overlayGroups = c("SOA", "EO","Hired/Active","Venues","Completed Venues"),
      options = layersControlOptions(collapsed = TRUE)
    ) 
  
  
 
  
  # Tables ------------------------------------------------------------------
  
  table_positions <- Opened_Positions_region %>% ungroup() %>% dplyr::select(-RO_Office,
                                                                            -StreetAddressID) 
  

  
  table_applicants <-data.frame(`Available applicants` = nrow(EOI %>% filter(AREACODE %in% RO_region$LGArea))
                         ,`Positions yet to fill` = sum(Opened_Positions_region$OPEN))
  
  # table__staffing_status <- SingleChoiceTable(EMA_staffing,'STAFFSTATUS',TRUE)
  
  # SingleChoiceTable(Venue_staffing,'POSITIONSTATUS',TRUE)
  
  
  Lan <- bind_rows(EOI %>% filter(AREACODE %in% RO_region$LGArea) %>% dplyr::select(SECONDLANGUAGE) %>% mutate(Status = 'EOI')
           ,EMPLOYED %>% filter(AREACODE %in% RO_region$LGArea) %>% dplyr::select(SECONDLANGUAGE) %>% mutate(Status = 'Hired')
            )
 
   if (nrow(Lan) > 0) {
    
  table_lan <- CrossTab(Lan, 'SECONDLANGUAGE','Status')
  
  Lan <- Lan %>% mutate(SECONDLANGUAGE = ifelse(is.na(SECONDLANGUAGE),'NONE',SECONDLANGUAGE))
  
  table_lan <- as.data.frame(table(Lan$SECONDLANGUAGE,Lan$Status)) %>% select(SECONDLANGUAGE = Var1,Status = Var2,Count = Freq)
  
  
  } else {
    
    table_lan <- data.frame()
  
  
  
  }
  
  # Charts ------------------------------------------------------------------
  
  Past_exp <- bind_rows(EOI %>% filter(AREACODE %in% RO_region$LGArea) %>% dplyr::select(PREVIOUSPOSITION) %>% mutate(Status = 'EOI')
                        ,EMPLOYED %>% filter(AREACODE %in% RO_region$LGArea) %>% dplyr::select(PREVIOUSPOSITION) %>% mutate(Status = 'Hired')
                              )
  
  gauge_exp <- round(nrow(Past_exp %>% filter(!is.na(PREVIOUSPOSITION)))/nrow(Past_exp),2)*100
  gauge_exp_hired <- round(nrow(Past_exp %>% filter(!is.na(PREVIOUSPOSITION), Status == 'Hired'))/nrow(Past_exp),2)*100
    
  if(nrow(Past_exp %>% filter(!is.na(PREVIOUSPOSITION), Status == 'Hired')) == 0) {
    gauge_exp_hired <- 0
    
  }
  
  ATSI <- bind_rows(EOI %>% filter(AREACODE %in% RO_region$LGArea) %>% dplyr::select(ATSI) %>% mutate(Status = 'EOI')
                                ,EMPLOYED %>% filter(AREACODE %in% RO_region$LGArea) %>% dplyr::select(ATSI) %>% mutate(Status = 'Hired')
                        )
    
  gauge_ATSI_hired <- nrow(ATSI %>% filter(Status =='Hired',ATSI == 'Y'))

  age_dist <- bind_rows(EOI %>% filter(AREACODE %in% RO_region$LGArea) %>% dplyr::select(AgeAtElection) %>% mutate(Status = 'EOI')
                              ,EMPLOYED %>% filter(AREACODE %in% RO_region$LGArea) %>% dplyr::select(AgeAtElection) %>% mutate(Status = 'Hired')
                              )
  if (nrow(age_dist) >0) {
    
    agehist <- hist(age_dist$AgeAtElection)
  
    } else {
    
    agehist <- data.frame()
  }
    
    
  library(highcharter)
    
    
    
  lan_stack <- ggplot(table_lan, aes(fill=Status, y=Count, x=SECONDLANGUAGE)) + 
                      geom_bar(position="stack", stat="identity")


  
  
#  # Write to file -------------------------------------------
#  
  
  setwd(root_dashboard)
  if(file.exists('02_03-EOI-maps.Rmd')) {
    file.remove('02_03-EOI-maps.Rmd')
    
  }
  
  rmarkdown::render(paste0(rmd_files,'Secured pages/','02_03 EOI maps.Rmd'),
                    output_file = paste0(unique(RO_Centroids$RO_OFFICE)[i],' EOI.HTML'),
                    output_dir = paste0(server_root_securedpages))
  
  