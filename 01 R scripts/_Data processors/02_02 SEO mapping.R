
# Title ---------SEO mapping----------------------------------------------------------



# Initialisation ----------------------------------------------------------

base_data_folder <- 'G:/Transfer/Data Analytics/LG2001 Basedata/'
base_data_file <- tail(list.files(base_data_folder, pattern = "LG2001 Basedata v*"), n= 1)


SEO_merit_folder <- 'G:/Election Events/LGE 2020 Programme/AP09 Data Management/21 Deliverables/03 SEO staffing map/02 Source files/'


# Local files source queries ----------------------------------------------
# read base data polling place

polling_venues <- read.xlsx(paste0(base_data_folder,base_data_file),sheet='Venues by Ward')

# read staff details from Janet  
setwd(SEO_merit_folder)

# remove duplicate column names
headers <- read.xlsx('NSWEC Merit List_Updated 20.9.18_merit and unsucc.xlsx',sheet = 'Merit list', rows = 1)
Merit <- read.xlsx('NSWEC Merit List_Updated 20.9.18_merit and unsucc.xlsx',sheet = 'Merit list',startRow = 1) 
colnames(Merit) <- c(make.unique(colnames(headers)),paste0('X',seq(length(Merit)-length(headers))))


# Data cleansing and analysis---------------------------------------------------------
LGA_RO <- polling_venues %>% group_by(LGAreaCode, ReturningOffice) %>% 
                  summarise(TotalProjectedVotes = sum(TotalProjectedVotes)) %>%
                  mutate(RO_Address = gsub('RO Office','Council NSW',ReturningOffice))


# Geos and statuses already on the server ---------------------------------
SEO_geoed_RO_Offices <- sqlExecute(hyperion_connection,
                             "SELECT [LGAreaCode]
                                    ,[ReturningOffice]
                                    ,[RO_Address]
                                    ,[lat]
                                    ,[long]
                                    ,[key]
                                FROM [ProcessingData].[dbo].[SEO_geoed_RO_Offices]
                              ",
                             fetch = TRUE,
                             stringsAsFactors = FALSE)


SEO_geoed_applicants <- sqlExecute(hyperion_connection,
                                   "SELECT [FirstName]
                                           ,[LastName]
                                           ,[Address]
                                           ,[lat]
                                           ,[long]
                                           ,[key]
                                       FROM [ProcessingData].[dbo].[SEO_geoed_applicants]
                              ",
                                   fetch = TRUE,
                                   stringsAsFactors = FALSE)

# join database with whats on the server ----------------------------------

ROs_requireGEO <- LGA_RO %>% anti_join(SEO_geoed_RO_Offices,by = c("LGAreaCode", "ReturningOffice", "RO_Address"))
Applications_requiredGEO <- Merit %>%
                              mutate(Unique.ID = Unique.ID) %>% anti_join(SEO_geoed_applicants,
                                                by=c('Last.Name'='LastName'
                                                     ,'First.Name'='FirstName'
                                                     ,'Unique.ID'='key'))


# Address lookups ----------------------------------------------------------
setwd(rmd_files)
if (nrow(ROs_requireGEO) >0) {
  
baseurl <- paste0('https://maps.googleapis.com/maps/api/geocode/json?address=',ROs_requireGEO$RO_Address,'&key=',api_key)

RO_Geocodes <- data.frame()

for (i in 1:nrow(ROs_requireGEO)) {
  
print(paste0(i,'/',nrow(ROs_requireGEO)))  
  
download.file(baseurl[i], destfile = "ram.html")

RO_Geocodes <- RO_Geocodes %>% bind_rows(readFromGoogledRam(ROs_requireGEO,'RO_Address'))

}

# RO Geos latter will get this from ems-----------------------------------------

LGA_RO_Geoed <- ROs_requireGEO %>% bind_cols(RO_Geocodes %>% select(-Address)) %>% ungroup() %>%
  select(-TotalProjectedVotes) %>%
  bind_rows(SEO_geoed_RO_Offices) %>%
  mutate(key = rownames(.))


sqlExecute(hyperion_connection,
           "DELETE [ProcessingData].[dbo].[SEO_geoed_RO_Offices]
           DBCC CHECKIDENT ('[ProcessingData].[dbo].[SEO_geoed_applicants]', RESEED, 0)",
           fetch = FALSE)

sqlExecute(hyperion_connection,
           "INSERT INTO [ProcessingData].[dbo].[SEO_geoed_RO_Offices]
           (
           [LGAreaCode]
      ,[ReturningOffice]
      ,[RO_Address]
      ,[lat]
      ,[long]
      ,[key]
           ) VALUES (?,?,?,?,?,?)",
           data = LGA_RO_Geoed)
} else {
  
  LGA_RO_Geoed <- SEO_geoed_RO_Offices
  
}


# get total votes
LGA_RO_Geoed <- LGA_RO_Geoed %>% left_join(LGA_RO)

if (nrow(Applications_requiredGEO) >0) {
# clear characters in the provided addresses
Applications_requiredGEO <- Applications_requiredGEO %>% 
         mutate(Residental.Address = gsub('#|NA',' ',iconv(Applications_requiredGEO$Residental.Address, from = 'UTF-8', to = 'ASCII//TRANSLIT'))) %>%
         mutate(Residental.Address = ifelse(grepl('NSW',Residental.Address),Residental.Address,paste0(Residental.Address,' NSW')))


baseurl <- paste0('https://maps.googleapis.com/maps/api/geocode/json?address=',Applications_requiredGEO$Residental.Address,'&key=',api_key)

Staff_Geocodes <- data.frame()

for (i in 1:nrow(Applications_requiredGEO)) {
  
  print(paste0(i,'/',nrow(Applications_requiredGEO)))  
  
  download.file(baseurl[i], destfile = "ram.html")
  
  Staff_Geocodes <- Staff_Geocodes %>% bind_rows(readFromGoogledRam(Applications_requiredGEO,'Residental.Address'))
  
}



# Staff Geos -----------------------------------------------

Staff_Geoed <- Applications_requiredGEO %>% select(FirstName = First.Name
                                ,LastName = Last.Name
                                ,Address = Residental.Address
                                ,PreviousExperience = Previous.election.manager
                                ,OverallRanking = Overall.Rank
                                ,key = Unique.ID) %>%
  bind_cols(Staff_Geocodes %>% select(-Address)) %>%
  select(-PreviousExperience
         ,-OverallRanking) %>%
  bind_rows(SEO_geoed_applicants)



sqlExecute(hyperion_connection,
           "DELETE [ProcessingData].[dbo].[SEO_geoed_applicants]
           DBCC CHECKIDENT ('[ProcessingData].[dbo].[SEO_geoed_applicants]', RESEED, 0)",
           fetch = FALSE)

sqlExecute(hyperion_connection,
           "INSERT INTO [ProcessingData].[dbo].[SEO_geoed_applicants] 
           (
           [FirstName]
          ,[LastName]
          ,[Address]
          ,[key]
          ,[lat]
          ,[long]
           ) VALUES (?,?,?,?,?,?)",
           data = Staff_Geoed)




} else {
  
  Staff_Geoed <- SEO_geoed_applicants
}


# Mapping -----------------------------------------------------------------
setwd('G:/Transfer/Data Analytics/R mapping files')
LG_2001_raster <- rgdal::readOGR(dsn="Compact_LGA", layer="Compact_LGA")
LG_2001_raster <- subset(LG_2001_raster,NSW_LGA__2 != 'UNINCORPORATED' & as.Date(DT_CREATE) > '2015-01-01')

# assign RO to LGA & color
LG_2001_raster@data <- LG_2001_raster@data %>% mutate(NSW_LGA_join = gsub(' SHIRE| COUNCIL| REGIONAL','',NSW_LGA__3)) %>% 
                            left_join(LGA_RO_Geoed %>% 
                                        mutate(LGAreaCode = toupper(LGAreaCode)) %>%
                                        select(LGAreaCode,ReturningOffice,TotalProjectedVotes), by=c('NSW_LGA_join'='LGAreaCode')) %>%
                                      mutate(ReturningOffice = ifelse(is.na(ReturningOffice),NA,ReturningOffice)
                                             ,TotalProjectedVotes = ifelse(is.na(TotalProjectedVotes),0,TotalProjectedVotes))

#LG_2001_raster@data <- LG_2001_raster@data %>% arrange(LG_2001_raster@data$LGA_PID)
#View(LG_2001_raster@data)
# Mapping attributes below -----------------------------------------------------------


#labels   

# polygon   
poly_popuplabels <- sprintf(
  "<strong>%s</strong><br>ReturningOffice: %s<br>Projected: %s<br>" ,
  LG_2001_raster$NSW_LGA__3
  ,LG_2001_raster$ReturningOffice
  ,LG_2001_raster$TotalProjectedVotes
  ) %>% lapply(htmltools::HTML) 


staff_popuplabels <- sprintf(
  "%s <strong> %s </strong><br>Address: %s<br>" , 
  Staff_Geoed$FirstName
  ,Staff_Geoed$LastName
  ,Staff_Geoed$Address
#  ,Staff_Geoed$PreviousExperience
#  ,Staff_Geoed$OverallRanking
  
) %>% lapply(htmltools::HTML) 


colorPal <- colorFactor(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]#palette(rainbow(78))
                        ,LG_2001_raster$ReturningOffice,na.color='#808080')


circle_color <- c('#4e7a09','#b50e90','#070460','#ed8c42')
circle_label <- c('VC','EVC','REO','DI')

#rm(Area_Summary_Map)


Area_Summary_Map <- leaflet(LG_2001_raster,height = 830, width=800, padding = 0) %>% 
  setView(148.5000486, -33.0727653, zoom = 7) %>%
  setMaxBounds(lng1 = 131.000333332,
               lat1 = -18.992662696,
               lng2 = 175.612793,
               lat2 = -43.840233) %>%
  addResetMapButton(.)
 # addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%  # this line now kills the map somthow needing investigation

Area_Summary_Map <- Area_Summary_Map %>% addPolygons(
  color= "#000000", 
  fillColor = ~colorPal(LG_2001_raster$ReturningOffice),
  weight = 2,
  fillOpacity = 0.9,
  highlight = highlightOptions(
    color = "#155e58",
    fillOpacity = 0.1,
    bringToFront = FALSE),
  label = LG_2001_raster$ReturningOffice,
 # labelOptions = labelOptions(
 #   noHide = T,
 #   textOnly = TRUE,
 #   style = list("font-weight" = "bold", padding = "3px 8px"),
 #   textsize = "25px",
 #   direction = "auto"),
   popup = poly_popuplabels, #LG_2001_raster$NSW_LGA__2,
  group = 'Regions') 

# REO
Area_Summary_Map <- Area_Summary_Map %>% addMarkers(LGA_RO_Geoed, lat = LGA_RO_Geoed$lat, lng = LGA_RO_Geoed$long,
                                                         
                                                          popup = LGA_RO_Geoed$ReturningOffice,
                                                          group = 'REO') 
# SEOs
  
  Area_Summary_Map <- Area_Summary_Map %>% addCircleMarkers(Staff_Geoed, lat = Staff_Geoed$lat, lng = Staff_Geoed$long,
                                                            radius = 5,
                                                            weight = 1,
                                                            color = '#FFFFFF',
                                                            fillColor = '#ff0000',
                                                            fillOpacity = 0.99,
                                                      popup = staff_popuplabels,
                                                      group = 'SEO')
  
#  addMarkers( searchables,lat = searchables$Latitude,lng = searchables$Longitude,
#              label = searchables$VenueName,
#              group = 'ALL') %>%
  # features 
  
#  addSearchFeatures(
#    targetGroups = "ALL",
#    options = searchFeaturesOptions(
#      zoom=18, openPopup = TRUE, firstTipSubmit = TRUE,
#      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE )) %>%
#  hideGroup('ALL') 



# Write to file -------------------------------------------
 

