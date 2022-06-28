####################################################################################################################
########################################### VENUES DATA GENERATOR ---- #############################################
####################################################################################################################





## This code pulls the data for the 'Candidate nominations' dashboard
message('Run R code to generate Venues data')


## The code below should run off the event ID - EG now the event has been created, Liam enters things into the 
## database which are captured by the event ID


## Note the administritive hierarchy :: Council - Venue - Ward


## Shared with venues, 
## duplicated so that each file can run independently if needed
Pre_poll_projections <- sqlExecute(hyperion_connection,
                                   "SELECT * FROM [ElectionData].[Views].[VenueProjectionsPrepoll]",
                                   fetch = TRUE,
                                   stringsAsFactors = FALSE)


## Districts.R, Venues.R
Other_Venues <- sqlExecute(hyperion_connection,
                           "SELECT * FROM [ElectionData].[Views].[VenueProjectionsOrdinary]",
                           fetch = TRUE,
                           stringsAsFactors = FALSE)

## Districts.R, Venues.R
opening_hour <- sqlExecute(hyperion_connection, 
                           "SELECT * FROM [ElectionData].[Views].[VenueOpeningHours]", 
                           fetch = TRUE,
                           stringsAsFactors = FALSE)


## Set the opening hours and times for Venues ---
opening_hours <- opening_hour %>% filter(IsOpen == 'Y')
opening_days  <- as.data.frame(table(opening_hours$StreetAddressID)) %>% 
  mutate(StreetAddressID = Var1,
         Count = Freq)


opening_hours <- opening_hours %>% 
  group_by(AreaCode,VenueName,StreetAddressID) %>%
  summarise(Days = n()) %>% 
  ungroup() %>%
  mutate(Days = ifelse((Days>11 | is.na(Days)), 11, Days))


Pre_poll_projection_districts <- Pre_poll_projections %>%
  left_join(opening_hours, by = c('AreaCode'='AreaCode'
                                 ,'VenueName'='VenueName'
                                 ,'StreetAddressID'='StreetAddressID'))


## Sydney Town hall venues
STH <- sqlExecute(hyperion_connection,
                  "SELECT 
                  [AddressName]
                  ,[Latitude]
                  ,[Longitude]
                  FROM [ElectionData].[Staging].[EMS_Address]
                  where AddressName like 'Sydney Town Hall'",
                  fetch=TRUE,
                  stringsAsFactors = FALSE) %>% distinct(Latitude, Longitude)





