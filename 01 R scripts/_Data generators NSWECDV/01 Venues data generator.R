
####################################### ----- VENUES DATA GENERATOR ---- #############################################



## This script extracts Venue-level data and sends to corresponding rmd for knitting
message('Run R code to create Venue level data')


## Projections
## Districts.R, Venues.R
Venue_Projections <- sqlExecute(EMS_connection,
                           "SELECT
	EMS_Event.EventGroupId,
	Ordinary.EventID,
	EMS_EventLocation.LocationTypeCode,
    Ordinary.VenueName,
	loc.StreetAddressID,
    Ordinary.VotingChannelCode,
    Ordinary.RedistributionCode,
    Ordinary.AreaCode,
    Ordinary.ProjectedVoters as OrdinaryVotes,
    ISNULL(Absent.ProjectedVoters,0) as AbsentVotes,
    ISNULL(Other.ProjectedVoters,0) as OtherVotes,
	ISNULL(Absent.ProjectedVoters,0) + ISNULL(Other.ProjectedVoters,0) as AllDecVotes

FROM
	Events.EMS_Event
	INNER JOIN Events.EMS_EventRollChannels as Ordinary ON
		Ordinary.EventID = EMS_Event.EventID
	INNER JOIN Redistributions.EMS_VotingChannel ON
		EMS_VotingChannel.VotingChannelCode = Ordinary.VotingChannelCode
	INNER JOIN Events.EMS_EventLocation ON
		EMS_EventLocation.EventID = EMS_Event.EventID AND
		EMS_EventLocation.VenueName = Ordinary.VenueName
	INNER JOIN Events.EMS_EventLocationStatus ON
		EMS_EventLocationStatus.LocationTypeCode = EMS_EventLocation.LocationTypeCode AND
		EMS_EventLocationStatus.LocationStatusCode = EMS_EventLocation.LocationStatusCode
	LEFT JOIN Events.EMS_EventRollChannels as Absent ON
		Absent.EventID = Ordinary.EventID and
		Absent.VenueName = Ordinary.VenueName and
		Absent.RedistributionCode = Ordinary.RedistributionCode and
		Absent.AreaCode = Ordinary.AreaCode and
		Absent.VotingChannelCode = 'Absent'
	LEFT JOIN Events.EMS_EventRollChannels as Other ON
		Other.EventID = Ordinary.EventID and
		Other.VenueName = Ordinary.VenueName and
		Other.RedistributionCode = Ordinary.RedistributionCode and
		Other.AreaCode = Ordinary.AreaCode and
		Other.VotingChannelCode = 'Other'
		  left join [Resources].[EMS_Location] loc on loc.VenueName = EMS_EventLocation.VenueName

WHERE
	EMS_VotingChannel.IsOrdinary = 'Y' and
--	Ordinary.VotingChannelCode = 'Ordinary' and
	Ordinary.ProjectedVoters IS NOT NULL and
	EMS_EventLocationStatus.CanExport = 'Y' and
	EMS_EventLocation.LocationStatusCode <> 'Handled Offline'
	and EventGroupId = ?",
                           event_group_ID,
                           fetch = TRUE,
                           stringsAsFactors = FALSE) %>%
  as_tibble()



## Opening_hour for prepolls
opening_hour <- sqlExecute(EMS_connection, 
                           "SELECT   evp.EventID,
                 ea.AreaCode,
                 evp.VenueName,
                 datename(dw, [date]) + ', ' + CONVERT(varchar, date, 106) OpeningDate, [Date], IsOpen,
                 loc.StreetAddressID
				  ,addr.Latitude
	  ,addr.Longitude
	  ,addr.AddressName
	  ,addr.AddressLine1
	  ,addr.AddressLine2
	  ,addr.LocalityName
	  ,addr.PostCode
                 FROM [Events].[EMS_EventLocationOpeningHours] evp
                 LEFT JOIN [Resources].[EMS_Location] loc on evp.VenueName = loc.VenueName
                 LEFT JOIN [Events].[EMS_EventRollArea] ea on EVP.EventID= ea.EventID
				 left join [Resources].[EMS_Address] addr on loc.StreetAddressID = addr.AddressID
				 where evp.eventid like ?", 
                           paste0(event_group_ID,'%'),
                           fetch = TRUE,
                           stringsAsFactors = FALSE) %>%
  as_tibble()



message('Data successfully run')
#------------------------------------------------------------------------------

