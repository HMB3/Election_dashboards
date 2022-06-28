
####################################### ----- COUNT CENTRE DATA GENERATOR ---- #############################################

# Dashboard to support the count centre operations management tool


# - Add more detial to the count centre generator/processor
# - Put the requirements in the R scripts.
# - Use the staffing and RMT scripts and RMD's as templates 



# Source of Data
# .	Base data
# .	Election Systems (e.g. EMA, PRCC, Gatekeeper)
# 
# For Mayor
# .	Base Data: Venue/Vote Type Projections (Ballot Papers for Mayor)
# .	EMA; Initial Count and Check Count
# 
# For Councillor
# .	Base Data; Venue/Vote Type Projections (Ballot Papers for Clr, % ATL)
# .	EMA; Initial Count and Check Count of formal Ballot Papers
# .	PRCC; Batch Registration details (Total BP's for Data Entry), BP R1 To Be Entered, BP R2 To Be Entered




## Forecast Data
forecastData                <- sqlExecute(hyperion_connection,
                                          "SELECT *
                                          FROM [CountCentreOps].[Schedule].[ForecastData]
                                          WHERE EventGroupID = ?",
                                          event_group_ID,
                                          fetch            = TRUE,
                                          stringsAsFactors = FALSE)


## Forecast summary by Contest
forecastSummaryByContest    <- sqlExecute(hyperion_connection,
                                          "SELECT *
                                          FROM [CountCentreOps].[Schedule].[ForecastSummaryContest]
                                          WHERE EventGroupID = ?",
                                          event_group_ID,
                                          fetch            = TRUE,
                                          stringsAsFactors = FALSE)


## Forecast summary by Count Location
forecastSummaryCountLocation <- sqlExecute(hyperion_connection,
                                           "SELECT *
                                           FROM [CountCentreOps].[Schedule].[ForecastSummaryCountLocation]                                          
                                           WHERE EventGroupID = ?",
                                           event_group_ID,
                                           fetch            = TRUE,
                                           stringsAsFactors = FALSE)


## Forecast summary by Team
forecastSummaryCountTeams    <- sqlExecute(hyperion_connection,
                                           "SELECT *
                                           FROM [CountCentreOps].[SourceData].[LGCountLocationContests]                                          
                                           WHERE EventGroupID = ?",
                                           event_group_ID,
                                           fetch            = TRUE,
                                           stringsAsFactors = FALSE)


## LG Count location summary by Contest
LGCountLocationContests       <- sqlExecute(hyperion_connection,
                                            "SELECT *
                                            FROM [CountCentreOps].[SourceData].[LGCountVenueDec]                                          
                                            WHERE EventGroupID = ?",
                                            event_group_ID,
                                            fetch            = TRUE,
                                            stringsAsFactors = FALSE)


## for the RO Progress data page...
RO_progress_data             <- sqlExecute(hyperion_connection,
                                           "SELECT *
                                            FROM [CountCentreOps].[Schedule].[ProgressSummaryROContest]
                                            WHERE EventGroupID = ?",
                                           event_group_ID,
                                           fetch            = TRUE,
                                           stringsAsFactors = FALSE)





# Close db connections
# odbcCloseAll()


