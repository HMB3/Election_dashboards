####################################################################################################################
###################################### HOW TO VOTE DASHBAORD ---- ##################################################
####################################################################################################################


## This code pulls data for the 'how to vote dashboard'
message('Run R code to create data for how-to-vote dashboard')


## Analytics database connections ----
hyperion_connection <- odbcDriverConnect('driver={SQL Server};server=SVRANALYTICS1;trusted_connection=true',
                                         readOnlyOptimize = TRUE)


## Pull data from databases
## Import HTV summary data from SQL database ----
how_to_vote_summary <- sqlExecute(hyperion_connection,
                                  "SELECT * FROM [ElectionData].[Storage].[HowToVote]",
                                  fetch = TRUE,
                                  stringsAsFactors = FALSE)


## Import HTV item data from SQL database ----
how_to_vote_data <- sqlExecute(hyperion_connection,
                               "SELECT * FROM [ElectionData].[Storage].[HTV_items]",
                               fetch = TRUE,
                               stringsAsFactors = FALSE)



####################################################################################################################
#################################################### TBC ###########################################################
####################################################################################################################