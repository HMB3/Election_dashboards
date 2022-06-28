#Ro Ops Data generator---------

#note - previous work saved in archive.
#data is generated in main dashboards, then the relevant files saved as csvs.


setwd(data_files)

# Pre-poll ----------------------------------------------------

Pre_Poll_Data <- read_csv("RO_Early_Vote_Training.csv")


#dec voting----------------


  Dec_Vote_Data <- read_csv("RO_Dec_Vote_Training.csv")
  




#ballot track-------------

Ballot_Track_Data <- read_csv("RO_Ballot_Track_Training.csv")




#additional data required------------


#use base data to get RO List.

base_data_councils <- read.csv(paste0(base_data_working_folder, "/Base_data_councils.csv"),
                               stringsAsFactors = FALSE, na.strings = "") %>%
  filter(!is.na(ReturningOffice))%>%
  as_tibble()



