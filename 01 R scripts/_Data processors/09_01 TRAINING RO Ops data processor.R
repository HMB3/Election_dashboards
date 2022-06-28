#This process will need to be run for each RO

#all RO Data-----------
#first, organise data that doesn't change from one RO to another.



RO_List <- base_data_councils %>%
  select(ReturningOffice)     %>%
  distinct()                  %>%
  
  #name can get a bit long, so limit to a number that works.
  mutate(Test=nchar(ReturningOffice)) %>%
  mutate(ShortName=str_trunc(ReturningOffice,width=30,ellipsis = "")) %>%
  
  #file structure uses names of regions without spaces
  mutate(FileName = str_remove(ReturningOffice," RO Office"),
         FileName = str_remove_all(FileName," "))


#also create a look up list for councils and ROs.

RO_Council_Lookup <- base_data_councils%>%
  select(EventID,ElectoralAreaName,LGAreaCode,ReturningOffice)%>%
  distinct()


#set up the status stage names for Ballot Track.


#Set RO File locations------------
#temp file to save in - eventually needs to be on F drive somewhere
# ROFileStructure <- paste0(Sys.getenv('source_control_local_path'),
#                           'Election_Events/LG2101/02 Dashboard/03 Working pages/RO Pages/')

ROFileStructure <- "Z:/"

#if testing on server, need to create a different location on server

if (Test_on_server) {
  ROFileStructure <- server_root_RO_testing_pages
  
}






#start loop for each RO------------------


for (i in 1:nrow(RO_List)) {
  
  ## i = 1
  RO_Name   <- RO_List$ShortName[i]
  # Title_var <- paste0("RO Ops ", RO_Name)
  Title_var <- RO_List$FileName[i]
  
  #get RO specific Lookup
  
  Single_RO_Lookup <- RO_Council_Lookup %>%
    filter(ReturningOffice == RO_List$ReturningOffice[i])
  
  #create individual pages.--------------
  #goal location for all ROs
  RO_File_Location <- paste0(ROFileStructure, Title_var)
  
  #if the correct file name doesn't already exist, skip to end
  #not able to create files here.
  FileList <- list.files(ROFileStructure)
  
  if (!(Title_var %in% FileList)){
    
    message(paste0("Could not find ", Title_var))
  } else {
    
    #place all training dashboards into a dashboard folder.
    RO_File_Dashboard <- paste0(RO_File_Location,"/17 - Training/Dashboard")
    
    #if it doesn't exist, create it.
    FileList <- list.files(RO_File_Location, full.names = TRUE)
    
    if(!(RO_File_Dashboard %in% FileList)){
      
      dir.create(RO_File_Dashboard)
    }
    
    
    #goal location for html sub files.
    RO_File_Subs <- paste0(RO_File_Dashboard,"/Sub files")
    
    #if it doesn't exist, create it.
    FileList <- list.files(RO_File_Dashboard, full.names = TRUE)
    
    if(!(RO_File_Subs %in% FileList)){
      
      dir.create(RO_File_Subs)
    }
    
    #generate all neccesary data required for each tab-----------
    #staffing-------------
    #place the corresponding file for staffing from the server team into the folder.
    
    #get all staffing pages.
    Staffing_Pages <- list.files(path    = server_root_securedpages, 
                                 pattern = 'HTML',full.names = TRUE)
    
    #find the one which matches Sydney
    Position <- which(str_detect(Staffing_Pages, paste0("/", "Sydney")))
    
    #if it can find it, copy into new folder location.
    if (length(Position)==1) {
      
      File_to_Copy <- Staffing_Pages[Position]
      file.copy(File_to_Copy, RO_File_Subs, overwrite = TRUE)
      
    }
    
    #placing it into the dashboard is done in the final rmd.
    
    #pre-poll-------------
    
    RO_Markoffs <- Pre_Poll_Data
    
    #create a separate html file to be inputted.
    
    
    setwd(paste0(rmd_files,"RO Pages/Sub pages"))
    rmarkdown::render('RO-Pre-Poll-sub.Rmd',
                      output_dir  = RO_File_Subs,
                      output_file = paste0(RO_Name," Pre-Poll.html")) 
    
    
    
    
    #dec vote---------------------
    
    #mostly based off the dec vote scrutiny page.
    

      RO_DecVotes <- Dec_Vote_Data 
      

    
    setwd(paste0(rmd_files, "RO Pages/Sub pages"))
    rmarkdown::render('RO-DecVote-Sub.Rmd',
                      output_dir  = RO_File_Subs,
                      output_file = paste0(RO_Name," DecVote.html")) 
    
    
    
    
    
    #ballot track--------------------
    


    RO_Ballot_Track <- Ballot_Track_Data
    
    setwd(paste0(rmd_files, "RO Pages/Sub pages"))
    
    rmarkdown::render('RO-BallotTrack-Sub.Rmd',
                      output_dir  = RO_File_Subs,
                      output_file = paste0(RO_Name, " BallotTrack.html")) 
    
    
    
    
    
    
    #run final rmd file binding -------------
    
    setwd(paste0(rmd_files, "RO Pages"))
    rmarkdown::render('09 RO Ops.Rmd',
                      output_dir  = RO_File_Dashboard,
                      output_file = paste0("Training Dashboard.html"))
    
    ## Also, create an index page for each RO for display purposes
    # rmarkdown::render(paste0(RO_File_Location, paste0(RO_Name, ' Index.Rmd')), 
    #                   output_dir = RO_File_Location)
    
  }
}
