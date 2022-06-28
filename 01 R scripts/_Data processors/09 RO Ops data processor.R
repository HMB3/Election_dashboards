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
         FileName = str_remove_all(FileName," ")) %>%
  #get rid of the apostrophe in hunter's hill
  mutate(FileName = ifelse(FileName ==	"Hunter'sHillRegion",
                            "HuntersHillRegion",
                           FileName))


#also create a look up list for councils and ROs.

RO_Council_Lookup <- base_data_councils%>%
  select(EventID,ElectoralAreaName,LGAreaCode,ReturningOffice)%>%
  distinct()


#set up the status stage names for Ballot Track.-----------

#note that status 5 can happen after status 5.1 etc
if (exists("Ballot_Track_Data")){
  
  
  Ballot_Track_Labels<-Ballot_Track_Data%>%
    select(`Status Stage`,`Status Notes`)%>%
    mutate(`Status Stage`=ifelse(is.na(`Status Stage`),0,`Status Stage`))%>%
    distinct()%>%
    
    #remove the first part of the string from the Status Notes
    
    mutate(NewStatusName=str_sub(`Status Notes`, start=8))%>%
    
    
    #add a list that will match with the column labels
    
    mutate(OldStatusName=str_c("StatusStage_",`Status Stage`))%>%
    
    mutate(NewStatusName=ifelse(is.na(NewStatusName),"Not Actioned Yet",NewStatusName))%>%
    select(OldStatusName,NewStatusName) %>%
    
    #finally, clean up all doubled up names.
    #first, there should be nothing after a - 
    
    mutate(NewStatusName=str_remove(NewStatusName, " - .*"),
           
           #nothing in brackets
           NewStatusName=str_remove(NewStatusName, " \\(.*"),
           
           #nothing after ;
           NewStatusName=str_remove(NewStatusName, ";.*")
    )  %>%
    
    #get rid of STH from all statuses to tidy them up - legacy name.
    
    mutate(NewStatusName=str_remove(NewStatusName,"STH/"))%>%
    distinct()
}


#set up the various tables for RMT. Copy and pasted from RO dashboard
#initial count tables ----------------------

Councillor_initial[,c('INITIALCOUNT','EXPECTED_BALLOTS','ALLOCATED','ACCOUNTED_BALLOTS_EN')] <- sapply(Councillor_initial[,c('INITIALCOUNT','EXPECTED_BALLOTS','ALLOCATED','ACCOUNTED_BALLOTS_EN')], as.numeric)

Councillor_interim_overall <- Councillor_initial %>% 
  mutate(Diff = INITIALCOUNT - ifelse(is.na(EXPECTED_BALLOTS),0,EXPECTED_BALLOTS)
         ,Diff2 = ACCOUNTED_BALLOTS_EN - ALLOCATED) %>%
  mutate(RequireChecking = 'No') %>%
  mutate(RequireChecking = case_when(abs(Diff/INITIALCOUNT) > 0.01 ~ 'Yes'
                                     ,abs(Diff2/ACCOUNTED_BALLOTS_EN) >0.01 ~ 'Yes'
                                     ,T ~ RequireChecking)) %>% 
  mutate(RequireChecking = case_when(!VOTINGCENTRETYPECODE %in% c('PP','Pre-Poll') & abs(Diff/INITIALCOUNT) > 0.01 ~'Yes'
                                     ,!VOTINGCENTRETYPECODE %in% c('PP','Pre-Poll') & Diff == 0 ~'No'
                                     ,T ~ RequireChecking)) %>%
  mutate(RequireChecking = ifelse(!is.na(forceApproval) & forceApproval == 'Y','No',RequireChecking)) %>%
  mutate(RequireChecking = as.factor(RequireChecking)
         ,VOTINGCENTRETYPECODE = as.factor(VOTINGCENTRETYPECODE)
         ,CONTESTAREACODE = as.factor(CONTESTAREACODE)
         ,AREACODE = as.factor(AREACODE)
  ) %>%
  arrange(CONTESTID) 
#       mutate(dontshow = ifelse(VOTINGCENTRETYPECODE == 'Postal' & is.na(EXPECTED_BALLOTS),T,F)) %>%
#       filter(dontshow == F)

Councillor_show_overall  <- Councillor_interim_overall %>% select(
  `Requires Checking` = RequireChecking
  ,LGArea = AREACODE
  ,`Area/Ward` = CONTESTAREACODE
  ,`Vote Type` = VOTINGCENTRETYPECODE
  ,Venue = GAZETTEDNAME
  ,`Expected (A)` = EXPECTED_BALLOTS
  ,`Initial Count (B)` = INITIALCOUNT
  ,`Diff1 (B-A)` = Diff
  ,`Allocated (C)` = ALLOCATED
  ,`Accounted (D)` = ACCOUNTED_BALLOTS_EN
  ,`Diff2 (D-C)` = Diff2
  ,Comments = COMMENTS
) %>%
  arrange(LGArea
          ,`Area/Ward`
          ,`Vote Type`
          ,Venue)

Mayor_initial[,c('INITIALCOUNT','EXPECTED_BALLOTS','ALLOCATED','ACCOUNTED_BALLOTS_EN')] <- sapply(Mayor_initial[,c('INITIALCOUNT','EXPECTED_BALLOTS','ALLOCATED','ACCOUNTED_BALLOTS_EN')], as.numeric)


Mayor_interim_overall <- Mayor_initial %>% 
  mutate(Diff = INITIALCOUNT - ifelse(is.na(EXPECTED_BALLOTS) & !is.na(INITIALCOUNT),0,EXPECTED_BALLOTS)
         ,Diff2 = ACCOUNTED_BALLOTS_EN - ALLOCATED
         ,CouncillorDiff = CouncillorBPs - INITIALCOUNT
  ) %>%
  mutate(RequireChecking = 'No') %>%
  mutate(RequireChecking = case_when(abs(Diff/INITIALCOUNT) > 0.01 ~ 'Yes' 
                                     ,abs(Diff2/ACCOUNTED_BALLOTS_EN) > 0.01 ~ 'Yes' 
                                     ,abs(CouncillorDiff/INITIALCOUNT) > 0.01 ~ 'Yes'
                                     ,is.na(CouncillorDiff) & abs(Diff/INITIALCOUNT) < 0.01 & abs(Diff2/ACCOUNTED_BALLOTS_EN) <0.01~ 'No'
                                     ,T ~ RequireChecking)) %>%
  mutate(RequireChecking = case_when(!VOTINGCENTRETYPECODE %in% c('PP','Pre-Poll') & abs(Diff/INITIALCOUNT) > 0.01 ~'Yes'
                                     ,!VOTINGCENTRETYPECODE %in% c('PP','Pre-Poll') & Diff == 0 ~'No'
                                     ,T ~ RequireChecking)) %>%
  mutate(RequireChecking = ifelse(!is.na(forceApproval) & forceApproval == 'Y','No',RequireChecking)) %>%   
  mutate(RequireChecking = as.factor(RequireChecking)
         ,VOTINGCENTRETYPECODE = as.factor(VOTINGCENTRETYPECODE)
         ,AREACODE = as.factor(AREACODE)) %>%
  arrange(CONTESTID) #%>% 
# mutate(dontshow = ifelse(VOTINGCENTRETYPECODE == 'Postal' & is.na(EXPECTED_BALLOTS),T,F)) %>%
# filter(dontshow == F) 

Mayor_show_overall <- Mayor_interim_overall %>% select(
  `Requires Checking` = RequireChecking
  ,`Area/Ward` = AREACODE
  ,`Vote Type` = VOTINGCENTRETYPECODE
  ,Venue = GAZETTEDNAME
  ,`Expected (A)` = EXPECTED_BALLOTS
  ,`Initial Count (B)` = INITIALCOUNT
  ,`Diff1 (B-A)` = Diff
  ,`Allocated (C)` = ALLOCATED
  ,`Accounted (D)` = ACCOUNTED_BALLOTS_EN
  ,`Diff2 (D-C)` = Diff2
  ,`Councillor (E)` = CouncillorBPs
  ,`Diff3 (E-B)`= CouncillorDiff
  ,Comments = COMMENTS
)  %>%
  arrange( `Area/Ward`
           ,`Vote Type`
           ,Venue)


Ref_Poll_initial[,c('INITIALCOUNT','EXPECTED_BALLOTS','ALLOCATED','ACCOUNTED_BALLOTS_EN')] <- sapply(Ref_Poll_initial[,c('INITIALCOUNT','EXPECTED_BALLOTS','ALLOCATED','ACCOUNTED_BALLOTS_EN')], as.numeric)

Ref_poll_interim_overall <- Ref_Poll_initial %>% 
  mutate(Diff = INITIALCOUNT - ifelse(is.na(EXPECTED_BALLOTS),0,EXPECTED_BALLOTS)
         ,Diff2 = ACCOUNTED_BALLOTS_EN - ALLOCATED
         ,CouncillorDiff = CouncillorBPs - INITIALCOUNT) %>%
  mutate(RequireChecking = 'No') %>%
  mutate(RequireChecking = case_when(abs(Diff/INITIALCOUNT) > 0.01 ~ 'Yes' 
                                     ,abs(Diff2/ACCOUNTED_BALLOTS_EN) > 0.01 ~ 'Yes' 
                                     ,abs(CouncillorDiff/INITIALCOUNT) > 0.01 ~ 'Yes'
                                     ,is.na(CouncillorDiff) & abs(Diff/INITIALCOUNT) < 0.01 & abs(Diff2/ACCOUNTED_BALLOTS_EN) <0.01~ 'No'
                                     ,T ~ RequireChecking)) %>%
  mutate(RequireChecking = case_when(!VOTINGCENTRETYPECODE %in% c('PP','Pre-Poll') & abs(Diff/INITIALCOUNT) > 0.01 ~'Yes'
                                     ,!VOTINGCENTRETYPECODE %in% c('PP','Pre-Poll') & Diff == 0 ~'No'
                                     ,T ~ RequireChecking)) %>%
  mutate(RequireChecking = ifelse(!is.na(forceApproval) & forceApproval == 'Y','No',RequireChecking)) %>%   
  mutate(RequireChecking = as.factor(RequireChecking)
         ,VOTINGCENTRETYPECODE = as.factor(VOTINGCENTRETYPECODE)
         ,CONTESTAREACODE = as.factor(CONTESTAREACODE)) %>%
  arrange(CONTESTID) #%>% 
#  mutate(dontshow = ifelse(VOTINGCENTRETYPECODE == 'Postal' & is.na(EXPECTED_BALLOTS),T,F)) %>%
#  filter(dontshow == F)

Ref_poll_show_overall <- Ref_poll_interim_overall %>% select(
  `Requires Checking` = RequireChecking
  ,`Area/Ward` = CONTESTAREACODE
  ,`Vote Type` = VOTINGCENTRETYPECODE
  ,Venue = GAZETTEDNAME
  ,`Expected (A)` = EXPECTED_BALLOTS
  ,`Initial Count (B)` = INITIALCOUNT
  ,`Diff1 (B-A)` = Diff
  ,`Allocated (C)` = ALLOCATED
  ,`Accounted (D)` = ACCOUNTED_BALLOTS_EN
  ,`Diff2 (D-C)` = Diff2
  ,`Councillor (E)` = CouncillorBPs
  ,`Diff3 (E-B)`= CouncillorDiff
  ,Comments = COMMENTS
  ,`Contest Type` = CONTESTTYPECODE
  ,Comments = COMMENTS
  ,Question = QUESTIONLABEL
) %>%
  arrange( `Area/Ward`
           ,`Vote Type`
           ,Venue)





#Set RO File locations------------
#temp file to save in - eventually needs to be on F drive somewhere
# ROFileStructure <- paste0(Sys.getenv('source_control_local_path'),
#                           'Election_Events/LG2101/02 Dashboard/03 Working pages/RO Pages/')

ROFileStructure <- "Z:/"

#if testing on server, need to create a different location on server

if (Test_on_server) {
  ROFileStructure <- server_root_RO_testing_pages
  
}

if (Test_on_transfer) {
  
  
  ROFileStructure <- "G:/Transfer/Data Analytics/RO Dashboard Examples/"
  
  RO_List <- RO_List %>%
    filter(str_detect(ShortName, "Hornsby|Broken Hill"))
}




#remove unwanted ROs from RO List

RO_List <- RO_List %>%
  filter(!(ReturningOffice %in% folder_exclusions))
  

#create a emptry string cell for unfound folder names

Unfound <- c()
 
#start loop for each RO------------------


for (i in 1:nrow(RO_List)) {
  
  #i <- 5
  #i = 63
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
    
    Unfound <- c(Unfound, paste0("Could not find ", Title_var))
    

  } else {
    
    #place all dashboards into a dashboard folder.
    RO_File_Dashboard <- paste0(RO_File_Location,"/14 - RO Dashboard")
    
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
    
    #find the one which matches it.
    Position <- which(str_detect(Staffing_Pages, paste0("/",RO_Name)))
    
    #if it can find it, copy into new folder location.
    if (length(Position)==1) {
      
      File_to_Copy <- Staffing_Pages[Position]
      file.copy(File_to_Copy, RO_File_Subs, overwrite = TRUE)
      
    }
    
    #placing it into the dashboard is done in the final rmd.
    
    #pre-poll-------------
    
    #need a table which:
    #is filtered to just relevant councils
    #provides venue and contest level vote counts
    #only if venue is already open.
    #filter for all contest info (ie councillor, mayor, poll)
    #including: AreaCode, ContestAreaCode, Contest Type, Venue Name
    #Also Status column showing AoBP complete/incomplete
    #Current total votes
    #current total votes EMA markoff (aggregatedn for mayor
    #percentage of ballot paper allocation remaining.)
    
    #get total mark-offs for mark-offs first.
    
    if(exists("Pre_Poll_Data")) {
      
      
      
      RO_Markoffs <- Pre_Poll_Data %>% 
        
        #get short ward names, replace Contest area with them
        left_join(Ward_Names, by = c("CONTESTAREA" = "WardAreaCode")) %>%
        mutate(CONTESTAREA = case_when(
          is.na(ShortWardName) ~ CONTESTAREA,
          TRUE ~ ShortWardName
        )) %>%
        select(-c("ShortWardName","EventID")) %>%
        
        
        filter(ISSUINGAREA %in% Single_RO_Lookup$LGAreaCode)
      
    }
    
    #create a separate html file to be inputted.
    
    
    setwd(paste0(rmd_files,"RO Pages/Sub pages"))
    rmarkdown::render('RO-Pre-Poll-sub.Rmd',
                      output_dir  = RO_File_Subs,
                      output_file = paste0(RO_Name," Pre-Poll.html")) 
    
    
    
    
    #dec vote---------------------
    
    #mostly based off the dec vote scrutiny page.
    
    if (exists("Dec_Vote_Data")) {
      
      RO_DecVotes <- Dec_Vote_Data %>%
        
        #get short ward names, replace Ward with them
        left_join(Ward_Names, by = c("Ward" = "WardAreaCode")) %>%
        mutate(Ward = case_when(
          is.na(ShortWardName) ~ Ward,
          TRUE ~ ShortWardName)
          ,Rejection_rate = as.numeric(Rejection_rate)) %>%
        select(-c("ShortWardName","EventID")) %>%
        
        filter(Council %in% Single_RO_Lookup$LGAreaCode)
      
    }
    
    setwd(paste0(rmd_files, "RO Pages/Sub pages"))
    rmarkdown::render('RO-DecVote-Sub.Rmd',
                      output_dir  = RO_File_Subs,
                      output_file = paste0(RO_Name," DecVote.html")) 
    
    
    
    
    
    #ballot track--------------------
    
    if (exists("Ballot_Track_Data")){
      
      ## Filter the status tables to each RO
      RO_2.0_scan_in_from_printer <- RO_2.0_scan_in_from_printer %>% 
        filter(`RO Name`== RO_Name)
      
      RO_3.0_scan_out_to_venue  <- RO_3.0_scan_out_to_venue  %>% 
        filter(`RO Name`== RO_Name)
      
      RO_4.0_scan_in_from_venue <- RO_4.0_scan_in_from_venue %>% 
        filter(`RO Name`== RO_Name)
      
      RO_5.10_scan_out_to_CC <- RO_5.10_scan_out_to_CC %>% 
        filter(`RO Name`== RO_Name)
      
      RO_5.11_scan_from_RO_to_CC <- RO_5.11_scan_from_RO_to_CC %>% 
        filter(`RO Name`== RO_Name)
      
      RO_5.0_scan_out_to_Ware <- RO_5.0_scan_out_to_Ware %>% 
        filter(`RO Name`== RO_Name)
      
      RO_5.14_CC_scan_in_to_ULD <- RO_5.14_CC_scan_in_to_ULD %>% 
        filter(`RO Name`== RO_Name)
      
      
      ## The full data - we might not need this
      RO_Ballot_Track <- Ballot_Track_Data %>%
        
        #get short ward names, replace Ward with them
        left_join(Ward_Names, by = c("Ward Name" = "WardAreaCode")) %>%
        mutate(`Ward Name` = case_when(
          is.na(ShortWardName) ~ `Ward Name` ,
          TRUE ~ ShortWardName
        )) %>%
        select(-c("ShortWardName","EventID")) %>%
        
        filter(`RO Name`== RO_Name)        %>%
        filter(!is.na(`RO Name`)) %>%
        select(`LGA Name`,
               Barcode,
               `Ward Name`,
               `Venue Name`,
               `Contest Name`,
               `Container Name`,
               # ,`Status Notes`
               `Status Stage`,
               `Status Date`) %>%
        mutate(Count = 1)      %>%
        
        #order by stage so the pivot labels are in the right order
        #first set NAs to zero for sorting purposes.
        
        mutate(`Status Stage`=ifelse(is.na(`Status Stage`),0,`Status Stage`)) %>%
        
        #for each barcode, only want the latest stage venue.
        
        group_by(Barcode) %>%
        mutate(Max_Date=max(`Status Date`)) %>%
        
        #some barcodes are being scanned twice at the same time, distinct will remove
        distinct() %>%
        filter(`Status Date`==Max_Date | is.na(`Status Date`)) %>%
        
        arrange(`Status Stage`) %>%
        
        pivot_wider(names_from=`Status Stage`,
                    names_prefix="StatusStage_",
                    values_from=Count) %>%
        group_by(`LGA Name`,
                 `Ward Name`,
                 `Venue Name`,
                 `Contest Name`,
                 `Container Name`) %>%
        #summarise(StatusNA=sum(StatusStage_NA,na.rm=TRUE))
        summarise(across(starts_with("StatusStage_"), ~sum(., na.rm = TRUE)))
      
      
      
      
      #finally need to update the names of the columns to something more human readable.
      #first, get a set of new names.
      Matching_Names <- names(RO_Ballot_Track) %>%
        as_tibble()%>%
        rename(OldNames = value)%>%
        left_join(Ballot_Track_Labels, by = c("OldNames" = "OldStatusName")) %>%
        mutate(NewStatusName = ifelse(is.na(NewStatusName),OldNames,NewStatusName))
      
      
      names(RO_Ballot_Track) <- Matching_Names$NewStatusName
      
      #if 'Not Actioned Yet' not in RO_Ballot_track, add it in, but set to zero.
      
      if (!("Not Actioned Yet" %in% names(RO_Ballot_Track))){
        
        RO_Ballot_Track <- RO_Ballot_Track%>%
          mutate(`Not Actioned Yet` = 0) %>%
          relocate(`Not Actioned Yet`, .after=`Container Name`)
        
      }
      
      
      #need to add in 'expected to be received' column and 'Received from Printer- actioned.
      
      RO_Ballot_Track <- RO_Ballot_Track %>%
        mutate(`Expected from Printer` = rowSums(across(where(is.numeric))),
               `Received from Printer` = `Expected from Printer` - 
                 `Not Actioned Yet`) %>%
        relocate(`Expected from Printer`,.after=`Container Name`)%>%
        relocate(`Received from Printer`,.after=`Expected from Printer`) %>%
        rename(Council=`LGA Name`,
               Ward = `Ward Name`)
      
    }
    
    setwd(paste0(rmd_files, "RO Pages/Sub pages"))
    
    rmarkdown::render('RO-BallotTrack-Sub.Rmd',
                      output_dir  = RO_File_Subs,
                      output_file = paste0(RO_Name, " BallotTrack.html")) 
    
    
    
    #rmt initial count data -----------------------------------
    
    
    if(exists("Councillor_initial")) {
      
      
      
      Councillor <- Councillor_initial %>%
        filter(AREACODE %in% Single_RO_Lookup$LGAreaCode)
      
      Councillor_show <- Councillor_show_overall %>%
        filter(LGArea %in% Single_RO_Lookup$LGAreaCode)
      
      Councillor_interim <- Councillor_interim_overall %>%
        filter(AREACODE %in% Single_RO_Lookup$LGAreaCode)
      
      Mayor <- Mayor_initial %>%
        filter(AREACODE %in% Single_RO_Lookup$LGAreaCode)
      
      Mayor_interim <- Mayor_interim_overall %>%
        filter(AREACODE %in% Single_RO_Lookup$LGAreaCode)
      
      Mayor_show <- Mayor_show_overall %>%
        filter(`Area/Ward` %in% Single_RO_Lookup$LGAreaCode)
      
      
      Ref_poll <- Ref_Poll_initial %>%
        filter(AREACODE %in% Single_RO_Lookup$LGAreaCode)
      
      Ref_poll_interim <- Ref_poll_interim_overall %>%
        filter(AREACODE %in% Single_RO_Lookup$LGAreaCode)
      
      Ref_poll_show <- Ref_poll_show_overall %>%
        filter(`Area/Ward` %in% Single_RO_Lookup$LGAreaCode)
      

      #copy and pasted from RMT dashboard. but only need to do for mayor and ref/poll if RO has it.
      
      
    

      
      # Knit related tables --------------------------------------------------------------------
      
      
      #first get councillor summary
      
      summary_1 <- bind_rows(Councillor %>% filter(VOTINGCENTRETYPECODE == 'Postal' & EXPECTED_BALLOTS > 0)
                             ,Councillor %>% filter(VOTINGCENTRETYPECODE == 'Postal' & is.na(EXPECTED_BALLOTS))
                             
                             ,Councillor %>% filter(VOTINGCENTRETYPECODE != 'Postal'))
      summary_2 <- Councillor %>% filter(INITIALCOUNT>0)
      summary_3 <- bind_rows(Councillor %>% filter(VOTINGCENTRETYPECODE != 'Postal', ACCOUNTED_BALLOTS_EN > 0),
                             Councillor %>% filter(VOTINGCENTRETYPECODE == 'Postal', EXPECTED_BALLOTS > 0)
      )
      summary_5 <- Councillor_interim %>% filter(!is.na(INITIALCOUNT)) %>% filter(RequireChecking == 'Yes')
      summary_4 <- Councillor_interim %>% filter(!is.na(INITIALCOUNT)) %>% filter(forceApproval == 'Y')
      
      if(nrow(summary_1) >0) {
        
        col1 <- SingleChoiceTable(summary_1, 'VOTINGCENTRETYPECODE') %>% rename(Total = Count)
      } else {
        
        col1 <- tibble(VOTINGCENTRETYPECODE = NA
                       ,Total = NA)
        
        
      }
      if(nrow(summary_2) >0) {
        
        col2 <- SingleChoiceTable(summary_2, 'VOTINGCENTRETYPECODE') %>% rename(`Initial Count Entered` = Count)
        
      } else {
        
        col2 <- tibble(VOTINGCENTRETYPECODE = NA
                       ,`Initial Count Entered` = NA)
        
      }
      if(nrow(summary_3) >0) {
        
        col3 <- SingleChoiceTable(summary_3, 'VOTINGCENTRETYPECODE') %>% rename(`Allocated/Expected` = Count)
      } else {
        
        col3 <- tibble(VOTINGCENTRETYPECODE = NA
                       ,`Allocated/Expected` = NA)
        
        
      }
      if(nrow(summary_4) >0) {
        
        col4 <- SingleChoiceTable(summary_4, 'VOTINGCENTRETYPECODE') %>% rename(`RMT Approved` = Count)
      } else {
        
        col4 <- tibble(VOTINGCENTRETYPECODE = NA
                       ,`RMT Approved` = NA)
        
      }
      if(nrow(summary_5) >0) {
        
        col5 <- SingleChoiceTable(summary_5, 'VOTINGCENTRETYPECODE') %>% rename(`Requires RMT Checking` = Count)
      } else{
        col5 <- tibble(VOTINGCENTRETYPECODE = NA
                       ,`Requires RMT Checking` = NA)
        
      }
      Councillor_summary <- col1 %>% left_join(col2) %>% 
        left_join(col3) %>%
        left_join(col5) %>%
        left_join(col4) %>%
        rename(`Vote Type` = VOTINGCENTRETYPECODE)
      
      # Councillor_summary[is.na(Councillor_summary)] <- 0
      
      #do mayor, assuming there is data for a mayor summary
      
      if (nrow(Mayor) >0) {
        
        summary_1 <- bind_rows(Mayor %>% filter(VOTINGCENTRETYPECODE == 'Postal' & EXPECTED_BALLOTS > 0)
                               ,Mayor %>% filter(VOTINGCENTRETYPECODE == 'Postal' & is.na(EXPECTED_BALLOTS))
                               ,Mayor %>% filter(VOTINGCENTRETYPECODE != 'Postal'))
        
        summary_2 <- Mayor %>% filter(INITIALCOUNT>0)
        
        summary_3 <- bind_rows(Mayor %>% filter(VOTINGCENTRETYPECODE != 'Postal', ACCOUNTED_BALLOTS_EN > 0),
                               Mayor %>% filter(VOTINGCENTRETYPECODE == 'Postal', EXPECTED_BALLOTS > 0)
        )
        summary_5 <- Mayor_interim %>% filter(!is.na(INITIALCOUNT)) %>% filter(RequireChecking == 'Yes')
        summary_4 <- Mayor_interim %>% filter(!is.na(INITIALCOUNT)) %>% filter(forceApproval == 'Y')
        
        
        if(nrow(summary_1) >0) {
          
          col1 <- SingleChoiceTable(summary_1, 'VOTINGCENTRETYPECODE') %>% rename(Total = Count)
        } else {
          
          col1 <- tibble(VOTINGCENTRETYPECODE = NA
                         ,Total = NA)
          
        }
        
        if(nrow(summary_2) >0) {
          
          col2 <- SingleChoiceTable(summary_2, 'VOTINGCENTRETYPECODE') %>% rename(`Initial Count Entered` = Count)
        } else {
          
          col2 <- tibble(VOTINGCENTRETYPECODE = NA
                         ,`Initial Count Entered` = NA)
          
        }
        
        if(nrow(summary_3) >0) {
          
          col3 <- SingleChoiceTable(summary_3, 'VOTINGCENTRETYPECODE') %>% rename(`Allocated/Expected` = Count)
        } else {
          
          col3 <- tibble(VOTINGCENTRETYPECODE = NA
                         ,`Allocated/Expected` = NA)
        }
        
        if(nrow(summary_4) >0) {
          
          col4 <- SingleChoiceTable(summary_4, 'VOTINGCENTRETYPECODE') %>% rename(`RMT Approved` = Count)
        } else {
          col4 <- tibble(VOTINGCENTRETYPECODE = NA
                         ,`RMT Approved` = NA)
          
        }
        if(nrow(summary_5) >0) {
          
          col5 <- SingleChoiceTable(summary_5, 'VOTINGCENTRETYPECODE') %>% rename(`Requires RMT Checking` = Count)
          
        } else {
          
          col5 <- tibble(VOTINGCENTRETYPECODE = NA
                         ,`Requires RMT Checking` = NA)
          
        }
        
        Mayor_summary <- col1 %>% left_join(col2,by = "VOTINGCENTRETYPECODE") %>% 
          left_join(col3) %>%
          left_join(col5) %>%
          left_join(col4) %>%
          rename(`Vote Type` = VOTINGCENTRETYPECODE)
        
      }
      
      #then do ref and poll, assuming there is data
      
      if (nrow(Ref_poll)>0) {
        
        summary_1 <- bind_rows(Ref_poll %>% filter(VOTINGCENTRETYPECODE == 'Postal' & EXPECTED_BALLOTS > 0)
                               ,Ref_poll %>% filter(VOTINGCENTRETYPECODE == 'Postal' & is.na(EXPECTED_BALLOTS))
                               
                               ,Ref_poll %>% filter(VOTINGCENTRETYPECODE != 'Postal'))
        summary_2 <- Ref_poll %>% filter(INITIALCOUNT>0)
        summary_3 <- bind_rows(Ref_poll %>% filter(VOTINGCENTRETYPECODE != 'Postal', ACCOUNTED_BALLOTS_EN > 0),
                               Ref_poll %>% filter(VOTINGCENTRETYPECODE == 'Postal', EXPECTED_BALLOTS > 0)
        )
        summary_4 <- Ref_poll_interim %>% filter(!is.na(INITIALCOUNT)) %>% filter(forceApproval == 'Y')
        summary_5 <- Ref_poll_interim %>% filter(!is.na(INITIALCOUNT)) %>% filter(RequireChecking == 'Yes')
        
        if(nrow(summary_1) >0) {
          
          col1 <- SingleChoiceTable(summary_1, 'VOTINGCENTRETYPECODE') %>% rename(Total = Count)
        } else {
          col1 <- tibble(VOTINGCENTRETYPECODE = NA
                         ,Total = NA)
          
        }
        if(nrow(summary_2) >0) {
          
          col2 <- SingleChoiceTable(summary_2, 'VOTINGCENTRETYPECODE') %>% rename(`Initial Count Entered` = Count)
        } else {
          col2 <- tibble(VOTINGCENTRETYPECODE = NA
                         ,`Initial Count Entered` = NA)
        }
        
        if(nrow(summary_3) >0) {
          
          col3 <- SingleChoiceTable(summary_3, 'VOTINGCENTRETYPECODE') %>% rename(`Allocated/Expected` = Count)
        } else{
          col3 <- tibble(VOTINGCENTRETYPECODE = NA
                         ,`Allocated/Expected` = NA)
        }
        
        if(nrow(summary_4) >0) {
          
          col4 <- SingleChoiceTable(summary_4, 'VOTINGCENTRETYPECODE') %>% rename(`RMT Approved` = Count)
        } else {
          
          col4 <- tibble(VOTINGCENTRETYPECODE = NA
                         ,`RMT Approved` = NA)
          
        }
        
        if(nrow(summary_5) >0) {
          
          col5 <- SingleChoiceTable(summary_5, 'VOTINGCENTRETYPECODE') %>% rename(`Requires RMT Checking` = Count)
        } else {
          
          col5 <- tibble(VOTINGCENTRETYPECODE = NA
                         ,`Requires RMT Checking` = NA)
          
        }
        Ref_poll_summary <- col1 %>% left_join(col2) %>% 
          left_join(col3) %>%
          left_join(col5) %>%
          left_join(col4) %>%
          rename(`Vote Type` = VOTINGCENTRETYPECODE)
        
        
        
      }
      
      
      
    }
    
    setwd(paste0(rmd_files, "RO Pages/Sub pages"))
    
    rmarkdown::render('RO-Initial-Count.Rmd',
                      output_dir  = RO_File_Subs,
                      output_file = paste0(RO_Name, " InitialCount.html")) 
    
    
    
    #RMT check count data------------------
    
    
    if (exists("Councillor_check")) {
      
      Councillor <- Councillor_check %>%
        filter(AREACODE %in% Single_RO_Lookup$LGAreaCode)
      
      Councillor_show <- Councillor_show_overall %>%
        filter(LGArea %in% Single_RO_Lookup$LGAreaCode)
      
      Councillor_interim <- Councillor_interim_overall %>%
        filter(AREACODE %in% Single_RO_Lookup$LGAreaCode)
      
      Mayor <- Mayor_check %>%
        filter(AREACODE %in% Single_RO_Lookup$LGAreaCode)
      
      Mayor_interim <- Mayor_interim_overall %>%
        filter(AREACODE %in% Single_RO_Lookup$LGAreaCode)
      
      Mayor_show <- Mayor_show_overall %>%
        filter(`Area/Ward` %in% Single_RO_Lookup$LGAreaCode)
      
      
      
      # make nums
      i_want_numeric <- c('INITIALCOUNT','OTHER_TOTAL','MAYOR_CHECK_COUNT','COUNCILLOR_EN_COUNT','Registered','Entered')
      
      Councillor[,i_want_numeric] <- sapply(Councillor[,i_want_numeric], as.numeric)
      
      
      Councillor_interim <- Councillor %>% 
        mutate(Diff = Registered - INITIALCOUNT
               ,Diff2 = Entered - Registered) %>%
        mutate(RequireChecking = 'No') %>%
        mutate(RequireChecking = case_when(abs(Diff/INITIALCOUNT) > 0.01 ~ 'Yes'
                                           ,abs(Diff2/Registered) > 0.01 ~ 'Yes'
                                           ,Diff==INITIALCOUNT ~ 'No'
                                           ,T ~ RequireChecking)) %>%
        mutate(RequireChecking = ifelse(!is.na(forceApproval) & forceApproval == 'Y','No',RequireChecking)) %>%
        
        mutate(RequireChecking = as.factor(RequireChecking)
               ,VENUEVOTETYPE = as.factor(VENUEVOTETYPE)
               ,CONTESTAREACODE = as.factor(CONTESTAREACODE)
               ,AREACODE = as.factor(AREACODE)
        ) %>%
        arrange(CONTESTID)
      
      
      Councillor_show <- Councillor_interim %>% select(
        `Requires Checking` = RequireChecking
        ,`LGAreaCode` = AREACODE
        ,`Area/Ward` = CONTESTAREACODE
        ,`Vote Type` = VENUEVOTETYPE
        ,Venue = GAZETTEDNAME
        ,`Initial Count (A)` = INITIALCOUNT
        ,`Registered/Approved Total (B)` = Registered
        ,`Diff1 (B-A)` = Diff
        ,`Data Entered/Reconciled Total (C)` = Entered
        ,`Diff2 (C-B)` = Diff2
        ,Comments = COMMENTS
      )
      
      summary_1 <- Councillor_interim
      
      summary_2 <- Councillor_interim %>% filter(!is.na(INITIALCOUNT))
      summary_3 <- Councillor_interim %>% filter(!is.na(Registered))
      summary_4 <- Councillor_interim %>% filter(!is.na(Entered))
      summary_5 <- Councillor_interim %>% filter(forceApproval == 'Y')
      summary_6 <- Councillor_interim %>% filter(RequireChecking == 'Yes')
      
      
      if(nrow(summary_1) >0) {
        
        col1 <- SingleChoiceTable(summary_1, 'VENUEVOTETYPE') %>% rename(Total = Count)
      } else {
        col1 <- tibble(VENUEVOTETYPE = NA
                       ,Total = NA)
        
      }
      if(nrow(summary_2) >0) {
        
        col2 <- SingleChoiceTable(summary_2, 'VENUEVOTETYPE') %>% rename(`Initial Count Entered` = Count)
      } else {
        col2 <- tibble(VENUEVOTETYPE = NA
                       ,`Initial Count Entered` = NA)
        
      }
      if(nrow(summary_3) >0) {
        
        col3 <- SingleChoiceTable(summary_3, 'VENUEVOTETYPE') %>% rename(`Batch Registered/Approved` = Count)
      } else {
        col3 <- tibble(VENUEVOTETYPE = NA
                       ,`Batch Registered/Approved` = NA)
        
      }
      if(nrow(summary_4) >0) {
        
        col4 <- SingleChoiceTable(summary_4, 'VENUEVOTETYPE') %>% rename(`Data Entered/Reconciled` = Count)
      } else {
        col4 <- tibble(VENUEVOTETYPE = NA
                       ,`Data Entered/Reconciled` = NA)
        
      }
      if(nrow(summary_5) >0) {
        
        col5 <- SingleChoiceTable(summary_5, 'VENUEVOTETYPE') %>% rename(`RMT Approved` = Count)
      } else {
        col5 <- tibble(VENUEVOTETYPE = NA
                       ,`RMT Approved` = NA)
        
      }
      
      if(nrow(summary_6) >0) {
        
        col6 <- SingleChoiceTable(summary_6, 'VENUEVOTETYPE') %>% rename(`Requires RMT Checking` = Count)
      } else {
        col6 <- tibble(VENUEVOTETYPE = NA
                       ,`Requires RMT Checking` = NA)
        
      }
      
      Councillor_summary <- col1 %>% 
        left_join(col2) %>% 
        left_join(col3) %>%
        left_join(col4) %>%
        left_join(col6) %>%
        left_join(col5) %>%
        
        rename(`Vote Type` = VENUEVOTETYPE)
      
      
      if (nrow(Mayor) > 0 ) {
        
        Mayor[,i_want_numeric] <- sapply(Mayor[,i_want_numeric], as.numeric)
        
        
        Mayor_interim <- Mayor %>% 
          mutate(Diff = Registered - INITIALCOUNT
                 ,Diff2 = Entered - Registered
                 ,Diff3 = CouncillorBPs - Registered) %>%
          mutate(RequireChecking = 'No') %>%
          mutate(RequireChecking = case_when(abs(Diff/INITIALCOUNT) > 0.01 ~ 'Yes'
                                             ,abs(Diff2/Registered) > 0.01 ~ 'Yes'
                                             ,abs(Diff3/INITIALCOUNT) > 0.01 ~ 'Yes'
                                             
                                             ,Diff==INITIALCOUNT ~ 'No'
                                             ,T ~ RequireChecking)) %>%
          mutate(RequireChecking = ifelse(!is.na(forceApproval) & forceApproval == 'Y','No',RequireChecking)) %>%
          
          mutate(RequireChecking = as.factor(RequireChecking)
                 ,VENUEVOTETYPE = as.factor(VENUEVOTETYPE)
                 ,AREACODE = as.factor(AREACODE)
          ) %>%
          arrange(CONTESTID) 
        
        Mayor_show <- Mayor_interim %>% select(
          `Requires Checking` = RequireChecking
          ,`Area/Ward` = AREACODE
          ,`Vote Type` = VENUEVOTETYPE
          ,Venue = GAZETTEDNAME
          ,`Initial Count (A)` = INITIALCOUNT
          ,`Registered/Approved Total (B)` = Registered
          ,`Diff1 (B-A)` = Diff
          ,`Data Entered/Reconciled Total (C)` = Entered
          ,`Diff2 (C-B)` = Diff2
          ,`Councillor IC (D)` = CouncillorBPs
          ,`Diff3 (D-B)` = Diff3
          ,Comments = COMMENTS
          
        )
        
        summary_1 <- Mayor_interim
        
        summary_2 <- Mayor_interim %>% filter(!is.na(INITIALCOUNT))
        
        summary_3 <- Mayor_interim %>% filter(!is.na(Registered))
        summary_4 <- Mayor_interim %>% filter(!is.na(Entered))
        summary_5 <- Mayor_interim %>% filter(forceApproval == 'Y')
        summary_6 <- Mayor_interim %>% filter(RequireChecking == 'Yes')
        
        
        if(nrow(summary_1) >0) {
          
          col1 <- SingleChoiceTable(summary_1, 'VENUEVOTETYPE') %>% rename(Total = Count)
        } else {
          col1 <- tibble(VENUEVOTETYPE = NA
                         ,Total = NA)
          
        }
        
        if(nrow(summary_2) >0) {
          
          col2 <- SingleChoiceTable(summary_2, 'VENUEVOTETYPE') %>% rename(`Initial Count Entered` = Count)
        } else {
          col2 <- tibble(VENUEVOTETYPE = NA
                         ,`Initial Count Entered` = NA)
          
        }
        if(nrow(summary_3) >0) {
          
          col3 <- SingleChoiceTable(summary_3, 'VENUEVOTETYPE') %>% rename(`Batch Registered/Approved` = Count)
        } else {
          col3 <- tibble(VENUEVOTETYPE = NA
                         ,`Batch Registered/Approved` = NA)
          
        }
        
        if(nrow(summary_4) >0) {
          
          col4 <- SingleChoiceTable(summary_4, 'VENUEVOTETYPE') %>% rename(`Data Entered/Reconciled` = Count)
        } else {
          col4 <- tibble(VENUEVOTETYPE = NA
                         ,`Data Entered/Reconciled` = NA)
        }
        
        if(nrow(summary_5) >0) {
          
          col5 <- SingleChoiceTable(summary_5, 'VENUEVOTETYPE') %>% rename(`RMT Approved` = Count)
        } else {
          
          col5 <- tibble(VENUEVOTETYPE = NA,
                         `RMT Approved` = NA)
        }
        
        if(nrow(summary_6) >0) {
          
          col6 <- SingleChoiceTable(summary_6, 'VENUEVOTETYPE') %>% rename(`Requires RMT Checking` = Count)
        } else {
          col6 <- tibble(VENUEVOTETYPE = NA
                         ,`Requires RMT Checking` = NA)
          
        }
        
        Mayor_summary <- col1 %>% 
          left_join(col2) %>% 
          left_join(col3) %>%
          left_join(col4) %>%
          left_join(col6) %>%
          left_join(col5) %>%
          rename(`Vote Type` = VENUEVOTETYPE)
        
      }
      
     
      
      
      
    }
    
    setwd(paste0(rmd_files, "RO Pages/Sub pages"))
    
    rmarkdown::render('RO-Check-Count.Rmd',
                      output_dir  = RO_File_Subs,
                      output_file = paste0(RO_Name, " CheckCount.html")) 
    
    

    
    #run final rmd file binding -------------
    
    
    
    
    setwd(paste0(rmd_files, "RO Pages"))
    rmarkdown::render('09 RO Ops.Rmd',
                      output_dir  = RO_File_Dashboard,
                      output_file = paste0(RO_Name, " Dashboard.html"))
    
    ## Also, create an index page for each RO for display purposes
    # rmarkdown::render(paste0(RO_File_Location, paste0(RO_Name, ' Index.Rmd')), 
    #                   output_dir = RO_File_Location)
    
  }
  
  
}

if (length(Unfound) > 0) {
  
  message(paste0("Could not find ", length(Unfound), " folders"))
  message("See Unfound for list")
}
