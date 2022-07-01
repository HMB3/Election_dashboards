######################### ---- DECLATAION VOTING DATA PROCESSOR ---- ###########################


## This code transforms the Declaration Voting data into the correct format for the dashboards
## Data comes from two sources: the EMA markoffs during the pre-poll period, and the EMA 
## Account of Ballot Paper from Election day (AoBP)


## To-do decVotes ----


## Remaining to be fulfilled -3
## 1.	Enrolment and NAMAV provisional mark off figures don't match Dec Vote Report
## 2.	Postal numbers show that 3 more applications were fulfilled than accepted. -3
## 3. The DV RMD will not knit....



## This code processes the data for the early-voting dashboard 
message('Run R code to analyse the Declaration Voting data')
# source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
# setwd(base_data_source_folder)


#check if data is available
#see if Dec Votes has data
if (nrow(EMA_markoffs)>0){
  Dec_Votes_Available <- TRUE
} else {
  Dec_Votes_Available <- FALSE
}




#see if Postal has data

if (nrow(Postal_applications_council)>0){
  Postal_Votes_Available <- TRUE
} else {
  Postal_Votes_Available <- FALSE
}




## 1). EMA MARK-OFF TABLES  ==============================================================

#check if there is data available, otherwise don't run.
#only run sections 1-3 if there are markoffs

if (Dec_Votes_Available){
  
  ## We need to combine markoffs votes during the pre-poll period 
  ## with the Account of Ballot Paper votes from Election day (AoBP).
  EMA_markoffs_rename <- EMA_markoffs %>%
    
    ## Sum of accepted, rejected and provisional markoffs
    ## Add a mutate, create a new column for total markoffs
    dplyr::rename(Ward                 = ISSUINGDISTAREACODE,
                  Venue                = GAZETTEDNAME,
                  Accepted_markoffs    = ACCEPTED_MARKOFFS,
                  Rejected_markoffs    = REJECTED_MARKOFFS,
                  Vote_type            = DEC_VOTE_TYPE,
                  Date                 = VOTEPROCESSDATE,
                  Venue_type           = VOTINGCENTRETYPECODE,
                  Provisional_markoffs = PROVISIONAL_MARKOFFS,
                  Total_markoffs       = TOTAL_MARKOFFS)
  
  ## Why are there are so many NAs for provisional mark-offs?
  ## To make it work, make these 0.
  
  #mutate(Provisional_markoffs=ifelse(is.na(Provisional_markoffs),0,Provisional_markoffs))
  
  
  ## Rename markoff columns ----
  Markoff_Venue <- EMA_markoffs_rename %>%
    
    ## Group by date and venue.
    ## Dont include Vote_type - they are all as one 
    ## This will aggregate enrollment and NAMAV into the accept and reject count
    group_by(Ward, Venue, Venue_type) %>%
    
    #want to keep the NAs where there are NAs.
    summarise(across(where(is.numeric), ~sum(.x)))%>%
    
    ## For either an enrollment or NAMAV vote at pre-poll venues, regardless of status
    mutate(Rejection_rate = round((Rejected_markoffs/
                                     (Accepted_markoffs+Rejected_markoffs)), 
                                  digits = 2), 
           Rejection_rate = ifelse(Rejection_rate == 'Inf', 0, Rejection_rate))
  
  ## Markoff counts grouped by date ----
  ## This is used for Graph 1). below
  
  EMA_markoff_group_date <- EMA_markoffs_rename %>% 
    
    #want only pre-poll votes, excluding NAs.
    filter(Venue_type=='Pre-Poll')%>%
    
    ## Group by date, and vote type
    ## This will create values for accepted and rejected
    group_by(Date,Vote_type) %>%
    summarise(Accepted               = sum(Accepted_markoffs,      na.rm = TRUE),
              Rejected               = sum(Rejected_markoffs,      na.rm = TRUE),
              `Total_markoffs`       = sum(`Total_markoffs`,       na.rm = TRUE),
              `Provisional_markoffs` = sum(`Provisional_markoffs`, na.rm = TRUE)) %>%
    
    
    arrange(Date) %>%
    
    #wantthe cumulaive sum to be by Vote type
    group_by(Vote_type)%>%
    mutate(Total_cum_markoff = cumsum(`Total_markoffs`),
           Total_cum_provis  = cumsum(`Provisional_markoffs`))%>%
    
    #change date to date type for better formatting.
    mutate(Date=as.Date(Date))
  
  ## Markoff counts by type ----
  ## This is used for Graph 2). below
  ## This should be correct
  ## Venue Type name is NULL....
  ## Number of Enrolment and NAMAV that doesn't have a venueType name
  Markoff_sum_type <- EMA_markoffs_rename %>%
    
    ## Create a table for the bar graph
    group_by(Venue_type, Vote_type)  %>%
    summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%
    ungroup() %>%
    
    ## Rename to actual
    mutate(Venue_type = gsub('PP', 'Polling Place', Venue_type),
           Venue_type = gsub('DI', 'Declared IN',   Venue_type),
           Venue_type = ifelse(is.na(Venue_type), 'Venue Not Listed',
                               Venue_type)) %>% 
    
    ## For venues not listed, aggregate them up to "polling place"
    ## As long 
    group_by(Venue_type, Vote_type)  %>%
    summarize_if(., is.numeric, sum)
  
  
  
  
  
  
  
  
  ## 2). EMA AOBP TABLES  =====================================================================
  
  
  ## Change AoBP variable names ----
  EMA_AoBP_summary     <- EMA_AoBP %>%
    
    dplyr::rename(Ward       = CONTESTAREACODE,
                  Council    = AREACODE,
                  Venue      = VENUE,
                  Contest    = CONTESTTYPECODE,
                  Venue_type = VOTINGCENTRETYPECODE,
                  Enrolment_envelopes = ENROL_ENVELOPES,
                  NAMAV_envelopes     = NAMAV_ENVELOPES) %>% 
    
    ## Depending on the date, filter out the Pre-Polls?
    {if (today() < "2021-12-04") {
      (.) %>%
        filter(Venue_type != "PP") }
      else {.}
    } %>%
    
    #currently polls and referendums appear in multiple rows, but only with relevant data in one row.
    #if a contest has multiple wards, appear as multiple contests, but only collected once.
    #to deal with that, will group them all and find the column with values.
    
    group_by(Ward,Council,Venue, Contest, Venue_type) %>%
    mutate(Count = n()) %>%
    
    #add a flag if count is more than 1 AND enrolment AND NAMAV is NA.
    mutate(Remove_Row=ifelse(Count > 1 & is.na(Enrolment_envelopes) & is.na(NAMAV_envelopes),
                             TRUE,FALSE)) %>%
    
    filter(Remove_Row == FALSE) %>%
    
    ## also need to ensure votes are only counted ONCE, not double counted if
    ## venue has multiple contests.
    group_by(Council, Ward, Venue) %>%
    
    ## need to determine which are being over-counted.
    mutate(Include = case_when(
      
      #prioritise Councillor, than Mayor, than referendum, than poll
      "Councillor" %in% Contest & Contest != "Councillor" ~ FALSE,
      "Councillor" %in% Contest & Contest == "Councillor" ~ TRUE,
      "Mayor"      %in% Contest & Contest != "Mayor" ~ FALSE,
      "Mayor"      %in% Contest & Contest == "Mayor" ~ TRUE,
      "Referendum" %in% Contest & Contest != "Referendum" ~ FALSE,
      "Referendum" %in% Contest & Contest == "Referendum" ~ TRUE,
      "Poll"       %in% Contest & Contest != "Poll" ~ FALSE,
      "Poll"       %in% Contest & Contest == "Poll" ~ TRUE,
      TRUE ~ TRUE
      
    )) %>%
    
    ## then double check for venues which are divided, but have both Councillor and whole council venues.
    group_by(Council, Venue) %>%
    
    mutate(Include = ifelse("Councillor" %in% Contest & Contest!= "Councillor", FALSE, Include)) %>%
    filter(Include == TRUE) %>%
    
    
    dplyr::select(-(Count:Include)) %>%
    
    
    ## Make sure the data comes in as numeric, not character
    ## This should work on either NA, or O
    mutate(Enrolment_envelopes = as.numeric(Enrolment_envelopes),
           NAMAV_envelopes     = as.numeric(NAMAV_envelopes),
           Total_envelopes     = as.numeric(Enrolment_envelopes + NAMAV_envelopes))
  
  
  ## Markoff counts grouped by enrolment/NAMAV ----
  AoBP_sum_type <- EMA_AoBP_summary %>% 
    group_by(Venue_type) %>%
    summarise(across(where(is.numeric), ~sum(.x,na.rm = TRUE))) %>%
    dplyr::select(-Total_envelopes) %>%
    
    ## make long
    pivot_longer(-Venue_type, 
                 names_to  = "Vote_type",
                 values_to = "Total_markoffs") %>%
    
    ## redo names.
    mutate(Vote_type  = str_remove(Vote_type, "_envelopes"),
           Venue_type = ifelse(Venue_type == "PP", "Polling Place", Venue_type)) %>%
    mutate(Venue_type = as.character(Venue_type))
  
  
  ## create a new table using Total Mark offs from EMA for pre-poll, and mark-offs from AoBP for polling place.
  Overall_DecVote_Chart_Table <- Markoff_sum_type %>%
    
    dplyr::select(Venue_type:Total_markoffs) %>%
    filter(Venue_type == "Pre-Poll")  %>%
    ungroup() %>%
    add_row(AoBP_sum_type)
  
  
  
  
  
  
  ## 3). MASTER DECVOTE SCRUTINY TABLE  ======================================================
  
  
  ## Now we need to combine markoffs votes during the pre-poll period 
  ## with the Account of Ballot Paper votes from Election day (AoBP).
  
  
  ## Create a list of the final columns we want to keep in 
  ## the decvote scruting dashboard table
  scrutiny_cols <- c('Updated',             'Council', 'Ward', 
                     'Venue',               'Venue_type', 
                     'Enrolment_envelopes', 'NAMAV_envelopes',   
                     'Total_markoffs',      'Provisional_markoffs',
                     'Accepted_markoffs',   'Rejected_markoffs', 
                     'Scrutinised',
                     'Unscrutinised',       'Rejection_rate') 
  
  
  ## AoBP grouped by Venue ----
  ## This will sum NAMAV and Enorlment votes of different types: referrendum, poll, etc.
  ## What if there
  
  
  
  AoBP_Venue <- EMA_AoBP_summary %>% 
    
    #only count Councillor votes to prevent double counting
    #also need to ensure votes are only counted ONCE, not double counted if
    #venue has multiple contests.
    group_by(Council, Ward, Venue) %>%
    #need to determine which are being over-counted.
    mutate(Include = case_when(
      #prioritise Councillor, than Mayor, than referendum, than poll
      "Councillor" %in% Contest & Contest!="Councillor" ~ FALSE,
      "Councillor" %in% Contest & Contest=="Councillor" ~ TRUE,
      "Mayor" %in% Contest & Contest!="Mayor" ~ FALSE,
      "Mayor" %in% Contest & Contest=="Mayor" ~ TRUE,
      "Referendum" %in% Contest & Contest!="Referendum" ~ FALSE,
      "Referendum" %in% Contest & Contest=="Referendum" ~ TRUE,
      "Poll" %in% Contest & Contest!="Poll" ~ FALSE,
      "Poll" %in% Contest & Contest=="Poll" ~ TRUE,
      TRUE ~ TRUE
    )) %>%
    #then double check for venues which are divided, but have both Councillor and whole council venues.
    
    group_by(Council, Venue) %>%
    
    mutate(Include = ifelse("Councillor" %in% Contest & Contest!="Councillor", FALSE, Include)) %>%
    filter(Include==TRUE) %>%
    ## Group by Venue, and count votes
    group_by(Council, Ward, Venue, Venue_type) %>%
    summarise(across(where(is.numeric), ~sum(.x,na.rm=TRUE))) %>% 
    ungroup() %>% 
    dplyr::select(-Total_envelopes, -Council)
  
  
  
  
  ## Master DecVote scrutiny ----
  
  
  ## Join the Markoffs and AoBP 
  ## Venue type 'PP' shouldn't be showing provisional mark off 
  ## Does this mean remove PP type from Markoff table?
  ## If we are binding rows, there shouldn't be the same venues in each table...
  DecVote_master <- Markoff_Venue %>%
    
    ## Join AOBP data with markoffs
    full_join(AoBP_Venue, by = c("Ward", "Venue", "Venue_type")) %>% 
    left_join(Contests_council_including_undivided %>% 
                dplyr::select(Council = AREACODE, Ward = CONTESTAREACODE),
              by = 'Ward') %>%
    
    ## Unscrutinised = Total_provisional_markoff - (Accepted_markoffs + Rejected_markoffs)
    mutate(Updated = now(),
           Scrutinised = Accepted_markoffs + Rejected_markoffs,
           Unscrutinised = abs(Total_markoffs - 
                                 (Accepted_markoffs + Rejected_markoffs))) %>%
    dplyr::select(all_of(scrutiny_cols))
  
  
  ## Check the output - The venues are not unique.....
  length(unique(DecVote_master$Venue));nrow(DecVote_master)
  table(duplicated(DecVote_master$Venue))
}





## 4). POSTAL APPLICATIONS ===========================================================================

#only run postal stuff if there's data.

if (Postal_Votes_Available) {
  
  ## Merge the Postal Tables together ----
  ## View(All_Postal_council[duplicated(All_Postal_council$ELECTORID),])
  
  ## need to remove Reject Reasons not attached to an elector.
  Reject_Reasons_Not_Attached_To_Elector <- c(
    
    "Elector not on roll",
    "Elector is GPV",
    "PDF Scan is unreadable",
    "Elector is already marked as voted",
    "PVA already accepted",
    "Silent elector escalation"
  )
  
  
  All_Postal_council <- Postal_applications_council %>% 
    
    ## join to get english text for Reject Reasons.
    left_join(Reject_Reasons %>%
                dplyr::select(VALUEENTERED, DESCRIPTION),
              by = c("REJECTEDREASON" = "DESCRIPTION")) %>%
    mutate(Reject_reason = VALUEENTERED) %>%
    
    
    ## remove all rows without a connection to an election
    filter(!(Reject_reason %in% Reject_Reasons_Not_Attached_To_Elector)) %>%
    dplyr::select(-c("VALUEENTERED","Reject_reason")) %>%
    
    
    left_join(., Postal_votes_council,
              by = c("ELECTIONEVENTID", "DECLARATIONEXCUSEVOTETYPECODE", 
                     "ELECTORID",       "APPLICATIONNUMBER", "TYPECODE")) %>% 
    
    ## Join on council from the decexcuse vote table
    left_join(., Dec_excuse_postal_council,
              by = c("ELECTIONEVENTID",
                     "APPLICATIONNUMBER",
                     "ELECTORID",
                     "DECLARATIONEXCUSEVOTETYPECODE")) %>%
    
    dplyr::rename(Council = ISSUINGDISTAREACODE)
  
  
  ## There are five categories of VOTESTATUSCODE, most of them are issued,
  ## This might be causing problems? 
  table(Dec_excuse_postal_council$VOTESTATUSCODE)
  
  
  
  
  
  ## Create Master Postal Tables ----
  Postal_Votes_master <- All_Postal_council %>% 
    
    ## Format Date for plotting (so if doesn't include the time) 
    mutate(
      
      ## How will these terms change, based on Reggie/Jason's definitions? 
      Total_applications = 1,
      PVA_Rejected       = ifelse(!is.na(REJECTED), 1, 0),
      
      PVA_Accepted       = ifelse(is.na(REJECTED), 1, 0),
      PVA_Fulfilled      = ifelse(!is.na(SENDFULFILLMENTDATE), 1, 0),
      
      ## There are five categories of VOTESTATUSCODE, most of them are issued,
      ## This might be causing problems? 
      Scrutiny_Accepted = ifelse(VOTESTATUSCODE == 'Accepted', 1, 0),
      Scrutiny_Rejected = ifelse(VOTESTATUSCODE == 'Rejected', 1, 0))
  
  
  
  ## Group by LGA, not Date
  Postal_master_council <- Postal_Votes_master %>% 
    
    dplyr::rename(Ward = Council) %>%
    
    ## get Councils in there
    ## join to get the Council names
    left_join(Contests_council%>%
                dplyr::select(AREACODE,CONTESTAREACODE),
              by = c("Ward" = "CONTESTAREACODE")) %>%
    dplyr::rename(Council = AREACODE) %>%
    
    group_by(Council) %>%
    
    ## Summarise the postal voting variables
    ## It's the lag between accepted and fulfilled
    ## I.e. how many accepted postal votes have not yet been sent (fulfilled) 
    summarise(Total_applications = sum(Total_applications, na.rm = TRUE),
              PVA_Accepted       = sum(PVA_Accepted,       na.rm = TRUE),
              PVA_Rejected       = sum(PVA_Rejected,       na.rm = TRUE),
              PVA_Fulfilled      = sum(PVA_Fulfilled,      na.rm = TRUE),
              Scrutiny_Accepted  = sum(Scrutiny_Accepted,  na.rm = TRUE),
              Scrutiny_Rejected  = sum(Scrutiny_Rejected,  na.rm = TRUE)) 
  
  
  ## Cumulative postal votes by day to plot ----
  
  
  # View(Postal_Votes_master %>% filter(is.na(REJECTED)) %>% dplyr::select(PROCESSEDDATE, ACCEPTED))
  Postal_fulfilment_plot <- Postal_Votes_master %>%
    
    
    ## Use PROCESSEDDATE for the cumulative graph. 
    ## If there is a row in the postal vote application table, and REJECTED is null, 
    ## then the application is accepted.0
    ## REGGIE :: The accepted column is not used. If the rejected column is null, 
    ## then the PVA record is accepted.
    mutate(SENDFULFILLMENTDATE = format(as.POSIXct(SENDFULFILLMENTDATE, 
                                                   format = '%m/%d/%Y %H:%M:%S'), 
                                        format ='%m/%d/%Y'),
           PVA_ACCEPTED = format(as.POSIXct(PROCESSEDDATE, 
                                            format = '%m/%d/%Y %H:%M:%S'), 
                                 format ='%m/%d/%Y')) %>% 
    
    {full_join(filter(., !is.na(SENDFULFILLMENTDATE)) %>%
                 group_by(SENDFULFILLMENTDATE) %>%
                 summarise(SENDFULFILLMENTDATE_count = n()),
               
               ## This definition of accepted is not the correct definition. 
               ## For display purposes only! 
               filter(., !is.na(PROCESSEDDATE)) %>%
                 group_by(PVA_ACCEPTED) %>%
                 summarise(ACCEPTED_count = n()),
               by = c("SENDFULFILLMENTDATE" = "PVA_ACCEPTED"))
    } %>% 
    
    dplyr::rename(Date      = SENDFULFILLMENTDATE,
           Accepted  = ACCEPTED_count,
           Fulfilled = SENDFULFILLMENTDATE_count) %>%
    arrange(Date) %>%
    
    ## Create cumulative early vote count
    mutate(Accepted  = cumsum(Accepted),
           Fulfilled = ifelse(is.na(Fulfilled), 0, Fulfilled),
           Fulfilled = cumsum(Fulfilled)) %>%  
    
    ## Probably don't need all the variables
    gather(key   = Status, 
           value = Count, -Date) %>%
    arrange(Date) %>% mutate(Status = as.factor(Status))
  
  
  
  
  
  ## Postal votes Aggregated count ----
  ## Use this for Graph 3). for postal Voting tab 
  Postal_categories_overall_count <- Postal_master_council %>%
    
    ## Make it wider
    pivot_longer(cols  = c("PVA_Accepted",
                           "PVA_Fulfilled",
                           "Scrutiny_Accepted", 
                           "Scrutiny_Rejected"),
                 names_to = "Status") %>% 
    group_by(Status) %>% 
    summarize_if(., is.numeric, sum) %>% 
    
    ## Re-order factor
    ## LGA - PVAs accepted - PVs Fulfilled(Issued) - Scrutiny Accepted - Scrutiny Rejected
    mutate(Status = factor(Status, levels = c("PVA_Accepted",
                                              "PVA_Fulfilled",
                                              "Scrutiny_Accepted", 
                                              "Scrutiny_Rejected"))) %>% 
    
    ## Add another column for the categories we want to lump together
    mutate(Display = case_when(Status == "PVA_Fulfilled"     ~ "PVA Fulfilled",
                               Status == "PVA_Accepted"      ~ "PVA Accepted",
                               Status == "Scrutiny_Accepted" ~ "Scrutiny",
                               Status == "Scrutiny_Rejected" ~ "Scrutiny", 
                               TRUE ~ "Other")) %>%
    #remove unneeded row
    dplyr::select(-PVA_Rejected)
  
  
  
  ## Table 3). Reggie tells us where the postal reject reasons are in EMA ----
  
  #some reasons shouldn't be included in this list, as they are not attached to a specific elector.
  
  Reject_Reasons_Not_Attached_To_Elector <- c(
    "Elector not on roll",
    "Elector is GPV",
    "PDF Scan is unreadable",
    "Elector is already marked as voted",
    "PVA already accepted",
    "Silent elector escalation"
  )
  
  
  ## Table of postal rejections ----
  ## Needs to have reasons in it 
  
  if (nrow(Postal_applications_council %>% 
           filter(!is.na(REJECTEDREASON))) > 0) {
    
    postal_reject_tab <- Postal_applications_council %>%
      
      #get rid of those with no reject reason
      filter(!is.na(REJECTEDREASON))     %>%
      
      ## join to get english text for Reject Reasons.
      left_join(Reject_Reasons%>%
                  dplyr::select(VALUEENTERED, DESCRIPTION),
                by = c("REJECTEDREASON" = "DESCRIPTION")) %>%
      mutate(Reject_reason = VALUEENTERED)%>%
      dplyr::select(-VALUEENTERED) %>%
      
      ## remove all rows without a connection to an election
      filter(!(Reject_reason %in% Reject_Reasons_Not_Attached_To_Elector)) %>%
      
      ## Join on decexcuse vote table
      left_join(., Dec_excuse_postal_council,
                by = c("ELECTIONEVENTID",
                       "ELECTORID",
                       "DECLARATIONEXCUSEVOTETYPECODE")) %>%
      
      #join to get the Council names
      left_join(Contests_council%>%
                  dplyr::select(AREACODE,CONTESTAREACODE),
                by = c("ISSUINGDISTAREACODE"="CONTESTAREACODE")) %>%
      
      
      
      group_by(Reject_reason, AREACODE) %>%
      tally() %>% dplyr::rename(Count = n) %>%
      
      dplyr::rename(Council = AREACODE) %>%
      
      
      
      
      ## get the total number of PVAs accepted
      left_join(Postal_master_council%>%
                  dplyr::select(Council,Total_applications),
                by = "Council") %>%
      
      ## For Postal Vote Application Reject Reasons, the % is the number of rejected PVAs, divided 
      ## by the total number of applications in that council...
      mutate(Percent_of_Applications = Count / Total_applications)
    
    
  }
  
  
  
  #Capture all Rejected, making sure we exclude those which have had their status changed
  
  Votestatus_council <- Votestatus_council_Capture_Changes %>%
    arrange(ELECTORID,VOTESTATUSRECORDDATE) %>%
    group_by(ELECTORID) %>%
    mutate(RowNumber = row_number()) %>%
    #only take ones with a vote status of 'rejected' AND the latest one, so hasn't been overruled.
    filter(VOTESTATUSTYPECODE == "Rejected" & RowNumber == max(RowNumber)) %>%
    group_by(ISSUINGDISTAREACODE,VOTESTATUSINFO) %>%
    summarise(COUNT=n()) %>%
    ungroup()
  
  
  ## Postal scrutiny rejections ----
  postal_scrutiny_reject <- Votestatus_council %>% 
    
    ## Join to get the Council names
    left_join(Contests_council%>%
                dplyr::select(AREACODE,CONTESTAREACODE),
              by = c("ISSUINGDISTAREACODE" = "CONTESTAREACODE"))%>%
    
    dplyr::rename(Reject_reason  = VOTESTATUSINFO,
           Council        = AREACODE,
           Count          = COUNT) %>%
    
    ## Group by Council
    group_by(Council, Reject_reason) %>%
    {if (nrow(.) > 0) {
      summarise(., Count = sum(Count, na.rm = TRUE))
    } else {.}
    } %>%
    
    ## Get the total number of Scrutinized votes
    left_join(Postal_master_council%>%
                dplyr::select(Council,Scrutiny_Accepted,Scrutiny_Rejected),
              by="Council") %>%
    
    ## For Postal Vote Scrutiny Reject reasons, the % is the number of rejected PVCs divided 
    ## by the total number of votes scrutinised in that council
    mutate(Percent_of_Scrutinized = Count / (Scrutiny_Accepted + Scrutiny_Rejected))
  
}





## 5). HEADLINE NUMBERS ======================================================================


## Enrollment Markoffs ----
if (Dec_Votes_Available) {
  
  ## 
  Enrol_prov_markoff <- sum(Markoff_sum_type %>% 
                              filter(Vote_type == 'Enrolment' & 
                                       Venue_type == 'Pre-Poll') %>% 
                              .$Total_markoffs)
  
  
  Enrol_prov_markoff_scrutinised <- Markoff_sum_type %>%
    filter(Vote_type == 'Enrolment'& 
             Venue_type == 'Pre-Poll') %>% 
    {.$Accepted_markoffs + .$Rejected_markoffs} %>% sum()
  
  
  Enrol_prov_markoff_rejected <- Markoff_sum_type %>%
    filter(Vote_type == 'Enrolment'& 
             Venue_type == 'Pre-Poll') %>% 
    {.$Rejected_markoffs} %>% sum()
  
  
  Enrol_prov_markoff_accepted <- Markoff_sum_type %>%
    filter(Vote_type == 'Enrolment'& 
             Venue_type == 'Pre-Poll') %>% 
    {.$Accepted_markoffs} %>% sum()
  
  
  Prepoll_markoff <- sum(Markoff_sum_type %>% 
                           filter(Venue_type == 'Pre-Poll') %>% 
                           .$Total_markoffs)
  
  
  
  
  
  ## NAMAV Markoffs ----
  NAMAV_prov_markoff <- sum(Markoff_sum_type %>% 
                              filter(Vote_type == 'NAMAV' & 
                                       Venue_type == 'Pre-Poll') %>% 
                              .$Total_markoffs)
  
  
  NAMAV_prov_markoff_scrutinised <- Markoff_sum_type %>%
    filter(Vote_type == 'NAMAV'& 
             Venue_type == 'Pre-Poll') %>% 
    {.$Accepted_markoffs + .$Rejected_markoffs} %>% sum()
  
  
  NAMAV_prov_markoff_rejected <- Markoff_sum_type %>%
    filter(Vote_type == 'NAMAV'& 
             Venue_type == 'Pre-Poll') %>% 
    {.$Rejected_markoffs} %>% sum()
  
  
  NAMAV_prov_markoff_accepted <- Markoff_sum_type %>%
    filter(Vote_type == 'NAMAV'& 
             Venue_type == 'Pre-Poll') %>% 
    {.$Accepted_markoffs} %>% sum()
  
  
  #total Dec Pre-poll mark-offs is sum of Enrolment and NamAV Mark-Offs.---
  Total_prov_markoff <- NAMAV_prov_markoff + Enrol_prov_markoff
  
  
  ## AoBP counts ---- 
  Total_election_day_issued  <- sum(EMA_AoBP_summary$Total_envelopes, na.rm=TRUE) %>%
    {ifelse (is.na(.), 0, .)}
  
  #need to ensure enevelopes aren't double counted for different contests
  
  
  Total_election_day_issued <- sum(EMA_AoBP_summary$Total_envelopes, na.rm=TRUE) %>%
    {ifelse (is.na(.), 0, .)}
  
  
  Enrol_election_day_issued  <- sum(EMA_AoBP_summary$Enrolment_envelopes, na.rm=TRUE) %>%
    {ifelse (is.na(.), 0, .)}
  
  
  NAMAV_election_day_issued  <- sum(EMA_AoBP_summary$NAMAV_envelopes, na.rm=TRUE) %>%
    {ifelse (is.na(.), 0, .)}
  
  
  
  
  ## Create flex table for markoff headline --------
  ## Names for values:
  DecHeadlineTable <- tibble(Names =
                               
                               ## Create headline numbers 
                               c("Pre-Poll Enrolment",
                                 "Enrolment - Provisional Mark-offs",
                                 "Enrolment - Accepted",
                                 "Enrolment - Rejected",
                                 
                                 "Pre-Poll NAMAV",
                                 "NAMAV - Provisional Mark-offs",
                                 "NAMAV - Accepted",
                                 "NAMAV - Rejected",
                                 "Total Pre-Poll Mark-offs",
                                 
                                 "Election Day Declaration Votes",
                                 "Enrolment Envelopes Issued",
                                 "NAMAV Envelopes Issued",
                                 "Total Declaration Votes Issued on Election Day"),
                             
                             ## Enter headline values
                             Values =
                               c("Count",
                                 Enrol_prov_markoff,
                                 Enrol_prov_markoff_accepted,
                                 Enrol_prov_markoff_rejected,
                                 
                                 " ",
                                 NAMAV_prov_markoff,
                                 NAMAV_prov_markoff_accepted,
                                 NAMAV_prov_markoff_rejected,
                                 Total_prov_markoff,
                                 
                                 " ",
                                 Enrol_election_day_issued,
                                 NAMAV_election_day_issued,
                                 Total_election_day_issued)) %>% 
    
    ## Turn into flextable
    flextable() %>% 
    
    ## Set background formatting for flext table
    bg(bg = "coral",       i=~ Values != "Count")                 %>%
    bg(bg = "deepskyblue", i=~ str_detect(Names,"Election Day"))  %>%
    bg(bg = "deepskyblue", i=~ str_detect(Names,"Envelopes"))     %>%
    bg(bg = "black",       i=~ Values == "Count" | Values == " ") %>%
    bg(bg = "#E7298A",     i=~ str_detect(Names,"Total"))         %>%
    
    ## Fontsize, color and bold 
    fontsize(size = flex_headline_size)                 %>%
    bold(i = 1,  j = NULL, bold = TRUE, part = "body")  %>%
    bold(i = 5,  j = NULL, bold = TRUE, part = "body")  %>%
    bold(i = 10, j = NULL, bold = TRUE, part = "body")  %>%
    bold(i = 9,  j = NULL, bold = TRUE, part = "body")  %>%
    bold(i = 13, j = NULL, bold = TRUE, part = "body")  %>%
    
    ## Set the colour and width
    color(color = "white", i=~ Values == "Count") %>%
    color(color = "white", part = "all")          %>% 
    
    width(j =~ Names,  width = 600) %>%
    width(j =~ Values, width = 100) %>%
    delete_part(part = "header")    %>%
    border(border = fp_border(color = "white", width = 3)) %>% 
    hrule(rule = "atleast", part = "all")%>%
    height_all(height = 0.75)
  
  
}


## Postal Summary ----
if (Postal_Votes_Available){
  
  ## All Postal headlines below need revising 
  Postal_headline_numbers <- Postal_master_council %>% 
    summarise_if(is.numeric, sum)
  
  
  ## Postal applications
  ## Count the NA rows for 'ACCEPTED' postal votea apps
  postal_total_accepted_applications <- Postal_headline_numbers %>%
    .$PVA_Accepted 
  
  #rejected applications can't be taken straight from Postal_master_Council
  #need to add the ones which aren't connected to an elector.
  postal_total_rejected_applications<- Postal_headline_numbers%>%
    .$PVA_Rejected
  
  ## Postal scrutiny 
  ## Count the !NA rows for 'PROCESSEDDATEBY'
  ## {} escapes the pipe
  postal_total_scrutinised <- Postal_headline_numbers %>%
    {.$Scrutiny_Accepted + .$Scrutiny_Rejected} 
  
  
  ## Postal rejected votes
  ## Number of scur
  postal_scrutiny_rejected <- Postal_headline_numbers %>%
    .$Scrutiny_Rejected 
  
  
  ## Postal accepted votes
  ## Count the !NA rows for 'CERTIFICATEACCEPTED'
  postal_scrutiny_accepted <- Postal_headline_numbers %>%
    .$Scrutiny_Accepted 
  
  
  ## Postal votes fulfilled 
  ## Count the !NA rows for 'SENDFULFILLMENTDATE'
  postal_total_fulfilled <- Postal_headline_numbers %>%
    .$PVA_Fulfilled 
  
  
  ## Postal votes remaining to be fulfilled 
  ## Count the NA rows for 'SENDFULFILLMENTDATE'
  postal_remain_fulfilled <- Postal_headline_numbers %>%
    {.$PVA_Accepted - .$PVA_Fulfilled} 
  
  #if postal_remain_fulfilled is negative, Dec Voting have asked it be set to zero.
  
  if (postal_remain_fulfilled<0){
    postal_remain_fulfilled <- 0
  }
  
  
  #create flex table for Postal headlines--------
  #create raw table first
  
  PostalHeadlineTable <- tibble(Names = c(
    
    ## Create headlines
    "Postal Vote Applications",
    "Applications - Accepted",
    "Applications - Rejected",
    
    "Postal Vote Fulfilment",
    "Fulfilled",
    "Awaiting Fulfilment",
    
    "Postal Vote Scrutiny",
    "Accepted",
    "Rejected",
    "Total Scrutinised"),
    
    ## Enter the values
    Values = c(
      "Count",
      postal_total_accepted_applications,
      postal_total_rejected_applications,
      
      " ",
      postal_total_fulfilled,
      postal_remain_fulfilled,
      
      " ",
      postal_scrutiny_accepted,
      postal_scrutiny_rejected,
      postal_total_scrutinised)) %>% 
    
    ## Now turn into flex table and format;
    flextable() %>% 
    
    ## Set background
    bg(bg = "coral", i =~ Values != "Count")                %>%
    bg(bg = "deepskyblue",i =~ Names=="Accepted"| 
         Names=="Total Scrutinised"|
         Names=="Rejected") %>%
    bg(bg="#E7298A", i =~ str_detect(Names,"Fulfil"))%>%
    bg(bg = "black",  i =~ Values == "Count" |Values == " " ) %>%
    
    ## Fontsize, color and bold 
    fontsize(size = flex_headline_size)                       %>%
    bold(., i = ~str_detect(Names,"Postal Vote"), j = NULL, bold = TRUE, part = "body")  %>%
    
    ## Set colour and width
    color(color = "white", i=~ Values == "Count") %>%
    color(color = "white", part = "all")          %>% 
    
    width(j=~ Names, width = 400) %>%
    width(j=~ Values,width = 100) %>%
    delete_part(part = "header")  %>%
    border(border = fp_border(color = "white", width = 3)) %>%   
    hrule(rule="atleast",part="all")%>%
    height_all(height = 0.75)
  
}

#data for ROs--------------
#write some csv files to RO source file so it can match what's shown here.

# if (Dec_Votes_Available){
#   setwd(server_data)
#   
#   write_csv(DecVote_master %>%
#               mutate(EventID=event_group_ID),"DecVote_Data.csv")
#   
# }





## CHECK DATA  ==========================================================



## Now find out which rows are in the endpoint, but not the webportal
# if(check_data) {
# 
#   ## Check the statuses
#   file_list <- c('Postal_applications_council',
#                  'Contests_council',
#                  'Reject_Reasons',
#                  'All_Postal_council',
#                  'Postal_Votes_master',
#                  'Postal_master_council')
# 
# 
#   ## Add data to workbook
#   DecVote_workbook <- createWorkbook()
# 
#   ## file = file_list[1]
#   for(file in file_list) {
# 
#     File_to_Write <- get(file)
# 
#     write_excel_csv(File_to_Write,
#                     paste0(DV_server_data, file, '.csv'))
# 
#     addWorksheet(DecVote_workbook, file)
# 
#     ## Write the data to the corresponding worksheet
#     openxlsx::writeData(wb       = DecVote_workbook,
#                         sheet    = file,
#                         x        = get(file),
#                         startCol = 1,
#                         startRow = 1,
#                         rowNames = FALSE)
# 
#   }
# 
#   ## Save the whole workbook
#   saveWorkbook(DecVote_workbook,
#                paste0(DV_server_data, 'DecVoting_Data_check.xlsx'),
#                overwrite = TRUE)
# }

