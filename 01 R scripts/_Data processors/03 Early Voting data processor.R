
################################# ---- EARLY VOTING DATA PROCESSOR ---- ###############################



## This code processes the data for the early-voting dashboard 
message('Analyse the early voting data')


## To do ----
## Changeset 11636 was when STH was removed





## 1). VENUE-LEVEL PROJECTIONS =======================================================================


## Table of weights for Three weeks of Daily Pre-poll :: 
# source(paste0(Sys.getenv("source_control_local_path"), "Main/R Scripts - Production/Folder_locations.R"))
# setwd(root_dashboard)


## Set variables
headline_size <- 22


# daily_loading_matrix <-  read.xlsx("./02 Source data/EarlyVoting/DailyPrePollWeights_LGv2.xlsx") %>% 
#   dplyr::select(-Dates)

#check if there is any mark-off data  available yet.

if (nrow(Markoffs_day_contests) > 0){
  Pre_Poll_Open <- TRUE
} else {
  Pre_Poll_Open <- FALSE
}



## Table of which days each venue is open
open_days <- opening_hour %>% 
  
  #some venues were open when they shouldn't have been, according to EMS.
  #set those days to open.
  mutate(IsOpen = ifelse(
     VenueName %in% Incorrect_Prepolls & 
      OpeningDate %in% Incorrect_Prepoll_days,
    "Y",
    IsOpen
  )) %>%
  
  filter(IsOpen == 'Y') %>% 
  mutate(Date = as.character(Date)) %>% 
  dplyr::select(-EventID, -AreaCode, -LocationStatusCode) %>% 
  distinct()


## Number of days each venue is open for
ndays <- open_days %>% 
  dplyr::select(VenueName, Date) %>% 
  group_by(VenueName) %>% 
  mutate(Day = row_number())


## Early Voting Centres (EVCs)
All_EVCs <- Pre_poll_projections %>% 
  mutate(LocationTypeCode = case_when(LocationTypeCode == 'Pre-polling Place' ~ 'EVC',
                                      LocationTypeCode == 'Returning Office'  ~ 'EMO',
                                      LocationTypeCode == 'Polling Place'     ~ 'VC')) %>%
  as_tibble()

if (Pre_Poll_Open){
  
  
  #find max date
  
  Last_Date <- ymd(max(Markoffs_day_contests$VOTEPROCESSEDDATE))
  
  Last_Date <- format(Last_Date,"%d-%b-%y")
  
}

## Daily projected voters ---- 
Projection_days <- All_EVCs %>% 
  
  ## These columns are not all in the dataset 
  arrange(EventID, AreaCode, VenueName) %>% 
  filter(LocationTypeCode == 'EVC' | LocationTypeCode == 'EMO') %>%
  left_join(open_days, by = c('VenueName')) %>% 
  
  ## Steve R.'s Rule :: if the venue does not have an opening day attached to it, remove
  ## Used to deal with non-client councils
  filter(!is.na(OpeningDate)) %>% 
  
  ## Group by all the context columns 
  left_join(ndays, by = c("VenueName", "Date")) %>% 
  group_by(EventID, AreaCode, VenueName, VotingChannelCode, 
           LocationTypeCode,  ProjectedVoters, StreetAddressID) %>% 
  
  mutate(Max_day = max(Day)) %>% 
  
  #in staging, some venues are open for more than 11 days. This breaks the join to daily loading matrix.
  #if its coming from staging, set all max days to 11. 
  #this will still break if in the real world, there's more than 11 days.
  
  {if (from_staging){
    mutate(.,Max_day=ifelse(Max_day > 11, 11, Max_day))
  } else {.}
    } %>%
  
  left_join(daily_loading_matrix, by = c('Max_day' = 'Days_Open')) %>%
  
  ## Wide to long
  pivot_longer(cols      = starts_with('Day_'),
               names_to  = 'Projection_Day', 
               values_to = 'Projection_prop') %>%
  filter(Projection_prop != 0) %>% 
  
  ## Group by context columns again
  group_by(EventID, AreaCode, VenueName, VotingChannelCode, 
           LocationTypeCode,  ProjectedVoters, StreetAddressID, Date) %>% 
  
  ## Use the matrix to multiply the projections across all the venue days
  mutate(Projection_Day = row_number()) %>% 
  filter(Day == Projection_Day) %>% 
  mutate(Projection = round(ProjectedVoters*Projection_prop, 0))



#for multi-ward venues, need to group together projections and replace AreaCode with full contest name for Mayor/Referendum/Poll votes.


Projection_days_Multi_Ward <- Projection_days %>%
  #get Issuingarea - council.
  left_join(Contests_council %>%
              select(AREACODE,CONTESTAREACODE),
            by=c("AreaCode" = "CONTESTAREACODE")) %>%
  ungroup()%>%
  rename(Council=AREACODE)%>%
  select(EventID,Council,AreaCode,VenueName,Date,Day,Projection) %>%
  
  
  #remove venues which have the same Council and Ward names - those don't need to be aggregated.
  
  filter(Council!=AreaCode) %>%
  
  #aggregate all remaining into one projection per day per venue
  group_by(EventID,Council,VenueName,Date,Day) %>%
  summarise(Projection=sum(Projection)) %>%
  
  
  #rename some variables so it will fit in with ordinary Projection_days.
  rename(AreaCode=Council) %>%
  ungroup()


#create a total that will be used for joining later.

Projection_days_total <- Projection_days %>%
  ungroup() %>%
  select(AreaCode,VenueName,Date,Day,Projection) %>%
  bind_rows(select(Projection_days_Multi_Ward, -EventID))


## Create the Days the Mark-off venues are open
## Only open for four days?
Markoff_days_open <- Markoffs_day_contests %>%
  
  dplyr::select(VOTEPROCESSEDDATE) %>% 
  distinct() %>% 
  arrange(VOTEPROCESSEDDATE) %>% 
  mutate(Days = row_number()) %>% 
  na.omit() %>%
  
  ## Join on to the main table
  left_join(., Markoffs_day_contests, by = 'VOTEPROCESSEDDATE') %>%
  as_tibble() %>%
  left_join(Contest_Status %>%
              select(contestareacode, contesttypecode, CONTESTSTATUSTYPECODE) %>%
              
              #contest_status has multiple rows for this, but we will only care if ANY of them are contested.
              #set to distinct, will be removed after filter
              distinct(),
            
            by=c("CONTESTAREA" = "contestareacode",
                 "CONTESTTYPECODE" = "contesttypecode"))  %>%
  filter(CONTESTSTATUSTYPECODE == "Contested") %>%
  select(-CONTESTSTATUSTYPECODE)


#need to use markoff days with all contests, to ensure venues with no councillors are captured.
Markoff_days_open_all_contests <- Markoffs_day_contests %>%
  
  dplyr::select(VOTEPROCESSEDDATE) %>% 
  distinct() %>% 
  arrange(VOTEPROCESSEDDATE) %>% 
  mutate(Days = row_number()) %>% 
  na.omit() %>%
  
  ## Join on to the main table
  left_join(., Markoffs_day_contests, by = 'VOTEPROCESSEDDATE') %>%
  as_tibble() 



## Join the projections onto the markoff table
## This has duplicates
## Add geom line and geom dot
Votes_projection_day <- Projection_days %>%
  
  ## Join using the day
  left_join(Markoff_days_open_all_contests, by = c('AreaCode'  = "CONTESTAREA",
                                      "VenueName" = "VENUENAME",
                                      'Day'       = 'Days'))




#only do the following if there is Pre_Poll_data available------------------


if (Pre_Poll_Open){
  

  
  venue_total_load <- Markoffs_day_contests %>%
    as_tibble() %>%
    filter(CONTESTTYPECODE == "Councillor") %>%
    left_join(Projection_days_total,
                by = c("CONTESTAREA" = "AreaCode",
                       "VENUENAME" = "VenueName",
                       "VOTEPROCESSEDDATE" = "Date")) %>%
    select(Day,
      AreaCode = CONTESTAREA,
           VenueName = VENUENAME,
           
           `Projected Votes`= Projection,
           `Pre-poll Votes` = PRE_POLL_MARKOFFS) %>%
    mutate(#`Voting load`       = Projection - `Pre-Poll Votes`, 
      `Voting Load Prop`  = ifelse(`Pre-poll Votes` > 0, 
                                   round((`Pre-poll Votes`/ `Projected Votes`), digits = 3), 
                                   0)) 
    
   
  
  
  #add projections to mark-off days-----------
  
  
  Markoff_days_contests_proj <- Markoffs_day_contests %>%
    as_tibble() %>%
    #add days to each Venue.
    group_by(ISSUINGAREA,CONTESTAREA,VENUENAME,CONTESTTYPECODE) %>%
    arrange(VOTEPROCESSEDDATE) %>%
    mutate(Day = row_number()) %>%
    
    #join to projections
    left_join(Projection_days_total %>%
                select(-Date),
              by=c(
                  "CONTESTAREA"="AreaCode",
                  "VENUENAME"="VenueName",
                  "Day"="Day"
                  #,"VOTEPROCESSEDDATE" = "Date"
                )) %>%
    
    filter(ymd(VOTEPROCESSEDDATE) <= dmy(Last_Date)) %>%
    
    #set pre_poll_markoffs to zero if NA.
    
    mutate(PRE_POLL_MARKOFFS=ifelse(is.na(PRE_POLL_MARKOFFS), 0, PRE_POLL_MARKOFFS)) %>%
    
    # select(-Date) %>%
    ## Create a Measure of 'Voting Load'
    mutate(#`Voting load`       = Projection - `Pre-Poll Votes`, 
      `Voting Load Prop`  = ifelse(PRE_POLL_MARKOFFS > 0, 
                                   round((PRE_POLL_MARKOFFS/ Projection), digits = 3), 
                                   0)) 
  
  
  
  
  ## 2). AGGREGATED EARLY VOTES ====================================================================
  
  
  ## Re-name the fields of the markoff table so they are more user-friendly
  
  
  ## Mark-off Venue totals ----
  Markoff_Venue_totals <- Markoffs_day_contests %>% 
    
    ## Make the names more user-friendly
    rename(LGA              = ISSUINGAREA,
           Contest_area     = CONTESTAREA,
           Venue            = VENUENAME,
           Contest_type     = CONTESTTYPECODE,
           Date_processed   = VOTEPROCESSEDDATE,
           `Pre-poll Votes` = PRE_POLL_MARKOFFS) %>% 
    
    ## Group by date and Council
    group_by(Venue) %>%
    
    ## Summarise early voting
    summarise(`Pre-poll Votes` = sum(`Pre-poll Votes`, na.rm = TRUE)) %>% 
    
    ## Create cumulative early vote count
    mutate(Updated = now()) %>% 
    dplyr::select(Updated, everything())
  
  
  
  
  
  ## Mark-off LGA totals ----
  
  #new version of Markoff_lga_totals
  Markoff_LGA_totals <- Markoff_days_open %>%
    #remove Sydney Town Hall
    filter(VENUENAME != "Sydney Town Hall") %>%
    group_by(ISSUINGAREA,CONTESTAREA,VENUENAME,CONTESTTYPECODE) %>%
    mutate(Most_Recent_Day = n()) %>%
    group_by(ISSUINGAREA,CONTESTAREA,VENUENAME,CONTESTTYPECODE) %>%
    summarise(Pre_Poll_Markoffs = sum(PRE_POLL_MARKOFFS),
              #find highest day
              Days = Most_Recent_Day) %>%
    distinct() %>%
    
    #join to projections
    left_join(Projection_days_total %>%
                group_by(AreaCode,VenueName) %>%
                mutate(Cum_Projection = cumsum(Projection)) %>%
                filter(ymd(Date) == dmy(Last_Date)) %>%
                select(AreaCode,VenueName,Day,Cum_Projection),
              by = c("CONTESTAREA"="AreaCode","VENUENAME"="VenueName")) %>%
    select(-Days) %>% 
    
    ## Make the names more user-friendly
    rename(LGA            = ISSUINGAREA,
           Contest_area   = CONTESTAREA,
           Venue          = VENUENAME,
           Contest_type   = CONTESTTYPECODE,
           `Pre-poll Votes`    = Pre_Poll_Markoffs,
           `Projected Votes`= Cum_Projection)%>%
    
    ## Create a Measure of 'Voting Load'
    mutate(#`Voting load`       = Projection - `Pre-Poll Votes`, 
      `Voting Load Prop`  = ifelse(`Pre-poll Votes` > 0, 
                                   round((`Pre-poll Votes`/ `Projected Votes`), digits = 3), 
                                   0)) 
  
  

  
  
  ## Total daily early votes for plotting ----

  
  daily_cumulative_pre_poll <- Markoffs_day_contests %>%
    as_tibble() %>%
    filter(CONTESTTYPECODE == "Councillor") %>%
    left_join(Projection_days_total,
              by = c("CONTESTAREA" = "AreaCode",
                     "VENUENAME" = "VenueName",
                     "VOTEPROCESSEDDATE" = "Date")) %>%
    group_by(VOTEPROCESSEDDATE) %>%
    summarise(`Pre-poll Votes` = sum(PRE_POLL_MARKOFFS),
              Projection = sum(Projection, na.rm = TRUE)) %>%
    pivot_longer(!(VOTEPROCESSEDDATE),
                 names_to = "Status",
                 values_to = "Count") %>%
    group_by(Status) %>%
    mutate(Day=row_number()) %>%
    ## Then arrange by day, and make a cumulative count
    arrange(Day) %>%
    mutate(Total_cum_count = cumsum(Count))
  
  
  #write relevant data to RO source file-----------
  
  #need to have the total pre-poll so far
  #by contest
  #versus projections.
  
  
  RO_export_data <- Markoff_days_open %>%
    #remove Sydney Town Hall
    filter(VENUENAME!="Sydney Town Hall") %>%
    group_by(ISSUINGAREA,CONTESTAREA,VENUENAME,CONTESTTYPECODE) %>%
    summarise(Pre_Poll_Markoffs=sum(PRE_POLL_MARKOFFS),
              #find highest day
              Days=max(Days)) %>%
    
    #join to projections
    left_join(Projection_days %>%
                group_by(VenueName,AreaCode) %>%
                mutate(Cum_Projection = cumsum(Projection)) %>%
                select(AreaCode,VenueName,Day,Cum_Projection), 
              by = c(
                  "CONTESTAREA"="AreaCode",
                  "VENUENAME"="VenueName",
                  "Days"="Day"
                )) %>%
    
    select(-Days) %>%
    #aggregrate councillor Projections to get Majoral Projections
    group_by(VENUENAME) %>%
    mutate(Cum_Projection=ifelse(CONTESTTYPECODE=="Mayor",
                                 sum(Cum_Projection,na.rm=TRUE),
                                 Cum_Projection)) %>%
    ungroup() %>%
    mutate(Proj_Prop=Pre_Poll_Markoffs/Cum_Projection) %>%
    mutate(EventID=event_group_ID)
  
  #create a list of areas which have venue not provided 
  Venue_Not_Provided_Tidy <- Venue_Not_Provided %>%
    group_by(issuingdistareacode) %>%
    summarise(Count = n()) %>%
    rename(Ward = issuingdistareacode)
  
  
  #create a list of closed venues which have taken votes, and open venues which have not-------------
  
  #if a venue's first day is after today, it should be closed
  Closed_Venues <- ndays %>%
    filter(Day == 1) %>%
    filter(Date > today())
  
  #if a venue's first day is today or ealier, it should be open
  Open_Venues <- ndays %>%
    filter(Day == 1) %>%
    filter(Date <= today())
  
  
  Update <- which(Markoff_days_open$VENUENAME %in% Closed_Venues$VenueName)
  
  Closed_Venues_with_Markoffs <- Markoff_days_open[Update,] %>%
    left_join(Contests_council %>%
                select(CONTESTAREACODE, conteststatustypecode),
              by = c("CONTESTAREA" = "CONTESTAREACODE")) %>%
    filter(conteststatustypecode == "Contested") %>%
    group_by(CONTESTAREA, VENUENAME, CONTESTTYPECODE) %>%
    summarise(PRE_POLL_MARKOFFS = sum(PRE_POLL_MARKOFFS))
  
  
  Update <- which(!(Open_Venues$VenueName %in% Markoff_days_open$VENUENAME))
  
  Open_Venues_with_no_markoffs <- Open_Venues[Update,]
  
  #need to remove the RO offices which aren't taking pre-poll.
  
  Update <- which(!(Open_Venues_with_no_markoffs$VenueName %in% Projection_days_total$VenueName))
  
  
  Open_Venues_no_markoffs_exclude_ROs <- Open_Venues_with_no_markoffs[-Update, ] %>%
    #get the areacode as well
    left_join(Pre_poll_projections %>%
                select(AreaCode, VenueName) %>%
                distinct(),
              by = "VenueName") %>%
    left_join(Contests_council %>%
                select(CONTESTAREACODE, conteststatustypecode),
              by = c("AreaCode" = "CONTESTAREACODE")) %>%
    filter(conteststatustypecode == "Contested") %>%

    select(AreaCode,
           VenueName,
           Opening_Date = Date) 
    
   
  
  
  
  
  setwd(server_data)
  
  write_csv(RO_export_data,"PrePoll_Data.csv")
  write_csv(Venue_Not_Provided_Tidy, "Markoffs_Marked_as_VenueNotProvided.csv")
  write_csv(Closed_Venues_with_Markoffs, "Closed_Venues_with_Markoffs.csv")
  write_csv(Open_Venues_no_markoffs_exclude_ROs, "Open_Venues_with_No_Markoffs.csv")

  
  
  

  
  
  
  ## 3). CREATE SUMMARY NUMBERS =================================================================
  
  
  
  ## Overall headlines ----
  
  
  ## Venue with most early votes to date?
  
  Highest_EV_venue <- Markoff_LGA_totals %>%
    ungroup() %>%
    filter(Venue != 'Sydney Town Hall') %>% 
    arrange(-`Pre-poll Votes`) %>%
    dplyr::select(Venue, `Pre-poll Votes`) %>% 
    head(., 1)
  
  
  ## Venue with least early votes 
  Lowest_EV_venue <- Markoff_LGA_totals %>%
    group_by(Contest_type, Venue) %>%
    summarise(`Pre-poll Votes` = sum(`Pre-poll Votes`)) %>%
    ungroup() %>%
    filter(Venue != 'Sydney Town Hall') %>% 
    arrange(`Pre-poll Votes`) %>%
    dplyr::select(Venue, `Pre-poll Votes`) %>% 
    head(., 1)
  
  
  #add in filter - under and over projected only among those with at least 100 projections.
  ## Most under-projected venue 
  Under_projected <- Markoff_LGA_totals %>%
    arrange(`Voting Load Prop`) %>%
    filter(`Projected Votes` > 100) %>%
    ungroup() %>%  
    select(Venue, `Voting Load Prop`) %>% 
    head(., 1)
  
  if (nrow(Under_projected) == 0) {
    
    Under_projected[1,1] <- "None available"
    Under_projected[1,2] <- 0
  }
  
  
  ## Most over-projected venue 
  Over_projected  <- Markoff_LGA_totals %>%
    arrange(-`Voting Load Prop`) %>%
    filter(`Projected Votes` > 100) %>%
    ungroup() %>%  
    dplyr::select(Venue, `Voting Load Prop`) %>% 
    head(., 1)
  
  if (nrow(Over_projected) == 0) {
    
    Over_projected[1,1] <- "None available"
    Over_projected[1,2] <- 0
  }
  
  
  
  ## Cumulative STH total? 
  EV_tally <- daily_cumulative_pre_poll %>%
    filter(Day == max(Day)) %>%
    filter(Status == "Pre-poll Votes") %>%
    select(Total_cum_count) %>%
    pull()
  
  

  
  ## Create a flex table for Pre-Poll Headlines ----
  PrePollHeadline <- tibble(Names = c(
    
    "Pre-poll Attendance",
    paste0('Highest Venue - ',   Highest_EV_venue[1]),
    paste0('Lowest Venue - ',    Lowest_EV_venue[1]),
    "Total Pre-poll ",
    "Venue Projections",
    paste0('Most Over-projected* - ',  Over_projected[1]),
    paste0('Most Under-projected* - ', Under_projected[1])),
    
    Values = c(
      "Count",
      Highest_EV_venue[2] %>% pull(),
      Lowest_EV_venue[2] %>% pull(),
      
      EV_tally,
      "% of Projection",
      paste0(Over_projected[2]*100,"%"),
      paste0(Under_projected[2]*100,"%"))) %>%
    flextable() %>% 
    
    ## Set background
    bg(bg = "deepskyblue", i =~ Values != "Count")          %>%
    bg(bg = "coral",       i =~ str_detect(Names, "Venue")) %>%
    bg(bg = "coral",       i =~ str_detect(Names, "Total Pre-poll"))  %>%
    bg(bg = "black",       i =~ Values == "Count")          %>%
    bg(bg = "black",       i =~ Values == "% of Projection")          %>%
    
    ## Fontsize, color and bold 
    fontsize(size = headline_size) %>%
    bold(i = 1,  j = NULL, bold = TRUE, part = "body")  %>%
    bold(i = 5,  j = NULL, bold = TRUE, part = "body") %>%
    
    color(color = "white", i=~ Values == "Count")           %>%
    color(color = "white", i=~ Names  == "Total Pre-poll:") %>%
    color(color = "white", part = "all")                    %>%
    
    width(j =~ Names, width = 600) %>%
    width(j =~ Values, width = 100) %>%
    
    #set alignment
    align(j=~Values,align="center",part="all") %>%
    
    #get size of table correct
    delete_part(part = "header")   %>%
    border(border = fp_border(color = "white", width = 3)) %>% 
    hrule(rule="atleast",part="all")%>%
    height_all(height = 1.1)
  
}



################################# ---- EARLY VOTING DATA PROCESSOR ---- ###############################
