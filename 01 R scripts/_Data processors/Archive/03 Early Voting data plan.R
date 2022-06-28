####################################################################################################################
###################################### EARLY VOTING DEPENDENCY PLAN ---- ###########################################
####################################################################################################################


## This code processes the data for the early-voting dashboard 
message('Create a drake plan of analysing the early voting data')



## Load in a table of weights for the Daily Pre-poll 
## Is this still needed?
early_voting_plan <- drake_plan(
  
  daily_loading_matrix = read.xlsx('./02 Source data/DailyPrePollWeights.xlsx', sheet = 1) %>% 
    dplyr::select(-Dates),
  
  
  ## Venue open days ----
  open_days = opening_hour %>% filter(IsOpen == 'Y') %>% 
    mutate(Date = as.character(Date)),
  
  
  ## Create a key for the date
  ndays = open_days %>% 
    group_by(EventID, AreaCode, VenueName) %>% 
    mutate(key = row_number()),
  
  
  ## Early Voting Centres (EVCs) ----
  All_EVCs = Pre_poll_projections %>% 
    mutate(LocationTypeCode = case_when(LocationTypeCode == 'Pre-polling Place' ~ 'EVC',
                                        LocationTypeCode == 'Returning Office'  ~ 'EMO',
                                        LocationTypeCode == 'Polling Place'     ~ 'VC')),
  
  
  ## Venue projected voters ----
  Projection_days = All_EVCs %>% 
    
    ## These columns are not all in the dataset 
    arrange(EventID, AreaCode, VenueName) %>% 
    filter(LocationTypeCode == 'EVC' | LocationTypeCode == 'EMO') %>%
    left_join(open_days, by = c('AreaCode', 'EventID', 'VenueName')) %>%  
    
    ## Do we need all these columns?
    ## Removed :: 'StreetAddressID', 'LocationStatusCode'
    group_by(EventID, AreaCode, VenueName, StreetAddressID,
             ProjectedVoters, 
             LocationStatusCode,
             LocationTypeCode) %>%
    
    summarise(Days = n()) %>% 
    left_join(daily_loading_matrix, by = c('Days' = 'Days_Open')) %>%
    
    gather(key,value, -EventID, -AreaCode, -VenueName, -StreetAddressID, 
           -LocationStatusCode, -LocationTypeCode,-Days, -ProjectedVoters) %>%
    mutate(Projection = round(ProjectedVoters*value,0)) %>%
    filter(Projection > 0) %>%
    
    group_by(EventID, AreaCode, VenueName, StreetAddressID,
             LocationStatusCode,
             LocationTypeCode) %>%
    
    mutate(key = row_number()) %>%
    left_join(ndays),
  
  
  ## Mark-off days ----
  ## Do we need this?
  Markoff_days = Markoff %>%
    
    group_by(ELECTIONEVENTID, 
             DECLARATIONEXCUSEVOTETYPECODE, 
             ISSUINGDISTAREACODE, 
             GAZETTEDNAME, 
             StreetAddressID, 
             VOTEPROCESSEDDATE) %>%
    
    summarise(`Early Voting` = n()) %>% 
    filter(DECLARATIONEXCUSEVOTETYPECODE == 'Pre-poll Ordinary') %>% 
    ungroup(),
  
  
  ## Sydney Town Hall not in this table, as it has no projections...
  Votes_projection_day <- Projection_days %>% 
    
    ## The dates don't match here....
    full_join(Markoff_days, by = c('AreaCode' = 'ISSUINGDISTAREACODE',
                                   'StreetAddressID',
                                   'Date' = 'VOTEPROCESSEDDATE')),
  
  
  ## Total daily early vote load ----
  ## Can we sort by Date and Vote count, 
  ## then top and tail the list of venues? 
  ## View(venue_total_load[rowSums(is.na(venue_total_load)) > 0,])
  venue_total_load <- Votes_projection_day %>%
    
    group_by(AreaCode, VenueName, Date) %>%
    summarise(`Early Voting` = sum(`Early Voting`, na.rm = TRUE),
              Projection     = sum(Projection,     na.rm = TRUE)) %>%
    
    ## Create a Measure of 'Voting Load'
    mutate(Voting_load        = Projection - `Early Voting`, 
           `Voting_load_prop` = ifelse(`Early Voting` > 0, 
                                       round((Projection/`Early Voting`), digits = 3), 
                                       round((Projection/`Voting_load`) , digits = 3))) %>%
    
    ## Order by date and number of votes
    .[order(as.Date(.$Date, format = "%d/%m/%Y"), 
            -.$`Early Voting`),], 
  
  
  ## Markoff table still produces duplicated rows, and NA rows
  Markoffs_day_contest_distinct <- Markoffs_day_contests %>%
    
    distinct() %>% 
    rename(LGA            = ISSUINGAREA,
           Contest_area   = CONTESTAREA,
           Venue          = VENUENAME,
           Contest_type   = CONTESTTYPECODE,
           Date_processed = VOTEPROCESSEDDATE,
           Early_votes    = PRE_POLL_MARKOFFS) %>% 
    na.omit(),
  
  
  
  
  
  ## 2). STH PROJECTIONS =======================================================================
  
  
  ## Sydney Town Hall Mark-offs ----
  STH_daily_markoff_dist <- Markoff %>%  
    
    ## Just the town hall data
    filter(GAZETTEDNAME == 'Sydney Town Hall') %>%
    
    group_by(ELECTIONEVENTID,
             DECLARATIONEXCUSEVOTETYPECODE,
             VOTEPROCESSEDDATE,
             ISSUINGDISTAREACODE,
             GAZETTEDNAME) %>%
    summarise(markoffs = n()),
  
  
  ## Now add the projections to this
  sth_dates     <- sort(unique(STH_daily_markoff_dist$VOTEPROCESSEDDATE)) %>% 
    as.data.frame() %>% 
    rename(VOTEPROCESSEDDATE = ".") %>% 
    mutate(PrePollDay = 1:nrow(.),
           VOTEPROCESSEDDATE = as.character(VOTEPROCESSEDDATE)),
  
  
  ## STH projection table ----
  STH_projection_date <- STH_projections %>% 
    
    ## Just get the columns we need
    dplyr::select(ContestAreaCode, PrePollDay, Projection, AreaCode) %>% 
    
    ## Join on the dates
    rename(ISSUINGDISTAREACODE = ContestAreaCode) %>% 
    left_join(., sth_dates, by = 'PrePollDay') %>% 
    dplyr::select(-PrePollDay),
  
  ## Sydney Town Hall Mark-offs with projections ----
  ## This has lots of NAs. So including projections removes 
  ## Venues for which you have markoff data...
  STH_daily_markoff_proj <- STH_daily_markoff_dist %>%  
    
    ## Join the STH pre-poll votes to the projections
    left_join(., STH_projection_date, by = c("VOTEPROCESSEDDATE", 
                                             "ISSUINGDISTAREACODE")),
  
  
  ## STH projection totals ----
  STH_LGA_prj <- STH_projections %>%
    
    ## Rename for join
    rename(LGA            = AreaCode,
           Contest_area   = ContestAreaCode) %>% 
    group_by(LGA) %>%
    summarise(Projection  = sum(Projection, na.rm = TRUE)),
  
  
  ## STH LGA Overall totals ----
  STH_LGA_totals <- Markoffs_day_contests %>%
    
    ## Make the names more user-friendly
    rename(LGA            = ISSUINGAREA,
           Contest_area   = CONTESTAREA,
           Venue          = VENUENAME,
           Contest_type   = CONTESTTYPECODE,
           Date_processed = VOTEPROCESSEDDATE,
           Early_votes    = PRE_POLL_MARKOFFS) %>% 
    
    ## Filter to STH, group by date
    filter(Venue == 'Sydney Town Hall') %>% 
    group_by(LGA) %>%
    summarise(Early_votes = sum(Early_votes, na.rm = TRUE)) %>% 
    
    ## Create cumulative early vote count
    mutate(Venue   = 'Sydney Town Hall',
           Updated = now()) %>% 
    left_join(., STH_LGA_prj, by = "LGA") %>% 
    
    ## Create a Measure of 'Voting Load'
    mutate(Voting_load        = Projection - Early_votes, 
           Voting_load_prop   = ifelse(Early_votes > 0, round((Projection/Early_votes), digits = 3), 
                                       round((Projection/Voting_load) , digits = 3))) %>% 
    
    dplyr::select(Updated,    LGA, Venue,  Early_votes, 
                  Projection, Voting_load, Voting_load_prop),
  
  
  
  
  
  ## 3). AGGREGATE TABLES ====================================================================
  
  
  ## We could re-name the fields of the markoff table so they are more user-friendly?
  ## Create template blocks of code to create tables (e.g. the below code could be a function)
  
  
  ## Cumulative Markoff totals ----
  Markoff_Venue_totals <- Markoffs_day_contests %>% 
    
    ## Get the unique rows :: for some reason, there are duplicated venue-date combinations
    distinct() %>% 
    
    ## Make the names more user-friendly
    rename(LGA            = ISSUINGAREA,
           Contest_area   = CONTESTAREA,
           Venue          = VENUENAME,
           Contest_type   = CONTESTTYPECODE,
           Date_processed = VOTEPROCESSEDDATE,
           Early_votes    = PRE_POLL_MARKOFFS) %>% 
    
    ## Group by date and Council
    group_by(Venue) %>%
    
    ## Summarise early voting
    summarise(Early_votes = sum(Early_votes, na.rm = TRUE)) %>% 
    
    ## Create cumulative early vote count
    mutate(Updated = now()) %>% 
    dplyr::select(Updated, everything()),
  
  
  ## Cumulative Markoff totals ----
  Markoff_LGA_totals <- Markoffs_day_contests %>% 
    
    ## Make the names more user-friendly
    rename(LGA            = ISSUINGAREA,
           Contest_area   = CONTESTAREA,
           Venue          = VENUENAME,
           Contest_type   = CONTESTTYPECODE,
           Date_processed = VOTEPROCESSEDDATE,
           Early_votes    = PRE_POLL_MARKOFFS) %>% 
    
    ## Group by date and Council
    group_by(LGA, Contest_area, Venue, Contest_type) %>%
    
    ## Summarise early voting
    summarise(Early_votes = sum(Early_votes, na.rm = TRUE)) %>% 
    
    ## Create cumulative early vote count
    mutate(Updated = now()) %>% 
    dplyr::select(Updated, everything()),
  
  

  
  
  ## STH LGA council or ward? ----
  LGA_council_mayor_lut <- table(Markoffs_day_contests$ISSUINGAREA, 
                                 Markoffs_day_contests$CONTESTTYPECODE) %>% 
    as.data.frame() %>% 
    rename(LGA          = Var1, 
           Contest_type = Var2, 
           Early_votes  = Freq) %>% 
    
    pivot_wider(names_from  = "Contest_type",
                values_from = "Early_votes") %>% 
    
    ## Create new columns for what the councils have
    mutate(Councillor_vote = ifelse(Councillor > 0 , 'Yes', 'No'),
           Mayor_vote      = ifelse(Mayor      > 0 , 'Yes', 'No'),
           
           Poll_vote       = ifelse(Poll       > 0 , 'Yes', 'No'),
           Referendum_vote = ifelse(Referendum > 0 , 'Yes', 'No')) %>% 
    dplyr::select(LGA, Councillor_vote, Mayor_vote, Poll_vote, Referendum_vote),
  
  
  
  
  
  ## 4). CREATE SUMMARY NUMBERS =================================================================
  
  
  ## Whether or not we use value boxes will depend on the requirements
  ## what is the purpose of the numbers? We need to know why the 
  
  
  ## Color brewer display ::
  # display.brewer.all(colorblindFriendly = TRUE)
  # brewer.pal(n = 12, name = "Paired")
  
  
  ## OVERALL HEADLINES ----
  
  
  ## Venue with most early votes to date? ----
  Highest_EV_venue = Markoff_Venue_totals %>%
    filter(Venue != 'Sydney Town Hall') %>% 
    arrange(-Early_votes) %>%
    dplyr::select(Venue, Early_votes) %>% 
    head(., 1),
  
  
  ## Venue with least early votes ----
  Lowest_EV_venue = Markoff_Venue_totals %>%
    filter(Venue != 'Sydney Town Hall') %>% 
    arrange(Early_votes) %>%
    dplyr::select(Venue, Early_votes) %>% 
    head(., 1),
  
  
  ## Most under-projected venue ----
  Under_projected  = venue_total_load %>%
    arrange(Voting_load_prop) %>%
    ungroup() %>%  
    dplyr::select(VenueName, Voting_load_prop) %>% 
    head(., 1),
  
  
  ## Most over-projected venue ----
  Over_projected  = venue_total_load %>%
    arrange(-Voting_load_prop) %>%
    ungroup() %>%  
    dplyr::select(VenueName, Voting_load_prop) %>% 
    head(., 1),
  
  
  ## Cumulative STH total? ----
  EV_tally <- Markoff_Venue_totals %>% 
    .$Early_votes %>%
    sum() %>% format(., big.mark = ","),
  
  
  ## STH HEADLINES ----
  
  
  ## LGA with the most early votes to date? ----
  STH_LGA_most = STH_LGA_totals %>%
    na.omit() %>% 
    arrange(-Early_votes) %>%
    dplyr::select(LGA, Early_votes) %>% 
    head(., 1),
  
  
  ## LGA with the least early votes to date? -----
  STH_LGA_least = STH_LGA_totals %>%
    na.omit() %>% 
    arrange(Early_votes) %>%
    dplyr::select(LGA, Early_votes) %>% 
    head(., 1),
  
  
  ## LGA most under projected to date? ----
  STH_LGA_under = STH_LGA_totals %>%
    na.omit() %>% 
    arrange(-Voting_load_prop) %>%
    ungroup() %>%  
    dplyr::select(LGA, Voting_load_prop) %>% 
    head(., 1),
  
  
  ## LGA least under projected  to date? -----
  STH_LGA_over = STH_LGA_totals %>%
    na.omit() %>% 
    arrange(Voting_load_prop) %>%
    ungroup() %>%  
    dplyr::select(LGA, Voting_load_prop) %>% 
    head(., 1),
  
  
  ## Cumulative STH total? ----
  STH_tally <- STH_LGA_totals %>% 
    .$Early_votes %>%
    sum() %>% format(., big.mark = ","),
  
  
  
  
  
  ## 5). CREATE PLOTS =============================================================
  
  
  ## EV Graph Requirements ---- 
  # -	Daily NSW vote summary 
  # o	Total number of votes issued by day (line chart)
  # o	Which venues took the most/least votes: overall + each day?
  # o	Identify which venues may have been overwhelmed on a particular day (overall, + each day?)
  # 
  # -	Sydney Town Hall summary
  # o	Total number of votes issued by day (line chart)
  
  
  ## Total daily early votes ----
  ## We always use old data for the projections
  daily_total_ev <- Votes_projection_day %>%
    
    group_by(OpeningDate, Date) %>%
    summarise(`Early Voting` = sum(`Early Voting`, na.rm = TRUE),
              Projection = sum(Projection, na.rm = TRUE)) %>%
    
    gather(key = Status, value = Count, -OpeningDate, -Date) %>% 
    arrange(Date) %>%
    mutate(Count = ifelse(Count == 0, NA, Count)),
  
  
  ## STH Total daily early votes ---- 
  ## This is not cumulative
  ## STH_daily_markoff_dist has 250 more rows that STH_daily_markoff_proj
  daily_total_STH <- STH_daily_markoff_proj %>%
    
    ## Group by date
    group_by(VOTEPROCESSEDDATE) %>%
    
    ## Summarise the markoffs and projections for each day
    summarise(`Early Voting` = sum(markoffs,   na.rm = TRUE),
              Projection     = sum(Projection, na.rm = TRUE)) %>%
    
    gather(key = Status, value = Count, -VOTEPROCESSEDDATE) %>% 
    arrange(VOTEPROCESSEDDATE) %>%
    mutate(Count = ifelse(Count == 0, NA, Count)) %>% 
    
    ## Convert date
    mutate(weekday = weekdays(as.Date(VOTEPROCESSEDDATE))) %>% 
    mutate(Date    = paste0(weekday, ', ', VOTEPROCESSEDDATE)),
  
  
  
  
  
  ## Plot 1). daily Early Votes ----
  ## Create a line plot of the total number of early votes issued by day
  cumul.pre.poll.plot <- group_line_plot(df         = daily_total_ev %>% completeFun(., 'Date'),
                                         title      = 'Fig 1). All Venues - Total Votes issued by day',
                                         subtitle  = '',
                                         caption    = '', 
                                         
                                         xvar       = 'Date',
                                         yvar       = 'Count',
                                         group_var  = 'Status',
                                         
                                         tsize      = 33,
                                         capt_size  = 33,
                                         xsize      = 33,
                                         ysize      = 33,
                                         ycol       = 'black',
                                         line_size  = 5,
                                         point_size = 8,
                                         
                                         lab_size   = 6,
                                         leg_size   = 33,
                                         leg_pos    = 'bottom',
                                         
                                         
                                         ylab       = 'Pre-Poll Votes',
                                         xlab       = '',
                                         lab_angle  = 45),

  
  
  ## Plot 2). Daily Early Votes for STH ----
  cumul.sth.plot <- group_line_plot(df        = daily_total_STH %>% na.omit(),
                                    title     = 'Fig 2). Sydney Town Hall - Total Votes issued by day',
                                    subtitle  = '',
                                    caption   = '', 
                                    
                                    xvar      = 'VOTEPROCESSEDDATE',
                                    yvar      = 'Count',
                                    group_var = 'Status',
                                    
                                    tsize      = 33,
                                    capt_size  = 33,
                                    xsize      = 33,
                                    ysize      = 33,
                                    ycol       = 'black',
                                    line_size  = 5,
                                    point_size = 8,
                                    
                                    lab_size   = 6,
                                    leg_size   = 33,
                                    leg_pos    = 'bottom',
                                    
                                    
                                    ylab       = 'Pre-Poll Votes',
                                    xlab       = '',
                                    lab_angle  = 45),

)





## 6). CREATE DEPENDENCY GRAPH =================================================================


## Then create a plot of the dependencies
early_voting_plan_graph = vis_drake_graph(early_voting_plan,
                                          main      = 'LG2021 Early Voting Dependency Graph',
                                          font_size = 20)


early_voting_plan_graph






####################################################################################################################
################################################# TBC ---- #########################################################
####################################################################################################################