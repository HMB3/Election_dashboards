####################################################################################################################
###################################### EARLY VOTING DATA PROCESSOR ---- ############################################
####################################################################################################################


## This code processes the data for the early-voting dashboard 
message('Create a drake plan of analysing the Declaration voting data')



## Load in a table of weights for the Daily Pre-poll 
## Is this still needed?
dec_voting_plan <- drake_plan(
  
  
  ## 1). EMA MARK-OFF TABLES  ==============================================================
  
  
  ## We need to combine markoffs votes during the pre-poll period 
  ## with the Account of Ballot Paper votes from Election day (AoBP).
  EMA_markoffs_rename <- EMA_markoffs %>%
    
    ## Sum of accepted, rejected and provisional markoffs
    ## Add a mutate, create a new column for total markoffs
    rename('Ward'                 = ISSUINGDISTAREACODE,
           'Venue'                = GAZETTEDNAME,
           'Accepted_markoffs'    = ACCEPTED_MARKOFFS,
           'Rejected_markoffs'    = REJECTED_MARKOFFS,
           'Vote_type'            = DEC_VOTE_TYPE,
           'Date'                 = VOTEPROCESSDATE,
           'Venue_type'           = VOTINGCENTRETYPECODE,
           'Provisional_markoffs' = PROVISIONAL_MARKOFFS,
           'Total_markoffs'       = TOTAL_MARKOFFS),
  
  
  ## Rename markoff columns ----
  Markoff_Venue <- EMA_markoffs_rename %>%
    
    ## Group by date and venue.
    ## Dont include Vote_type - they are all as one 
    ## This will aggregate enrollment and NAMAV into the accept and reject count
    group_by(Ward, Venue, Venue_type) %>%
    summarize_if(., is.numeric, sum) %>%
    
    ## For either an enrollment or NAMAV vote at pre-poll venues, regardless of status
    mutate(Rejection_rate = round((Rejected_markoffs/Accepted_markoffs), 
                                  digits = 2), 
           Rejection_rate = ifelse(Rejection_rate == 'Inf', 0, Rejection_rate)),
  
  ## Markoff counts grouped by date ----
  ## This is used for Graph 1). below
  ## This won't work, because the dates will be overidden
  EMA_markoff_group_date <- EMA_markoffs_rename %>% 
    
    ## Group by date, but not enrollment type.
    ## This will create values for accepted and rejected
    group_by(Date) %>%
    summarise(Accepted               = sum(Accepted_markoffs,  na.rm = TRUE),
              Rejected               = sum(Rejected_markoffs,  na.rm = TRUE),
              `Total_markoffs`       = sum(`Total_markoffs`,   na.rm = TRUE),
              `Provisional_markoffs` = sum(`Provisional_markoffs`, na.rm = TRUE)) %>%
    
    ## Ungroup, arrange by date and count total votes
    ## Should the votes be Total, or Provisional
    ungroup() %>% 
    arrange(Date) %>%
    mutate(Total_cum_markoff = cumsum(`Total_markoffs`),
           Total_cum_provis  = cumsum(`Provisional_markoffs`)),
  
  ## Markoff counts by type ----
  ## This is used for Graph 2). below
  ## This should be correct
  Markoff_sum_type <-  EMA_markoffs_rename %>%
    
    ## Create a table for the bar graph
    group_by(Venue_type, Vote_type) %>%
    summarize_if(., is.numeric, sum),
  
  
  ## 2). EMA AOBP TABLES  ==========================================================================
  
  
  ## Change AoBP variable names ----
  EMA_AoBP_summary <- EMA_AoBP %>%
    rename('Ward'       = CONTESTAREACODE,
           'Council'    = AREACODE,
           'Venue'      = VENUE,
           'Contest'    = CONTESTTYPECODE,
           'Venue_type' = VOTINGCENTRETYPECODE,
           'Enrolment_envelopes' = ENROL_ENVELOPES,
           'NAMAV_envelopes'     = NAMAV_ENVELOPES) %>% 
    
    ## there are NAs in here too
    ## We can't get total provisional from AoBP
    na.omit() %>% 
    mutate('Total_envelopes' = Enrolment_envelopes + NAMAV_envelopes),
  
  
  ## Markoff counts grouped by enrolment/NAMAV ----
  AoBP_sum_type <- EMA_AoBP_summary %>% 
    group_by(Venue) %>%
    summarize_if(., is.numeric, sum),
  
  
  
  
  
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
                     'Unscrutinised',       'Rejection_rate'), 
  
  
  ## AoBP grouped by Venue ----
  ## This will sum NAMAV and Enorlment votes of different types: referrendum, poll, etc.
  AoBP_Venue <- EMA_AoBP_summary %>% 
    
    ## Group by Venue, and count votes
    group_by(Council, Ward, Venue, Venue_type) %>%
    summarize_if(., is.numeric, sum) %>% 
    ungroup() %>% 
    dplyr::select(-Total_envelopes, -Council),
  
  
  ## Master DecVote scrutiny ----
  
  
  ## Join the Markoffs and AoBP 
  ## Venue type 'PP' shouldn't be showing provisional mark off 
  ## Does this mean remove PP type from Markoff table?
  ## If we are binding rows, there shouldn't be the same venues in each table...
  DecVote_master <- Markoff_Venue %>%
    
    ## Join AOBP data with markoffs
    full_join(AoBP_Venue, by = c("Ward", "Venue", "Venue_type")) %>% 
    left_join(., Contests_council %>% dplyr::select(Council = AREACODE, Ward = CONTESTAREACODE),
              by = 'Ward') %>%
    
    ## Unscrutinised = Total_provisional_markoff - (Accepted_markoffs + Rejected_markoffs)
    mutate(Updated = now(),
           Unscrutinised = abs(Total_markoffs - 
                                 (Accepted_markoffs + Rejected_markoffs))) %>%
    dplyr::select(scrutiny_cols),
  
  
  
  
  ## 4). POSTAL APPLICATIONS ===========================================================================
  
  
  ## All Postal code below needs revising ----
  ## Try joining on Application ID
  ## Need to know what's fulfilled, but not accepted
  ## Find Jason or Reggie's SQL that defines the variables
  ## Ask reggie about the transaction log
  
  
  ## Table 3). Reggie tells us where the postal reject reasons are in EMA ----
  
  
  ## Table of postal rejections ----
  ## Needs to have reasons in it 
  postal_reject_tab <- Postal_applications_council %>%
    
    ## Join on decexcuse vote table
    left_join(., Dec_excuse_postal_council,
              by = c("ELECTIONEVENTID",
                     "ELECTORID",
                     "DECLARATIONEXCUSEVOTETYPECODE")) %>%
    
    ## Get the table of full reasons from EMA
    filter(!is.na(REJECTEDREASON)) %>%
    group_by(REJECTEDREASON, ISSUINGDISTAREACODE) %>%
    tally() %>% rename(Count = n) %>%
    
    rename(Reject_reason              = REJECTEDREASON,
           Issuing_district_area_code = ISSUINGDISTAREACODE) %>% 
    mutate(Updated = now()) %>% dplyr::select(Updated, everything()),
  
  
  ## Postal scrutiny rejections ----
  postal_scrutiny_reject <- Votestatus_council %>% 
    rename(Reject_reason  = VOTESTATUSINFO,
           Council        = ISSUINGDISTAREACODE,
           Count          = COUNT) %>% 
    mutate(Updated = now()) %>% dplyr::select(Updated, everything()),
  
  
  
  
  
  ## Merge the Postal Tables together ----
  ## View(All_Postal_council[duplicated(All_Postal_council$ELECTORID),])
  All_Postal_council <- Postal_applications_council %>% 
    left_join(., Postal_votes_council,
              by = c("ELECTIONEVENTID", "DECLARATIONEXCUSEVOTETYPECODE", 
                     "ELECTORID",       "APPLICATIONNUMBER", "TYPECODE")) %>% 
    
    ## Join on council from the decexcuse vote table
    left_join(., Dec_excuse_postal_council,
              by = c("ELECTIONEVENTID",
                     "APPLICATIONNUMBER",
                     "ELECTORID",
                     "DECLARATIONEXCUSEVOTETYPECODE")) %>%
    rename(Council = ISSUINGDISTAREACODE),
  
  
  ## Create Master Postal Tables ----
  Postal_Votes_master <- All_Postal_council %>% 
    
    ## Format Date for plotting (so if doesn't include the time) 
    mutate(
      
      ## Myf - fulfilment should show how many PV applications : 
      ## LGA - PVAs accepted  - PVs Fulfilled(Issued)this table - Scrutiny Accepted - Scrutiny Rejected
      ## Eg. Burwood - 100 - 100 - 50 -50 Mys Numbers are wrong....
      ## Burwood	199	199	8	0
      
      ## Use PROCESSEDDATE for the cumulative graph. 
      ## If there is a row in the postal vote application table, and REJECTED is null, then the application is accepted.0
      ## REGGIE :: The accepted column is not used. If the rejected column is null, then the PVA record is accepted.
      
      ## How will these terms change, based on Reggie/Jason's definitions? 
      Total_applications = 1,
      PVA_Rejected       = ifelse(!is.na(REJECTED), 1, 0),
      
      PVA_Accepted       = ifelse(is.na(REJECTED), 1, 0),
      PVA_Fulfilled      = ifelse(!is.na(SENDFULFILLMENTDATE), 1, 0),
      
      ## There are five categories of VOTESTATUSCODE, most of them are issued,
      ## This might be causing problems? 
      Scrutiny_Accepted = ifelse(VOTESTATUSCODE == 'Accepted', 1, 0),
      Scrutiny_Rejected = ifelse(VOTESTATUSCODE == 'Rejected', 1, 0)),
  
  
  
  ## Group by LGA, not Date
  Postal_master_council <- Postal_Votes_master %>% 
    
    group_by(Council) %>%
    
    ## Summarise the postal voting variables
    ## It's the lag between accepted and fulfilled
    ## I.e. how many accepted postal votes have not yet been sent (fulfilled) 
    summarise(Total_applications = sum(Total_applications, na.rm = TRUE),
              PVA_Accepted       = sum(PVA_Accepted,       na.rm = TRUE),
              PVA_Rejected       = sum(PVA_Rejected,       na.rm = TRUE),
              PVA_Fulfilled      = sum(PVA_Fulfilled,      na.rm = TRUE),
              Scrutiny_Accepted  = sum(Scrutiny_Accepted,  na.rm = TRUE),
              Scrutiny_Rejected  = sum(Scrutiny_Rejected,  na.rm = TRUE)) %>%  
    mutate(Updated = now()) %>% dplyr::select(Updated, everything()),
  
  
  ## Cumulative postal votes by day to plot ----
  ## Try joining on Application ID
  ## Need to know what's fulfilled but not accepted
  ## Find Jason or Reggie's SQL that defines the variables
  ## Ask reggie about the transaction log
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
    
    rename(Date = SENDFULFILLMENTDATE,
           Accepted = ACCEPTED_count,
           Fulfilled = SENDFULFILLMENTDATE_count) %>%
    arrange(Date) %>%
    
    ## Create cumulative early vote count
    mutate(Accepted  = cumsum(Accepted),
           Fulfilled = ifelse(is.na(Fulfilled), 0, Fulfilled),
           Fulfilled = cumsum(Fulfilled)) %>%  
    
    ## Probably don't need all the variables
    gather(key   = Status, 
           value = Count, -Date) %>%
    arrange(Date) %>% mutate(Status = as.factor(Status)),
  
  
  
  
  
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
    mutate(Display = case_when(Status == "PVA_Fulfilled"     ~ 2,
                               Status == "PVA_Accepted"      ~ 1,
                               Status == "Scrutiny_Accepted" ~ 3,
                               Status == "Scrutiny_Rejected" ~ 3, TRUE ~ 4)),
  
  
  
  
  ## 5). HEADLINE NUMBERS ======================================================================
  
  
  ## DECVOTE SUMMARY ----
  ## This needs checking too
  
  ## Markoff counts ----
  ## Change to total, and filter to venue type == pre-poll
  Total_prov_markoff        <- sum(Markoff_sum_type %>% 
                                     filter(Venue_type == 'Pre-Poll') %>% 
                                     .$Total_markoffs),
  
  
  Enrol_prov_markoff        <- sum(Markoff_sum_type %>% 
                                     filter(Vote_type == 'Enrolment' & 
                                              Venue_type == 'Pre-Poll') %>% 
                                     .$Total_markoffs),
  
  
  NAMAV_election_day_markoff <- sum(Markoff_sum_type %>% 
                                      filter(Vote_type == 'NAMAV' & 
                                               Venue_type == 'Pre-Poll') %>% 
                                      .$Total_markoffs),
  
  
  ## AoBP counts ----
  Total_election_day_issued  <- sum(EMA_AoBP_summary$Total_envelopes),
  Enrol_election_day_issued  <- sum(EMA_AoBP_summary$Enrolment_envelopes),
  NAMAV_election_day_issued  <- sum(EMA_AoBP_summary$NAMAV_envelopes),
  
  
  
  ## POSTAL SUMMARY ----
  
  
  ## All Postal headlines below need revising ----
  ## Try joining on Application ID
  ## Need to know what's fulfilled, but not accepted
  ## Find Jason or Reggie's SQL that defines the variables
  ## Ask reggie about the transaction log
  Postal_headline_numbers <- Postal_master_council %>% 
    summarise_if(is.numeric, sum),
  
  
  ## Postal applications ----
  ## Count the NA rows for 'ACCEPTED' postal votea apps
  postal_total_accepted_applications <- Postal_headline_numbers %>%
    .$PVA_Accepted,
  
  
  ## Postal scrutiny -----
  ## Count the !NA rows for 'PROCESSEDDATEBY'
  ## {} escapes the pipe
  postal_total_scrutinised <- Postal_headline_numbers %>%
    {.$Scrutiny_Accepted + .$Scrutiny_Rejected}, 
  
  
  ## Postal rejected votes ----
  ## Number of scur
  postal_scrutiny_rejected <- Postal_headline_numbers %>%
    .$Scrutiny_Rejected,
  
  
  ## Postal accepted votes ----
  ## Count the !NA rows for 'CERTIFICATEACCEPTED'
  postal_scrutiny_accepted <- Postal_headline_numbers %>%
    .$Scrutiny_Accepted, 
  
  
  ## Postal votes fulfilled ----
  ## Count the !NA rows for 'SENDFULFILLMENTDATE'
  postal_total_fulfilled <- Postal_headline_numbers %>%
    .$PVA_Fulfilled, 
  
  
  ## Postal votes remaining to be fulfilled ----
  ## Count the NA rows for 'SENDFULFILLMENTDATE'
  postral_remain_fulfilled <- Postal_headline_numbers %>%
    {.$PVA_Accepted - .$PVA_Fulfilled},
  
  
  
  
  
  ## 6). CREATE PLOTS ==============================================================================
  
  
  ## Declaration Voting Graph Requirements ---- 
  
  
  # Graph 1). Total provisional markoffs over time
  # o	Single line, sum of enrolment and NAMAV
  # o	Cumulative from beginning of pre-poll to election day
  # 
  # 1. Data sourced from EMA markoffs
  # 2. Data sourced from polling place AoBP in EMA 
  
  
  ## Graph 1). Barplot of Total provisional markoffs over time ----
  ## - Bar chart for the different categories
  decvotes.barplot  <- dash_bar_chart_two_factor(df = Markoff_sum_type, 
                                                 
                                                 title     = '',
                                                 caption   = '',
                                                 
                                                 xvar      = 'Vote_type',
                                                 yvar      = 'Total_markoffs',
                                                 fill_var  = 'Venue_type',
                                                 
                                                 tsize     = 35,
                                                 capt_size = 35,
                                                 xsize     = 35,
                                                 ysize     = 35,
                                                 ycol      = 'black',
                                                 lab_size  = 10,
                                                 leg_size  = 35,
                                                 leg_pos   = 'top',
                                                 axis_size = 1.3,
                                                 h_just    = -0.5,
                                                 
                                                 ymin      = 0,
                                                 ymax      = max(Markoff_sum_type$Total_markoffs) +
                                                   (max(Markoff_sum_type$Total_markoffs)*0.1),
                                                 ylab      = 'Declaration Vote Count',
                                                 xlab      = ''),
  
  
  ## Use the same colors every time
  postal.reject.count <- stacked_bar_chart_xvar(df        = Postal_categories_overall_count,
                                                title     = '',
                                                caption   = '',
                                                
                                                xvar      = 'Display',
                                                yvar      = 'value',
                                                fill_var  = 'Status',
                                                
                                                
                                                tsize     = 35,
                                                capt_size = 35,
                                                xsize     = 35,
                                                ysize     = 35,
                                                ycol      = 'black',
                                                leg_size  = 35,
                                                axis_size = 1.7,
                                                leg_pos = 'top',
                                                
                                                ymin      = 0,
                                                ymax      = max(Postal_categories_overall_count$value)+
                                                  max(Postal_categories_overall_count$value)*0.05,
                                                ylab      = '',
                                                xlab      = ''),
  
  
  
  
  
  ## Graph 3). Line plot of total provisional markoffs over time ----
  ## - Single line, sum of enrolment and NAMAV
  ## - Cumulative from beginning of pre-poll to election day
  # 1. Data sourced from EMA markoffs
  # 2. Data sourced from polling place AoBP from hyperion staging (Election Data)
  ## Plot total provisional markoffs ----
  markoffs.cumul.linplot <- single_line_plot(df         = EMA_markoff_group_date,
                                             title      = '', 
                                             caption    = '',
                                             
                                             xvar       = 'Date',
                                             yvar       = 'Total_cum_markoff',
                                             ylast      = max(EMA_markoff_group_date$Total_cum_markoff),
                                             
                                             tsize      = 26,
                                             capt_size  = 26,
                                             xsize      = 26,
                                             ysize      = 26,
                                             line_size  = 4.5,
                                             point_size = 7,
                                             leg_pos    = 'none',
                                             
                                             lab_size   = 6,
                                             leg_size   = 35,
                                             axis_size  = 0.0,
                                             
                                             ylab       = 'Total Markoffs',
                                             xlab       = '',
                                             lab_angle  = 45),
  
  
  
  
  
  # Graph 4). Postal Fulfilment votes over time ----
  # 
  # -	Series one - cumulative total, accepted applications
  # -	Series two - cumulative total, applications fulfilled
  # -	Group by day (similar to Total EV by date plot in 'summary' early voting tab
  
  ## Add a label 
  decvote.cumul.linplot <- group_line_plot_skip_x_axes(df = Postal_fulfilment_plot,
                                                       title      = '', 
                                                       caption    = '',
                                                       
                                                       xvar       = 'Date',
                                                       yvar       = 'Count',
                                                       group_var  = 'Status',
                                                       x_int      = 2,
                                                       
                                                       tsize      = 35,
                                                       capt_size  = 35,
                                                       xsize      = 35,
                                                       ysize      = 35,
                                                       ycol       = 'black',
                                                       line_size  = 5,
                                                       point_size = 6,
                                                       leg_pos    = 'bottom',
                                                       
                                                       lab_size   = 6,
                                                       leg_size   = 35,
                                                       axis_size  = 0.7,
                                                       
                                                       ylab       = 'Postal Vote Applications',
                                                       xlab       = '',
                                                       lab_angle  = 45),
  
  
)


## 5). CREATE DEPENDENCY GRAPH ==========================================================================================


## Then create a plot of the dependencies
declaration_voting_plan_graph <- vis_drake_graph(dec_voting_plan,
                                                 main      = 'LG2021 Declaration Voting Dependency Graph',
                                                 font_size = 20)


#sankey_drake_graph(dec_voting_plan)
declaration_voting_plan_graph







####################################################################################################################
################################################# TBC ---- #########################################################
####################################################################################################################