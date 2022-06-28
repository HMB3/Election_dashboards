################################# ---- CANDIDATE NOMINATIONS DASHBAORD ---- #####################################


## This code processes the data to create the 'Candidate nominations' dashboard
message('Run R code to process Candidate Nominations data')


## To do ----
## Create one master table that has everything - converted noms, date, council, etc


## Set variables
headline_size = 24
time_stamp    = gsub("-", "_", now()) %>% 
  gsub(" ", "_", .)                   %>% 
  gsub(":", "",  .)


## Create a switch for the whole data set
status_length = length(unique(noms_extract$NOMINATIONSTATUS))
date_length   = noms_extract$DATEOFLODGEMENT %>% str_sub(start = 1, 10) 
date_length   = subset(date_length, date_length != '') %>% unique() %>% length()


## Function to make empty character strings NA
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}


##
if(class(event_group_ID) != "NULL" & date_length > 1) {
  
  message('Process the Noms data, the endpoint is populated')
  ## Add a Switch if Noms data incomplete
  # noms_extract$DATEOFLODGEMENT[!is.na(noms_extract$DATEOFLODGEMENT)] <- NA
  status_length = length(unique(noms_extract$NOMINATIONSTATUS))
  date_length   = noms_extract$DATEOFLODGEMENT %>% str_sub(start = 1, 10) 
  date_length   = subset(date_length, date_length != '') %>% unique() %>% length()
  
  
  ## Display categories from requirements
  # Incoming   (Incomplete, ready to lodge)
  # Processing (Lodged, Checked, Supervisor Review)
  # Pending (Pending, Reviewed)
  # Rejected
  # Withdrawn
  # Accepted
  noms_display <- c('Incoming', 
                    'Processing',
                    'Pending', 
                    'Rejected', 
                    'Withdrawn',             
                    'Accepted')
  
  
  
  
  
  ## 1). Data preparation =======================================================
  
  
  ## Count Available positions by council
  Contest_positions <- EMA_contests %>%
    
    ## Select the number of positions by contest
    filter(CONTESTTYPECODE   == 'General Ward' | 
             CONTESTTYPECODE == 'General LGA' |
             CONTESTTYPECODE == 'General Mayoralty') %>%
    
    dplyr::select(CONTESTID,
                  Council        = AREACODE,
                  `Contest Area` = CONTESTAREACODE,
                  Contest        = CONTESTTYPECODE,
                  `Available positions` = NUMBEROFPOSITIONSCONTESTED) %>%
    
    mutate(Contest = case_when(Contest == 'General Ward'      ~ 'Councillor', 
                               Contest == 'General LGA'       ~ 'Councillor',
                               Contest == 'General Mayoralty' ~ 'Mayor'))
  
  
  
  
  
  ## Modify NOMs data table depending on whether nominations have been lodged or not
  if(date_length > 3) {
    
    message('Use Noms data with dates and INCOMPLETE Status')
    noms_extract_date <- noms_extract %>% 
      
      ## Convert the date field in Noms extract from characer to date
      ## This might be a problem if the data field is partially complete - e.g. some dates filled in, some not
      mutate(DATEOFLODGEMENT = gsub('AEST', '', DATEOFLODGEMENT))                    %>% 
      separate(., col        = DATEOFLODGEMENT, into = c('Date', 'Time'), sep = ' ') %>%
      mutate(Date            = empty_as_na(Date))        %>% 
      mutate(Date            = as.Date(Date))            %>% 
      mutate(weekday         = wday(Date, label = TRUE)) %>% 
      dplyr::rename(Status   = NOMINATIONSTATUS)
    
    
  } else {
    
    message('Use Noms data without dates and INCOMPLETE Status')
    noms_extract_date <- noms_extract %>% 
      dplyr::rename(Status = NOMINATIONSTATUS)
    
  }
  
  
  ## Add areas to the contest data
  noms_extract_areacode <- noms_extract_date %>% left_join(., dplyr::select(EMA_contests,
                                                                            AREACODE,
                                                                            CONTESTID,
                                                                            CONTESTAREACODE),
                                                           by = c("CONTESTID"))
  ## write_csv(noms_extract_areacode, './02 Source data/Nominations/Noms/noms_extract_areacode.csv')
  
  
  
  
  
  ## 2). Nominations Calculation =======================================================
  
  
  ## Convert nomination status to display category
  converted_nomination_status <- noms_extract_date %>% 
    
    ## Do the mutate first
    mutate(Status = str_to_title(Status),
           Status = case_when(
             
             ## These are Ambers Display categories
             grepl("Ready To Lodge",    Status, ignore.case = TRUE) ~ "Incoming",
             grepl("Incomplete",        Status, ignore.case = TRUE) ~ "Incoming",
             
             grepl("Checked",           Status, ignore.case = TRUE) ~ "Processing",
             grepl("Lodged",            Status, ignore.case = TRUE) ~ "Processing",
             grepl("Supervisor Review", Status, ignore.case = TRUE) ~ "Processing",
             
             grepl("Pending",           Status, ignore.case = TRUE) ~ "Pending",
             grepl("Reviewed",          Status, ignore.case = TRUE) ~ "Accepted",
             grepl("Accepted",          Status, ignore.case = TRUE) ~ "Accepted",
             
             grepl("Rejected",          Status, ignore.case = TRUE) ~ "Rejected",
             grepl("Withdrawn",         Status, ignore.case = TRUE) ~ "Withdrawn"))
  
  
  
  
  
  ## Aggregate Confirmed Councillor Nominations counts by council 
  Confirmed_Council_Noms <- noms_extract_areacode %>%
    
    ## Just count the fully accepted nominations
    filter(Status  == "ACCEPTED" | Status  == "REVIEWED") %>% 
    
    group_by(CONTESTID, CONTESTAREACODE, AREACODE, CONTESTTYPECODE) %>%
    summarise(`Accepted Nominations` = n())   %>% 
    ungroup() %>%
    
    ## Calculate the shortfall of nominations by council 
    right_join(., Contest_positions, by = "CONTESTID") %>% 
    
    mutate(`Accepted Nominations` = ifelse(is.na(`Accepted Nominations`), 0, `Accepted Nominations`),
           Shortfall              = `Available positions` - `Accepted Nominations`,
           Shortfall              = ifelse(Shortfall <= 0, 0, Shortfall),
           `Contest Area`         = str_wrap(`Contest Area`, 40),
           `Contest Area`         = gsub("\\ - .*","", `Contest Area`),
           Uncontested            = ifelse(`Available positions` >= `Accepted Nominations`,
                                           TRUE, FALSE)) %>% 
    
    dplyr::select(`Contest Area`, 
                  `Contest Type` = Contest, 
                  `Accepted Nominations`, 
                  `Available positions`, 
                  Shortfall, Uncontested)
  
  
  
  
  
  ## Aggregate Confirmed Councillor Nominations counts by council 
  All_Council_Noms <- noms_extract_areacode %>%
    
    ## Just count the fully accepted nominations
    # filter(Status  == "ACCEPTED" | Status  == "REVIEWED") %>% 
    
    group_by(CONTESTID, CONTESTAREACODE, AREACODE, CONTESTTYPECODE) %>%
    summarise(`Accepted Nominations` = n())                         %>% 
    ungroup()                                                       %>%
    
    ## Calculate the shortfall of nominations by council 
    right_join(., Contest_positions, by = "CONTESTID") %>% 
    
    mutate(`Accepted Nominations` = ifelse(is.na(`Accepted Nominations`), 0, `Accepted Nominations`),
           Shortfall              = `Available positions` - `Accepted Nominations`,
           Shortfall              = ifelse(Shortfall <= 0, 0, Shortfall),
           `Contest Area`         = str_wrap(`Contest Area`, 40),
           `Contest Area`         = gsub("\\ - .*","", `Contest Area`),
           Uncontested            = ifelse(`Available positions` >= `Accepted Nominations`,
                                           TRUE, FALSE)) %>% 
    
    dplyr::select(`Contest Area`, 
                  `Contest Type` = Contest, 
                  `Accepted Nominations`, 
                  `Available positions`, 
                  Shortfall, Uncontested)
  
  
  
  
  
  ## Nomination counts by type ----
  
  
  ## All Nominations + groups for Registered Political Parties 
  ## Registered party nominations
  Complete_Nominations <- noms_extract_areacode %>%
    
    ## Just count the accepted nominations
    filter(Status != "INCOMPLETE") %>% 
    group_by(CONTESTTYPECODE, Status, METHODOFNOMINATION, PAYMENTMETHOD) %>%
    summarise(Nominations = n()) %>% 
    
    ## Rename
    dplyr::rename(Contest             = CONTESTTYPECODE,
                  Status              = Status,
                  `Nomination Method` = METHODOFNOMINATION,
                  `Payment Method`    = PAYMENTMETHOD) %>%
    
    dplyr::select(`Nomination Method`, Contest, Status, `Payment Method`, Nominations) %>% 
    as.data.frame()
  
  
  
  ## All Nominations + groups for Registered Political Parties 
  ## Registered party nominations
  ## Make a list of those that are zero
  Party_Nominations <- noms_extract %>%
    
    ## Just count the accepted nominations
    filter(CONTESTTYPECODE == 'COUNCILLOR' & (NOMINATIONSTATUS == "ACCEPTED"| NOMINATIONSTATUS  == "REVIEWED"))  %>% 
    group_by(RPPNAME) %>%
    summarise(`Accepted Nominations` = n()) %>% 
    
    ## Join on EMA Registered 
    # using 'noms_party' from endpoint causes errors because the case is different and cannot join
    right_join(EMA_Eligible_Parties,
               by = 'RPPNAME') %>%
    
    mutate(`Accepted Nominations` = ifelse(is.na(`Accepted Nominations`), 0, `Accepted Nominations`)) %>% 
    dplyr::rename(`Registered Political Party` = RPPNAME) %>%
    arrange(`Registered Political Party`)
  
  
  
  
  
  ## Overall Nominations counts
  ## This table has counts for all the categories of nominations, but excludes the party name
  ## Use this table to create bar-plots of each category
  overall_incomplete_nominations <- converted_nomination_status %>%  
    
    ## Group by contest type, Nomination Status, and contest ID
    dplyr::rename(Status = Status) %>% 
    group_by(Status) %>%
    
    ## Then count up the nominations, and make status a factor. 
    ## We need this, so we can plot different statuses at different times
    ## Then count up the nominations 
    summarise(Nominations   = n()) %>%  
    mutate(Status = factor(Status, 
                           levels = noms_display),
           Type = 'Overall Nominations by status') %>% 
    dplyr::select(Type, everything()) %>% 
    na.omit()
  
  
  
  
  
  ## Group nominations
  group_nominations <- converted_nomination_status %>% 
    
    ## Group by contest type, Nomination Status, and contest ID
    ## There is currently 36 groups in total (including all status categories),
    group_by(GROUPEXTERNALIDENTIFIER, Status)   %>%
    
    ## Then count up the nominations 
    summarise(Nominations   = n()) %>%  
    mutate(Status = factor(Status, 
                           levels = noms_display),
           Type = 'Groups') %>% 
    dplyr::select(Type, -Nominations, everything()) %>%
    group_by(Status) %>% 
    summarise(Nominations = n()) %>% 
    
    ## Mutate
    mutate(Type = 'Groups') %>% 
    na.omit()
  
  
  ## Mayor nominations
  mayor_nominations <- converted_nomination_status %>% 
    
    ## Group by contest type, Nomination Status, and contest ID
    filter(CONTESTTYPECODE == 'MAYOR') %>% 
    group_by(Status) %>%
    
    ## Then count up the nominations 
    summarise(Nominations   = n()) %>%  
    mutate(Status = factor(Status, 
                           levels = noms_display),
           Type = 'Mayor') %>% 
    dplyr::select(Type, everything()) %>% 
    na.omit()
  
  
  ## Council nominations
  councillor_nominations <- converted_nomination_status %>% 
    
    ## Group by contest type, Nomination Status, and contest ID
    filter(CONTESTTYPECODE == 'COUNCILLOR') %>% 
    group_by(Status) %>%
    
    ## Then count up the nominations 
    summarise(Nominations   = n()) %>%  
    mutate(Status = factor(Status, 
                           levels = noms_display),
           Type = 'Councillor') %>% 
    dplyr::select(Type, everything()) %>% 
    na.omit()
  
  
  ## Combined status counts
  combined_status_counts <- bind_rows(overall_incomplete_nominations, 
                                      councillor_nominations, 
                                      mayor_nominations, 
                                      group_nominations)
  
  
  
  
  
  ## Lodged nominations by date ----
  
  
  ## Overall count for Mayor/Council
  if(date_length > 3) {
    
    message('Use Noms data with dates')
    council_mayor_count_date <- noms_extract_date %>%
      
      ## 
      filter(Status != "INCOMPLETE")    %>% 
      group_by(CONTESTTYPECODE, Date)   %>%
      summarise(Nominations   = n())    %>% 
      mutate(Noms_cum_count   = cumsum(Nominations)) %>% 
      mutate(Date = as.Date(Date, format = "%b-%d")) %>% 
      na.omit()
    
  } else {
    message('Use Noms data without dates and INCOMPLETE Status')
    council_mayor_count_date <- noms_extract_date %>%
      
      ## 
      group_by(CONTESTTYPECODE) %>%
      summarise(Nominations   = n())    %>% 
      mutate(Noms_cum_count   = cumsum(Nominations)) %>% 
      na.omit()
  }
  
  
  
  
  
  ## 3). Headline Numbers ==========================================================
  
  
  ## Payment Method
  # 1.	Credit card = Online
  # 2.	All other   = Paper
  # 3.	Empty("")   = Undefined
  
  
  ## Check that these still work when there are no dates
  
  
  ## Total Paper Applications 
  Total_Paper_Applications <- Complete_Nominations %>% 
    filter(`Payment Method` == "Bank Cheque" | `Payment Method` == "Cash") %>% 
    .$Nominations %>% 
    sum()
  
  
  ## Total Online Applications
  Total_Online_Applications <- Complete_Nominations %>% 
    filter(`Payment Method` == "Credit Card") %>% 
    .$Nominations %>% 
    sum()
  
  
  ## Total Unpaid Applications 
  ## [10:42 AM] Andrew Hall
  ## means they have lodged their nomination without any payment. 
  ## so any that are lodged, checked, pending, supervisor review, approved etc and have no payment 
  Total_Nonfinancial_Applications <- Complete_Nominations %>% 
    filter(`Payment Method` == "No Payment") %>% 
    .$Nominations %>% 
    sum()
  
  
  ## Total Applications - sum of the nominations, rather than nrow() of the endpoint
  Total_Complete_nominations <- sum(Complete_Nominations$Nominations)
  
  
  ## % Online Applications
  ## The denominator here is the paper applications, maybe it should be the
  ## total applications
  Percent_online_applications <- paste0(round(Total_Online_Applications/
                                                Total_Complete_nominations 
                                              * 100, digits = 1), "%")
  
  
  ## Nominations by Registered Political Party 
  Nom_by_RPP <- Complete_Nominations %>% 
    filter(`Nomination Method` == 'RPP') %>% 
    .$Nominations %>% 
    sum()
  
  
  Percent_by_RPP <- paste0(round(Nom_by_RPP/
                                   Total_Complete_nominations
                                 * 100, digits = 1), "%")
  
  
  ## Nominations by two Electors
  ## Does this mean 
  Nom_by_electors <- Complete_Nominations %>% 
    filter(`Nomination Method` == 'ELECTORS') %>% 
    .$Nominations %>% 
    sum()
  
  
  Percent_by_electors <- paste0(round(Nom_by_electors/
                                        Total_Complete_nominations
                                      * 100, digits = 1), "%")
  
  
  
  ## Uncontested contests ----
  
  ## Current total states 33 uncontested, I would say there's 35 uncontested, 
  ## being 2 with the ballot paper draw completed and uncontested and 33 where the ballot paper draw hasnt taken place yet. 
  #  could we look at this further, im just not sure how we are calculting this. 
  #  my numbers for councillor and mayor as per below: 
  #  councillor 35 
  #  mayor 4 
  
  
  ## I don't know how to check where the ballot paper draw has or hasnt taken place...
  uncontested_counts <- Confirmed_Council_Noms %>%
    mutate(Uncontested = ifelse(`Available positions` >= `Accepted Nominations`,
                                TRUE, FALSE)) %>%
    group_by(`Contest Type`) %>%
    summarise(UncontestedCount = sum(Uncontested))
  
  
  Uncontested_Mayoral_Elections <- uncontested_counts$UncontestedCount[uncontested_counts$`Contest Type` == 'Mayor']
  Uncontested_Council_Elections <- uncontested_counts$UncontestedCount[uncontested_counts$`Contest Type` == 'Councillor']
  
  
  ## Number of RPPs with !No Coucillor Nominations
  RPP_no_council_noms <- Party_Nominations %>% 
    filter(`Accepted Nominations` == 0)    %>%
    .$`Registered Political Party`         %>% 
    length()
  
  
  
  
  
  ## 4). Headline Flextable ==========================================================
  
  
  ## Create flex table for Nominations headline
  ## Names for values:
  NomsHeadlineTable_tibb <- tibble(Names =
                                     
                                     ## Create headline numbers 
                                     c("Lodged Nominations by online vs paper",
                                       "Paper nominations",
                                       "Online nominations",
                                       "Non-financial nominations",
                                       "Total nominations",
                                       "% Online nominations",
                                       "Lodged nominations by Method",
                                       "By registered Political Party",
                                       "By electors",
                                       "Uncontested Elections (by contest)",
                                       "Councillor",
                                       "Mayoral"),
                                   
                                   ## Enter headline values
                                   Values =
                                     
                                     c("Count",
                                       Total_Paper_Applications,
                                       Total_Online_Applications,
                                       Total_Nonfinancial_Applications,
                                       Total_Complete_nominations,
                                       Percent_online_applications,
                                       "",
                                       paste0(Nom_by_RPP,      ' / ', Percent_by_RPP),
                                       paste0(Nom_by_electors, ' / ', Percent_by_electors),
                                       "",
                                       Uncontested_Council_Elections,
                                       Uncontested_Mayoral_Elections))
  
  
  NomsHeadlineTable_flex <- NomsHeadlineTable_tibb %>% 
    
    ## Turn into flextable
    flextable() %>% 
    
    ## Set background formatting for flext table
    bg(bg = "coral",       i=~ Values != "Count")                                  %>%
    bg(bg = "deepskyblue", i=~ str_detect(Names, "By registered Political Party")) %>%
    bg(bg = "deepskyblue", i=~ str_detect(Names, "By electors"))                   %>%
    bg(bg = "deepskyblue", i=~ str_detect(Names, "Councillor"))                    %>%
    bg(bg = "deepskyblue", i=~ str_detect(Names, "Mayoral"))                       %>%
    bg(bg = "black",       i=~ Values == "Count")                                  %>%
    bg(bg = "black",       i=~ Values == "")                                       %>%
    
    ## Fontsize, color and bold 
    fontsize(size = headline_size)                      %>%
    bold(i = 1,  j = NULL, bold = TRUE, part = "body")  %>%
    bold(i = 7,  j = NULL, bold = TRUE, part = "body")  %>%
    bold(i = 10, j = NULL, bold = TRUE, part = "body")  %>%
    
    ## Set the colour and width
    color(color = "white", i=~ Values == "Count") %>%
    color(color = "white", part = "all")          %>% 
    
    width(j =~ Names,  width = 600) %>%
    width(j =~ Values, width = 100) %>%
    delete_part(part = "header")    %>%
    border(border = fp_border(color = "white", width = 3)) %>% 
    height_all(., height = 10)
  
} else {
  message('Do not process Noms data, the endpoint is NULL, create blank headline table')
  ## Display categories from requirements
  # Incoming   (Incomplete, ready to lodge)
  # Processing (Lodged, Checked, Supervisor Review)
  # Pending (Pending, Reviewed)
  # Rejected
  # Withdrawn
  # Accepted
  noms_display <- c('Incoming', 
                    'Processing',
                    'Pending', 
                    'Rejected', 
                    'Withdrawn',             
                    'Accepted')
  
  
  
  
  
  ## 5). Data preparation =======================================================
  
  
  ## Count Available positions by council
  Contest_positions <- EMA_contests %>%
    
    ## Select the number of positions by contest
    filter(CONTESTTYPECODE   == 'General Ward' | 
             CONTESTTYPECODE == 'General LGA' |
             CONTESTTYPECODE == 'General Mayoralty') %>%
    
    dplyr::select(CONTESTID,
                  Council        = AREACODE,
                  `Contest Area` = CONTESTAREACODE,
                  Contest        = CONTESTTYPECODE,
                  `Available positions` = NUMBEROFPOSITIONSCONTESTED) %>%
    
    mutate(Contest = case_when(Contest == 'General Ward'      ~ 'Councillor', 
                               Contest == 'General LGA'       ~ 'Councillor',
                               Contest == 'General Mayoralty' ~ 'Mayor'))
  
  
  
  
  
  ## Modify NOMs data table depending on whether nominations have been lodged or not
  if(date_length > 3) {
    
    message('Use Noms data without dates and INCOMPLETE Status')
    noms_extract_date <- noms_extract %>% 
      
      ## Convert the date field in Noms extract from characer to date
      mutate(DATEOFLODGEMENT = gsub('AEST', '', DATEOFLODGEMENT))                    %>% 
      separate(., col        = DATEOFLODGEMENT, into = c('Date', 'Time'), sep = ' ') %>% 
      mutate(Date            = as.Date(Date))            %>% 
      mutate(weekday         = wday(Date, label = TRUE)) %>% 
      dplyr::rename(Status   = NOMINATIONSTATUS)
    
    
  } else {
    
    message('Use Noms data without dates and INCOMPLETE Status')
    noms_extract_date <- noms_extract %>% 
      dplyr::rename(Status          = NOMINATIONSTATUS)
    
  }
  
  
  ## Add areas to the contest data
  noms_extract_areacode <- noms_extract_date %>% left_join(., dplyr::select(EMA_contests,
                                                                            AREACODE,
                                                                            CONTESTID,
                                                                            CONTESTAREACODE),
                                                           by = c("CONTESTID"))

  
  
  
  ## 6). Nominations Calculation =======================================================
  
  
  ## Convert nomination status to display category
  converted_nomination_status <- noms_extract_date %>% 
    
    ## Do the mutate first
    mutate(Status = str_to_title(Status),
           Status = case_when(
             
             ## These are Ambers Display categories
             grepl("Ready To Lodge",    Status, ignore.case = TRUE) ~ "Incoming",
             grepl("Incomplete",        Status, ignore.case = TRUE) ~ "Incoming",
             
             grepl("Checked",           Status, ignore.case = TRUE) ~ "Processing",
             grepl("Lodged",            Status, ignore.case = TRUE) ~ "Processing",
             grepl("Supervisor Review", Status, ignore.case = TRUE) ~ "Processing",
             
             grepl("Pending",           Status, ignore.case = TRUE) ~ "Pending",
             grepl("Reviewed",          Status, ignore.case = TRUE) ~ "Accepted",
             grepl("Accepted",          Status, ignore.case = TRUE) ~ "Accepted",
             
             grepl("Rejected",          Status, ignore.case = TRUE) ~ "Rejected",
             grepl("Withdrawn",         Status, ignore.case = TRUE) ~ "Withdrawn"))
  
  
  
  
  
  ## Aggregate Confirmed Councillor Nominations counts by council 
  Confirmed_Council_Noms <- noms_extract_areacode %>%
    
    ## Just count the fully accepted nominations
    filter(Status  == "ACCEPTED" | Status  == "REVIEWED") %>% 
    
    group_by(CONTESTID, CONTESTAREACODE, AREACODE, CONTESTTYPECODE) %>%
    summarise(`Accepted Nominations` = n())   %>% 
    ungroup() %>%
    
    ## Calculate the shortfall of nominations by council 
    right_join(., Contest_positions, by = "CONTESTID") %>% 
    
    mutate(`Accepted Nominations` = ifelse(is.na(`Accepted Nominations`), 0, `Accepted Nominations`),
           Shortfall              = `Available positions` - `Accepted Nominations`,
           Shortfall              = ifelse(Shortfall <= 0, 0, Shortfall),
           `Contest Area`         = str_wrap(`Contest Area`, 40),
           `Contest Area`         = gsub("\\ - .*","", `Contest Area`),
           Uncontested            = ifelse(`Available positions` >= `Accepted Nominations`,
                                           TRUE, FALSE)) %>% 
    
    dplyr::select(`Contest Area`, 
                  `Contest Type` = Contest, 
                  `Accepted Nominations`, 
                  `Available positions`, 
                  Shortfall, Uncontested)
  
  
  
  
  
  ## Aggregate Confirmed Councillor Nominations counts by council 
  All_Council_Noms <- noms_extract_areacode %>%
    
    ## Just count the fully accepted nominations
    # filter(Status  == "ACCEPTED" | Status  == "REVIEWED") %>% 
    
    group_by(CONTESTID, CONTESTAREACODE, AREACODE, CONTESTTYPECODE) %>%
    summarise(`Accepted Nominations` = n())                         %>% 
    ungroup()                                                       %>%
    
    ## Calculate the shortfall of nominations by council 
    right_join(., Contest_positions, by = "CONTESTID") %>% 
    
    mutate(`Accepted Nominations` = ifelse(is.na(`Accepted Nominations`), 0, `Accepted Nominations`),
           Shortfall              = `Available positions` - `Accepted Nominations`,
           Shortfall              = ifelse(Shortfall <= 0, 0, Shortfall),
           `Contest Area`         = str_wrap(`Contest Area`, 40),
           `Contest Area`         = gsub("\\ - .*","", `Contest Area`),
           Uncontested            = ifelse(`Available positions` >= `Accepted Nominations`,
                                           TRUE, FALSE)) %>% 
    
    dplyr::select(`Contest Area`, 
                  `Contest Type` = Contest, 
                  `Accepted Nominations`, 
                  `Available positions`, 
                  Shortfall, Uncontested)
  
  
  
  
  
  ## Nomination counts by type ----
  
  
  ## All Nominations + groups for Registered Political Parties 
  ## Registered party nominations
  Complete_Nominations <- noms_extract_areacode %>%
    
    ## Just count the accepted nominations
    filter(Status != "INCOMPLETE") %>% 
    group_by(CONTESTTYPECODE, Status, METHODOFNOMINATION, PAYMENTMETHOD) %>%
    summarise(Nominations = n()) %>% 
    
    ## Rename
    dplyr::rename(Contest             = CONTESTTYPECODE,
                  Status              = Status,
                  `Nomination Method` = METHODOFNOMINATION,
                  `Payment Method`    = PAYMENTMETHOD) %>%
    
    dplyr::select(`Nomination Method`, Contest, Status, `Payment Method`, Nominations) %>% 
    as.data.frame()
  
  
  
  ## All Nominations + groups for Registered Political Parties 
  ## Registered party nominations
  ## Make a list of those that are zero
  Party_Nominations <- noms_extract %>%
    
    ## Just count the accepted nominations
    filter(CONTESTTYPECODE == 'COUNCILLOR' & (NOMINATIONSTATUS == "ACCEPTED"| NOMINATIONSTATUS  == "REVIEWED"))  %>% 
    group_by(RPPNAME) %>%
    summarise(`Accepted Nominations` = n()) %>% 
    
    ## Join on EMA Registered 
    ## using 'noms_party' from endpoint causes errors because the case is different and cannot join
    
    right_join(EMA_Eligible_Parties, 
               by = 'RPPNAME') %>% 
    
    mutate(`Accepted Nominations` = ifelse(is.na(`Accepted Nominations`), 0, `Accepted Nominations`)) %>% 
    dplyr::rename(`Registered Political Party` = RPPNAME) %>%
    arrange(`Registered Political Party`)
  
  
  
  
  
  ## Overall Nominations counts
  ## This table has counts for all the categories of nominations, but excludes the party name
  ## Use this table to create bar-plots of each category
  overall_incomplete_nominations <- converted_nomination_status %>%  
    
    ## Group by contest type, Nomination Status, and contest ID
    # dplyr::rename(Status = Status) %>% 
    group_by(Status) %>%
    
    ## Then count up the nominations, and make status a factor. 
    ## We need this, so we can plot different statuses at different times
    ## Then count up the nominations 
    summarise(Nominations   = n()) %>%  
    mutate(Status = factor(Status, 
                           levels = noms_display),
           Type = 'Overall Nominations by status') %>% 
    dplyr::select(Type, everything()) %>% 
    na.omit()
  
  
  
  
  
  ## Group nominations
  group_nominations <- converted_nomination_status %>% 
    
    ## Group by contest type, Nomination Status, and contest ID
    ## There is currently 36 groups in total (including all status categories),
    group_by(GROUPEXTERNALIDENTIFIER, Status)   %>%
    
    ## Then count up the nominations 
    summarise(Nominations   = n()) %>%  
    mutate(Status = factor(Status, 
                           levels = noms_display),
           Type = 'Groups') %>% 
    dplyr::select(Type, -Nominations, everything()) %>%
    group_by(Status) %>% 
    summarise(Nominations = n()) %>% 
    
    ## Mutate
    mutate(Type = 'Groups') %>% 
    na.omit()
  
  
  ## Mayor nominations
  mayor_nominations <- converted_nomination_status %>% 
    
    ## Group by contest type, Nomination Status, and contest ID
    filter(CONTESTTYPECODE == 'MAYOR') %>% 
    group_by(Status) %>%
    
    ## Then count up the nominations 
    summarise(Nominations   = n()) %>%  
    mutate(Status = factor(Status, 
                           levels = noms_display),
           Type = 'Mayor') %>% 
    dplyr::select(Type, everything()) %>% 
    na.omit()
  
  
  ## Council nominations
  councillor_nominations <- converted_nomination_status %>% 
    
    ## Group by contest type, Nomination Status, and contest ID
    filter(CONTESTTYPECODE == 'COUNCILLOR') %>% 
    group_by(Status) %>%
    
    ## Then count up the nominations 
    summarise(Nominations   = n()) %>%  
    mutate(Status = factor(Status, 
                           levels = noms_display),
           Type = 'Councillor') %>% 
    dplyr::select(Type, everything()) %>% 
    na.omit()
  
  
  ## Combined status counts
  combined_status_counts <- bind_rows(overall_incomplete_nominations, 
                                      councillor_nominations, 
                                      mayor_nominations, 
                                      group_nominations)
  
  
  
  
  
  ## Lodged nominations by date ----
  
  
  ## Overall count for Mayor/Council
  if(date_length > 3) {
    
    message('Use Noms data with dates')
    council_mayor_count_date <- noms_extract_date %>%
      
      ## 
      filter(Status != "INCOMPLETE")   %>% 
      group_by(CONTESTTYPECODE, Date)  %>%
      summarise(Nominations   = n())   %>% 
      mutate(Noms_cum_count   = cumsum(Nominations)) %>% 
      mutate(Date = as.Date(Date, format = "%b-%d")) %>% 
      na.omit()
    
  } else {
    message('Use Noms data without dates and INCOMPLETE Status')
    council_mayor_count_date <- noms_extract_date %>%
      
      ## 
      group_by(CONTESTTYPECODE) %>%
      summarise(Nominations   = n())    %>% 
      mutate(Noms_cum_count   = cumsum(Nominations)) %>% 
      na.omit()
  }
  
  ## Show the headline table
  NomsHeadlineTable <- tibble(Names =
                                
                                ## Create headline numbers 
                                c("Lodged Nominations by online vs paper",
                                  "Paper nominations",
                                  "Online nominations",
                                  "Non-financial nominations",
                                  "Total nominations",
                                  "% Online nominations",
                                  "Lodged nominations by Method",
                                  "By registered Political Party",
                                  "By electors",
                                  "Uncontested Elections (by contest)",
                                  "Councillor",
                                  "Mayoral"),
                              
                              ## Enter headline values
                              Values =
                                
                                c("Count",
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  "",
                                  paste0(0, ' / 0%'),
                                  paste0(0, ' / 0%'),
                                  "",
                                  0,
                                  0)) %>% 
    
    ## Turn into flextable
    flextable() %>% 
    
    ## Set background formatting for flext table
    bg(bg = "coral",       i=~ Values != "Count")                                  %>%
    bg(bg = "deepskyblue", i=~ str_detect(Names, "By registered Political Party")) %>%
    bg(bg = "deepskyblue", i=~ str_detect(Names, "By electors"))                   %>%
    bg(bg = "deepskyblue", i=~ str_detect(Names, "Councillor"))                    %>%
    bg(bg = "deepskyblue", i=~ str_detect(Names, "Mayoral"))                       %>%
    bg(bg = "black",       i=~ Values == "Count")                                  %>%
    bg(bg = "black",       i=~ Values == "")                                       %>%
    
    ## Fontsize, color and bold 
    fontsize(size = headline_size)                      %>%
    bold(i = 1,  j = NULL, bold = TRUE, part = "body")  %>%
    bold(i = 7,  j = NULL, bold = TRUE, part = "body")  %>%
    bold(i = 10, j = NULL, bold = TRUE, part = "body")  %>%
    
    ## Set the colour and width
    color(color = "white", i=~ Values == "Count") %>%
    color(color = "white", part = "all")          %>% 
    
    width(j =~ Names,  width = 600) %>%
    width(j =~ Values, width = 100) %>%
    delete_part(part = "header")    %>%
    border(border = fp_border(color = "white", width = 3)) %>% 
    height_all(., height = 10)
  
}





## 7). Nominations Calculation =======================================================


## Check the statuses
file_list <- c('NomsHeadlineTable_tibb',
               'converted_nomination_status', 
               'Contest_positions',
               'Party_Nominations',
               'combined_status_counts')


## Add data to workbook
Noms_workbook <- createWorkbook()

## file = file_list[1]
for(file in file_list) {
  
  ## Get required columns.
  File_to_Write <- get(file)
  
  ## Get the columns we want
  # select(all_of(Required_Cols))
  
  ## Save the CSV
  # write_excel_csv(File_to_Write,
  #                 paste0(BT_server_data, file, '.csv'))
  
  ## Add worksheet to the spread sheet
  message('writing ', file,  ' to check ballot track data')
  addWorksheet(Noms_workbook, file)
  
  ## Write the data to the corresponding worksheet
  writeDataTable(wb       = Noms_workbook, 
                 sheet    = file,
                 x        = get(file),
                 startCol = 1, 
                 startRow = 1, 
                 rowNames = FALSE,
                 tableStyle = "TableStyleMedium2")
  
}

## Save the whole workbook
saveWorkbook(Noms_workbook, 
             paste0(Noms_server_data, "Noms_Dashboard_LG2101_data.xlsx"),
             overwrite = TRUE)



# Nomination status notes -------------------------------------------------



# Andrew Hall says : Lodged is a status in the system, when a user makes payment and submit the status is 
# 'lodged' it means it has not yet been actioned by a reviewer yet  it means it has not 
# yet been actioned by a reviewer yet


# A quick summary of the status's are:

# -	Lodged (lodged by candidate, ready for Round 1 checker to review)
# 
# -	Checked (ready for round 2 review of nomination)
# 	
# -	Supervisor review (supervisor to review and approve, reject or send back to candidate)
# 
# -	Pending (issue found and candidate must login and fix)
# 	
# -	Reviewed/Approved (essentially the same, just depends if we have finalised the rolls)
# 
# -	 Rejected -( unfixable issue found and nom rejected)

