####################################################################################################################
###################################### CANDIDATE NOMINATIONS DASHBAORD ---- ########################################
####################################################################################################################


## This code processes the data to create the 'Candidate nominations' dashboard
message('Create a drake plan of analysing the Nominations data')




## 1). KEY FIELDS ================================================================================


## Create the drake plan 
nominations_plan <- drake_plan(
  
  ## Main nominations fields ----
  noms_fields = c('ELECTIONEVENTID',
                  #'WARDNAME',
                  #'CONTESTAREACODE',
                  'CONTESTID',
                  'CONTESTTYPECODE',
                  'NUMBEROFPOSITIONSCONTESTED',
                  'UNIQUENOMINATIONIDENTIFIER',
                  'NOMINATIONSTATUS',
                  'GROUPEXTERNALIDENTIFIER',
                  'GROUPNAME',
                  'GROUPLABEL',
                  'GROUPSTATUS',
                  'METHODOFNOMINATION',
                  'RPPNAME'),
  
  
  ## Subset zoho data
  zoho_subset = zoho_extract %>% 
    dplyr::select(noms_fields),
  
  
  ## Aggregated nominations fields ----
  noms_aggr    = c(#'CONTESTAREACODE', 
    'NOMINATIONSTATUS', 
    'CONTESTTYPECODE', 'GROUPSTATUS'),
  noms_display = c('Contest', 'Incoming', 'Processing', 'Rejected', 'Withdrawn', 'Accepted'),
  
  
  ## Intersect the list of RPP's from EMA with list provided by nominations.
  ## The sample data doesn't yet have a proper list of candidates - what should this list look like?
  ## The larger sample data from Fabian might have a better match
  RPP_LG_list  = Registered_Parties %>% 
    filter(PARTYSTATUSCODE != "Deregistered" & PARTYSTATUSCODE != "Expired") %>%
    .$PARTYNAME %>% 
    intersect(., zoho_extract$RPPNAME),
  
  
  ## 2). AGGREGATE NOMINATIONS COUNTS ================================================================================
  
  
  ## Nominations for Registered Political Parties
  Nom_Party_format = Nom_Party_report %>%
    dplyr::select(`Nomination_by_RPP/Electors`, Number_of_Mayoral_Candidates, Number_of_Councillor_Candidates) %>%
    rename(`Registered Political Parties Nominations` = `Nomination_by_RPP/Electors`,
           `Number of Mayoral Candidates`    = Number_of_Mayoral_Candidates,
           `Number of Councillor Candidates` = Number_of_Councillor_Candidates),
  
  
  ## Nominations for Registered Political Parties ----
  ## Registered party nominations
  Registered_party_nominations = zoho_extract %>%
    
    ## Only NUMBEROFPOSITIONSCONTESTED is numeric
    group_by(RPPNAME) %>%
    summarize_if(., is.numeric, sum) %>%
    
    ## Rename
    rename(`Registered Political Party` = RPPNAME,
           `Number of Candidates` = NUMBEROFPOSITIONSCONTESTED) %>% 
    as.data.frame(),
  
  
  ## Nominations by electors
  Elector_nominations = zoho_extract %>%
    
    ## Only NUMBEROFPOSITIONSCONTESTED is numeric
    group_by(METHODOFNOMINATION) %>%
    summarize_if(., is.numeric, sum) %>%
    
    ## Rename
    rename(`Nomination Method`    = METHODOFNOMINATION,
           `Number of Candidates` = NUMBEROFPOSITIONSCONTESTED) %>% 
    as.data.frame(),
  
  
  ## Mayor nominations
  Mayor_party_nominations = zoho_extract %>%
    
    ## Only NUMBEROFPOSITIONSCONTESTED is numeric
    filter(CONTESTTYPECODE == 'MAYOR') %>% 
    group_by(RPPNAME) %>%
    summarize_if(., is.numeric, sum) %>%
    
    ## Rename
    rename(`Registered Political Party`      = RPPNAME,
           `Number of Mayoral Candidates` = NUMBEROFPOSITIONSCONTESTED) %>% 
    as.data.frame(),
  
  
  ## Council nominations
  council_party_nominations = zoho_extract %>%
    
    ## Only NUMBEROFPOSITIONSCONTESTED is numeric
    filter(CONTESTTYPECODE == 'COUNCILLOR') %>% 
    group_by(RPPNAME) %>%
    summarize_if(., is.numeric, sum) %>%
    
    ## Rename
    rename(`Registered Political Party`      = RPPNAME,
           `Number of Councillor Candidates` = NUMBEROFPOSITIONSCONTESTED) %>% 
    as.data.frame(),
  
  
  ## Combine data 
  Council_mayor_nominations = bind_rows(Mayor_party_nominations, council_party_nominations),
  
  
  ## Nominations candidate summary ----
  ## This table has counts for all the categories of nominations, but excludes the party name
  Candidate_Contest_zoho = zoho_subset %>% 
    
    ## Group by LGA - there is no ward in this table
    ## Note that incoming is missing
    group_by(#CONTESTAREACODE, 
      NOMINATIONSTATUS, 
      CONTESTTYPECODE, GROUPSTATUS) %>% 
    summarize_if(., is.numeric, sum) %>%
    
    ## Convert from long to wide
    pivot_wider(id_cols     = noms_aggr,
                names_from  = "NOMINATIONSTATUS",
                values_from = "NUMBEROFPOSITIONSCONTESTED"), 
  
  
  ## Count of nominations by contest type and status ----
  ## :: from the requirements
  ## Incoming   = sum(Incomplete	Ready to lodge)
  ## Processing = sum(Supervisor review	Pending	Lodged	Checked)
  Candidate_Contest_count = Candidate_Contest_summary %>%
    
    ## 
    filter(Status == 'Contested') %>%
    group_by(Contest) %>%
    summarize_if(., is.numeric, sum) %>%
    
    ## Aggregate the nominations categories
    mutate(Incoming   = Incoming + Incomplete + Ready_to_lodge) %>%
    mutate(Processing = Supervisor_review + Pending + Lodged + Checked) %>%
    mutate(Accepted   = Accepted + Reviewed) %>% 
    dplyr::select(-Reviewed) %>%
    
    ## Select the columns
    dplyr::select(noms_display) %>%
    gather(key = Status, value = Nominations, -Contest) %>%
    as.data.frame() %>%
    
    ## Re-order the Status factor for plotting
    mutate(Status = factor(Status,levels = c('Rejected',
                                             'Withdrawn',
                                             'Accepted',
                                             'Processing',
                                             'Incoming'))),
  
  
  ## Count of nominations for groups ---
  ## - i.e. groups on the ballot paper
  Group_Contest_count = Group_Contest_summary %>%
    
    filter(Status == 'Contested') %>%
    group_by(Contest) %>%
    summarize_if(., is.numeric, sum) %>%
    
    ## Aggregate the nominations categories
    mutate(Incoming   = Incoming + Incomplete + Ready_to_lodge) %>%
    mutate(Processing = Supervisor_review + Pending + Lodged + Checked) %>%
    mutate(Accepted   = Accepted + Reviewed) %>% dplyr::select(-Reviewed) %>%
    
    ## Select the columns
    dplyr::select(noms_display) %>%
    gather(key = Status, value = Nominations, -Contest) %>%
    as.data.frame() %>%
    
    ## Re-order the Status factor for plotting
    mutate(Status = factor(Status,levels = c('Rejected',
                                             'Withdrawn',
                                             'Accepted',
                                             'Processing',
                                             'Incoming'))),
  
  
  ## 
  Candidate_Contest_count_group = Candidate_Contest_count %>%
    group_by(Status) %>%
    summarize_if(., is.numeric, sum),
  
  
  
  
  
  ## 2). CREATE HEADLINE NUMBERS ================================================================================
  
  
  ## Total Paper Applications  ----
  Total_Paper_Applications    = round(sum(Candidate_Contest_count$Nominations)/3),
  
  
  ## Total Online Applications ----
  Total_Online_Applications   = round(sum(Candidate_Contest_count$Nominations)/1.5),
  
  
  ## % Online Applications     ----
  Percent_online_applications = paste0(round(round(sum(Candidate_Contest_count$Nominations)/1.5)
                                             /sum(Candidate_Contest_count$Nominations) * 100, digits = 1), "%"),
  
  
  ## Nominations by Registered Political Party ----
  Nom_by_RPP = Elector_nominations %>% 
    filter(`Nomination Method` == 'RPP') %>% 
    .$`Number of Candidates` %>% 
    sum(),
  
  
  ## Nominations by Electors ----
  Nom_by_electors = Elector_nominations %>% 
    filter(`Nomination Method` == 'ELECTORS') %>% 
    .$`Number of Candidates` %>% 
    sum(),
  
  
  ## Uncontested Mayoral Elections ---- 
  Uncontested_Mayoral_Elections = zoho_extract %>% 
    filter(CONTESTTYPECODE == 'MAYOR') %>% 
    .$NUMBEROFPOSITIONSCONTESTED,
  
  
  ## Uncontested Council Elections ----
  Uncontested_Council_Elections = zoho_extract %>% 
    filter(CONTESTTYPECODE == 'COUNCILLOR') %>% 
    .$NUMBEROFPOSITIONSCONTESTED,
  
  
  
  
  ## 3). PLOT NOMINATIONS DATA ================================================================================
  
  
  ## Need a summary of
  ## Number of uncontested elections by contest type3
  ## -Accepted + Reviewed determines contested status
  
  
  ## Plot counts of lodged nominations by contest type : 'Councillor/Mayor' ----
  ## ggplot
  lodged.nom.type = dash_bar_chart_small(df        = Candidate_Contest_count %>%
                                           group_by(Contest) %>%
                                           summarize_if(., is.numeric, sum),
                                         
                                         title     = '',
                                         caption   = '',
                                         
                                         xvar      = 'Contest',
                                         yvar      = 'Nominations',
                                         
                                         colours   = c('Councillor' = 'chocolate1',
                                                       'Mayor'      = 'darkturquoise'),
                                         
                                         tsize     = 24,
                                         capt_size = 22,
                                         xsize     = 28,
                                         ysize     = 28,
                                         ycol      = 'black',
                                         lab_size  = 15,
                                         
                                         ymin      = 0,
                                         ymax      = 350,
                                         ylab      = 'Lodged Nominations by Contest Type',
                                         xlab      = ''),
  
  
  ## Use the same colors every time
  overall.nom.count = dash_bar_chart_xvar(df       = Candidate_Contest_count %>%
                                            group_by(Status) %>%
                                            summarize_if(., is.numeric, sum),
                                          title     = '',
                                          caption   = '',
                                          
                                          xvar      = 'Status',
                                          yvar      = 'Nominations',
                                          
                                          tsize     = 24,
                                          capt_size = 22,
                                          xsize     = 28,
                                          ysize     = 28,
                                          ycol      = 'black',
                                          lab_size  = 15,
                                          
                                          ymin      = 0,
                                          ymax      = 350,
                                          ylab      = 'Overall Nominations',
                                          xlab      = ''),
  
  
  group.nom.count = dash_bar_chart_xvar(df        = Group_Contest_count,
                                        title     = '',
                                        caption   = '',
                                        
                                        xvar      = 'Status',
                                        yvar      = 'Nominations',
                                        
                                        tsize     = 24,
                                        capt_size = 22,
                                        xsize     = 28,
                                        ysize     = 28,
                                        ycol      = 'black',
                                        lab_size  = 15,
                                        
                                        ymin      = 0,
                                        ymax      = 300,
                                        ylab      = 'Group Contest Nominations',
                                        xlab      = ''),
  
  
  ## Plot nom count by councillor and Mayor ----
  mayor.count  = dash_bar_chart_xvar(df = Candidate_Contest_count %>%
                                       filter(Contest == 'Mayor'),
                                     
                                     title     = '',
                                     caption   = '',
                                     
                                     xvar      = 'Status',
                                     yvar      = 'Nominations',
                                     
                                     tsize     = 24,
                                     capt_size = 22,
                                     xsize     = 28,
                                     ysize     = 28,
                                     ycol      = 'black',
                                     lab_size  = 10,
                                     
                                     ymin      = 0,
                                     ymax      = 20,
                                     ylab      = 'Mayor Nominations',
                                     xlab      = ''),
  
  
  ## Plot nom count by councillor and Mayor ----
  council.count  = dash_bar_chart_xvar(df = Candidate_Contest_count %>%
                                         filter(Contest == 'Councillor'),
                                       
                                       title     = '',
                                       caption   = '',
                                       
                                       xvar      = 'Status',
                                       yvar      = 'Nominations',
                                       
                                       tsize     = 24,
                                       capt_size = 22,
                                       xsize     = 28,
                                       ysize     = 28,
                                       ycol      = 'black',
                                       lab_size  = 10,
                                       
                                       ymin      = 0,
                                       ymax      = 300,
                                       ylab      = 'Councillor Nominations',
                                       xlab      = ''),
  
  
  ## Combine the Nominations graphs ----
  ## Change wd
  
  
  ## Create the ggplot
  p1 = ggdraw() + draw_image("Overall_Nomination_counts.jpg"),
  p2 = ggdraw() + draw_image("Group_Nomination_counts.jpg"),
  p3 = ggdraw() + draw_image("Mayor_Nomination_counts.jpg"),
  p4 = ggdraw() + draw_image("Councillor_Nomination_counts.jpg"),
  
  
  ## Arrange the grids
  combine.plot = grid.arrange(p1, p2,
                              p3, p4,
                              ncol = 2,
                              nrow = 2),
  
)

## 4). CREATE DEPENDENCY GRAPH ==============================================================


## Then create a plot of the dependencies
nomiations_plan_graph = vis_drake_graph(nominations_plan,
                                        main      = 'LG2021 Nominations Dependency Graph',
                                        font_size = 20)


nomiations_plan_graph




####################################################################################################################
#################################################### TBC ###########################################################
####################################################################################################################