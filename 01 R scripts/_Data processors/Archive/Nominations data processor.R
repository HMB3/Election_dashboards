####################################################################################################################
###################################### CANDIDATE NOMINATIONS DASHBAORD ---- ########################################
####################################################################################################################


## This code creates the 'Candidate nominations' dashboard
## Note the details of the code need to be reviewed with the reqirements.
message('Run R code to process Candidate Nominations data')





## 1). CALCULATE TOTAL NOMINATIONS ================================================================================


## Data preparation 
reg           <- cand_reg_noms %>% filter(Process == "Registrations")
latest_reg    <- reg[nrow(reg),]

online        <- cand_reg_noms %>% filter(Process == "OnlineNoms")
latest_online <- online[nrow(online),]


## ================== TOTAL BY STATUS ======================================================================== 
cand_nom_totals_status <- addStatus(cand_nom_totals_status, 'Incomplete', 
                                    (latest_online$LA_candidates + latest_online$LC_candidates) - 
                                      sum(cand_nom_totals_status$n))


cand_nom_totals_status <- addStatus(cand_nom_totals_status, 'Entering',  0)
cand_nom_totals_status <- addStatus(cand_nom_totals_status, 'Checked',   0)
cand_nom_totals_status <- addStatus(cand_nom_totals_status, 'Reviewed',  0)
cand_nom_totals_status <- addStatus(cand_nom_totals_status, 'Confirmed', 0)
cand_nom_totals_status <- addStatus(cand_nom_totals_status, 'Pending',   0)
cand_nom_totals_status <- addStatus(cand_nom_totals_status, 'Withdrawn', 0)
cand_nom_totals_status <- addStatus(cand_nom_totals_status, 'Rejected',  0)



colnames(cand_nom_totals_status)[1]      <- 'STATUS'
colnames(cand_nom_totals_area_status)[2] <- 'STATUS'
cand_nom_totals_status$STATUS      <- as.character(cand_nom_totals_status$STATUS)
cand_nom_totals_area_status$STATUS <- as.character(cand_nom_totals_area_status$STATUS)


cand_nom_totals_status <- updateStatus(cand_nom_totals_status)
cand_nom_totals_status <- cand_nom_totals_status %>% group_by(STATUS) %>% summarise(n = sum(n))



## ====================== TOTAS BY STATUS BY AREA ===========================================================
cand_nom_totals_area_status    <- updateStatus(cand_nom_totals_area_status)
cand_nom_totals_area_status    <- cand_nom_totals_area_status %>% group_by(AREACODE, STATUS) %>% summarise(n = sum(n))

cand_nom_totals_area_status_la <- cand_nom_totals_area_status %>% filter(AREACODE != 'NSW')
cand_nom_totals_area_status_lc <- cand_nom_totals_area_status %>% filter(AREACODE == 'NSW')





## 4). PLOT NOMINATIONS DATA ================================================================================


## Notes on plotting - we make the colors for the categories the same, EG :
## Confirmed, withdrawn and rejected are all the same colors in each plot


## Plot candiate nomination by status ----
## Plot highchart
#statusplot  <- generateCandidateStatusPlot(cand_nom_totals_status)


## Where do the pictures get saved under Jerry's new structure?


## Plot ggplot
jpeg(filename = paste0(candidate_noms_input_folder, "/Candidate_nominations_status.jpg"), 
     width = 16, height = 10, res = 500,
     units = "in", pointsize = 12,
     quality = 75,
     bg = "white")


dash_bar_chart_vert(df        = cand_nom_totals_status %>% na.omit(),
                    title     = '', 
                    caption   = '',
                    
                    xvar      = 'STATUS',
                    yvar      = 'n',
                    colours = c('6.0 Withdrawn' = 'red2', 
                                '4.0 Confirmed' = 'royalblue2',
                                '7.0 Rejected'  = 'darkorange1'),
                    
                    tsize     = 24,
                    capt_size = 22,
                    xsize     = 22,
                    ysize     = 22,
                    ycol      = 'black',
                    lab_size  = 10,
                    
                    ymin      = 0, 
                    ymax      = 110,
                    ylab      = '',
                    xlab      = '')


dev.off()



## Plot candidate nominations by date ----
## Create dates
NomDate     <- '2019-02-24'
NomEndDate  <- '2019-03-10'
Total       <- 0


## Create tables
ft          <- as.data.frame(cbind(NomDate, Total))
ft$Total    <- as.numeric(as.character(ft$Total))
ft$NomDate  <- as.Date(as.character(ft$NomDate))


##
cand_cumsum_totals_date <- rbind(ft, cand_cumsum_totals_date)
cand_cumsum_totals_date[cand_cumsum_totals_date$NomDate > Sys.Date(),]$Total <- NA
cand_cumsum_totals_date <- cand_cumsum_totals_date %>% filter(NomDate < NomEndDate)



## Plot highchart
#dateplot <- generateCandidateTotalsByDatePlot(cand_cumsum_totals_date)


## Plot ggplot
jpeg(paste0(candidate_noms_input_folder, "/Cumulative_nomination_total.jpg"), 
     width = 16, height = 10, res = 500,
     units = "in", pointsize = 12,
     quality = 75,
     bg = "white")


gg_line_plot(df        = cand_cumsum_totals_date,
             title     = '', 
             caption   = '',
             
             xvar      = 'NomDate',
             yvar      = 'Total',
             
             tsize     = 24,
             capt_size = 22,
             xsize     = 16,
             ysize     = 16,
             ycol      = 'black',
             line_size = 1.8,
             point_size = 4,
             
             lab_size  = 6,
             
             ylab      = 'Cumulative nominations',
             xlab      = 'Date',
             lab_angle = 45)

dev.off()


## Plot party nominations ----
## Highchart
#partyPlot <- generateCandidateStatusByPartyPlot(cand_party_totals)


## ggplot 
jpeg(paste0(candidate_noms_input_folder, "/Party_nominations.jpg"), 
     width = 16, height = 10, res = 500,
     units = "in", pointsize = 12,
     quality = 75,
     bg = "white")


dash_bar_chart_factor(df        = cand_party_totals,
                      title     = '', 
                      caption   = '',
                      
                      xvar      = 'PARTY',
                      yvar      = 'n',
                      factor    = 'STATUS',
                      colours   = c('6.0 Withdrawn' = 'red2', 
                                    '4.0 Confirmed' = 'royalblue2',
                                    '7.0 Rejected'  = 'darkorange1'),
                      width     = 0.5,
                      
                      tsize     = 24,
                      capt_size = 22,
                      xsize     = 10,
                      ysize     = 15,
                      ycol      = 'black',
                      
                      hjust     = 0.6,
                      vjust     = 2.1,
                      lab_size  = 0,
                      lab_angle = 45,
                      leg_size  = 20,
                      
                      ymin      = 0, 
                      ymax      = 100,
                      ylab      = 'Nominations',
                      xlab      = '')

dev.off()



## Plot district nominations ----
## Highchart
#partyPlot <- generateCandidateStatusByPartyPlot(cand_party_totals)


## ggplot 
jpeg(paste0(candidate_noms_input_folder, "/District_nominations.jpg"), 
     width = 16, height = 10, res = 500,
     units = "in", pointsize = 12,
     quality = 75,
     bg = "white")


dash_bar_chart_factor(df        = cand_nom_totals_area_status_la,
                      title     = '', 
                      caption   = '',
                      
                      xvar      = 'AREACODE',
                      yvar      = 'n',
                      factor    = 'STATUS',
                      colours   = c('6.0 Withdrawn' = 'red2', 
                                    '4.0 Confirmed' = 'royalblue2',
                                    '7.0 Rejected'  = 'darkorange1'),
                      width     = 0.5,
                      
                      tsize     = 24,
                      capt_size = 22,
                      xsize     = 8,
                      ysize     = 12,
                      ycol      = 'black',
                      
                      hjust     = 0.6,
                      vjust     = 2.1,
                      lab_size  = 0,
                      lab_angle = 45,
                      leg_size  = 20,
                      
                      ymin      = 0, 
                      ymax      = 11,
                      ylab      = 'Nominations by district',
                      xlab      = '')

dev.off()





####################################################################################################################
#################################################### TBC ###########################################################
####################################################################################################################