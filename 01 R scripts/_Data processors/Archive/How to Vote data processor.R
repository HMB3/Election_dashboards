####################################################################################################################
###################################### HOW TO VOTE DASHBAORD ---- ##################################################
####################################################################################################################


## This code processes the 'how to vote dashboard'
message('Run R code to process how to vote data ')



## 1). PULL HOW TO VOTE DATA FROM DATABASE =========================================================================


## Create Summary plot data
how_to_vote_summary$In_progress <- how_to_vote_summary$PiecesUniqueMaterial - 
  how_to_vote_summary$Uploaded - 
  how_to_vote_summary$Approved - 
  how_to_vote_summary$Rejected


how_to_vote_status_summary <- how_to_vote_summary %>% 
  arrange(ReportDate) %>% 
  tail(1) %>%
  select(In_progress, Uploaded, Approved, Rejected)


htv_t               <- as.data.frame(t(how_to_vote_status_summary))
htv_t               <- cbind(Status = rownames(htv_t), htv_t)
rownames(htv_t)     <- 1:nrow(htv_t)
colnames(htv_t)[2]  <- 'Total'
htv_t               <- htv_t %>% na.omit()


## Create Summary plot 
status_summary_plot <- generateHowToVoteStatusPlot(htv_t, "Item count by status", chart_name = "")


## Create status plot data
how_to_vote_status_plot_data <- how_to_vote_data %>%
  
  mutate(DateLodged = as.Date(DateLodged),
         CurrentStatus = case_when(CurrentStatus   == "Approved" | 
                                     CurrentStatus == "Rejected" | 
                                     CurrentStatus == "Uploaded" ~ CurrentStatus,
                                   TRUE ~ "In_progress")) %>%
  
  group_by(DateLodged, CurrentStatus) %>%
  summarise(ItemCount = n()) %>%
  spread(key = CurrentStatus, value = ItemCount, fill = 0)


## Create status plot ----
how_to_vote_status_plot <- plotHowToVoteByDate(how_to_vote_status_plot_data, 
                                               chart_name = "Status of how to vote material")





## 2). CREATE HOW TO VOTE PLOTS -=================================================================================


## How to Vote Time Process 
## Calcualte the time used to refresh the page?
how_to_vote_summary$ActionTimeAvgMins <- 
  
  sapply(strsplit(how_to_vote_summary$ActionTimeAvg, ":"),
         
         function(x) {
           
           x <- as.numeric(x)
           x[1]*60+x[2]+x[3]/60
           
         }
         
  )


## Calcualte the time used to refresh the page?
how_to_vote_summary$ActionTimeMinMins <-
  
  sapply(strsplit(how_to_vote_summary$ActionTimeMin, ":"),
         
         function(x) {
           
           x <- as.numeric(x)
           x[1]*60+x[2]+x[3]/60
           
         }
         
  )

## Calcualte the maximum minutes taken to vote?
how_to_vote_summary$ActionTimeMinMax <-
  
  sapply(strsplit(how_to_vote_summary$ActionTimeMax, ":"),
         
         function(x) {
           
           x <- as.numeric(x)
           x[1]*60+x[2]+x[3]/60
           
         }
         
  )


## Set thresholds ======================================================================================
## TODO: Update these thresholds accordinghly to Amber/Jasons desired batches
how_to_vote_summary$ActionTimeGroup <- "0"
threshold1 <- 10
threshold2 <- 20
threshold3 <- 30
threshold4 <- 40
threshold5 <- 5


## Loop over the action times
if(nrow(how_to_vote_summary[how_to_vote_summary$ActionTimeAvgMins >= 0  & 
                            how_to_vote_summary$ActionTimeAvgMins < threshold1,]) > 0) {
  
  how_to_vote_summary[how_to_vote_summary$ActionTimeAvgMins >= 0  & 
                        how_to_vote_summary$ActionTimeAvgMins < threshold1,]$ActionTimeGroup <- 
    
    paste0(toString(0), "-", toString(threshold1),"mins")
}


## Loop over 
if(nrow(how_to_vote_summary[how_to_vote_summary$ActionTimeAvgMins >= threshold1 & 
                            how_to_vote_summary$ActionTimeAvgMins < threshold2,]) > 0) {
  
  how_to_vote_summary[how_to_vote_summary$ActionTimeAvgMins >= threshold1  & 
                        how_to_vote_summary$ActionTimeAvgMins < threshold2,]$ActionTimeGroup <- 
    paste0(toString(threshold1), "-", toString(threshold2),"mins")
  
}


## Loop over 
if(nrow(how_to_vote_summary[how_to_vote_summary$ActionTimeAvgMins >= threshold2  & 
                            how_to_vote_summary$ActionTimeAvgMins < threshold3,]) > 0) {
  
  how_to_vote_summary[how_to_vote_summary$ActionTimeAvgMins >= threshold2  & 
                        how_to_vote_summary$ActionTimeAvgMins < threshold3,]$ActionTimeGroup <- 
    paste0(toString(threshold2), "-", toString(threshold3),"mins")
  
}


## Loop over 
if(nrow(how_to_vote_summary[how_to_vote_summary$ActionTimeAvgMins >= threshold3  & 
                            how_to_vote_summary$ActionTimeAvgMins < threshold4,]) > 0) {
  
  how_to_vote_summary[how_to_vote_summary$ActionTimeAvgMins >= threshold3  & 
                        how_to_vote_summary$ActionTimeAvgMins < threshold4,]$ActionTimeGroup <- 
    paste0(toString(threshold3), "-", toString(threshold4),"mins")
  
}


## Loop over 
if(nrow(how_to_vote_summary[how_to_vote_summary$ActionTimeAvgMins >= threshold4  & 
                            how_to_vote_summary$ActionTimeAvgMins < threshold5,]) > 0) {
  
  how_to_vote_summary[how_to_vote_summary$ActionTimeAvgMins >= threshold4  & 
                        how_to_vote_summary$ActionTimeAvgMins < threshold5,]$ActionTimeGroup <- 
    paste0(toString(threshold4), "-", toString(threshold5),"mins")
  
}


## Group by action time
how_to_vote_data_action_time               <- how_to_vote_summary %>% group_by(ActionTimeGroup) %>% tally()
colnames(how_to_vote_data_action_time)[2]  <- 'Total'
how_to_vote_action_time_plot               <- generateHowToVoteActionTimePlot(how_to_vote_data_action_time, 
                                                                              "Docs Last Updated",
                                                                              chart_name = 'how_to_vote_time_plot')



## Create PNG files for the RMD plots ----
## Save plot of how to vote item counts by status


## Where do the figures get saved in Jerry's new scheme?
jpeg(paste0(how_to_vote_input_folder, "/Howvote_status_summary_plot.jpg"), 
     width = 16, height = 10, res = 500,
     units = "in", pointsize = 12,
     quality = 75,
     bg = "white")


## ggplotly(object) creates plotly, but it's crap
dash_bar_chart_vert(df        = htv_t %>% filter(Total > 0),
                    title     = 'Item count by status', 
                    caption   = '',
                    
                    xvar      = 'Status',
                    yvar      = 'Total',
                    colours = c('Approved'  = 'royalblue2',
                                'Rejected'  = 'red2'),
                    
                    tsize     = 24,
                    capt_size = 22,
                    xsize     = 22,
                    ysize     = 22,
                    ycol      = 'black',
                    lab_size  = 10,
                    
                    ymin      = 0, 
                    ymax      = 1100,
                    ylab      = '',
                    xlab      = '')

dev.off()


how_to_vote_status_ggplot <- how_to_vote_status_plot_data %>% 
  gather(key = "Status", value = "Total_number", -DateLodged) %>% 
  as.data.frame()


## Save plot of how to vote item counts by status
jpeg(paste0(how_to_vote_input_folder, "/Howvote_status_date_plot.jpg"), 
     width = 16, height = 10, res = 500,
     units = "in", pointsize = 12,
     quality = 75,
     bg = "white")


dash_bar_chart_factor(df        = how_to_vote_status_ggplot,
                      title     = 'Status of how to vote material', 
                      caption   = '',
                      
                      xvar      = 'DateLodged',
                      yvar      = 'Total_number',
                      factor    = 'Status',
                      width     = 0.8,
                      colours = c('Approved'  = 'royalblue2',
                                  'Rejected'  = 'red2'),
                      
                      tsize     = 24,
                      capt_size = 22,
                      xsize     = 22,
                      ysize     = 22,
                      ycol      = 'black',
                      
                      hjust     = 0.6,
                      vjust     = 2.1,
                      lab_size  = 6,
                      lab_angle = 45,
                      leg_size  = 20,
                      
                      ymin      = 0, 
                      ymax      = 220,
                      ylab      = 'Total Number of Items',
                      xlab      = '')

dev.off()





####################################################################################################################
#################################################### TBC ###########################################################
####################################################################################################################