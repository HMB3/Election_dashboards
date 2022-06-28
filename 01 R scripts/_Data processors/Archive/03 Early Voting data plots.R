
################################# ---- EARLY VOTING DATA PLOT ---- ###############################


## This code processes the data for the early-voting dashboard 
message('plot the early voting data')


## To do ----
## Fix NAs in markoff Table
## Fix projections for PRD LG2003
## Make cumulative plots



## 5). CREATE EARLY VOTING PLOTS =============================================================


## Plot 1). daily Early Votes ----
## Create a line plot of the total number of early votes issued by day
## Check the NAs when Jason has fixed the SQL ----
jpeg(paste0(sub_pages, '/Early_voting/Early_Voting_Cumulative_counts.jpg'), 
     width   = 15,   height = 10, res = 300,
     units   = "in", pointsize = 12,
     quality = 75,
     bg      = "white")


options(scipen = 999)
pre.poll.plot <- group_line_plot(df         = daily_cumulative_pre_poll,
                                 title      = 'All Venues - Total Votes issued by day',
                                 subtitle   = '',
                                 caption    = '', 
                                 
                                 xvar       = 'Day',
                                 yvar       = 'Total_cum_count', 
                                 group_var  = 'Status',
                                 
                                 tsize      = 40,
                                 capt_size  = 33,
                                 xsize      = 40,
                                 ysize      = 40,
                                 ycol       = 'black',
                                 line_size  = 5,
                                 point_size = 7,
                                 
                                 lab_size   = 6,
                                 leg_size   = 40,
                                 leg_pos    = 'bottom',
                                 mar        = 1,
                                 
                                 
                                 ylab       = 'Pre-Poll Votes',
                                 xlab       = 'Days of Pre-polling',
                                 lab_angle  = 0)


print(pre.poll.plot)
dev.off()





## Plot 2). Daily Early Votes for STH ----
# cumul.sth.plot <- group_line_plot(df        = daily_cumulative_STH,
#                                   title     = 'Fig 2). Sydney Town Hall - Total Votes issued by day',
#                                   subtitle  = '',
#                                   caption   = '', 
#                                   
#                                   xvar      = 'Day',
#                                   yvar      = 'Count',
#                                   group_var = 'Status',
#                                   
#                                   tsize      = 33,
#                                   capt_size  = 33,
#                                   xsize      = 33,
#                                   ysize      = 33,
#                                   ycol       = 'black',
#                                   line_size  = 7,
#                                   point_size = 9,
#                                   
#                                   lab_size   = 6,
#                                   leg_size   = 33,
#                                   leg_pos    = 'bottom',
#                                   
#                                   
#                                   ylab       = 'Pre-Poll Votes',
#                                   xlab       = 'Days of Pre-polling',
#                                   lab_angle  = 0)





########################################## ---- TBC ---- ###########################################
