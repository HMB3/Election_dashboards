###################################### DECLATAION VOTING DATA PROCESSOR ---- #######################################


## This code creates plots for the graphs of 


## To-do decVotes ----


## Remaining to be fulfilled -3
## 1.	Enrolment and NAMAV provisional mark off figures don't match Dec Vote Report
## 2.	Postal numbers show that 3 more applications were fulfilled than accepted. -3
## 3. The DV RMD will not knit....



## This code processes the data for the early-voting dashboard 
message('Run R code to analyse the Declation Voting data')





## 6). CREATE PLOTS ==============================================================================


## Graph 1). Barplot of Total provisional markoffs over time ----
if(nrow(Markoff_sum_type) > 0) {
  
  ## Create plot
  decvotes.barplot  <- dash_bar_chart_two_factor(df = Markoff_sum_type, 
                                                 
                                                 title      = '',
                                                 caption    = '',
                                                 
                                                 xvar       = 'Vote_type',
                                                 yvar       = 'Total_markoffs',
                                                 fill_var   = 'Venue_type',
                                                 
                                                 colours    = c('Polling Place' = "#E7298A", 
                                                                'Pre-Poll'      = "#1B9E77"),
                                                 
                                                 tsize      = 40,
                                                 capt_size  = 33,
                                                 xsize      = 40,
                                                 ysize      = 40,
                                                 ycol       = 'black',
                                                 lab_size   = 10,
                                                 leg_size   = 40,
                                                 leg_pos    = 'top',
                                                 axis_size  = 1.3,
                                                 h_just     = -0.5,
                                                 
                                                 ymin       = 0,
                                                 ymax       = max(Markoff_sum_type$Total_markoffs) +
                                                   (max(Markoff_sum_type$Total_markoffs)*0.1),
                                                 ylab       = 'Declaration Votes',
                                                 xlab       = '',
                                                 mar        = 1.5)
  
} else {
  message('No data for graph, create message instead of plot')
  decvotes.barplot <- 'Insufficient data for graph, try again later :]'
}





## Graph 2). Barplot of Total Postal Vote category count ----


## Use the same colors every time
if(nrow(Postal_categories_overall_count) > 0) {
  
  postal.reject.count <- stacked_bar_chart_xvar(df        = Postal_categories_overall_count,
                                                title     = '',
                                                caption   = '',
                                                
                                                xvar      = 'Display',
                                                yvar      = 'value',
                                                fill_var  = 'Status',
                                                
                                                # colours   = c('Scrutiny_Rejected' = '#F8766D',
                                                #               'Scrutiny_Accepted' = '#00BFC4'),
                                                
                                                tsize      = 40,
                                                capt_size  = 33,
                                                xsize      = 40,
                                                ysize      = 40,
                                                ycol      = 'black',
                                                #lab_size  = 25,
                                                leg_size  = 35,
                                                axis_size = 1.7,
                                                leg_pos = 'top',
                                                mar        = 0.5,
                                                
                                                ymin      = 0,
                                                ymax      = max(Postal_categories_overall_count$value) +
                                                  max(Postal_categories_overall_count$value)*0.05,
                                                
                                                ylab      = '',
                                                xlab      = '')
  
} else {
  message('No data for graph, create message instead of plot')
  postal.reject.count <-'Insufficient data for graph, try again later :]'
}





## Graph 3). Line plot of total provisional markoffs over time ----


## Plot total provisional markoffs ----
if(nrow(EMA_markoff_group_date) > 0) {
  
  markoffs.cumul.linplot <- single_line_plot_date(df         = EMA_markoff_group_date,
                                                  title      = 'Total mark-offs over time', 
                                                  caption    = '',
                                                  
                                                  xvar       = 'Date',
                                                  yvar       = 'Total_cum_markoff',
                                                  
                                                  ylast      = max(EMA_markoff_group_date$Total_cum_markoff),
                                                  
                                                  tsize      = 70,
                                                  capt_size  = 50,
                                                  xsize      = 50,
                                                  ysize      = 50,
                                                  line_size  = 7,
                                                  point_size = 9,
                                                  leg_pos    = 'none',
                                                  
                                                  lab_size   = 6,
                                                  leg_size   = 35,
                                                  axis_size  = 0.0,
                                                  mar        = 2.2,
                                                  
                                                  ylab       = 'Markoffs',
                                                  xlab       = '',
                                                  lab_angle  = 90,
                                                  date_labs  = "%b-%d",
                                                  date_break = "1 day")
  
} else {
  message('No data for graph, create message instead of plot')
  markoffs.cumul.linplot <-'Insufficient data for graph, try again later :]'
}






## Graph 4). Postal Fulfilment votes over time ----


## Add a label 
if(nrow(Postal_fulfilment_plot) > 0) {
  
  postal.linplot <- group_line_plot_skip_x_axes(df         = Postal_fulfilment_plot,
                                                title      = 'Postal fulfillment', 
                                                caption    = '',
                                                
                                                xvar       = 'Date',
                                                yvar       = 'Count',
                                                group_var  = 'Status',
                                                x_int      = 2,
                                                
                                                tsize      = 40,
                                                capt_size  = 33,
                                                xsize      = 40,
                                                ysize      = 40,
                                                ycol       = 'black',
                                                line_size  = 5,
                                                point_size = 6,
                                                leg_pos    = 'bottom',
                                                mar        = 0.5,
                                                
                                                lab_size   = 6,
                                                leg_size   = 35,
                                                axis_size  = 0.7,
                                                
                                                ylab       = 'Postal Vote Applications',
                                                xlab       = '',
                                                lab_angle  = 45)
  
  
} else {
  message('No data for graph, create message instead of plot')
  postal.linplot <-'Insufficient data for graph, try again later :]'
}


# group_line_plot_date(df          = overall_nominations_date,
#                      data_ends   = overall_noms_date_ends,
#                      
#                      title       = '',
#                      subtitle    = '',
#                      caption     = '', 
#                      
#                      xvar        = 'Date',
#                      yvar        = 'Noms_cum_count', 
#                      group_var   = 'Status',
#                      
#                      tsize       = 90,
#                      capt_size   = 90,
#                      xsize       = 90,
#                      ysize       = 90,
#                      ycol        = 'black',
#                      line_size   = 10,
#                      point_size  = 14,
#                      
#                      lab_size    = 20,
#                      leg_size    = 90,
#                      leg_pos     = 'bottom',
#                      
#                      ylab        = 'Nominations',
#                      xlab        = 'Date',
#                      lab_angle   = 45,
#                      date_labs   = "%y-%d-%m",
#                      date_break  = "5 day")






#################################################### TBC ###########################################################