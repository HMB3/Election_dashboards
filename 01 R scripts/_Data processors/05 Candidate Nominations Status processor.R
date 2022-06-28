################################# ---- CANDIDATE NOMINATIONS DASHBAORD ---- #####################################


## This code processes the data to create the 'Candidate nominations' dashboard
message('Run R code to process Candidate Nominations Status data')



## Create the Nominations status data
noms_status_data <- noms_status$candidateStatusLog
unique(noms_status_data$status)


## So we want a cumulative daily plot of each status, probably on different panels because the numbers are different
## Not sure how typical these numbers are, but it seems there would always be less withdrawn and rejected
table(noms_status_data$status)


## Tom's solution from Total Provisional Markoffs seems like the best idea.


## Markoff counts grouped by date ----
## This is used for Graph 1). below
Noms_status_group_date <- noms_status_data %>% 
  
  ## We might want to filter this
  # filter(status ==' Pre-Poll' | is.na(Venue_type)) %>%
  
  ## Split the date into date/time 
  separate(., col  = dateAndTime, into = c('Date', 'MinsSeconds'), sep = 'T') %>%
  mutate(Date      = empty_as_na(Date)) %>% 
  mutate(Date      = as.Date(Date)) %>% 
  
  ## Group by date and take the cumulative sum
  group_by(Date, status)       %>%
  summarise(Nominations = n()) %>% 
  arrange(Date) %>%
  
  #wantthe cumulaive sum to be by Vote type
  group_by(status) %>%
  mutate(Noms_cum_count = cumsum(Nominations))




## 
if(nrow(Noms_status_group_date) > 3)  {
  
  nominations.cumul.linplot <- ggplot(Noms_status_group_date %>%
                                        mutate(LastLabel = ifelse(Date == max(Date), Nominations, NA)), 
                                      aes(x = Date, 
                                          y = Nominations,
                                          colour = status)) +
    geom_line(size = 2) +
    
    facet_wrap(vars(status),
               nrow = 2, ncol = 4, scales = 'free_y') +
    geom_point(size = 3, colour = 'grey') +
    
    theme_light(base_size = 16) +
    ylab("Nomiations") +
    labs(title = "") +
    
    geom_text(aes(label = LastLabel,
                  vjust = 2),
              size = 5) +
    
    scale_x_date(date_labels = "%b-%d", date_breaks = "7 day") +
    
    theme(plot.margin      = unit(c(1, 1, 1, 1), "cm"),
          plot.title       = element_text(vjust  = 5,        size  = 25,     face  = "bold"),
          axis.text.x      = element_text(size   = 10,        angle = 45,     hjust = 1, vjust = 1),
          axis.title.x     = element_text(size   = 20,       face  = "bold", vjust = -5),
          axis.line        = element_line(colour = 'black',  size  = 0),
          strip.text       = element_text(size   = 15,       face  = 'bold', colour = 'black'),
          strip.background = element_rect(color = "black",   fill  = "white"),
          legend.position  = 'none',
          
          axis.title.y  = element_text(size = 20,  face = "bold", vjust = 4),
          axis.text.y   = element_text(size = 10),
          plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = 20, hjust = 0.5, face = "italic", color = "black"))
  
  nominations.cumul.linplot
  
} else {
  message('No data for graph, create message instead of plot')
  nominations.cumul.linplot <- 'Insufficient data for graph, try again later :]'
  nominations.cumul.linplot
}





#################################################### TBC ###########################################################