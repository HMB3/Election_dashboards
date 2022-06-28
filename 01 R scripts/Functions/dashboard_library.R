####################################################################################################################
###################################### DASHBAORD FUNCTIONS ---- ####################################################
####################################################################################################################


## DATA WRANGLING FUNCTIONS  ===============================================================


## Reverse of '%in%'
'%!in%' <- function(x,y)!('%in%'(x,y))


is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))



## GGPLOT FUNCTIONS  ===============================================================



## Bar chart horozontally stacked, few categories ----
dash_bar_chart_small = function(df, title, xvar, yvar, fill_var, colours,
                                ylab, xlab, ymin, ymax,
                                tsize, caption, xsize, ysize, ycol,
                                lab_size, bar_width, capt_size) {
  
  ## Use variable names for x and y
  ggplot(df,
         aes(
           x    = reorder(!!sym(xvar), !!sym(yvar)),
           y    = !!sym(yvar),
           fill = !!sym(xvar))) +
    geom_bar(stat = "identity") +
    
    coord_flip() +
    scale_fill_manual(values = colours, na.value = "grey") +
    
    geom_text(aes(label = !!sym(yvar), hjust = + 0.5),
              hjust     = -0.5,
              position  = position_dodge(width = 1),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(0.5,0.5,0.5,0.5), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          legend.position = 'none',
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
}





## Bar chart vertically stacked and ordered by value ----
dash_bar_chart_yvar = function(df, title, xvar, yvar,
                               ylab, xlab, ymin, ymax,
                               tsize, caption, xsize, ysize, ycol,
                               lab_size, bar_width, capt_size) {
  
  ## Use variable names for x and y
  ggplot(df,
         aes(x    = reorder(!!sym(xvar), !!sym(yvar)),
             y    = !!sym(yvar),
             fill = !!sym(xvar))) +
    
    geom_bar(stat = "identity") +
    
    coord_flip() +
    scale_color_brewer(palette = "Dark2") +
    
    geom_text(aes(label = !!sym(yvar), hjust = + 0.5),
              hjust     = -0.5,
              position  = position_dodge(width = 1),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(0.5,0.5,0.5,0.5), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          legend.position = 'none',
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
}





## Bar chart vertically stacked and ordered by factor ----
dash_bar_chart_xvar = function(df, title, xvar, yvar,
                               ylab, xlab, ymin, ymax,
                               tsize, caption, xsize, ysize, ycol,
                               lab_size, bar_width, capt_size) {
  
  ## Use variable names for x and y
  ggplot(df,
         aes(x    = !!sym(xvar), !!sym(yvar),
             y    = !!sym(yvar),
             fill = !!sym(xvar))) +
    
    geom_bar(stat = "identity") +
    
    coord_flip() +
    scale_color_brewer(palette = "Dark2") +
    
    geom_text(aes(label = !!sym(yvar), hjust = + 0.5),
              hjust     = -0.5,
              position  = position_dodge(width = 1),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(0.5,0.5,0.5,0.5), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          legend.position = 'none',
          
          axis.title.y  = element_text(size = ysize, face  = "bold",   vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
}





## Bar chart horizontal with factor ---- 
dash_bar_chart_factor = function(df, title, xvar, yvar, factor, width, colours,
                                 ylab, xlab, ymin, ymax,
                                 tsize, caption, xsize, ysize, ycol, 
                                 lab_size, hjust, vjust, lab_angle, leg_size,
                                 bar_width, capt_size) {
  
  ## Use variable names for x and y
  ggplot(df, 
         aes(
           x = reorder(!!sym(xvar), !!sym(xvar)),
           y = !!sym(yvar), 
           fill = factor(!!sym(factor)))) +  
    geom_bar(stat = "identity", position = "stack", width = width) + 
    
    scale_fill_manual(values = colours, na.value = "grey") +
    
    geom_text(aes(label = !!sym(yvar), hjust = hjust), 
              hjust = 0.6, vjust = vjust, 
              position = position_dodge(width = 0),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(1,1,1,1), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size = xsize, face = "bold", hjust = 2),
          legend.position = 'bottom',
          legend.title    = element_blank(),
          legend.text     = element_text(size = leg_size),
          
          axis.line       = element_line(colour = 'black', size = width),
          axis.ticks      = element_line(colour = "black", size = width),
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
}





## Grouped line plot both axes ----
group_line_plot_axes = function(df,       title, xvar, yvar, line_size, point_size,
                                ylab,     xlab, group_var, lab_angle,   leg_size,
                                tsize,    caption, xsize, ysize, ycol,
                                lab_size, bar_width, capt_size) {
  
  ## Use variable names for x and y
  ## Replaced group = 1
  ggplot(df,
         aes(
           x = reorder(!!sym(xvar), !!sym(yvar)),
           y = !!sym(yvar), group = !!sym(group_var), colour = !!sym(group_var))) +
    
    geom_line(size  = line_size) +
    geom_point(size = point_size, colour = 'grey') +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    theme(plot.margin   = unit(c(1, 1, 1, 1), "cm"),
          plot.title    = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x   = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x  = element_text(size  = xsize, face  = "bold",    vjust = -5),
          legend.text   = element_text(size  = leg_size),
          legend.title  = element_blank(),
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
}



## Grouped line plot both axes ----
group_line_plot_y_axes = function(df,       title, xvar, yvar, line_size, point_size,
                                  ylab,     xlab, group_var, lab_angle,   leg_size,
                                  tsize,    caption, xsize, ysize, ycol,
                                  lab_size, bar_width, capt_size) {
  
  ## Use variable names for x and y
  ## Replaced group = 1
  ggplot(df,
         aes(
           x = reorder(!!sym(xvar), !!sym(yvar)),
           y = !!sym(yvar), group = !!sym(group_var), colour = !!sym(group_var))) +
    
    geom_line(size  = line_size) +
    geom_point(size = point_size, colour = 'grey') +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    theme(plot.margin   = unit(c(1, 1, 1, 1), "cm"),
          plot.title    = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x   = element_blank(),
          axis.title.x  = element_blank(),
          legend.text   = element_text(size  = leg_size),
          legend.title  = element_blank(),
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
}





## Single line plot ----
single_line_plot = function(df,  title,  xvar, yvar,
                            ylab, xlab,  lab_angle, line_size, point_size,
                            tsize, caption, xsize, ysize, ycol,
                            lab_size, bar_width, capt_size, leg_size) {
  
  ## Use variable names for x and y
  ggplot(df, aes(!!sym(xvar), !!sym(yvar))) +
    geom_line(aes(color = 'blue'), size = line_size) +
    
    geom_line(size  = line_size) +
    geom_point(size = point_size, colour = 'orange') +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    theme(plot.margin   = unit(c(1, 1, 1, 1), "cm"),
          plot.title    = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x   = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x  = element_text(size  = xsize, face  = "bold",    vjust = -5),
          legend.text   = element_text(size  = leg_size),
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
}





## HI-CHART FUNCTIONS  ===============================================================


## Function for plotting How to vote by date ----
plotHowToVoteByDate <- function(htv_data, chart_name) {
  
  plot <- highchart() %>%
    
    hc_xAxis(categories = htv_data$DateLodged, title = list(text = "Date Lodged")) %>%
    hc_yAxis(title = list(text = "Total Number of Items")) %>%
    hc_title(text  = "Status of how to vote material") %>%
    
    # hc_add_series(name = "In_progress",
    #               data = htv_data,
    #               type = "column",
    #               hcaes(y = In_progress),
    #               showInLegend = TRUE) %>%
    
    # hc_add_series(name = "Uploaded",
    #               data = htv_data,
    #               type = "column",
    #               hcaes(y = Uploaded),
  #               showInLegend = TRUE) %>%
  
  hc_add_series(name = "Approved",
                data = htv_data,
                type = "column",
                hcaes(y = Approved),
                showInLegend = TRUE) %>%
    
    hc_add_series(name = "Rejected",
                  data = htv_data,
                  type = "column",
                  hcaes(y = Rejected),
                  showInLegend = TRUE) %>%
    
    hc_plotOptions(series = list(stacking = 'normal')) %>%
    hc_add_theme(hc_theme_flat()) %>%
    
    hc_exporting(enabled = TRUE,
                 filename = chart_name)
  
  return(plot)
  
}


## Function for plotting How to vote by status ----
generateHowToVoteStatusPlot <- function(htv_status, heading, chart_name){
  
  plot_bar <- highchart() %>%
    hc_chart(type = "column") %>%
    
    hc_xAxis(categories = htv_status$Status,  label = "") %>%
    hc_add_series(data = htv_status$Total,
                  colorByPoint = TRUE,
                  name="Count") %>%
    
    hc_add_theme(hc_theme_flat()) %>%
    hc_title(text = heading) %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(hc_theme_538()) %>%
    
    hc_exporting(enabled = TRUE,
                 filename = chart_name)
  
  return(plot_bar)
  
}


## Function for plotting How to vote action time plot ----
generateHowToVoteActionTimePlot <- function(htv_status, heading, chart_name) {
  
  plot_bar <- highchart() %>%
    hc_chart(type = "column") %>%
    
    hc_xAxis(categories = htv_status$ActionTimeGroup,  label = "") %>%
    hc_add_series(data  = htv_status$Total,
                  colorByPoint = TRUE,
                  name="Count") %>%
    
    hc_add_theme(hc_theme_flat()) %>%
    hc_title(text = heading) %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(hc_theme_538()) %>%
    
    hc_exporting(enabled = TRUE,
                 filename = chart_name)
  
  return(plot_bar)
  
}



## CANDIDATE NOMINATIONS FUNCTIONS =====================================

## generate Candidate Totals By Date Plot----
generateCandidateTotalsByDatePlot <- function(cand_cumsum_by_date){
  p <-  cand_cumsum_by_date %>%
    hchart(.,
           type = "line",
           hcaes(x = NomDate,
                 y = "Total"),
           name="Count") %>%  hc_xAxis(title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Cumulative Nomination Total"))
  return(p)
}


## generate Candidate Status Plot----
generateCandidateStatusPlot <- function(cand_by_status){
  colnames(cand_by_status)[2] <- 'n'
  plot_bar <- highchart() %>%
    hc_chart(type = "column")
  
  if(nrow(cand_by_status) == 1){
    plot_bar <- plot_bar %>%
      hc_xAxis(categories = list(cand_by_status$STATUS),  label = "")
  }else
  {
    plot_bar <- plot_bar %>%
      hc_xAxis(categories =cand_by_status$STATUS,  label = "")
  }
  
  
  plot_bar <- plot_bar %>%
    hc_add_series(data = cand_by_status$n,
                  colorByPoint = TRUE,
                  name="Count") %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_title(text = "Candidate Nominations by Status") %>%
    hc_legend(enabled = FALSE)
  return(plot_bar)
}


## generate Candidate Status by area Plot----
generateCandidateStatusByAreaPlot <- function(cand_by_status_by_area){
  
  
  grpByType <- dcast(cand_by_status_by_area, AREACODE ~ STATUS)
  grpByType[is.na(grpByType)] <- 0
  
  plot <- highchart() %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_plotOptions(series=list(stacking='normal'))
  
  if(nrow(grpByType) > 1){
    
    plot <- plot %>%
      hc_xAxis(categories = grpByType$AREACODE)
  }
  else
  {
    if(grepl("NSW", grpByType$AREACODE)){
      plot <- plot %>%
        hc_xAxis(categories = list("NSW Legislative Council"))
    }else{
      plot <- plot %>%
        hc_xAxis(categories = list(grpByType$AREACODE))
    }
    
  }
  
  if("2.0 Processing" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "2.0 Processing",
                    data = grpByType,
                    type = "column",
                    hcaes(y = `2.0 Processing`),
                    showInLegend = TRUE,
                    color = "#6de081")
  }
  
  if("3.0 Reviewed" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "3.0 Reviewed",
                    data = grpByType,
                    type = "column",
                    hcaes(y = `3.0 Reviewed`),
                    showInLegend = TRUE,
                    color = "#397745")
  }
  
  if("4.0 Confirmed" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "4.0 Confirmed",
                    data = grpByType,
                    type = "column",
                    hcaes(y = `4.0 Confirmed`),
                    showInLegend = TRUE,
                    color = "#4a5091")
  }
  
  if("5.0 Pending" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "5.0 Pending",
                    data = grpByType,
                    type = "column",
                    hcaes(y = `5.0 Pending`),
                    showInLegend = TRUE,
                    color = "#528fb2")
  }
  
  
  if("6.0 Withdrawn" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "6.0 Withdrawn",
                    data = grpByType,
                    type = "column",
                    hcaes(y = `6.0 Withdrawn`),
                    showInLegend = TRUE,
                    color = "#efd43b")
  }
  
  if("7.0 Rejected" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "7.0 Rejected",
                    data = grpByType,
                    type = "column",
                    hcaes(y = `7.0 Rejected`),
                    showInLegend = TRUE,
                    color = "#b25275")
  }
  
  
  return(plot)
}


## generate Candidate Status by party Plot----
generateCandidateStatusByPartyPlot <- function(cand_party_by_area){
  
  
  grpByType <- dcast(cand_party_by_area, PARTY ~ STATUS)
  grpByType[is.na(grpByType)] <- 0
  
  plot <- highchart() %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_plotOptions(series=list(stacking='normal'))
  
  if(nrow(grpByType) > 1){
    plot <- plot %>%
      hc_xAxis(categories = grpByType$PARTY)
  } else
  {
    plot <- plot %>%
      hc_xAxis(categories = list(grpByType$PARTY))
    
  }
  
  if("2.0 Processing" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "2.0 Processing",
                    data = grpByType,
                    type = "column",
                    hcaes(y = `2.0 Processing`),
                    showInLegend = TRUE,
                    color = "#6de081")
  }
  
  if("3.0 Reviewed" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "3.0 Reviewed",
                    data = grpByType,
                    type = "column",
                    hcaes(y = `3.0 Reviewed`),
                    showInLegend = TRUE,
                    color = "#397745")
  }
  
  if("4.0 Confirmed" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "4.0 Confirmed",
                    data = grpByType,
                    type = "column",
                    hcaes(y = `4.0 Confirmed`),
                    showInLegend = TRUE,
                    color = "#4a5091")
  }
  
  if("5.0 Pending" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "5.0 Pending",
                    data = grpByType,
                    type = "column",
                    hcaes(y = `5.0 Pending`),
                    showInLegend = TRUE,
                    color = "#528fb2")
  }
  
  
  if("6.0 Withdrawn" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "6.0 Withdrawn",
                    data = grpByType,
                    type = "column",
                    hcaes(y = `6.0 Withdrawn`),
                    showInLegend = TRUE,
                    color = "#efd43b")
  }
  
  if("7.0 Rejected" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "7.0 Rejected",
                    data = grpByType,
                    type = "column",
                    hcaes(y = `7.0 Rejected`),
                    showInLegend = TRUE,
                    color = "#b25275")
  }
  
  return(plot)
}


## Generate Candidate Status Plot online ----
generateCandidateStatusPlotOnline <- function(cand_by_status){
  
  cand_by_status[is.na(cand_by_status)] <- 0
  
  plot <- highchart() %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_plotOptions(series=list(stacking='normal'))
  
  plot <- plot %>%
    hc_xAxis(categories = cand_by_status$District)
  
  
  if("Incomplete" %in% colnames(cand_by_status)){
    plot <- plot %>%
      hc_add_series(name = "Incomplete",
                    data = cand_by_status,
                    type = "column",
                    hcaes(y = Incomplete),
                    showInLegend = TRUE,
                    color = "#efd43b")
  }
  
  if("ReadyToLodge" %in% colnames(cand_by_status)){
    plot <- plot %>%
      hc_add_series(name = "ReadyToLodge",
                    data = cand_by_status,
                    type = "column",
                    hcaes(y = ReadyToLodge),
                    showInLegend = TRUE,
                    color = "#a7a1c9")
  }
  
  return(plot)
}



addStatus <- function(frame, status, value){
  
  if(nrow(frame[frame$CandidateStatus == status,]) == 0){
    CandidateStatus <- status
    nn <- max(0, value)
    generated_frame <- as.data.frame(cbind(CandidateStatus, n))
    generated_frame$n <- as.numeric(as.character(generated_frame$n))
    frame <- rbind(generated_frame, frame)
  }
  return(frame)
}



updateStatus <- function(frame){
  #Update Names
  if(nrow(frame[frame$STATUS == 'Incomplete',]) > 0){
    frame[frame$STATUS == 'Incomplete',]$STATUS = '1.0 Incomplete'
  }
  
  if(nrow(frame[frame$STATUS == 'Entering',]) > 0){
    frame[frame$STATUS == 'Entering',]$STATUS = '2.0 Processing'
  }
  
  if(nrow(frame[frame$STATUS == 'Checked',]) > 0){
    frame[frame$STATUS == 'Checked',]$STATUS = '2.0 Processing'
  }
  
  if(nrow(frame[frame$STATUS == 'Reviewed',]) > 0){
    frame[frame$STATUS == 'Reviewed',]$STATUS = '3.0 Reviewed'
  }
  
  if(nrow(frame[frame$STATUS == 'Confirmed',]) > 0){
    frame[frame$STATUS == 'Confirmed',]$STATUS = '4.0 Confirmed'
  }
  
  if(nrow(frame[frame$STATUS == 'Pending',]) > 0){
    frame[frame$STATUS == 'Pending',]$STATUS = '5.0 Pending'
  }
  
  if(nrow(frame[frame$STATUS == 'Withdrawn',]) > 0){
    frame[frame$STATUS == 'Withdrawn',]$STATUS = '6.0 Withdrawn'
  }
  
  if(nrow(frame[frame$STATUS == 'Rejected',]) > 0){
    frame[frame$STATUS == 'Rejected',]$STATUS = '7.0 Rejected'
  }
  
  return(frame)
}





## EARLY VOTING FUNCTIONS =====================================

generate_eec_plot <- function(eec_summary){
  eec_summary_plot <- highchart() %>%
    hc_chart(animation = FALSE) %>%
    hc_title(text = "EEC Forecast vs Actual") %>%
    hc_subtitle(text = "Elector Enquiry Call Centre") %>%
    hc_xAxis(categories = eec_summary$Display) %>%
    hc_plotOptions(
      column = list(
        stacking = "normal"
      ),
      line = list(
        cursor = "ns-resize"
      )
    ) %>%
    hc_tooltip(yDecimals = 2) %>%
    hc_add_series(name = "Forecast",
                  data = eec_summary$Forecast_calls,
                  type = "line",
                  minPointLength = 2,
                  dashStyle = 'longdash'
    ) %>%
    hc_add_series(name = "Actual",
                  data = eec_summary$Answered_calls,
                  type = "line",
                  minPointLength = 2
    )%>%
    hc_add_series(name = "Staff",
                  data = eec_summary$Actual_staff,
                  type = "column"
    )
  return(eec_summary_plot)
  
}

generate_handling_plot <- function(eec_summary){
  
  eec_handling_plot <- highchart() %>%
    hc_chart(animation = FALSE) %>%
    hc_title(text = "EEC Handling & Wait Times") %>%
    hc_subtitle(text = "Elector Enquiry Call Centre") %>%
    hc_xAxis(categories = eec_summary$Display) %>%
    hc_plotOptions(
      column = list(
        stacking = "normal"
      ),
      line = list(
        cursor = "ns-resize"
      )
    ) %>%
    hc_tooltip(yDecimals = 2) %>%
    hc_add_series(name = "Average Handling Time (Mins)",
                  data = eec_summary$AHT_Mins,
                  type = "line",
                  minPointLength = 2,
                  dashStyle = 'longdash'
    ) %>%
    hc_add_series(name = "Max Wait Time (Mins)",
                  data = eec_summary$MWT_Mins,
                  type = "line",
                  minPointLength = 2)
  return(eec_handling_plot)
  
}

generate_gos30_plot <- function(eec_summary){
  eec_gos_plot <- highchart() %>%
    hc_chart(animation = FALSE) %>%
    hc_title(text = "EEC Grade of Service 30s") %>%
    hc_subtitle(text = "Elector Enquiry Call Centre") %>%
    hc_xAxis(categories = eec_summary$Display) %>%
    hc_plotOptions(
      column = list(
        stacking = "normal"
      ),
      line = list(
        cursor = "ns-resize"
      )
    ) %>%
    hc_tooltip(yDecimals = 2) %>%
    hc_add_series(name = "Service <30s (%)",
                  data = eec_summary$GOS30,
                  type = "line",
                  minPointLength = 2,
                  dashStyle = 'longdash'
    )
  return(eec_gos_plot)
  
}




## STAFFING FUNCTIONS =============================================
generateStaffingPositionsPlot <- function(staff_pos_details, type = "VC") {
  
  stPosCountSummary <- staff_pos_details %>% filter(!grepl("TH-", PositionType)) %>%
    dplyr::select(PositionType, Status)
  
  if(type!="VC" && nrow(stPosCountSummary[stPosCountSummary$Status == "TRAINED",]) > 0){
    stPosCountSummary[stPosCountSummary$Status == "TRAINED",]$Status = "EMPLOYED"
  }
  
  colnames(stPosCountSummary) <- c('Position', 'Status')
  stPosCountSummaryGrp <- stPosCountSummary %>% group_by(Position, Status) %>% tally()
  grpByType <- dcast(stPosCountSummaryGrp, Position ~ Status)
  grpByType[is.na(grpByType)] <- 0
  
  plot <- highchart() %>%
    hc_add_theme(hc_theme_flat())%>%
    hc_plotOptions(series=list(stacking='normal'))
  
  if(nrow(grpByType) <= 1){
    plot <- plot %>% hc_xAxis(categories = list(grpByType$Position))
  }
  else {
    plot <- plot %>% hc_xAxis(categories = grpByType$Position)
  }
  
  if("OPEN" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "Open",
                    data = grpByType,
                    type = "column",
                    hcaes(y = OPEN),
                    showInLegend = TRUE,
                    color = "#efd43b")
  }
  
  if("OFFERED" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "Offered",
                    data = grpByType,
                    type = "column",
                    hcaes(y = OFFERED),
                    showInLegend = TRUE,
                    color = "#4a5091")
  }
  
  if("EMPLOYED" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "Employed",
                    data = grpByType,
                    type = "column",
                    hcaes(y = EMPLOYED),
                    showInLegend = TRUE,
                    color = "#397745")
  }
  
  if("TRAINED" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "Trained",
                    data = grpByType,
                    type = "column",
                    hcaes(y = TRAINED),
                    showInLegend = TRUE,
                    color = "#6de081")
  }
  
  return(plot)
}

generateStaffingPositionsPlotByLocation <- function(staff_pos_details){
  stPosCountSummary <- staff_pos_details %>% filter(!grepl("TH-", PositionType)) %>%
    dplyr::select(District, Status)
  colnames(stPosCountSummary) <- c('District', 'Status')
  
  stPosCountSummaryGrp <- stPosCountSummary %>% group_by(District, Status) %>% tally()
  grpByType <- dcast(stPosCountSummaryGrp, District ~ Status)
  grpByType[is.na(grpByType)] <- 0
  
  plot <- highchart() %>%
    hc_xAxis(categories = grpByType$District) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_plotOptions(series=list(stacking='normal'))
  
  if("OPEN" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "Open",
                    data = grpByType,
                    type = "column",
                    hcaes(y = OPEN),
                    showInLegend = TRUE,
                    color = "#efd43b")
  }
  
  if("OFFERED" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "Offered",
                    data = grpByType,
                    type = "column",
                    hcaes(y = OFFERED),
                    showInLegend = TRUE,
                    color = "#4a5091")
  }
  
  if("EMPLOYED" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "Employed",
                    data = grpByType,
                    type = "column",
                    hcaes(y = EMPLOYED),
                    showInLegend = TRUE,
                    color = "#397745")
  }
  
  if("TRAINED" %in% colnames(grpByType)){
    plot <- plot %>%
      hc_add_series(name = "Trained",
                    data = grpByType,
                    type = "column",
                    hcaes(y = TRAINED),
                    showInLegend = TRUE,
                    color = "#6de081")
  }
  return(plot)
}

generateSADPlot <- function(staff_age_density){
  breaksx = scales::pretty_breaks(20)
  h2 <- hist(staff_age_density$Age, breaks = breaksx, plot = FALSE)
  plot <- hchart(h2) %>%
    hc_xAxis(type = "category", categories = breaksx, title = list(text = "Age")) %>%
    hc_title(text = "Staff Age Distribution") %>% hc_legend(enabled = F)
  return(plot)
}

generateStaffLangPlot <- function(lang_summary, textsize){
  maxa = max(lang_summary$Count) + 2
  plang <- ggplot(lang_summary, aes(Language, Count,  color=Language, label = Count)) +
    geom_col()  +
    scale_fill_brewer(palette="Dark2") +
    scale_y_continuous(limits = c(0, maxa)) +
    labs(x = "", y="# Applicants", axis.text.x = element_text(angle = 90, hjust = -1, vjust=-1)) +
    geom_text(size=textsize, hjust = 0.5, vjust = -0.5) +
    theme(legend.position="none",
          axis.text.x = element_text(size = textsize, angle = 45, hjust = 1),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  return(plang)
}
age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

staff_birth_ages <- function(filtered_staff_details){
  datex <- sprintf("%s-%s-%s", substring(filtered_staff_details$DATEOFBIRTH, 1, 4), substring(filtered_staff_details$DATEOFBIRTH, 5, 6), substring(filtered_staff_details$DATEOFBIRTH, 7, 8))
  birth_ages <- as.data.frame(age(as.Date(datex), rep(Sys.Date(), length(datex))))
  colnames(birth_ages) <- c('Age')
  return(birth_ages)
}



generatePolyMap <- function(loc, Applicant_Summary, Home_Ag, NonHome_Ag){
  
  #Filled positions
  Filled_Ag_H  <- Home_Ag %>% filter(PositionStatus == "EMPLOYED")
  Filled_Ag_NH <- NonHome_Ag %>% filter(PositionStatus == "EMPLOYED")
  Filled_Ag    <- rbind(Filled_Ag_H, Filled_Ag_NH)
  
  #Offered positions
  Offered_Ag_H  <- Home_Ag %>% filter(PositionStatus == "OFFERED")
  Offered_Ag_NH <- NonHome_Ag %>% filter(PositionStatus == "OFFERED")
  Offered_Ag    <- rbind(Offered_Ag_H, Offered_Ag_NH)
  
  #Home_Ag
  Home_Ag <- Home_Ag %>% filter(PositionStatus == "APPLIED")
  NonHome_Ag <- NonHome_Ag %>% filter(PositionStatus == "APPLIED")
  
  popupHomelabels <- sprintf(
    "<strong>%s</strong><br>%s<br>Applied For: %s" ,
    Home_Ag$FullName
    ,Home_Ag$LOGINID
    ,Home_Ag$Desired) %>% lapply(htmltools::HTML)
  
  popupNonHomelabels <- sprintf(
    "<strong>%s</strong><br>%s<br>Applied For: %s",
    NonHome_Ag$FullName
    ,NonHome_Ag$LOGINID
    ,NonHome_Ag$Desired) %>% lapply(htmltools::HTML)
  
  popupOfferedlabels <- sprintf(
    "<strong>%s</strong><br>%s<br>Applied For: %s",
    Offered_Ag$FullName
    ,Offered_Ag$LOGINID
    ,Offered_Ag$Desired) %>% lapply(htmltools::HTML)
  
  popupFilledlabels <- sprintf(
    "<strong>%s</strong><br>%s<br>Applied For: %s",
    Filled_Ag$FullName
    ,Filled_Ag$LOGINID
    ,Filled_Ag$Desired) %>% lapply(htmltools::HTML)
  
  StaffPolygon_Shape <- readOGR(dsn=paste0(reference_folder,'NSW_compact_boundary'), layer="NSW_compact")
  
  StaffPolygon_Shape@data <- StaffPolygon_Shape@data %>% left_join(Applicant_Summary,
                                                                   by=c('Name'='AreaCode')) %>%
    rename(AreaCode=Name)
  staff_map_polygons <- StaffPolygon_Shape
  
  StaffReportMap <- leaflet(staff_map_polygons,
                            height = 500,
                            width=1600,
                            padding = 0,
                            options = leafletOptions(zoomControl = FALSE,minZoom = 6)) %>%
    setView(150.9376823, -33.8607351, zoom = 10) %>%
    setMaxBounds(lng1 = 131.000333332,
                 lat1 = -18.992662696,
                 lng2 = 175.612793,
                 lat2 = -43.840233) %>%
    addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% addResetMapButton()
  
  #Home
  StaffReportMap <- StaffReportMap %>% addMarkers(Home_Ag,
                                                  lat = Home_Ag$Latitude,
                                                  lng = Home_Ag$Longitude,
                                                  label = Home_Ag$FullName,
                                                  labelOptions = labelOptions(textsize = 15),
                                                  popup = popupHomelabels,
                                                  icon = black_icon,
                                                  group = 'Home')
  #NonHome
  StaffReportMap <- StaffReportMap %>% addMarkers(NonHome_Ag,
                                                  lat = NonHome_Ag$Latitude,
                                                  lng = NonHome_Ag$Longitude,
                                                  label = NonHome_Ag$VenueName,
                                                  popup = popupNonHomelabels,
                                                  icon = red_icon,
                                                  group = 'NonHome')
  
  #Offered
  if(nrow(Offered_Ag) > 0){
    StaffReportMap <- StaffReportMap %>% addMarkers(Offered_Ag,
                                                    lat = Offered_Ag$Latitude,
                                                    lng = Offered_Ag$Longitude,
                                                    label = Offered_Ag$VenueName,
                                                    popup = popupOfferedlabels,
                                                    icon = blue_icon,
                                                    group = 'Offered')
  }
  
  #Filled
  if(nrow(Filled_Ag) >0){
    StaffReportMap <- StaffReportMap %>% addMarkers(Filled_Ag,
                                                    lat = Filled_Ag$Latitude,
                                                    lng = Filled_Ag$Longitude,
                                                    label = Filled_Ag$VenueName,
                                                    popup = popupFilledlabels,
                                                    icon = green_icon,
                                                    group = 'Filled')
  }
  
  StaffReportMap <- StaffReportMap %>%
    addControl(html = html_legend, position = "bottomleft")
  
  # number of votes by district (summing prepoll)
  staff_map_polygons@data <- staff_map_polygons@data %>% left_join(Home_Ag %>% group_by(AreaCode) %>%
                                                                     summarise(People = n())
                                                                   ,by = 'AreaCode')
  
  labels <- sprintf(
    "<strong>%s</strong><br>Remaining Applicants: %s<br/>",
    staff_map_polygons$AreaCode,staff_map_polygons$People) %>% lapply(htmltools::HTML)
  
  StaffReportMap <- StaffReportMap %>% addPolygons(
    color= "#000000",
    fillColor = pal(staff_map_polygons$People),
    weight = 2,
    fillOpacity = 0.3,
    popup = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"),
    group = 'District') %>%
    addLegend('bottomright',pal = pal, values=staff_map_polygons$People,
              title = 'Number of applicants',
              opacity = 0.5)
  
  selected_polygon <- subset(staff_map_polygons,staff_map_polygons$AreaCode==loc)
  polygon_labelPt <- selected_polygon@polygons[[1]]@labpt
  StaffReportMap <- StaffReportMap %>% removeShape("highlighted_polygon")
  StaffReportMap <- StaffReportMap %>% setView(lng=polygon_labelPt[1],lat=polygon_labelPt[2],zoom=7)
  StaffReportMap <- StaffReportMap %>% addPolylines(stroke = TRUE,
                                                    fill = FALSE,
                                                    weight = 2,
                                                    fillOpacity = 0.5,
                                                    color = "red",
                                                    fillColor = "red",
                                                    data = selected_polygon,
                                                    group = "highlighted_polygon")
  
  ## Map
  StaffReportMap <- StaffReportMap %>%
    addSearchFeatures(
      targetGroups = c('Home','Non-Home'),
      options = searchFeaturesOptions(
        zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
        autoCollapse = TRUE, hideMarkerOnCollapse = TRUE )) %>%
    addLayersControl(
      overlayGroups = c("Home", "Non-Home"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  return(setView(map=StaffReportMap,lng=polygon_labelPt[1],lat=polygon_labelPt[2],zoom=12))
}
