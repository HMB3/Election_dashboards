####################################################################################################################
###################################### DASHBAORD FUNCTIONS ---- ####################################################
####################################################################################################################



## Reverse of '%in%'
'%!in%' <- function(x,y)!('%in%'(x,y))



## GGPLOT GENERIC FUNCTIONS  ===============================================================



## Bar chart horozontally stacked, few categories ----
dash_bar_chart_small = function(df,       title,     xvar,     yvar,  fill_var, colours,
                                ylab,     xlab,      ymin,     ymax,  leg_pos,  h_just,
                                tsize,    caption,   xsize,    ysize,  ycol,
                                lab_size, capt_size, leg_size, axis_size, mar) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
                 aes(
                   x    = reorder(!!sym(xvar), !!sym(yvar)),
                   y    = !!sym(yvar),
                   fill = !!sym(xvar))) +
    geom_bar(stat = "identity") +
    
    coord_flip() +
    #scale_fill_manual(values = colours, na.value = "grey", drop = FALSE) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    
    geom_text(aes(label   = !!sym(yvar), hjust = h_just),
              position    = position_dodge(width = 1),
              inherit.aes = TRUE,
              size        = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size  = xsize),
          axis.title.x    = element_text(size  = xsize, face = "bold", vjust=-5),
          
          axis.title.y    = element_blank(),
          axis.text.y     = element_blank(),
          axis.ticks.y    = element_blank(),
          axis.line       = element_line(colour = 'black', size = axis_size),
          
          legend.position = leg_pos,
          legend.text     = element_text(size = leg_size),
          legend.title    = element_blank(),
          
          plot.subtitle   = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption    = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Bar chart vertically stacked and ordered by x value ----
dash_bar_chart_xvar = function(df,       title,     xvar,      yvar,  ordervar, h_just, colours,
                               ylab,     xlab,      ymin,      ymax,  leg_pos,
                               tsize,    caption,   xsize,     ysize, ycol,
                               lab_size, capt_size, axis_size, mar,   pan_mar) {
  
  ## Use variable names for x and y
  plot  <- ggplot(df,
                  aes(x    = reorder(!!sym(xvar), !!sym(ordervar)),
                      y    = !!sym(yvar),
                      fill = !!sym(xvar))) +
    
    geom_bar(stat = "identity") +
    
    coord_flip() +
    scale_fill_manual(values = colours, na.value = "grey") +
    scale_x_discrete(drop = FALSE) +
    
    geom_text(aes(label   = !!sym(yvar), hjust = h_just),
              position    = position_dodge(width = 1),
              inherit.aes = TRUE,
              size        = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          panel.spacing   = unit(pan_mar, "lines"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          axis.line       = element_line(colour = 'black', size = axis_size),
          legend.position = leg_pos,
          
          axis.title.y  = element_text(size = ysize, face  = "bold",   vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}



## Bar chart vertically stacked and ordered by x value ----
dash_bar_chart_xvar_facet = function(df,       title,     xvar,      yvar,  h_just, colours,
                                     ylab,     xlab,      ymin,      ymax,  leg_pos,
                                     tsize,    caption,   xsize,     ysize, ycol, pan_mar,
                                     lab_size, capt_size, axis_size, group_var, mar, wrap_scale,
                                     facet_cols, facet_rows) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
                 aes(x    = !!sym(xvar), !!sym(yvar),
                     y    = !!sym(yvar),
                     fill = !!sym(xvar))) +
    
    geom_bar(stat = "identity") +
    
    coord_flip() +
    scale_fill_manual(values = colours, na.value = "grey") +
    scale_x_discrete(drop = FALSE) +
    
    geom_text(aes(label   = !!sym(yvar), hjust = h_just),
              position    = position_dodge(width = 1),
              inherit.aes = TRUE,
              size        = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          strip.text      = element_text(size  = tsize, face = 'bold'),
          panel.spacing   = unit(pan_mar, "lines"),
          
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          axis.line       = element_line(colour = 'black', size = axis_size),
          legend.position = leg_pos,
          
          axis.title.y  = element_text(size = ysize, face  = "bold",   vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black")) +
    
    facet_wrap(as.formula(paste("~", group_var)), scales = wrap_scale, ncol = facet_cols, nrow = facet_rows)
  
  return(plot)
  
}





## Bar chart vertically stacked and ordered by y value ----
dash_bar_chart_yvar = function(df,       title,     xvar,    yvar,
                               ylab,     xlab,      ymin,    ymax,
                               tsize,    caption,   xsize,   ysize, ycol,
                               lab_size, capt_size, leg_pos, mar) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
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
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          legend.position = leg_pos,
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Bar chart horizontally stacked, ordered by date ----
date_chart_two_factor = function(df,       title,     xvar,     yvar,      fill_var, lab_angle,    colours,
                                 ylab,     xlab,      ymin,     ymax,      leg_pos,  date_break,   x_int,
                                 tsize,    caption,   xsize,    ysize,     xtitle,   ytitle, ycol, h_just,
                                 lab_size, capt_size, leg_size, axis_size, mar) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
                 aes(x    = reorder(!!sym(xvar), !!sym(xvar)),
                     y    = !!sym(yvar),
                     fill = !!sym(fill_var))) +
    
    geom_bar(stat     = "identity",  width = 0.4, position = position_dodge(width = 1.5)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = colours, na.value = "grey") +
    
    ## Control the date
    scale_x_discrete(breaks = every_nth(n = x_int)) +
    #scale_x_date(date_labels = "%d-%m", date_breaks = date_break) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize,    angle = lab_angle, hjust = h_just),
          axis.title.x    = element_text(size   = xtitle,   face = "bold", vjust =-5),
          axis.line       = element_line(colour = 'black', size = axis_size),
          
          legend.position = leg_pos,
          legend.text     = element_text(size = leg_size),
          legend.title    = element_blank(),
          
          axis.title.y    = element_text(size = ytitle,  face = "bold", vjust = 4),
          axis.text.y     = element_text(size = ysize, color = ycol),
          plot.subtitle   = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption    = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Bar chart vertically stacked, ordered by value, no x-axis ----
dash_bar_chart_two_factor = function(df,       title,     xvar,     yvar,  fill_var, colours,
                                     ylab,     xlab,      ymin,     ymax,  leg_pos,
                                     tsize,    caption,   xsize,    ysize, ycol, h_just,
                                     lab_size, capt_size, leg_size, axis_size, mar) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
                 aes(x    = reorder(!!sym(xvar), !!sym(yvar)),
                     y    = !!sym(yvar),
                     fill = !!sym(fill_var))) +
    
    #geom_bar(stat = "identity") +
    geom_col(position = "dodge") +
    
    coord_flip() +
    scale_fill_manual(values = colours, na.value = "grey") +
    scale_x_discrete(drop = FALSE) +
    
    geom_text(aes(label = !!sym(yvar), hjust = h_just),
              position  = position_dodge(width = 1),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_blank(),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          axis.line       = element_line(colour = 'black', size = axis_size),
          
          legend.position = leg_pos,
          legend.text     = element_text(size = leg_size),
          legend.title    = element_blank(),
          
          axis.title.y  = element_text(size = ysize, face = "bold", hjust = 5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Bar chart vertically stacked, ordered by value, no x-axis ----
dash_bar_chart_xblank = function(df,       title,     xvar,  yvar,  h_just, colours,
                                 ylab,     xlab,      ymin,  ymax,  leg_pos,
                                 tsize,    caption,   xsize, ysize, ycol,
                                 lab_size, capt_size, axis_size, mar) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
                 aes(x    = !!sym(xvar), !!sym(yvar),
                     y    = !!sym(yvar),
                     fill = !!sym(xvar))) +
    
    geom_bar(stat = "identity") +
    
    coord_flip() +
    scale_fill_manual(values = colours, na.value = "grey") +
    #scale_color_brewer(palette = "Dark2") +
    scale_x_discrete(drop = FALSE) +
    
    geom_text(aes(label   = !!sym(yvar), hjust = h_just),
              position    = position_dodge(width = 1),
              inherit.aes = TRUE,
              size        = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_blank(),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          legend.position = leg_pos,
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}



## Bar chart vertically stacked, ordered by value, no x-axis ----
dash_bar_chart_yblank = function(df,       title,     xvar,  yvar,  h_just, colours,
                                 ylab,     xlab,      ymin,  ymax,  leg_pos,
                                 tsize,    caption,   xsize, ysize, ycol,
                                 lab_size, capt_size, axis_size, mar) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
                 aes(x    = !!sym(xvar), !!sym(yvar),
                     y    = !!sym(yvar),
                     fill = !!sym(xvar))) +
    
    geom_bar(stat = "identity") +
    
    coord_flip() +
    scale_fill_manual(values = colours, na.value = "grey") +
    scale_x_discrete(drop = FALSE) +
    
    geom_text(aes(label   = !!sym(yvar), hjust = h_just),
              position    = position_dodge(width = 1),
              inherit.aes = TRUE,
              size        = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          axis.line       = element_line(colour = 'black', size = axis_size),
          legend.position = leg_pos,
          
          axis.title.y  = element_blank(),
          axis.text.y   = element_blank(),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}




## Bar chart vertically stacked and ordered by factor ----
dash_bar_chart_yvar_samecol_facet = function(df,         title,     xvar,  yvar,   ordervar, h_just, colour,
                                             ylab,       xlab,      ymin,  ymax,   leg_pos,
                                             tsize,      caption,   xsize, ysize,  ycol,
                                             lab_size,   capt_size, axis_size,     mar, pan_mar, group_var,
                                             wrap_scale, date_labs, date_break,    facet_cols, facet_rows) {
  
  ## Use variable names for x and y
  plot  <- ggplot(df,
                  aes(x    = reorder(!!sym(xvar), !!sym(ordervar)),
                      y    = !!sym(yvar),
                      fill = !!sym(xvar))) +
    
    geom_bar(stat = "identity", fill = colour) +
    
    coord_flip() +
    scale_x_discrete(drop = FALSE) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          strip.text      = element_text(size  = tsize, face = 'bold'),
          panel.spacing   = unit(pan_mar, "lines"),
          
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          axis.line       = element_line(colour = 'black', size = axis_size),
          legend.position = leg_pos,
          
          axis.title.y  = element_text(size = ysize, face  = "bold",   vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black")) +
    
    facet_wrap(as.formula(paste("~", "`", group_var, "`", sep = "")), scales = wrap_scale, ncol = facet_cols, nrow = facet_rows)
  
  return(plot)
  
}




## Bar chart vertically stacked and ordered by factor ----
dash_bar_chart_yvar_samecol_lab_facet = function(df,         title,     xvar,  yvar,   ordervar, h_just, colour,
                                                 ylab,       xlab,      ymin,  ymax,   leg_pos,
                                                 tsize,      caption,   xsize, ysize,  ycol,
                                                 lab_size,   capt_size, axis_size,     mar, pan_mar, group_var,
                                                 wrap_scale, date_labs, date_break,    facet_cols, facet_rows) {
  
  ## Use variable names for x and y
  plot  <- ggplot(df,
                  aes(x    = reorder(!!sym(xvar), !!sym(ordervar)),
                      y    = !!sym(yvar),
                      fill = !!sym(xvar))) +
    
    geom_bar(stat = "identity", fill = colour) +
    
    coord_flip() +
    scale_x_discrete(drop = FALSE) +
    
    geom_text(aes(label   = !!sym(yvar), hjust = h_just),
              position    = position_dodge(width = 1),
              inherit.aes = TRUE,
              size        = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          strip.text      = element_text(size  = tsize, face = 'bold'),
          panel.spacing   = unit(pan_mar, "lines"),
          
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          axis.line       = element_line(colour = 'black', size = axis_size),
          legend.position = leg_pos,
          
          axis.title.y  = element_text(size = ysize, face  = "bold",   vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black")) +
    
    facet_wrap(as.formula(paste("~", "`", group_var, "`", sep = "")), scales = wrap_scale, ncol = facet_cols, nrow = facet_rows)
  
  return(plot)
  
}





## Bar chart vertically stacked and ordered by factor ----
stacked_bar_chart_xvar = function(df,    title, xvar, yvar,  fill_var, colours,
                                  ylab,  xlab,  ymin, ymax, leg_size, axis_size,
                                  tsize, caption, xsize, ysize, ycol, leg_pos,
                                  capt_size, mar) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
                 aes(x    = !!sym(xvar),
                     y    = !!sym(yvar),
                     fill = !!sym(fill_var))) +
    
    geom_bar(stat = "identity") +
    
    scale_color_brewer(palette = "Dark2") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_x_discrete(drop = FALSE) +
    coord_flip() +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size  = xsize),
          axis.title.x    = element_text(size  = xsize, face = "bold", vjust=-5),
          
          axis.title.y    = element_blank(),
          axis.text.y     = element_blank(),
          axis.ticks.y    = element_blank(),
          axis.line       = element_line(colour = 'black', size = axis_size),
          
          legend.position = leg_pos,
          legend.text     = element_text(size = leg_size),
          legend.title    = element_blank(),
          
          plot.subtitle   = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption    = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}




## Bar chart vertically stacked and ordered by factor ----
stacked_bar_chart_blank = function(df,        title,   xvar,  yvar,  fill_var, colours,
                                   ylab,      xlab,    ymin,  ymax,  leg_size, axis_size,
                                   tsize,     caption, xsize, ysize, ycol,     leg_pos,
                                   capt_size, mar) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
                 aes(x    = !!sym(xvar),
                     y    = !!sym(yvar),
                     fill = !!sym(fill_var))) +
    
    geom_bar(stat = "identity") +
    
    scale_fill_manual(values = colours, na.value = "grey") +
    scale_x_discrete(drop = FALSE) +
    coord_flip() +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_blank(),
          axis.title.x    = element_text(size  = xsize, face = "bold", vjust=-5),
          panel.grid      = element_blank(),
          
          axis.title.y    = element_blank(),
          axis.text.y     = element_blank(),
          axis.ticks.y    = element_blank(),
          axis.line       = element_line(colour = 'black', size = axis_size),
          
          legend.position = leg_pos,
          legend.text     = element_text(size = leg_size),
          legend.title    = element_blank(),
          
          plot.subtitle   = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption    = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Bar chart horizontal with factor ---- 
dash_bar_chart_factor = function(df,        title,   xvar,  yvar,      factor, width, colours,
                                 ylab,      xlab,    ymin,  ymax,      leg_pos,
                                 tsize,     caption, xsize, ysize,     ycol, 
                                 lab_size,  hjust,   vjust, lab_angle, leg_size,
                                 capt_size, mar) {
  
  ## Use variable names for x and y
  plot <- ggplot(df, 
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
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size = xsize, face = "bold", hjust = 2),
          legend.position = leg_pos,
          legend.title    = element_blank(),
          legend.text     = element_text(size = leg_size),
          
          axis.line       = element_line(colour = 'black', size = width),
          axis.ticks      = element_line(colour = "black", size = width),
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}




## Grouped line plot ordered by y-value ----
group_line_plot_order_y = function(df,       title, subtitle,  caption,
                                   xvar,     yvar,  line_size, point_size,
                                   ylab,     xlab,  group_var, lab_angle, leg_size, 
                                   tsize,    xsize, ysize, ycol,
                                   lab_size, capt_size, mar) {
  
  ## Use variable names for x and y
  ## Replaced group = 1
  plot <- ggplot(df,
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
    
    theme(plot.margin   = unit(c(mar, mar, mar, mar), "cm"),
          plot.title    = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x   = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x  = element_text(size  = xsize, face  = "bold",    vjust = -5),
          legend.text   = element_text(size  = leg_size),
          legend.title  = element_blank(),
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Grouped line plot ordered by y-value ----
group_line_plot = function(df,        title,      subtitle, caption, xvar,      yvar,       colours,
                           tsize,     capt_size,  xsize,    ysize,   line_size, point_size, leg_size, 
                           ylab,      xlab, ycol, leg_pos,
                           group_var, lab_angle,  lab_size, mar) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
                 aes(
                   x = !!sym(xvar),
                   y = !!sym(yvar), 
                   group = !!sym(group_var), colour = !!sym(group_var))) +
    
    geom_line(size  = line_size) +
    geom_point(size = point_size, colour = 'grey') +
    scale_fill_manual(values = colours, na.value = "grey") +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ## Add Title, sub-title and caption
    labs(label    = title,
         subtitle = subtitle,
         caption  = caption) +
    
    ggtitle(title) +
    
    ## Add themes
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(size = tsize, face = "bold"),
          plot.subtitle   = element_text(size  = capt_size, hjust = 0.5,   face = "italic", color = "black"),
          plot.caption    = element_text(size  = capt_size, hjust = 0.5,   face = "italic", color = "Grey"),
          
          axis.text.x     = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -0.5),
          legend.text     = element_text(size  = leg_size),
          legend.title    = element_blank(),
          legend.position = leg_pos,
          
          axis.title.y    = element_text(size = ysize, face  = "bold", vjust = 3),
          axis.text.y     = element_text(size = ysize, color = ycol))
  
  return(plot)
  
}





## Grouped line plot ordered by date ----
group_line_plot_date = function(df,        data_ends,  title,   subtitle,   caption,   xvar, yvar, colours,
                                tsize,     capt_size,  xsize,   ysize,      line_size, point_size, leg_size, 
                                ylab,      xlab, ycol, leg_pos, date_break, date_labs,
                                group_var, lab_angle,  lab_size, mar) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
                 aes(
                   x = !!sym(xvar),
                   y = !!sym(yvar), 
                   group = !!sym(group_var), colour = !!sym(group_var))) +
    
    geom_line(size  = line_size) +
    geom_point(size = point_size, colour = 'grey') +
    scale_fill_manual(values = colours, na.value = "grey") +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    labs(caption = caption) +
    
    ## Control the date, add label at the ends of the axis
    scale_x_date(date_labels    = date_labs, date_breaks = date_break) +
    scale_y_continuous(sec.axis = sec_axis(~ .,   breaks = data_ends)) +
    
    ## Add Title, sub-title and caption
    labs(label    = title,
         subtitle = subtitle,
         caption  = caption) +
    
    ## Add themes
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5,          size = tsize, face = "bold"),
          plot.subtitle   = element_text(size  = capt_size, hjust = 0.5,   face = "italic", color = "black"),
          plot.caption    = element_text(size  = capt_size, hjust = 0.5,   face = "italic", color = "Grey"),
          
          axis.text.x     = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -0.5),
          legend.text     = element_text(size  = leg_size),
          legend.title    = element_blank(),
          legend.position = leg_pos,
          
          axis.title.y    = element_text(size = ysize, face  = "bold", vjust = 3),
          axis.text.y     = element_text(size = ysize, color = ycol))
  
  return(plot)
  
}





## Grouped line plot ordered by date ----
group_line_plot_date_facet = function(df,        ylast,      title,    subtitle,   caption,   xvar, yvar, colours,
                                      tsize,     capt_size,  xsize,    ysize,      line_size, point_size, leg_size, 
                                      ylab,      xlab, ycol, leg_pos,  date_break, date_labs,
                                      group_var, lab_angle,  lab_size, mar, pan_mar) {
  
  ## Use variable names for x and y
  plot <- ggplot(df, aes(!!sym(xvar), !!sym(yvar)), group = 1) +
    geom_line(size = line_size, group = 1) +
    
    geom_line(size  = line_size,   group = 1, colour = 'coral') +
    geom_point(size = point_size, colour = 'grey') +
    scale_fill_manual(values = colours, na.value = "grey") +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    labs(caption = caption) +
    
    ## Control the date, add label at the ends of the axis
    scale_x_date(date_labels    = date_labs, date_breaks = date_break) +
    scale_y_continuous(sec.axis = sec_axis(~ .,   breaks = ylast)) +
    
    ## Add Title, sub-title and caption
    labs(label    = title,
         subtitle = subtitle,
         caption  = caption) +
    
    ## Add themes
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5,     size = tsize, face = "bold"),
          strip.text      = element_text(size  = tsize, face = 'bold'),
          panel.spacing   = unit(pan_mar, "lines"),
          
          plot.subtitle   = element_text(size  = capt_size, hjust = 0.5,   face = "italic", color = "black"),
          plot.caption    = element_text(size  = capt_size, hjust = 0.5,   face = "italic", color = "Grey"),
          
          axis.text.x     = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -0.5),
          legend.text     = element_text(size  = leg_size),
          legend.title    = element_blank(),
          legend.position = leg_pos,
          
          axis.title.y    = element_text(size = ysize, face  = "bold", vjust = 3),
          axis.text.y     = element_text(size = ysize, color = ycol)) +
    
    ## Facet by vote type
    facet_wrap(as.formula(paste("~", group_var)))
  return(plot)
  
}


## Only display every nth y-axis value
every_nth = function(n) {
  
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
  
}





## Grouped line plot both axes ----
group_line_plot_skip_x_axes = function(df,       title,     xvar,       yvar,        line_size, point_size, x_int,
                                       ylab,     xlab,      group_var,  lab_angle,   leg_size, 
                                       tsize,    caption,   xsize,      ysize, ycol, axis_size,
                                       lab_size, capt_size, leg_pos,    mar) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
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
    
    ## Limit the number of x axis values if the axis is cluttered,
    ## EG if there are too many dates
    scale_x_discrete(breaks = every_nth(n = x_int)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x     = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -0.5),
          legend.text     = element_text(size  = leg_size),
          axis.line       = element_line(colour = 'black', size = axis_size),
          legend.position = leg_pos,
          legend.title    = element_blank(),
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 3),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Grouped line plot both axes ----
group_line_plot_y_axes = function(df,       title,     xvar,      yvar,      line_size, point_size,
                                  ylab,     xlab,      group_var, lab_angle, leg_size,
                                  tsize,    caption,   xsize,     ysize,     ycol,
                                  lab_size, capt_size, leg_pos,   mar) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
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
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x     = element_blank(),
          axis.title.x    = element_blank(),
          legend.text     = element_text(size  = leg_size),
          legend.title    = element_blank(),
          legend.position = leg_pos,
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Single line plot ----
single_line_plot_date = function(df,       title,     xvar,      yvar, ylast,
                                 ylab,     xlab,      date_labs, date_break, lab_angle, line_size, point_size,
                                 tsize,    caption,   xsize,     ysize,      axis_size,
                                 lab_size, capt_size, leg_size,  leg_pos,    mar) {
  
  ## Use variable names for x and y
  plot <- ggplot(df, aes(!!sym(xvar), !!sym(yvar)), group = 1) +
    geom_line(size = line_size, group = 1) +
    
    geom_line(size  = line_size,   group = 1, colour = 'coral') +
    geom_point(size = point_size, colour = 'grey') +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    # ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption,
         title   = paste(title, " (up to " , 
                         format(Sys.Date(), format = "%d-%b-%y"),")", "\n", sep = "")) +
    
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = ylast)) +
    scale_x_date(date_labels    = date_labs, date_breaks = date_break) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x     = element_text(size  = xsize, angle = lab_angle, hjust = 1, vjust = 0.5),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -5),
          axis.line       = element_line(colour = 'black', size = axis_size),
          legend.position = leg_pos,
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}



## Single line plot ----
single_line_plot_date_facet = function(df,       title,     xvar,      yvar,       group_var,
                                       ylab,     xlab,      date_labs, date_break, lab_angle, line_size, point_size,
                                       tsize,    caption,   xsize,     ysize,      axis_size,
                                       lab_size, capt_size, leg_size,  leg_pos,    mar, pan_mar, wrap_scale) {
  
  ## Use variable names for x and y
  plot <- ggplot(df, aes(!!sym(xvar), !!sym(yvar)), group = 1) +
    geom_line(size = line_size, group = 1) +
    
    geom_line(size  = line_size,   group = 1, colour = 'coral') +
    geom_point(size = point_size, colour = 'grey') +
    
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    #scale_y_continuous(sec.axis = sec_axis(~ ., breaks = ylast)) +
    scale_x_date(date_labels    = date_labs, date_breaks = date_break) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          strip.text      = element_text(size  = tsize, face = 'bold'),
          panel.spacing   = unit(pan_mar, "lines"),
          
          axis.text.x     = element_text(size  = xsize, angle = lab_angle, hjust = 1, vjust = 0.5),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -5),
          axis.line       = element_line(colour = 'black', size = axis_size),
          legend.position = leg_pos,
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black")) +
    
    ## Create a label for the max value after subsetting by the facet variable
    geom_label(data = df %>% group_by_(.dots = group_var) %>%
                 filter(!!yvar == max(!!yvar)),
               aes(label = !!sym(yvar)), 
               hjust = 1.5, size = lab_size) +
    
    facet_wrap(as.formula(paste("~", group_var)), scales = wrap_scale)
  return(plot)
  
}




## Single line plot ----
single_line_plot_facet = function(df,       title,     xvar, yvar, ylast,
                                  ylab,     xlab,      date_labs,  date_break, lab_angle, line_size, point_size,
                                  tsize,    caption,   xsize,      ysize,      axis_size,
                                  lab_size, capt_size, leg_size,   leg_pos,    mar, wrap_scale) {
  
  ## Use variable names for x and y
  plot <- ggplot(df, aes(!!sym(xvar), !!sym(yvar)), group = 1) +
    geom_line(size = line_size, group = 1) +
    
    geom_line(size  = line_size,   group = 1, colour = 'coral') +
    geom_point(size = point_size, colour = 'grey') +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = ylast)) +
    scale_x_date(date_labels    = date_labs, date_breaks = date_break) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x     = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -5),
          axis.line       = element_line(colour = 'black', size = axis_size),
          legend.position = leg_pos,
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black")) +
    
    ## Add facets
    facet_wrap(as.formula(paste("~", group_var)), scales = wrap_scale)
  return(plot)
  
}





## Histogram ----
plot_histogram = function(df,   title,  xvar,
                          bin, col_var,
                          ylab, xlab,  lab_angle, line_size, point_size,
                          tsize, caption, xsize, ysize, 
                          lab_size, capt_size, leg_size, leg_pos, mar) {
  
  
  ## Use the 'SOURCE' column to create a histogram for each source.
  plot <- ggplot(df, 
                 aes(!!sym(xvar), 
                     fill = !!sym(col_var), 
                     color = !!sym(col_var), 
                     group = !!sym(col_var))) +
    
    geom_histogram(position = "identity",
                   alpha = 0.3,
                   binwidth = bin) +
    
    ## 
    theme_light(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x     = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -5),
          legend.position = leg_pos,
          legend.title    = element_blank(),
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}




