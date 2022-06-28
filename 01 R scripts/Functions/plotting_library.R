####################################################################################################################
###################################### DASHBAORD FUNCTIONS ---- ####################################################
####################################################################################################################



## Reverse of '%in%'
'%!in%' <- function(x,y)!('%in%'(x,y))



## GGPLOT GENERIC FUNCTIONS  ===============================================================



## Bar chart horozontally stacked, few categories ----
dash_bar_chart_small = function(df, title, xvar, yvar, fill_var, colours,
                                ylab, xlab, ymin, ymax, leg_pos, h_just,
                                tsize, caption, xsize, ysize, ycol,
                                lab_size, capt_size, leg_size, axis_size) {
  
  ## Use variable names for x and y
  ggplot(df,
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
    
    theme(plot.margin     = unit(c(0.5,0.5,0.5,0.5), "cm"),
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
  
}





## Bar chart vertically stacked and ordered by x value ----
dash_bar_chart_xvar = function(df, title, xvar, yvar, h_just, colours,
                               ylab, xlab, ymin, ymax, leg_pos,
                               tsize, caption, xsize, ysize, ycol,
                               lab_size, capt_size, axis_size) {
  
  ## Use variable names for x and y
  ggplot(df,
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
    
    theme(plot.margin     = unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          axis.line       = element_line(colour = 'black', size = axis_size),
          legend.position = leg_pos,
          
          axis.title.y  = element_text(size = ysize, face  = "bold",   vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
}





## Bar chart vertically stacked and ordered by y value ----
dash_bar_chart_yvar = function(df, title, xvar, yvar,
                               ylab, xlab, ymin, ymax,
                               tsize, caption, xsize, ysize, ycol,
                               lab_size, capt_size, leg_pos) {
  
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
          legend.position = leg_pos,
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
}





## Bar chart horizontally stacked, ordered by date ----
horizontal_bar_chart_two_factor = function(df, title, xvar, yvar, fill_var,
                                           ylab, xlab, ymin, ymax, leg_pos,
                                           tsize, caption, xsize, ysize, ycol, h_just,
                                           lab_size, capt_size, leg_size, axis_size) {
  
  ## Use variable names for x and y
  ggplot(df,
         aes(x    = reorder(!!sym(xvar), !!sym(yvar)),
             y    = !!sym(yvar),
             fill = !!sym(fill_var))) +
    
    #geom_bar(stat = "identity") +
    geom_col(position = "dodge") +
    
    #coord_flip() +
    scale_color_brewer(palette = "Dark2") +
    
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
    
    theme(plot.margin     = unit(c(0.5,0.5,0.5,0.5), "cm"),
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
  
}





## Bar chart vertically stacked, ordered by value, no x-axis ----
dash_bar_chart_two_factor = function(df, title, xvar, yvar, fill_var,
                                     ylab, xlab, ymin, ymax, leg_pos,
                                     tsize, caption, xsize, ysize, ycol, h_just,
                                     lab_size, capt_size, leg_size, axis_size) {
  
  ## Use variable names for x and y
  ggplot(df,
         aes(x    = reorder(!!sym(xvar), !!sym(yvar)),
             y    = !!sym(yvar),
             fill = !!sym(fill_var))) +
    
    #geom_bar(stat = "identity") +
    geom_col(position = "dodge") +
    
    coord_flip() +
    scale_color_brewer(palette = "Dark2") +
    
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
    
    theme(plot.margin     = unit(c(0.5,0.5,0.5,0.5), "cm"),
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
  
}





## Bar chart vertically stacked, ordered by value, no x-axis ----
dash_bar_chart_xblank = function(df, title, xvar, yvar, h_just, colours,
                                 ylab, xlab, ymin, ymax, leg_pos,
                                 tsize, caption, xsize, ysize, ycol,
                                 lab_size, capt_size, axis_size) {
  
  ## Use variable names for x and y
  ggplot(df,
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
    
    theme(plot.margin     = unit(c(0.5,0.5,0.5,0.5), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_blank(),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          legend.position = leg_pos,
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
}



## Bar chart vertically stacked, ordered by value, no x-axis ----
dash_bar_chart_yblank = function(df, title, xvar, yvar, h_just, colours,
                                 ylab, xlab, ymin, ymax, leg_pos,
                                 tsize, caption, xsize, ysize, ycol,
                                 lab_size, capt_size, axis_size) {
  
  ## Use variable names for x and y
  ggplot(df,
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
    
    theme(plot.margin     = unit(c(0.5,0.5,0.5,0.5), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          axis.line       = element_line(colour = 'black', size = axis_size),
          legend.position = leg_pos,
          
          axis.title.y  = element_blank(),
          axis.text.y   = element_blank(),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  
}




## Bar chart vertically stacked and ordered by factor ----
dash_bar_chart_yvar_samecol = function(df, title, xvar, yvar,  h_just, colour,
                                       ylab, xlab, ymin, ymax, leg_pos,
                                       tsize, caption, xsize, ysize, ycol,
                                       lab_size, capt_size, axis_size) {
  
  ## Use variable names for x and y
  ggplot(df,
         aes(x    = reorder(!!sym(xvar), !!sym(yvar)),
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
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(0.5,0.5,0.5,0.5), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          axis.line       = element_line(colour = 'black', size = axis_size),
          legend.position = leg_pos,
          
          axis.title.y  = element_text(size = ysize, face  = "bold",   vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
}





## Bar chart vertically stacked and ordered by factor ----
stacked_bar_chart_xvar = function(df, title, xvar, yvar,  fill_var, colours,
                                  ylab, xlab, ymin, ymax, leg_size, axis_size,
                                  tsize, caption, xsize, ysize, ycol, leg_pos,
                                  capt_size) {
  
  ## Use variable names for x and y
  ggplot(df,
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
    
    theme(plot.margin     = unit(c(0.5,0.5,0.5,0.5), "cm"),
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
  
}




## Bar chart vertically stacked and ordered by factor ----
stacked_bar_chart_blank = function(df, title, xvar, yvar,  fill_var, colours,
                                   ylab, xlab, ymin, ymax, leg_size, axis_size,
                                   tsize, caption, xsize, ysize, ycol, leg_pos,
                                   capt_size) {
  
  ## Use variable names for x and y
  ggplot(df,
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
    
    theme(plot.margin     = unit(c(0.5,0.5,0.5,0.5), "cm"),
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
  
}





## Bar chart horizontal with factor ---- 
dash_bar_chart_factor = function(df, title, xvar, yvar, factor, width, colours,
                                 ylab, xlab, ymin, ymax, leg_pos,
                                 tsize, caption, xsize, ysize, ycol, 
                                 lab_size, hjust, vjust, lab_angle, leg_size,
                                 capt_size) {
  
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
          legend.position = leg_pos,
          legend.title    = element_blank(),
          legend.text     = element_text(size = leg_size),
          
          axis.line       = element_line(colour = 'black', size = width),
          axis.ticks      = element_line(colour = "black", size = width),
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
}




## Grouped line plot ordered by y-value ----
group_line_plot_order_y = function(df,       title, subtitle, caption,
                                   xvar, yvar, line_size, point_size,
                                   ylab,     xlab, group_var, lab_angle,   leg_size, 
                                   tsize,    xsize, ysize, ycol, y_break,
                                   lab_size, capt_size) {
  
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





## Grouped line plot ordered by y-value ----
group_line_plot = function(df,        title, subtitle, caption, xvar, yvar, 
                           tsize,     capt_size, xsize, ysize,  line_size, point_size, leg_size, 
                           ylab,      xlab, ycol, y_break, leg_pos,
                           group_var, lab_angle, lab_size) {
  
  ## Use variable names for x and y
  ggplot(df,
         aes(
           x = !!sym(xvar),
           y = !!sym(yvar), group = !!sym(group_var), colour = !!sym(group_var))) +
    
    geom_line(size  = line_size) +
    geom_point(size = point_size, colour = 'grey') +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ## Add Title, sub-title and caption
    labs(label    = title,
         subtitle = subtitle,
         caption  = caption) +
    
    ## Add themes
    theme(plot.margin     = unit(c(1, 1, 1, 1), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          plot.subtitle   = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption    = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "Grey"),
          
          axis.text.x     = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -0.5),
          legend.text     = element_text(size  = leg_size),
          legend.title    = element_blank(),
          legend.position = leg_pos,
          
          axis.title.y    = element_text(size = ysize, face  = "bold", vjust = 3),
          axis.text.y     = element_text(size = ysize, color = ycol))
  
}


## Only display every nth y-axis value
every_nth = function(n) {
  
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
  
}





## Grouped line plot both axes ----
group_line_plot_skip_x_axes = function(df,       title, xvar, yvar, line_size, point_size, x_int,
                                       ylab,     xlab, group_var, lab_angle,   leg_size, 
                                       tsize,    caption, xsize, ysize, ycol,  axis_size,
                                       lab_size, capt_size, leg_pos) {
  
  ## Use variable names for x and y
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
    
    ## Limit the number of x axis values if the axis is cluttered,
    ## EG if there are too many dates
    scale_x_discrete(breaks = every_nth(n = x_int)) +
    
    theme(plot.margin     = unit(c(1, 1, 1, 1), "cm"),
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
  
}





## Grouped line plot both axes ----
group_line_plot_y_axes = function(df,       title, xvar, yvar, line_size, point_size,
                                  ylab,     xlab, group_var, lab_angle,   leg_size,
                                  tsize,    caption, xsize, ysize, ycol,
                                  lab_size, capt_size, leg_pos) {
  
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
    
    theme(plot.margin     = unit(c(1, 1, 1, 1), "cm"),
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
  
}





## Single line plot ----
single_line_plot = function(df,  title,  xvar, yvar, ylast,
                            ylab, xlab,  lab_angle, line_size, point_size,
                            tsize, caption, xsize, ysize, axis_size,
                            lab_size, capt_size, leg_size, leg_pos) {
  
  ## Use variable names for x and y
  ggplot(df, aes(!!sym(xvar), !!sym(yvar)), group = 1) +
    geom_line(size = line_size, group = 1) +
    
    geom_line(size  = line_size, group = 1, colour = 'coral') +
    geom_point(size = point_size, colour = 'grey') +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = ylast)) +
    
    theme(plot.margin     = unit(c(1, 1, 1, 1), "cm"),
          plot.title      = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x     = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -5),
          axis.line       = element_line(colour = 'black', size = axis_size),
          legend.position = leg_pos,
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
}





## Histogram ----
plot_histogram = function(df,   title,  xvar,
                          bin, col_var,
                          ylab, xlab,  lab_angle, line_size, point_size,
                          tsize, caption, xsize, ysize, 
                          lab_size, capt_size, leg_size, leg_pos) {
  
  
  ## Use the 'SOURCE' column to create a histogram for each source.
  ggplot(df, 
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
    
    theme(plot.margin     = unit(c(1, 1, 1, 1), "cm"),
          plot.title      = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x     = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -5),
          legend.position = leg_pos,
          legend.title    = element_blank(),
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
}




