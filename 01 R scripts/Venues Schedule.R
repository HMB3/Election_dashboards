################################### ----- LG2101 Venues SCHEDULE ---- ######################################


## Run the Nominations dashboard on a loop
root_dashboard   <- paste0(Sys.getenv('source_control_local_path'), 
                           'Election_Events/LG2101/02 Dashboard/')
script_files     <- paste0(root_dashboard, '01 R scripts/')
Venue_interval    <- 86400
counter          <- 1


## Run while loop
while(Sys.time() < as.POSIXct("2021-10-28 14:06:07 AEDT"))
  
{
  setwd(script_files)
  source("Venues Dash board launch.R")
  message(paste0('You have completed ', counter))
  counter <- counter + 1
  Sys.sleep(Venue_interval)
  
}


################################### ----- LG2101 NOMINATIONS SCHEDULE ---- ######################################