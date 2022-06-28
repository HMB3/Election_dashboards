#open database connections
## Hyperion is also needed

#placing here so it is easy to run multiple times.

source(paste0(Sys.getenv("source_control_local_path"),
              "Main/R Scripts - Production/Database connections/EMS.R"))

source(paste0(Sys.getenv("source_control_local_path"),
              "Main/R Scripts - Production/Database connections/EMA.R"))


source(paste0(Sys.getenv("source_control_local_path"),
              "Main/R Scripts - Production/Database connections/Hyperion.R"))
