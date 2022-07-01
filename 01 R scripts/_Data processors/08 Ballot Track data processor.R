
################################## ---- BALLOT TRACK DASHBAORD ---- ################################################


## This code processes the data to create the 'Candidate nominations' dashboard
message('Run R code to process Ballot Track data')


## To do ----


## 1). Check Deepak's exceptions
## 2). Get the Smart Freight Data refresh working

## 3). Would it be useful to have % complete graphs for multiple levels - i.e. how many of the total Venues, Count 
##     Centres, ROs, 
##     and LGAs are completely scanned? This is for the broader audience (EOG, RO, etc.), not just for Wayne/Deepak
##     Can we have a total % scanned - i.e. % complete? If so, how would it be calculated? For the last status, 5.0?


## 
time_stamp <- gsub("-", "_", now()) %>% 
  gsub(" ", "_", .)                 %>% 
  gsub(":", "",  .)


## Function to add 0 column if it doesn't already exist
add_zero_cols <- function(data, cname) {
  
  add         <- cname[!cname %in% names(data)]
  if(length(add)!=0) data[add] <- 0
  data
  
}



## Create a list of barcodes that need to be re-classifie 
barcode_reclassify <- c(110000768521, 120001289022) 





## CREATE MASTER TABLE ============================================================================


## The data from smart freight comes in three tables ::
## For example, The Con-note table contains consignment numbers from the printer. 
## Each consignment from the printer will contain a number of cartons - EG 3 cartons which each have x ballot boxes in them. 
## As the cartons move around, they are scanned at different stages (i.e. statuses).


# Define the statuses ::
# -	We need to  count the number of RO offices that have fully scanned all expected cartons for each process
# -	Processes are :
# o	2.0  - Returning Office/STH/CPVC Scan-in from Printer
# o	3.0  - Returning Office Scan-out to Venue
# o	4.0  - Returning Office Scan-in from Venue 
# o	5.10 - Returning Office Scan-out to Count Centre 
# o	5.11 - Count Centre Scan-in from Returning Office
# o	5.00 - Returning Office Scan-out to Warehouse


## Join tables together ::
## Assume the containers tables is the complete table, at the barcode level
## We need to take the barcode, and join on the context columns
Barcode_endpoint <- Ballot_Track_containers %>% 
  
  ## Join on the container status, but use Line Number too
  left_join(Ballot_Track_container_status, by = c("ConsKey", "LineNo")) %>% 
  
  ## Now join on the Con-note
  left_join(., Ballot_Track_connote, by = c("ConsKey"))


## Change the column names to match the   
names(Barcode_endpoint) <- gsub("([a-z])([A-Z])", "\\1 \\2", 
                                names(Barcode_endpoint)) %>% 
  gsub("LGA", "LGA ",            .)                      %>% 
  gsub("Returning Office", "RO", .)


## The combined table is a matrix of cartons*status (~40k rows).
## This table starts as a complete - including all possible combinations, with many blank rows. 
## All the rows will then be populated, as the cartons are progressively scanned.
# write_csv(Barcode_endpoint,              paste0(BT_server_data, 'Barcode_endpoint',         '.csv'))
# write_csv(Ballot_Track_containers,       paste0(BT_server_data, 'Ballot_Track_containers',  '.csv'))
# write_csv(Ballot_Track_connote,          paste0(BT_server_data, 'Ballot_Track_connote',     '.csv'))
# write_csv(Ballot_Track_container_status, paste0(BT_server_data, 'Ballot_Track_status',      '.csv'))




## Get the overlapping columns between the endpoint and the web report
overlap_cols <- intersect(names(Barcode_Report_webport), names(Barcode_endpoint))
missing_cols <- setdiff(names(Barcode_Report_webport),   names(Barcode_endpoint))





## Just get the final columns for presentation 
Ballot_track_RO_data  <- Barcode_endpoint %>% 
  
  ## select columns
  dplyr::select(., one_of(overlap_cols), Deleted) %>% arrange(`RO Name`) %>%
  
  mutate(EventID = event_group_ID)


## Create a count of all the Statuses
Status_notes_table <- Barcode_endpoint %>% 
  
  ## Summarise all the statuses
  group_by(`Status Stage`, `Status Notes`) %>%
  distinct(Barcode, .keep_all = TRUE)      %>% 
  tally(., name = "Carton Count")


## Create denominator for summary tables and % complete graphs ---
Total_ROs           <- Barcode_endpoint %>% dplyr::select(`RO Name`)           %>% distinct() %>% na.omit () %>% filter(grepl("Office", `RO Name`)) %>% nrow() 
Total_Count_centres <- Barcode_endpoint %>% dplyr::select(`Count Centre Name`) %>% distinct() %>% na.omit () %>% nrow()
Total_LGAs          <- Barcode_endpoint %>% dplyr::select(`LGA Name`)          %>% distinct() %>% na.omit () %>% nrow()
Total_Wards         <- Barcode_endpoint %>% dplyr::select(`Ward Name`)         %>% distinct() %>% na.omit () %>% nrow()
Total_Venues        <- Barcode_endpoint %>% dplyr::select(`Venue Name`)        %>% distinct() %>% na.omit () %>% nrow()





## MASTER STATUS TABLE ===================================================================


# Define the statuses ::
# -	Count of RO offices that have fully scanned all expected cartons for each process
# -	Processes are :
# o	2.0  Scan In from Printer
# o	3.0  Scan Out to venue
# o	4.0  Scan In from venue
# o	5.10 Scan out to Count centre
# o	5.11 Scan In from RO to count centre
# o	5.0  Scan Out to Warehouse


## Our approach is to use the endpoint to generate ALL the values for the Ballot Track Dashboard,
## rather than rely on the PPDM


## So the endpoint has data from the PPDM - we need these, but the names are a bit mixed up!
sort(unique(Barcode_endpoint$`Status Stage`))
table(Ballot_track_RO_data $`Contest Name`);
table(Ballot_track_RO_data $`Container Name`)


## Create count for all statuses
RO_all_status <- Barcode_endpoint %>% 
  
  ## Removes blank label rows
  #  filter(grepl('Black', `RO Name`)) %>%
  
  ## Create new columns that convert the values in 'contest name' to ones we can use
  mutate(ContestType = case_when(`Contest Name` == 'Councillor RESERVE STOCK' ~ 'Councillor',
                                 `Contest Name` == 'Mayor RESERVE STOCK'      ~ 'Mayor',
                                 `Contest Name` == 'Declaration Votes'        ~ 'Mixed',
                                 `Contest Name` == 'Referendum RESERVE STOCK' ~ 'Referendum',
                                 `Contest Name` == 'Poll RESERVE STOCK'       ~ 'Poll',
                                 TRUE ~ `Contest Name`),
         
         UseCase = case_when(`Contest Name` == 'Councillor RESERVE STOCK' ~ 'RESERVE STOCK',
                             `Contest Name` == 'Mayor RESERVE STOCK'      ~ 'RESERVE STOCK',
                             `Contest Name` == 'Declaration Votes'        ~ 'DECLARATION VOTES',
                             `Contest Name` == 'Referendum RESERVE STOCK' ~ 'RESERVE STOCK',
                             `Contest Name` == 'Poll RESERVE STOCK'       ~ 'RESERVE STOCK',
                             TRUE ~ 'STANDARD'),
         
         ## Count the barcodes - we don't want to exclude the duplicates.
         ## Also, create an indicator for whether or not 'RO name' is the same as 'Count centre' name
         # Dupe_barcode  = duplicated(Barcode),
         RO_CC_match = if_else(`RO Name` == `Count Centre Name`, 'TRUE', 'FALSE')) %>% 
  
  ## Finally re-order the columns
  dplyr::select(`Cons Key`, `Line No`, `Container No`, `Barcode`, everything())





## 2.0). SCAN IN FROM PRINTER ============================================================================


## Now take the master table above, and apply the business rules to calculate the - 'expected', 'actual', 'variance'.
## This is the first stage of the process - cartons being scanned in from the Printer


## Create a table of % scanned for status 2
RO_2.0_scan_in_from_printer <- RO_all_status %>%
  
  ## Apply business rules for status 2.0
  filter(`Container Name`   ==  'Carton')               %>%
  filter(`Venue Type Name`  !=  'Declaration Votes')    %>% 
  filter(`Venue Type Name`  !=  'Declared Institution') %>%
  filter(`Contest Name`     !=  'Mixed')                %>%
  filter(is.na(Deleted))                                %>%
  dplyr::select(-`Cons Key`)                            %>% 
  
  ## Remove the CPVCC
  filter(`RO Name` != 'CPVC') %>% 
  
  ## Filter out the later statuses, but include the NAs
  subset(`Status Stage` < 2.10 | is.na(`Status Stage`)) %>%
  
  ## Classify barcodes as scanned or unscanned 
  mutate(Status = ifelse(is.na(`Status Stage`), 'Unscanned', 'Scanned')) %>%
  
  ## Reclassify barcodes that are incorrectly marked as 'unscanned'
  mutate(Status = ifelse(Barcode %in% barcode_reclassify, 'Scanned', Status)) %>%
  
  ## Save out the barcodes.
  {. ->> RO_2.0_scan_in_from_printer_bc } %>%
  
  ## Group by and count unique barcodes - not container quantity
  group_by(`RO Name`,    Status, 
           ContestType,  UseCase, 
           `Container Name`, 
           `Venue Type Name`) %>%
  tally(., name = 'Barcode Count') %>%   
  
  ## If no cartons have been scanned, 
  pivot_wider(names_from  = Status, 
              values_from = `Barcode Count`) %>%
  
  ## Add scanned or unscanned == 0 if they don't exist
  ## Then make the NA values 0
  add_zero_cols(., 'Scanned')   %>%
  add_zero_cols(., 'Unscanned') %>%
  mutate_if(., is.numeric , replace_na, replace = 0) %>% 
  
  ## Calculate the expected cartons for status 2.0
  mutate(Expected = Scanned + Unscanned,
         Received = Scanned) %>%
  
  ## Calculate the % remaining
  mutate(Variance         = Expected - Received,
         PercentScanned   = round(Received/Expected,  digits = 3),
         PercentRemaining = round(1 - PercentScanned, digits = 3)) %>%  
  
  ## We don't need the additional columns for each stage
  dplyr::select(-Scanned, -Unscanned) 





## Count the RO's completely scanned for status 2.0
RO_2.0_percent_scanned <- RO_2.0_scan_in_from_printer %>% 
  dplyr::select(`RO Name`, Expected, Received) %>% group_by(`RO Name`) %>% 
  
  ## Sum the expected and recieved
  summarise(Expected = sum(Expected),
            Received = sum(Received)) %>% 
  
  ## Calculate the % remaining and % scanned
  mutate(Scanned   = Received/Expected,
         Scanned   = round(Scanned*100),
         Remaining = 100 - Scanned) %>% na.omit()


RO_2.0_percent_scanned_graph <- RO_2.0_percent_scanned %>% pivot_longer(., cols = c('Scanned', 'Remaining'),
                                                                        values_to = 'Percent',
                                                                        names_to  = 'Scanned')


## Count the RO's who completely scanned for status 2.0
RO_2.0_tally   <- RO_2.0_percent_scanned %>% filter(Scanned == 100) %>% distinct(`RO Name`) %>% nrow()
RO_2.0_percent <- round(RO_2.0_tally/Total_ROs,       digits = 3)       
RO_2.0_remain  <- round((1 - RO_2.0_tally/Total_ROs), digits = 3) 





## 3.0). RO SCAN OUT TO VENUE ===================================================================================


## Describe this stage here -  Returning Office 'Scan-out' to Venue


## Create a table of % scanned for status 3
RO_3.0_scan_out_to_venue <- RO_all_status %>%
  
  ## Apply business rules for status 3.0
  filter(`Container Name`  == 'Carton')                     %>%
  filter(`Venue Type Name` != 'Declaration Votes')          %>% 
  filter(`Venue Type Name` != 'Declared Institution')       %>%
  filter(`Contest Name`    != 'Mixed')                      %>%
  filter(is.na(Deleted))                                    %>%
  
  ## Remove the CPVCC
  filter(`RO Name` != 'CPVC') %>% 
  
  ## Don't filter out the NA Ward Names?!
  filter(`Ward Name`       != 'Mixed' | is.na(`Ward Name`)) %>%
  filter(UseCase           != 'RESERVE STOCK')              %>%
  dplyr::select(-`Cons Key`) %>% 
  
  ## Filter out the statuses later than 3.0, but include the NAs
  subset(`Status Stage` >= 2.0 | is.na(`Status Stage`)) %>% 
  subset(`Status Stage` <= 3.0 | is.na(`Status Stage`)) %>%
  
  
  ## More business knowledge
  group_by(Barcode) %>% 
  mutate(Barcode_count = n(),
         Filter_out = case_when((Barcode_count == 2 & `Status Stage` == 3)   ~ 'Keep',
                                (Barcode_count == 1 & `Status Stage` == 2.0) ~ 'Keep',
                                (Barcode_count == 1 & `Status Stage` == 2.1) ~ 'Keep',
                                is.na(`Status Stage`) ~ 'Keep',
                                TRUE ~ 'Discard')) %>%
  filter(Filter_out == 'Keep') %>%
  
  ## Classify barcodes as 'scanned' or 'unscanned' 
  mutate(Status = case_when(is.na(`Status Stage`)  ~ 'Unscanned', 
                            `Status Stage`  == 2.1 ~ 'Unscanned',
                            `Status Stage`  == 2.0 ~ 'Unscanned',
                            `Status Stage`  == 3   ~ 'Scanned',
                            TRUE ~ 'ERROR')) %>% 
  
  ## Save out the barcodes before doing the group_by
  {. ->> RO_3.0_scan_out_to_venue_bc } %>%
  
  ## Group by and count unique barcodes
  group_by(`RO Name`,      Status, 
           ContestType,    UseCase, 
           `Container Name`, 
           `Venue Type Name`) %>%
  tally(., name = 'Barcode Count') %>%   
  
  ## If no cartons have been scanned 
  pivot_wider(names_from  = Status, 
              values_from = `Barcode Count`) %>%
  
  ## Add scanned or unscanned == 0, if they don't exist
  ## Then make the NA values 0
  add_zero_cols(., 'Scanned')   %>%
  add_zero_cols(., 'Unscanned') %>%
  mutate_if(., is.numeric, replace_na, replace = 0) %>% 
  
  ## Calculate the expected cartons for status 3.0
  mutate(Expected   = Scanned + Unscanned,
         ScannedOut = Scanned) %>%
  
  ## Calculate the % remaining
  mutate(Variance         = Expected - ScannedOut,
         PercentScanned   = round(ScannedOut/Expected,  digits = 3),
         PercentRemaining = round(1 - PercentScanned,   digits = 3)) %>%  
  
  ## We don't need the additional columns for each stage
  dplyr::select(-Scanned, -Unscanned)





## Count the RO's completely scanned for status 2.0
RO_3.0_percent_scanned <- RO_3.0_scan_out_to_venue %>% 
  dplyr::select(`RO Name`, Expected, ScannedOut) %>% group_by(`RO Name`) %>% 
  
  ## Sum the expected and recieved
  summarise(Expected   = sum(Expected),
            ScannedOut = sum(ScannedOut)) %>% 
  
  ## Calculate the % remaining and % scanned
  mutate(Scanned   = ScannedOut/Expected,
         Scanned   = round(Scanned*100),
         Remaining = 100 - Scanned) %>% na.omit()


RO_3.0_percent_scanned_graph <- RO_3.0_percent_scanned %>% pivot_longer(., cols   = c('Scanned', 'Remaining'),
                                                                        values_to = 'Percent',
                                                                        names_to  = 'Scanned')


## Count the RO's who completely scanned for status 3.0
RO_3.0_tally   <- RO_3.0_percent_scanned %>% filter(Scanned == 100) %>% distinct(`RO Name`) %>% nrow()
RO_3.0_percent <- round(RO_3.0_tally/Total_ROs, digits = 3)       
RO_3.0_remain  <- round((1 - RO_3.0_tally/Total_ROs), digits = 3) 





## 4.0). RO SCAN IN FROM VENUE =======================================================================


## Describe this stage here - this should include all the cartons from above, but
## Just need a tally of the ballot boxes and the secure bags, but they don't need to be
## include in the expected.


## Every Venue has a secure bags of decvotes
## Don't need to include in expected, but we need to add another row.


## The flow - scan out, then scan back in. 
## Backwards complications - e.g. sit in the expected all the way through.
## Cascading problem - all the way through the sheets.
## Isolate the problem in each sheet - 
## As the scan's start happening, it will need to flow to



## Create a table of % scanned for status 4.0
RO_4.0_scan_in_from_venue <- RO_all_status %>%
  
  ## Apply business rules for status 4.0
  filter(`Venue Type Name` != 'Declared Institution') %>%
  filter(UseCase           != 'RESERVE STOCK')        %>%
  dplyr::select(-`Cons Key`) %>% 
  
  ## Filter the statuses to 3.0 and 4.0, but exclude NAs
  subset(`Status Stage` == 3.0 | `Status Stage` == 4.0) %>%
  
  ## Create barcode count
  group_by(Barcode) %>% 
  mutate(Barcode_count = n(),
         Filter_out    = case_when((Barcode_count == 1 & `Status Stage` == 4.0) ~ 'Keep',
                                   (Barcode_count == 2 & `Status Stage` == 4.0) ~ 'Keep',
                                   (Barcode_count == 1 & `Status Stage` == 3.0) ~ 'Keep',
                                   TRUE ~ 'Discard')) %>%
  filter(Filter_out == 'Keep') %>%
  
  ## Classify barcodes as scanned or unscanned 
  mutate(Status = case_when(`Status Stage`  == 4.0 ~ 'Scanned',
                            `Status Stage`  == 3.0 ~ 'Unscanned',
                            TRUE ~ 'ERROR')) %>%
  
  ## Save out the barcodes
  {. ->> RO_4.0_scan_in_from_venue_bc } %>%
  
  ## Group by and count unique barcode
  group_by(`RO Name`,     Status, 
           ContestType,   UseCase, 
           `Container Name`, 
           `Venue Type Name`) %>%
  tally(., name = 'Barcode Count') %>%   
  
  ## If no cartons have been scanned, 
  pivot_wider(names_from  = Status, 
              values_from = `Barcode Count`) %>%
  
  ## Add scanned or unscanned == 0 if they don't exist
  ## Then make the NA values 0
  add_zero_cols(., 'Scanned')   %>%
  add_zero_cols(., 'Unscanned') %>%
  mutate_if(., is.numeric , replace_na, replace = 0) %>% 
  
  ## Calculate the expected cartons for status 4.0
  mutate(Unscanned = ifelse(is.na(Unscanned), 0, Unscanned),
         Expected  = Scanned + Unscanned,
         ScannedIn = Scanned) %>%
  
  ## Calculate the % remaining
  mutate(Variance         = Expected - ScannedIn,
         PercentScanned   = round(ScannedIn/Expected,  digits = 3),
         PercentRemaining = round(1 - PercentScanned,  digits = 3)) %>%
  
  ## Clean up tables 
  mutate(Expected         = ifelse(`Container Name` != 'Carton', NA, Expected),
         PercentScanned   = ifelse(`Container Name` != 'Carton', NA, PercentScanned),
         PercentRemaining = ifelse(`Container Name` != 'Carton', NA, PercentRemaining ),
         Variance         = ifelse(`Container Name` != 'Carton', NA, Variance)) %>% 
  
  ## We don't need the additional columns for each stage
  dplyr::select(-Scanned, -Unscanned)




## 
RO_4.0_scan_in_from_venue_no_BB <- RO_all_status %>%
  
  ## Apply business rules for status 4.0
  filter(`Venue Type Name` != 'Declared Institution') %>%
  filter(UseCase           != 'RESERVE STOCK')        %>%
  filter(`Container Name`  != 'Ballot box')           %>% 
  filter(`Container Name`  != 'Secure bag - used')    %>%  
  dplyr::select(-`Cons Key`) %>% 
  
  ## Filter the statuses to 3.0 and 4.0, but exclude NAs
  subset(`Status Stage` == 3.0 | `Status Stage` == 4.0) %>%
  filter(is.na(Deleted))                                %>%
  
  ## Create barcode count
  group_by(Barcode) %>% 
  mutate(Barcode_count = n(),
         Filter_out    = case_when((Barcode_count == 1 & `Status Stage` == 4.0) ~ 'Keep',
                                   (Barcode_count == 2 & `Status Stage` == 4.0) ~ 'Keep',
                                   (Barcode_count == 1 & `Status Stage` == 3.0) ~ 'Keep',
                                   TRUE ~ 'Discard')) %>%
  filter(Filter_out == 'Keep') %>%
  
  ## Classify barcodes as scanned or unscanned 
  mutate(Status = case_when(`Status Stage`  == 4.0 ~ 'Scanned',
                            `Status Stage`  == 3.0 ~ 'Unscanned',
                            TRUE ~ 'ERROR')) %>%
  
  ## Group by and count unique barcode
  group_by(`RO Name`,     Status, 
           ContestType,   UseCase, 
           `Container Name`, 
           `Venue Type Name`) %>%
  tally(., name = 'Barcode Count') %>%   
  
  ## If no cartons have been scanned, 
  pivot_wider(names_from  = Status, 
              values_from = `Barcode Count`) %>%
  
  ## Add scanned or unscanned == 0 if they don't exist
  ## Then make the NA values 0
  add_zero_cols(., 'Scanned')   %>%
  add_zero_cols(., 'Unscanned') %>%
  mutate_if(., is.numeric , replace_na, replace = 0) %>% 
  
  ## Calculate the expected cartons for status 4.0
  mutate(Unscanned = ifelse(is.na(Unscanned), 0, Unscanned),
         Expected  = Scanned + Unscanned,
         ScannedIn = Scanned) %>%
  
  ## Calculate the % remaining
  mutate(Variance         = Expected - ScannedIn,
         PercentScanned   = round(ScannedIn/Expected,  digits = 3),
         PercentRemaining = round(1 - PercentScanned,  digits = 3)) %>%
  
  ## Clean up tables 
  mutate(Expected         = ifelse(`Container Name` != 'Carton', NA, Expected),
         PercentScanned   = ifelse(`Container Name` != 'Carton', NA, PercentScanned),
         PercentRemaining = ifelse(`Container Name` != 'Carton', NA, PercentRemaining ),
         Variance         = ifelse(`Container Name` != 'Carton', NA, Variance)) %>% 
  
  ## We don't need the additional columns for each stage
  dplyr::select(-Scanned, -Unscanned)





## We need to remove ballot boxes from the grap tables


## Count the RO's completely scanned for status 2.0
RO_4.0_percent_scanned <- RO_4.0_scan_in_from_venue_no_BB %>% 
  dplyr::select(`RO Name`, Expected, ScannedIn) %>% group_by(`RO Name`) %>% 
  
  ## Sum the expected and recieved
  summarise(Expected   = sum(Expected),
            ScannedIn  = sum(ScannedIn)) %>% 
  
  ## Calculate the % remaining and % scanned
  mutate(Scanned   = ScannedIn/Expected,
         Scanned   = round(Scanned*100),
         Remaining = 100 - Scanned) %>% na.omit()


RO_4.0_percent_scanned_graph <- RO_4.0_percent_scanned %>% pivot_longer(., cols   = c('Scanned', 'Remaining'),
                                                                        values_to = 'Percent',
                                                                        names_to  = 'Scanned')


## Count the RO's completely scanned for status 4.0
RO_4.0_tally   <- RO_4.0_percent_scanned %>% filter(Scanned == 100) %>% distinct(`RO Name`) %>% nrow()
RO_4.0_percent <- round(RO_4.0_tally/Total_ROs, digits = 3)       
RO_4.0_remain  <- round((1 - RO_4.0_tally/Total_ROs), digits = 3) 





## 5.10). RO SCAN OUT TO COUNT CENTRE =======================================================================


## Include all contest types
## Only the normal ROs.
## ALL contest types
## Where RO name is NOT same as Count centre name.


## When an RO is scanned out to the count centre?
## 


## If Count centre is the same as RO office, they are counting everything.
## Metro RO offices - in that case. Has to be a councillor contest type,
## Whatever we have scanned in, + 4.12 for councillor contest type.
## Open up a secure bag, do scrutiny, etc.  
## Item by item basis.


## Note that the contest types currently have no REF or POLL
table(RO_all_status$ContestType);
table(RO_all_status$`Venue Type Name`)


## Create a table of % scanned for status 5.10
RO_5.10_scan_out_to_CC <- RO_all_status %>%
  
  ## First, apply business rules for status 5.10
  ## We are not including declared institutions here
  ## Count centre is wrong on the PPDM
  filter(RO_CC_match      == 'FALSE')                  %>%
  filter(ContestType      == 'Councillor')             %>%
  filter(`Container Name` == 'Carton')                 %>%
  filter(`Venue Type Name`  != 'Offline Location')     %>%
  filter(`Venue Type Name`  != 'Declared Institution') %>%
  filter(is.na(Deleted))                               %>%
  dplyr::select(-`Cons Key`) %>% 
  
  ## Filter the statuses to 5.1 and 4.12, but exclude NAs
  ## filters are correct 
  subset(`Status Stage` == 5.1 | `Status Stage` == 4.12 | `Status Stage` == 4.00) %>%
  
  ## Create barcode count
  group_by(Barcode) %>% 
  mutate(Barcode_count = n(),
         Filter_out    = case_when((Barcode_count == 1 & `Status Stage` == 5.1)  ~ 'Keep',
                                   (Barcode_count == 2 & `Status Stage` == 5.1)  ~ 'Keep',
                                   (Barcode_count == 1 & `Status Stage` == 4.12) ~ 'Keep',
                                   (Barcode_count == 1 & `Status Stage` == 4.00) ~ 'Keep',
                                   TRUE ~ 'Discard')) %>%
  filter(Filter_out == 'Keep') %>%
  
  ## Classify barcodes as scanned or unscanned 
  mutate(Status = case_when(`Status Stage` == 5.1  ~ 'Scanned',
                            `Status Stage` == 4.12 ~ 'Unscanned',
                            `Status Stage` == 4.00 ~ 'Unscanned',
                            TRUE ~ 'ERROR')) %>%
  
  ## Save out the barcodes
  {. ->> RO_5.10_scan_out_to_CC_bc } %>%
  
  ## Group by and count unique barcodes
  group_by(`RO Name`,  
           `Count Centre Name`,  
           Status, 
           ContestType,  
           UseCase, 
           `Container Name`, 
           `Venue Type Name`) %>%
  tally(., name = 'Barcode Count') %>%   
  
  ## If no cartons have been scanned, 
  pivot_wider(names_from  = Status, 
              values_from = `Barcode Count`) %>%
  
  ## Add scanned or unscanned == 0 if they don't exist
  ## Then make the NA values 0
  add_zero_cols(., 'Scanned')   %>%
  add_zero_cols(., 'Unscanned') %>%
  mutate_if(., is.numeric , replace_na, replace = 0) %>% 
  
  ## Calculate the expected cartons for status 5.10
  mutate(Expected   = Scanned + Unscanned,
         ScannedOut = Scanned) %>%
  
  ## Calculate the % remaining
  mutate(Variance         = Expected - ScannedOut,
         PercentScanned   = round(ScannedOut/Expected,  digits = 3),
         PercentRemaining = round(1 - PercentScanned,   digits = 3)) %>%
  
  ## Clean up tables 
  mutate(Expected         = ifelse(`Container Name` != 'Carton', NA, Expected),
         PercentScanned   = ifelse(`Container Name` != 'Carton', NA, PercentScanned),
         PercentRemaining = ifelse(`Container Name` != 'Carton', NA, PercentRemaining ),
         Variance         = ifelse(`Container Name` != 'Carton', NA, Variance)) %>%
  
  ## We don't need the additional columns for each stage
  dplyr::select(-Scanned, -Unscanned)





## Count the RO's completely scanned for status 2.0
RO_5.10_percent_scanned <- RO_5.10_scan_out_to_CC %>% 
  dplyr::select(`RO Name`, Expected, ScannedOut) %>% group_by(`RO Name`) %>% 
  
  ## Sum the expected and recieved
  summarise(Expected   = sum(Expected),
            ScannedOut = sum(ScannedOut)) %>% 
  
  ## Calculate the % remaining and % scanned
  mutate(Scanned   = ScannedOut/Expected,
         Scanned   = round(Scanned*100),
         Remaining = 100 - Scanned) %>% na.omit()


RO_5.10_percent_scanned_graph <- RO_5.10_percent_scanned %>% pivot_longer(., cols   = c('Scanned', 'Remaining'),
                                                                          values_to = 'Percent',
                                                                          names_to  = 'Scanned')


## Count the RO's completely scanned for status 5.10
RO_5.10_tally   <- RO_5.10_percent_scanned %>% filter(Scanned == 100) %>% distinct(`RO Name`) %>% nrow()
RO_5.10_percent <- round(RO_5.10_tally/Total_ROs, digits = 3)       
RO_5.10_remain  <- round((1 - RO_5.10_tally/Total_ROs), digits = 3) 





## 5.11). COUNT CENTRE SCAN IN FROM RO =======================================================================


## Include all contest types
## Only the normal ROs.
## ALL contest types
## Where RO name is NOT same as Count centre name


## Show an extra row with the count centre details?
## Create a page for each - just use 5.11?




## Note that the contest types currently have no REF or POLL
table(RO_all_status$ContestType)


## Create a table of % scanned for status 5.10
RO_5.11_scan_from_RO_to_CC <- RO_all_status %>%
  
  ## First, apply business rules for status 5.10
  filter(RO_CC_match   == 'FALSE')                        %>%
  filter(ContestType   == 'Councillor')                   %>%
  filter(`Container Name` == 'Carton')                    %>%
  filter(`Venue Type Name`  != 'Offline Location')        %>%
  filter(`Venue Type Name`  != 'Declared Institution')    %>%
  filter(is.na(Deleted))                                  %>%
  dplyr::select(-`Cons Key`) %>%
  
  ## Filter the statuses to 5.1 and 4.12, but exclude NAs
  subset(`Status Stage` == 5.11 | `Status Stage` == 5.10) %>%
  
  ## Create barcode count
  group_by(Barcode) %>% 
  mutate(Barcode_count = n(),
         Filter_out = case_when((Barcode_count == 2 & `Status Stage` == 5.11) ~ 'Keep',
                                (Barcode_count == 1 & `Status Stage` == 5.10) ~ 'Keep',
                                TRUE ~ 'Discard')) %>%
  filter(Filter_out == 'Keep') %>%
  
  ## Classify barcodes as scanned or unscanned 
  mutate(Status = case_when(`Status Stage`  == 5.11 ~ 'Scanned',
                            `Status Stage`  == 5.10 ~ 'Unscanned',
                            TRUE ~ 'ERROR')) %>%
  
  ## Save out the barcodes
  {. ->> RO_5.11_scan_from_RO_to_CC_bc } %>%
  
  ## Group by and count unique barcodes
  group_by(`RO Name`,  
           `Count Centre Name`,  
           Status, 
           ContestType,  
           UseCase, 
           `Container Name`, 
           `Venue Type Name`) %>%
  tally(., name = 'Barcode Count') %>%   
  
  ## If no cartons have been scanned, 
  pivot_wider(names_from  = Status, 
              values_from = `Barcode Count`) %>%
  
  ## Add scanned or unscanned == 0 if they don't exist
  ## Then make the NA values 0
  add_zero_cols(., 'Scanned')   %>%
  add_zero_cols(., 'Unscanned') %>%
  mutate_if(., is.numeric , replace_na, replace = 0) %>% 
  
  
  ## Calculate the expected cartons for status 5.11
  ## We are no longer getting the 'Actual' cartons from the 'next' status
  mutate(Expected  = Scanned + Unscanned,
         ScannedIn = Scanned) %>%
  
  ## Calculate the % remaining
  mutate(Variance         = Expected - ScannedIn,
         PercentScanned   = round(ScannedIn/Expected,  digits = 3),
         PercentRemaining = round(1 - PercentScanned,   digits = 3)) %>%
  
  ## Clean up tables 
  mutate(Expected         = ifelse(`Container Name` != 'Carton', NA, Expected),
         PercentScanned   = ifelse(`Container Name` != 'Carton', NA, PercentScanned),
         PercentRemaining = ifelse(`Container Name` != 'Carton', NA, PercentRemaining ),
         Variance         = ifelse(`Container Name` != 'Carton', NA, Variance)) %>%
  
  ## We don't need the additional columns for each stage
  dplyr::select(-Scanned, -Unscanned)





## Count the RO's completely scanned for status 2.0
RO_5.11_percent_scanned <- RO_5.11_scan_from_RO_to_CC %>% 
  dplyr::select(`Count Centre Name`, Expected, ScannedIn) %>% group_by(`Count Centre Name`) %>% 
  
  ## Sum the expected and recieved
  summarise(Expected   = sum(Expected),
            ScannedOut = sum(ScannedIn)) %>% 
  
  ## Calculate the % remaining and % scanned
  mutate(Scanned   = ScannedOut/Expected,
         Scanned   = round(Scanned*100),
         Remaining = 100 - Scanned) %>% na.omit()


RO_5.11_percent_scanned_graph <- RO_5.11_percent_scanned %>% pivot_longer(., cols   = c('Scanned', 'Remaining'),
                                                                          values_to = 'Percent',
                                                                          names_to  = 'Scanned')


## Count the RO's completely scanned for status 5.11
RO_5.11_tally   <- RO_5.11_percent_scanned %>% filter(Scanned == 1) %>% distinct(`Count Centre Name`) %>% nrow()
RO_5.11_percent <- round(RO_5.11_tally/Total_Count_centres, digits = 3)       
RO_5.11_remain  <- round((1 - RO_5.11_tally/Total_Count_centres), digits = 3) 






## 5.0). RO SCAN OUT TO WAREHOUSE =======================================================================


## Where contest type = mayor/referrendum/poll.
## Normal/metro RO offices and ROs are also count centre.


# EXPECTED RESULTS:
# The Page shows the data for all the RO offices


# ACTUAL RESULTS:
# Metro RO should not be included for Councillor BPs as they sent to Count centre. 
# Example Blacktown Councillor BPs for Dec Votes and Declared Institutions included


## Note that the contest types currently have no REF or POLL
table(RO_all_status$ContestType)


## Create a table of % scanned for status 5.0
RO_5.0_scan_out_to_Ware <- RO_all_status %>%
  
  ## First, apply business rules for status 5.10
  filter(RO_CC_match       == 'TRUE')   %>%
  filter(`Container Name`  == 'Carton') %>%
  filter(`Venue Type Name` != 'Declared Institution') %>%
  filter(is.na(Deleted))                              %>%
  dplyr::select(-`Cons Key`) %>% 
  
  ## Filter the statuses to 5.1 and 4.12, but exclude NAs
  ## Then group by barcode
  subset(`Status Stage`   == 2.0  | UseCase == 'RESERVE STOCK' |
           `Status Stage` == 4.0 | `Status Stage` == 4.12 | `Status Stage` == 5.0) %>%
  
  ## Create barcode count 
  ## This is missing data
  group_by(Barcode) %>% 
  mutate(Barcode_count = n(),
         Filter_out = case_when((Barcode_count == 1 & `Status Stage` == 2.0)  ~ 'Keep',
                                (Barcode_count == 1 & `Status Stage` == 4.0)  ~ 'Keep',
                                (Barcode_count == 2 & `Status Stage` == 5.0)  ~ 'Keep',
                                
                                (Barcode_count == 2 & `Status Stage` == 4.0)  ~ 'Keep',
                                (Barcode_count == 3 & `Status Stage` == 5.0)  ~ 'Keep',
                                (Barcode_count == 1 & `Status Stage` == 4.12) ~ 'Keep',
                                TRUE ~ 'Discard')) %>%
  
  filter(Filter_out == 'Keep') %>%
  
  ## Classify barcodes as scanned or unscanned 
  mutate(Status = case_when(`Status Stage`  == 2.0  ~ 'Unscanned',
                            `Status Stage`  == 4.0  ~ 'Unscanned',
                            `Status Stage`  == 4.12 ~ 'Unscanned',
                            `Status Stage`  == 5.0  ~ 'Scanned',
                            TRUE ~ 'ERROR')) %>%
  
  ## Save out the barcodes
  {. ->> RO_5.0_scan_out_to_Ware_bc } %>%
  
  ## Group by `Contest Name` too, and count unique barcodes
  group_by(`RO Name`,  
           `Count Centre Name`,  
           Status, 
           ContestType,  
           UseCase, 
           `Container Name`,
           `Contest Name`, 
           `Venue Type Name`) %>%
  tally(., name = 'Barcode Count') %>%   
  
  ## If no cartons have been scanned, 
  pivot_wider(names_from  = Status, 
              values_from = `Barcode Count`) %>%
  
  ## Add scanned or unscanned == 0 if they don't exist
  ## Then make the NA values 0
  add_zero_cols(., 'Scanned')   %>%
  add_zero_cols(., 'Unscanned') %>%
  mutate_if(., is.numeric , replace_na, replace = 0) %>% 
  
  ## Calculate the expected cartons for status 5.0
  ## We are no longer getting the 'Actual' cartons from the 'next' status
  mutate(Expected   = Scanned + Unscanned,
         ScannedOut = Scanned) %>%
  
  ## Calculate the % remaining
  mutate(Variance         = Expected - ScannedOut,
         PercentScanned   = round(ScannedOut/Expected,  digits = 3),
         PercentRemaining = round(1 - PercentScanned,   digits = 3)) %>%
  
  ## Clean up tables 
  mutate(Expected         = ifelse(`Container Name` != 'Carton', NA, Expected),
         PercentScanned   = ifelse(`Container Name` != 'Carton', NA, PercentScanned),
         PercentRemaining = ifelse(`Container Name` != 'Carton', NA, PercentRemaining),
         Variance         = ifelse(`Container Name` != 'Carton', NA, Variance)) %>%
  
  ## We don't need the additional columns for each stage
  dplyr::select(-Scanned, -Unscanned) 





## Count the RO's completely scanned for status 2.0
RO_5.0_percent_scanned <- RO_5.0_scan_out_to_Ware %>% 
  dplyr::select(`RO Name`, Expected, ScannedOut) %>% group_by(`RO Name`) %>% 
  
  ## Sum the expected and recieved
  summarise(Expected   = sum(Expected),
            ScannedOut = sum(ScannedOut)) %>% 
  
  ## Calculate the % remaining and % scanned
  mutate(Scanned   = ScannedOut/Expected,
         Scanned   = round(Scanned*100),
         Remaining = 100 - Scanned) %>% na.omit()


RO_5.0_percent_scanned_graph <- RO_5.0_percent_scanned %>% pivot_longer(., cols   = c('Scanned', 'Remaining'),
                                                                        values_to = 'Percent',
                                                                        names_to  = 'Scanned')


## Count the RO's completely scanned for status 4
RO_5.0_tally   <- RO_5.0_percent_scanned %>% filter(Scanned == 100) %>% distinct(`RO Name`) %>% nrow()
RO_5.0_percent <- round(RO_5.0_tally/Total_ROs, digits = 3)       
RO_5.0_remain  <- round((1 - RO_5.0_tally/Total_ROs), digits = 3) 





## 5.14). COUNT CENTRE SCAN IN TO ULD =======================================================================


## Describe the process here - 
## Parent barcodes are in 5.14. If there is no barcode, it isn't asctually in status 5.14, so we don't count that
## Bunch of cartons going to ULD, including empty cartons.


## Create a table of % scanned for status 5.14...
RO_5.14_CC_scan_in_to_ULD <- RO_all_status %>%
  
  ## First, apply business rules for status 5.14
  filter(RO_CC_match        == 'FALSE')   %>%
  filter(`Container Name`   == 'Carton') %>%
  filter(`Venue Type Name`  != 'Declared Institution') %>%
  filter(str_starts(Barcode, '11') | str_starts(Barcode, '12')) %>%
  filter(is.na(Deleted))     %>%
  
  dplyr::select(-`Cons Key`) %>% 
  
  ## Filter the statuses to 5.1 and 4.12, but exclude NAs
  ## Then group by barcode
  subset(`Status Stage` == 5.11 | `Status Stage` == 5.14) %>%
  
  ## Create parent barcode column
  mutate(Parent_Barcode = str_extract(`Status Notes`, "(?<=\')(.*)(?=\')"), .after = Barcode) %>% 
  group_by(Barcode) %>%
  
  ## Create barcode count
  mutate(Barcode_count = n(),
         Filter_out = case_when((Barcode_count == 1 & `Status Stage` == 5.11) ~ 'Keep',
                                (Barcode_count == 2 & `Status Stage` == 5.14) ~ 'Keep',
                                TRUE ~ 'Discard')) %>%
  filter(Filter_out == 'Keep') %>%
  
  ## Classify barcodes as scanned or unscanned
  ## Need to add logica here
  mutate(Status = case_when(`Status Stage`  == 5.11 ~ 'Unscanned',
                            `Status Stage`  == 5.14 & is.na(Parent_Barcode) ~ 'Unscanned',
                            `Status Stage`  == 5.14 ~ 'Scanned',
                            TRUE ~ 'ERROR')) %>%
  
  ## Save out the barcodes
  {. ->> RO_5.14_CC_scan_in_to_ULD_bc } %>%
  
  ## Group by and count unique barcodes
  group_by(`RO Name`,  
           `Count Centre Name`,  
           Status, 
           ContestType,  
           UseCase, 
           `Container Name`, 
           `Venue Type Name`) %>%
  tally(., name = 'Barcode Count') %>%
  
  ## If no cartons have been scanned,
  pivot_wider(names_from  = Status,
              values_from = `Barcode Count`) %>%
  
  ## Add scanned or unscanned == 0 if they don't exist
  ## Then make the NA values 0
  add_zero_cols(., 'Scanned')   %>%
  add_zero_cols(., 'Unscanned') %>%
  mutate_if(., is.numeric , replace_na, replace = 0) %>%
  
  ## Calculate the expected cartons for status 5.14
  ## We are no longer getting the 'Actual' cartons from the 'next' status
  mutate(Expected   = Scanned + Unscanned,
         ScannedOut = Scanned) %>%
  
  ## Calculate the % remaining
  mutate(Variance         = Expected - ScannedOut,
         PercentScanned   = round(ScannedOut/Expected, digits = 3),
         PercentRemaining = round(1 - PercentScanned,  digits = 3)) %>%
  
  ## Clean up tables
  mutate(Expected         = ifelse(`Container Name` != 'Carton', NA, Expected),
         PercentScanned   = ifelse(`Container Name` != 'Carton', NA, PercentScanned),
         PercentRemaining = ifelse(`Container Name` != 'Carton', NA, PercentRemaining),
         Variance         = ifelse(`Container Name` != 'Carton', NA, Variance)) %>%
  
  ## We don't need the additional columns for each stage
  dplyr::select(-Scanned, -Unscanned)





## Count the RO's completely scanned for status 2.0
RO_5.14_percent_scanned <- RO_5.14_CC_scan_in_to_ULD %>% 
  dplyr::select(`Count Centre Name`, Expected, ScannedOut) %>% group_by(`Count Centre Name`) %>% 
  
  ## Sum the expected and recieved
  summarise(Expected   = sum(Expected),
            ScannedOut = sum(ScannedOut)) %>% 
  
  ## Calculate the % remaining and % scanned
  mutate(Scanned   = ScannedOut/Expected,
         Scanned   = round(Scanned*100),
         Remaining = 100 - Scanned) %>% na.omit()


RO_5.14_percent_scanned_graph <- RO_5.14_percent_scanned %>% pivot_longer(., cols   = c('Scanned', 'Remaining'),
                                                                          values_to = 'Percent',
                                                                          names_to  = 'Scanned')


## Count the RO's completely scanned for status 4
RO_5.14_tally   <- RO_5.14_percent_scanned %>% filter(Scanned == 1) %>% distinct(`Count Centre Name`) %>% nrow()
RO_5.14_percent <- round(RO_5.14_tally/Total_Count_centres,      digits = 3)       
RO_5.14_remain  <- round((1 - RO_5.14_tally/Total_Count_centres), digits = 3) 





## CHECK DATA  ==========================================================


## This info needs to be on the RO dashboard.
## 


## Save csv of all data for ROs
# message('Save csv data of ROs')
# write_csv(Ballot_track_RO_data , paste0(BT_server_data, 'Ballot_track_RO_data.csv'))


## Now find out which rows are in the endpoint, but not the webportal
if(test_data) {
  
  ## Check the statuses
  file_list <- c('Status_notes_table',
                 'Ballot_track_RO_data', 
                 
                 'RO_2.0_scan_in_from_printer',
                 'RO_2.0_scan_in_from_printer_bc',
                 
                 'RO_3.0_scan_out_to_venue',
                 'RO_3.0_scan_out_to_venue_bc',
                 
                 'RO_4.0_scan_in_from_venue',
                 'RO_4.0_scan_in_from_venue_bc',
                 
                 'RO_5.10_scan_out_to_CC',
                 'RO_5.10_scan_out_to_CC_bc',
                 
                 'RO_5.11_scan_from_RO_to_CC',
                 'RO_5.11_scan_from_RO_to_CC_bc',
                 
                 'RO_5.0_scan_out_to_Ware',
                 'RO_5.0_scan_out_to_Ware_bc',
                 
                 'RO_5.14_CC_scan_in_to_ULD',
                 'RO_5.14_CC_scan_in_to_ULD_bc')
  
  
  ## Add data to workbook
  Ballot_workbook <- createWorkbook()
  
  ## file = file_list[1]
  for(file in file_list) {
    
    ## Get required columns.
    File_to_Write <- get(file)
    
    ## Get the columns we want
    # select(all_of(Required_Cols))
    
    ## Save the CSV
    write_excel_csv(File_to_Write,
                    paste0(BT_server_data, file, '.csv'))
    
    ## Add worksheet to the spread sheet
    message('writing ', file,  ' to check ballot track data')
    addWorksheet(Ballot_workbook, file)
    
    ## Write the data to the corresponding worksheet
    writeDataTable(wb       = Ballot_workbook, 
                   sheet    = file,
                   x        = get(file),
                   startCol = 1, 
                   startRow = 1, 
                   rowNames = FALSE,
                   tableStyle = "TableStyleMedium2")
    
  }
  
  ## Save the whole workbook
  saveWorkbook(Ballot_workbook, 
               paste0(BT_server_data, "Ballot_Track_status_check.xlsx"),
               overwrite = TRUE)
}





## HEADLINE FLEXTABLE  ==========================================================


## Progress Scanned
Status  <- c(paste0('RO Scan-in from Printer : ',      RO_2.0_tally, ' / ',  Total_ROs, ' ROs scanned in all cartons'),
             paste0('RO Scan-in from Printer : ',      RO_2.0_tally, ' / ',  Total_ROs, ' ROs scanned in all cartons'),
             
             paste0('RO Scan-out to Venue : ',         RO_3.0_tally, ' / ',  Total_ROs, ' ROs scanned out all cartons'),
             paste0('RO Scan-out to Venue : ',         RO_3.0_tally, ' / ',  Total_ROs, ' ROs scanned out all cartons'),
             
             paste0('RO Scan-in from Venue : ',        RO_4.0_tally, ' / ',  Total_ROs, ' ROs scanned in all cartons'),
             paste0('RO Scan-in from Venue : ',        RO_4.0_tally, ' / ',  Total_ROs, ' ROs scanned in all cartons'),
             
             paste0('RO scan-out to CC : ',            RO_5.10_tally, ' / ', Total_ROs, ' ROs scanned out all cartons'),
             paste0('RO scan-out to CC : ',            RO_5.10_tally, ' / ', Total_ROs, ' ROs scanned out all cartons'),
             
             paste0('Count Centre Scan-in from RO : ', RO_5.11_tally, ' / ', Total_Count_centres, ' CCs scanned in all cartons'),
             paste0('Count Centre Scan-in from RO : ', RO_5.11_tally, ' / ', Total_Count_centres, ' CCs scanned in all cartons'),
             
             paste0('RO Scan-out to Warehouse : ',     RO_5.0_tally,  ' / ', Total_ROs, ' ROs scanned out all cartons'),
             paste0('RO Scan-out to Warehouse : ',     RO_5.0_tally,  ' / ', Total_ROs, ' ROs scanned out all cartons'),
             
             paste0('Count Centre Scan in to ULD : ',  RO_5.14_tally, ' / ', Total_Count_centres, ' CCs scanned in all cartons'),
             paste0('Count Centre Scan in to ULD : ',  RO_5.14_tally, ' / ', Total_Count_centres, ' CCs scanned in all cartons'))


Scanned <- rep(c('Scanned',
                 'Remaining'), 7)


Percent <- c(round(RO_2.0_tally/Total_ROs,        digits = 3), 
             round((1 - RO_2.0_tally/Total_ROs),  digits = 3),
             
             round(RO_3.0_tally/Total_ROs,        digits = 3), 
             round((1 - RO_3.0_tally/Total_ROs),  digits = 3),
             
             round(RO_4.0_tally/Total_ROs,        digits = 3), 
             round((1 - RO_4.0_tally/Total_ROs),  digits = 3),
             
             round(RO_5.10_tally/Total_ROs,       digits = 3), 
             round((1 - RO_5.10_tally/Total_ROs), digits = 3),
             
             round(RO_5.11_tally/Total_ROs,       digits = 3), 
             round((1 - RO_5.11_tally/Total_Count_centres), digits = 3),
             
             round(RO_5.0_tally/Total_ROs,        digits = 3), 
             round((1 - RO_5.0_tally/Total_ROs),  digits = 3),
             
             round(RO_5.14_tally/Total_ROs,       digits = 3), 
             round((1 - RO_5.14_tally/Total_Count_centres), digits = 3))





## Create a dataframe with % for labels 
ballot_track_overall_progress <- data.frame(Status, Scanned, Percent) %>% 
  mutate(Percent = round(Percent*100)) %>% 
  mutate(Status    = factor(Status, 
                            levels = rev(unique(Status))))





## Create flex table for Ballot Track headline
BallotTrack_Status_Headline <- tibble(
  
  ## Row names
  Names = c('Ballot Track Scan Status',
            'Returning Office/STH/CPVC Scan-in from Printer',
            'Returning Office Scan-out to Venue',
            'Returning Office Scan-in from Venue',
            'Returning Office Scan-out to Count Centre',
            'Count Centre Scan-in from Returning Office',
            'Returning Office Scan-out to Warehouse'),
  
  ## Values
  Values = c("Count of RO Offices fully scanned",
             paste0(RO_2.0_tally,   '/', Total_ROs, ' (', RO_2.0_percent, ')'),
             paste0(RO_3.0_tally,   '/', Total_ROs, ' (', RO_3.0_percent, ')'),
             paste0(RO_4.0_tally,   '/', Total_ROs, ' (', RO_4.0_percent, ')'),
             paste0(RO_5.10_tally,  '/', Total_ROs, ' (', RO_5.10_percent, ')'),
             paste0(RO_5.11_tally,  '/', Total_ROs, ' (', RO_5.11_percent, ')'),
             paste0(RO_5.0_tally,   '/', Total_ROs, ' (', RO_5.0_percent, ')'))) %>%
  
  flextable() %>%
  
  ## Set background
  bg(bg = "deepskyblue", i =~ Values != "Count of RO Offices fully scanned") %>%
  bg(bg = "coral",       i =~ str_detect(Names, "5."))                       %>%
  bg(bg = "black",       i =~ Values == "Count of RO Offices fully scanned") %>%
  
  ## Fontsize, color and bold
  fontsize(size = flex_headline_size) %>%
  bold(i = 1,  j = NULL, bold = TRUE, part = "body") %>%
  
  color(color = "white", i=~ Values == "Count of RO Offices fully scanned") %>%
  color(color = "white", i=~ Names  == "Total Pre-Poll:") %>%
  color(color = "white", part = "all") %>%
  
  width(j =~ Names, width = 600) %>%
  width(j =~ Values,width = 100) %>%
  delete_part(part = "header")   %>%
  border(border    = fp_border(color = "white", width = 3)) %>%
  height_all(., height = 10)


## Final message
message('All BallotTrack processor code run ')





################################## ---- TBC ---- ########################################
