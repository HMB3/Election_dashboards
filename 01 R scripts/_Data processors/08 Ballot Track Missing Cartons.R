################################## ---- BALLOT TRACK DASHBAORD ---- ################################################


## This code processes the data to create the 'Candidate nominations' dashboard
message('Run R code to Track Ballot Track Cartons')


## ALL BARCODES
RO_all_barcodes <- RO_all_status %>% distinct(Barcode)





## 2.0). SCAN IN FROM PRINTER BARCODES ============================================================================


## Stage 2.0 barcodes - these filters need to match what we've put below
RO_2.0_scan_in_from_printer_barcodes <- RO_all_status %>%
  
  ## Apply business rules for status 2.0
  filter(ContainerType  ==  'Carton')               %>%
  filter(LocationType   !=  'Declaration Votes')    %>% 
  filter(LocationType   !=  'Declared Institution') %>%
  filter(`Contest Name` !=  'Mixed')                %>%
  
  ## Filter out the later statuses, but include the NAs
  subset(`Status Stage` < 2.10 | is.na(`Status Stage`)) %>%
  
  ## Classify barcodes as scanned or unscanned 
  mutate(Status = ifelse(is.na(`Status Stage`), 'Unscanned', 'Scanned')) %>% distinct(Barcode)



stage_2.0_standard_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 2.0 & ! str_detect(`Contest Name`, "RESERVE STOCK", negate = TRUE)) %>% distinct(Barcode)

stage_2.0_all_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 2.0) %>% distinct(Barcode)







## Stage 3.0 barcodes
stage_3.0_reserve_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 3.0 & ! str_detect(`Contest Name`, "RESERVE STOCK")) %>% distinct(Barcode)

stage_3.0_standard_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 3.0 & ! str_detect(`Contest Name`, "RESERVE STOCK", negate = TRUE)) %>% distinct(Barcode)

stage_3.0_all_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 3.0) %>% distinct(Barcode)


## Stage 4.0 barcodes
stage_4.0_reserve_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 4.0 & ! str_detect(`Contest Name`, "RESERVE STOCK")) %>% distinct(Barcode)

stage_4.0_standard_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 4.0 & ! str_detect(`Contest Name`, "RESERVE STOCK", negate = TRUE)) %>% distinct(Barcode)

stage_4.0_all_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 4.0) %>% distinct(Barcode)


## Stage 4.12 barcodes
stage_4.12_reserve_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 4.12 & ! str_detect(`Contest Name`, "RESERVE STOCK")) %>% distinct(Barcode)

stage_4.12_standard_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 4.12 & ! str_detect(`Contest Name`, "RESERVE STOCK", negate = TRUE)) %>% distinct(Barcode)

stage_4.12_all_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 4.12) %>% distinct(Barcode)


## Stage 5.1 barcodes
stage_5.1_reserve_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 5.1 & ! str_detect(`Contest Name`, "RESERVE STOCK")) %>% distinct(Barcode)

stage_5.1_standard_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 5.1 & ! str_detect(`Contest Name`, "RESERVE STOCK", negate = TRUE)) %>% distinct(Barcode)


stage_5.1_all_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 5.1) %>% distinct(Barcode)


## Stage 5.11 barcodes
stage_5.11_reserve_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 5.11 & ! str_detect(`Contest Name`, "RESERVE STOCK")) %>% distinct(Barcode)

stage_5.11_standard_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 5.11 & ! str_detect(`Contest Name`, "RESERVE STOCK", negate = TRUE)) %>% distinct(Barcode)

stage_5.11_all_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 5.11) %>% distinct(Barcode)


## Stage 5.0 barcodes
stage_5.0_reserve_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 5.0 & ! str_detect(`Contest Name`, "RESERVE STOCK")) %>% distinct(Barcode)

stage_5.0_standard_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 5.0 & ! str_detect(`Contest Name`, "RESERVE STOCK", negate = TRUE)) %>% distinct(Barcode)

stage_5.0_all_barcodes <- Barcode_endpoint %>% 
  filter(`Status Stage` == 5.0) %>% distinct(Barcode)


## List of all the barcodes in the endpoint
## Do we need a table, or do we need just a list (i.e. vector) of barcodes?
## Also, need to clarify what is compared with what
all_barcodes               <- Barcode_endpoint %>% distinct(Barcode)
all_barcode_without_status <- Barcode_endpoint %>% filter(is.na(`Status Stage`))  %>% distinct(Barcode)
all_barcode_with_status    <- Barcode_endpoint %>% filter(!is.na(`Status Stage`)) %>% distinct(Barcode)
nrow(all_barcodes);nrow(all_barcode_without_status);nrow(all_barcode_with_status)


## Create Anti-joins to track the barcodes ----
## Anti-join returns all rows in 'X', that don't have a match in 'Y'
## What list of barcodes are we comparing with initially, and for each status?
## Does each stage need an additional column?

## Purpose of this section is to Track the barcodes that might go missing at each stage
## What does deepak need to see then?


## Stage 2
starting_barcodes_not_in_s2 <- anti_join(all_barcode_with_status,  stage_2.0_all_barcodes,  by = "Barcode") %>% 
  mutate(`Scanning change` = 'None to 2.0')

S2.1_barcodes_not_in_s2     <- anti_join(stage_2.0_all_barcodes, stage_2.1_all_barcodes,    by = "Barcode") %>% 
  mutate(`Scanning change` = '2.0 to 2.1')


S2_barcodes_not_in_S3       <- anti_join(stage_2.0_all_barcodes,   stage_3.0_all_barcodes,  by = "Barcode") %>% 
  mutate(`Scanning change` = '2.0 to 3.0')


S3_barcodes_not_in_S4       <- anti_join(stage_3.0_all_barcodes,   stage_4.0_all_barcodes,  by = "Barcode") %>% 
  mutate(`Scanning change` = '3.0 to 4.0')


S4_barcodes_not_in_S4.12    <- anti_join(stage_4.0_all_barcodes,   stage_4.12_all_barcodes, by = "Barcode") %>% 
  mutate(`Scanning change` = '4.0 to 4.12')


S4.12_barcodes_not_in_S5.1  <- anti_join(stage_4.12_all_barcodes,  stage_5.1_all_barcodes,  by = "Barcode") %>% 
  mutate(`Scanning change` = '4.12 to 5.1')


S5.1_barcodes_not_in_S5.11  <- anti_join(stage_5.1_all_barcodes,   stage_5.11_all_barcodes, by = "Barcode") %>% 
  mutate(`Scanning change` = '5.1 to 5.11')


S5.11_barcodes_not_in_S5.0  <- anti_join(stage_5.11_all_barcodes,  stage_5.0_all_barcodes,  by = "Barcode") %>% 
  mutate(`Scanning change` = '5.11 to 5.0')


## Combine the tables
#barcode_tracking <- join_all(list(starting_barcodes_not_in_s2, S2.1_barcodes_not_in_s2, S2_barcodes_not_in_S3))
