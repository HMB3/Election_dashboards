
################################## ---- HOW TO VOTE DASHBAORD ---- ################################################



## This code processes the data to create the 'How To Vote' dashboard
message('Run R code to process How to Vote data')






# Functions to add missing groups to data ---------------------------------



## Function to add 0/NA column if it doesn't already exist
add_zero_cols <- function(data, cname) {
  
  add         <- cname[!cname %in% names(data)]
  if(length(add)!=0) data[add] <- 0
  data
}


add_NA_cols <- function(data, cname) {
  
  add         <- cname[!cname %in% names(data)]
  if(length(add)!=0) data[add] <- NA
  data
  
}


## Function to make empty character strings NA
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}





## 1). REGO MATERIAL BY DATE / STATUS =============================================================================


# How can we value add? 
# 
# Trying to show when people lodge, best to show
# non-cumulative lodgements over time.


# Registration of Electoral Material by date / status:
#   
# Lodged 
# HO 1 Approved = processing 
# HO 1 Rejected = supervisor review
# HO 2 Rejected = supervisor review
# Accepted 
# Rejected
# Withdrawn

if(file_list >= 5) {
  
  ## Convert statuses
  message('HTV data exists, create dashboard')
  Materials_Export_status_convert <- Materials_Export %>% 
    
    ## Convert Statuses first
    mutate(Status = str_to_title(Status),
           Status = case_when(
             
             ## These are Ambers Display categories
             grepl("Lodged",            Status, ignore.case = TRUE) ~ "Processing",
             grepl("HO 1 Approved",     Status, ignore.case = TRUE) ~ "Processing",
             grepl("HO 1 Rejected",     Status, ignore.case = TRUE) ~ "Processing",
             grepl("HO 2 Rejected",     Status, ignore.case = TRUE) ~ "Processing",
             grepl("Supervisor Review", Status, ignore.case = TRUE) ~ "Supervisor Review",
             grepl("Approved",          Status, ignore.case = TRUE) ~ "Accepted",
             grepl("Withdrawn",         Status, ignore.case = TRUE) ~ "Withdrawn",
             grepl("Rejected",          Status, ignore.case = TRUE) ~ "Rejected"),
           
           ## Convert the date so we can aggregate
           DateLodged = as_date(`Date/Time Lodged`, tz = NULL))
  
  
  ## Count statuses
  Total_status_count <- Materials_Export_status_convert %>% 
    
    ## Group by status and tally
    group_by(Status) %>% 
    tally(., name = "Total pieces of material") %>% 
    arrange(-`Total pieces of material`) 
  
  
  ## Need to add the statuses in steadily
  if("Rejected" %!in% Total_status_count$Status) {
    Total_status_count <- Total_status_count %>% add_row(Status = "Rejected",
                                                         `Total pieces of material` = 0)  
  }
  
  # if("Supervisor Review" %!in% Total_status_count$Status) {
  #   Total_status_count <- Total_status_count %>% add_row(Status = "Supervisor Review",
  #                                                        `Total pieces of material` = 0)  
  # }
  
  if("Withdrawn" %!in% Total_status_count$Status) {
    Total_status_count <- Total_status_count %>% add_row(Status = "Withdrawn",
                                                         `Total pieces of material` = 0)  
  }
  
  # if("Lodged" %!in% Total_status_count$Status) {
  #   Total_status_count <- Total_status_count %>% add_row(Status = "Lodged",
  #                                                        `Total pieces of material` = 0)  
  # }
  
  if("Accepted" %!in% Total_status_count$Status) {
    Total_status_count <- Total_status_count %>% add_row(Status = "Accepted",
                                                         `Total pieces of material` = 0)  
  }
  
  
  if("Processing" %!in% Total_status_count$Status) {
    Total_status_count <- Total_status_count %>% add_row(Status = "Processing",
                                                         `Total pieces of material` = 0)  
  }
  
  
  
  
  
  ## Create Status grouped by date for plotting
  Materials_status_date <- Materials_Export_status_convert %>%
    
    ## Then aggregate data
    group_by(DateLodged, Status)   %>%
    summarise(NumberOfItems = n()) %>%
    ungroup()                      %>%
    mutate(Status = fct_relevel(Status, "Accepted", "Processing", "Supervisor Review", "Lodged", "Rejected"))
  
  
  
  
  
  ## 2). DAILY PROCESSING TIME  =====================================================================================
  
  
  # Daily processing time
  # Min
  # Max
  # Average
  KPI_Summary_Report_convert <- KPI_Summary_Report %>%
    
    mutate(Min = as.numeric(Min),
           Max = as.numeric(Max),
           Avg = as.numeric(Avg)) %>% 
    mutate_all(~replace(., is.na(.), 0))
  
  
  ## How do summarise the units here? Minutes, seconds 
  KPI_Summary_Report_convert_numbers <- KPI_Summary_Report_convert %>% 
    
    ## Just the rows and columns we want from this table
    filter(From == "Lodged") %>% 
    filter(To   == "Accepted" | To == "Rejected") %>% 
    
    group_by(To) %>% 
    summarise(Min = mean(Min),
              Max = mean(Max),
              Avg = mean(Avg)) %>% 
    
    mutate_all(~replace(., is.na(.), 0))
  
  
  
  
  
  ## KPI detail report ----
  ## What do they want here? 
  ## Do they want to know the distribution of time taken to process the material, from the start to the end?
  ## So for each item number, calculate the difference between the lodged time, and the first status it hits out of:
  ## max/mean/min of: lodged time - 1st of Accetped/Rejected/SR/Withdrawn times
  
  
  ## 
  KPI_Detail_Report_date_time <- KPI_Detail_Report %>% 
    
    ## Re-level the factors for the plot
    mutate(`Status Date` = convertToDateTime(`Status Date`),
           `Material Id` = as.character(`Material Id`),
           
           ## Change the statuses
           Status = case_when(
             
             ## These are Ambers Display categories
             grepl("HO 1 Approved",  Status, ignore.case = TRUE) ~ "Processing",
             grepl("HO 1 Rejected",  Status, ignore.case = TRUE) ~ "Supervisor Review",
             grepl("HO 2 Rejected",  Status, ignore.case = TRUE) ~ "Supervisor Review",
             grepl("Approved",       Status, ignore.case = TRUE) ~ "Accepted",
             grepl("Withdrawn",      Status, ignore.case = TRUE) ~ "Withdrawn",
             grepl("Rejected",       Status, ignore.case = TRUE) ~ "Rejected",
             grepl("Lodged",         Status, ignore.case = TRUE) ~ "Lodged")) %>% 
    
    ## Pivot the columns into
    group_by(`Material Id`) %>% 
    dplyr::select(-`Is Status Deleted`, -`Status Deleted Date`) %>% 
    mutate(Status = fct_relevel(Status, 
                                "Lodged", 
                                "Processing", 
                                "Supervisor Review", 
                                "Rejected", 
                                "Accepted", 
                                "Withdrawn")) %>% 
    
    ## Re-level the factors for the time calculations
    pivot_wider(names_from  = Status, 
                values_from = `Status Date`, 
                values_fn   = mean) %>% 
    
    ## Add the factors if they don't exist 
    add_NA_cols(., 'Processing')        %>%
    add_NA_cols(., 'Supervisor Review') %>%
    add_NA_cols(., 'Accepted')          %>%
    
    add_NA_cols(., 'Withdrawn')         %>%
    add_NA_cols(., 'Rejected')          %>%
    add_NA_cols(., 'Lodged')            %>%
    
    
    ## Re-order the columns
    dplyr::select(`Material Id`, Lodged, Processing, `Supervisor Review`, Rejected, Accepted, Withdrawn) %>% 
    
    ## Then make a new column for each status, AND also an overall one
    ## Time taken from Lodged to supervisor review, rejected, accepted or withdrawn
    ## Supervisor review counts as a final status - could sit there for hours, don't want to catch that
    mutate(`Supervisor Review Time` = ifelse(!is.na(`Supervisor Review`), 
                                             abs(difftime(`Supervisor Review`, 
                                                          Lodged, units = "hours")), NA),
           
           `Rejected Time` = ifelse(!is.na(Rejected), 
                                    abs(difftime(Rejected, 
                                                 Lodged, units = "hours")), NA),
           
           `Accepted Time` = ifelse(!is.na(`Accepted`), 
                                    abs(difftime(`Accepted`, 
                                                 Lodged, units = "hours")), NA),
           
           `Withdrawn Time` = ifelse(!is.na(Withdrawn), 
                                     abs(difftime(Withdrawn, 
                                                  Lodged, units = "hours")), NA),
           
           ## Then make a column that combines the review times together
           `Review Time` = coalesce(`Supervisor Review Time`, 
                                    `Rejected Time`, 
                                    `Accepted Time`,
                                    `Withdrawn Time`)) %>% 
    
    ## Finally, convert all the NAs to zero, so we can do arithemtic below
    # mutate_if(., is.numeric,   replace_na, replace = 0) %>% 
    mutate_if(., is.logical, replace_na, replace = 0)
  
  
  ## Then we need to calculate the time for each item 
  
  
  ## Double check this table has all the materal inside it
  identical(length(unique(KPI_Detail_Report$`Material Id`)),
            length(unique(KPI_Detail_Report_date_time $`Material Id`)))
  
  
  
  ## Create numbers for each total
  ## We want it to get to 
  Min_process_time <- KPI_Detail_Report_date_time %>% .$`Review Time`   %>% min(.,  na.rm = T)
  Av_process_time  <- KPI_Detail_Report_date_time %>% .$`Review Time`   %>% mean(., na.rm = T)
  Max_process_time <- KPI_Detail_Report_date_time %>% .$`Review Time`   %>% max(.,  na.rm = T)
  
  Approved_min     <- KPI_Detail_Report_date_time %>% .$`Accepted Time` %>% min(.,  na.rm = T)
  Approved_av      <- KPI_Detail_Report_date_time %>% .$`Accepted Time` %>% mean(., na.rm = T)
  Approved_max     <- KPI_Detail_Report_date_time %>% .$`Accepted Time` %>% max(.,  na.rm = T)
  
  Rejected_min     <- KPI_Detail_Report_date_time %>% .$`Rejected Time` %>% min(.,  na.rm = T)
  Rejected_av      <- KPI_Detail_Report_date_time %>% .$`Rejected Time` %>% mean(., na.rm = T)
  Rejected_max     <- KPI_Detail_Report_date_time %>% .$`Rejected Time` %>% max(.,  na.rm = T)
  
  
  
  
  
  ## HTV material grouped by date ----
  KPI_HTV_group_date <- KPI_Detail_Report %>% 
    
    ## Re-level the factors for the plot
    mutate(`Status Date` = convertToDateTime(`Status Date`),
           `Material Id` = as.character(`Material Id`),
           
           ## Change the statuses
           Status = case_when(
             
             ## These are Ambers Display categories
             grepl("HO 1 Approved",  Status, ignore.case = TRUE) ~ "Processing",
             grepl("HO 1 Rejected",  Status, ignore.case = TRUE) ~ "Supervisor Review",
             grepl("HO 2 Rejected",  Status, ignore.case = TRUE) ~ "Supervisor Review",
             grepl("Approved",       Status, ignore.case = TRUE) ~ "Accepted",
             grepl("Withdrawn",      Status, ignore.case = TRUE) ~ "Withdrawn",
             grepl("Rejected",       Status, ignore.case = TRUE) ~ "Rejected",
             grepl("Lodged",         Status, ignore.case = TRUE) ~ "Lodged")) %>% 
    
    ## Pivot the columns into
    # group_by(Status) %>% 
    dplyr::select(-`Is Status Deleted`, - `Status Deleted Date`) %>% 
    mutate(Status = fct_relevel(Status, 
                                "Lodged", 
                                "Processing", 
                                "Supervisor Review", 
                                "Rejected", 
                                "Accepted", 
                                "Withdrawn")) %>%
    
    ## Split the date into date/time 
    separate(., col  =`Status Date`, into = c('Date', 'Time'), sep = ' ') %>%
    mutate(Date      = empty_as_na(Date)) %>% 
    mutate(Date      = as.Date(Date)) %>% 
    
    ## Group by date and take the cumulative sum
    group_by(Date, Status)   %>%
    summarise(Lodgements = n())    %>% 
    arrange(Date) %>%
    
    #wantthe cumulaive sum to be by Vote type
    group_by(Status) %>%
    mutate(Lodg_cum_count = cumsum(Lodgements))
  
  
  
  
  
  ## 4). TOTAL PIECES OF MATERIAL ====================================================================================
  
  
  # Total pieces of material containing voting instructions for: 
  # Councillor  
  # Mayor   
  # Referendum
  # Poll  
  # Other
  contest_type <- c('Mayor', 
                    'Councillor',
                    'Referendum',
                    'Poll',
                    'Other')
  
  
  ## Build the totals for each type separately, because the totals clash
  ## Where material references several types, ie poll/referendum, 
  ## we need to add +1 to both Poll and Referendum - is this taken care of below, anyway?
  Total_pieces_material <- tibble(
    
    ## List the types
    `Material Type` = c('Mayor',
                        'Councillor',
                        'Referendum',
                        'Poll', 
                        'Other'),
    
    ## Count the material types separately, rather than making a composite table
    `Total pieces of material` = c(Materials_Export_status_convert %>% filter(!is.na(`Mayor`))         %>% nrow(),
                                   Materials_Export_status_convert %>% filter(!is.na(`Councillor(s)`)) %>% nrow(),
                                   Materials_Export_status_convert %>% subset(grepl('Referendum', `Referendum / Poll / Other`)) %>% nrow(),
                                   Materials_Export_status_convert %>% subset(grepl('Poll',       `Referendum / Poll / Other`)) %>% nrow(),
                                   Materials_Export_status_convert %>% subset(grepl('Other',      `Referendum / Poll / Other`)) %>% nrow())) %>% 
    
    ## Re-level the factors for the plot
    mutate(`Material Type` = fct_relevel(`Material Type`, rev(contest_type)))
  
  
  
  
  
  ## 5). TOTAL PIECES OF MATERIAL BY COUNCIL ==========================================================================
  
  
  ## Table of accepted pieces of material by Council 
  ## Sorted Smallest to largest  
  Total_pieces_material_council <- Materials_Export_status_convert %>% 
    
    ## Just take the RPP column
    group_by(`Council Name`)                                  %>%
    tally(., name = "Total pieces of material")               %>% 
    arrange(`Council Name`, desc(`Total pieces of material`)) %>% 
    mutate(Status = 'All Statuses')                   
  
  
  
  
  
  ## 6). TOTAL PIECES OF MATERIAL BY RPP ==============================================================================
  
  
  ## Aggregate and count these three columns :
  # "Mayor", "Councillor(s)", "Referendum / Poll / Other"
  
  # Number of applications & material submitted by RPP.  
  # Calculate the number of RPP candidates and groups prior
  # RPP summary report only calculates the No. of councillor/mayor material lodged
  
  
  # We want to know how many materials have been submitted for each RPP vs how many candidates 
  # for a specific RPP exist. 
  
  # I.e. labour material = 3 but there 50 labour candidates.
  # The current report from 'RPP summary report' notes how many materials relate to the RPP. 
  # However, DAGS team will need to work out or determine how many candidates from each RPP there is.
  
  
  # Join RPP_summary to Noms_extract. 
  # Then aggregate by RPP and count Candidates (total items lodged, mayor and councillor)
  
  
  # 1)	Number of groups - count the distinct combinations of ContestID and group label by party)
  # 2)	Number of candidates with no group - count the number of candidates with no group label by party 
  #     (this will include all mayoral candidates, but we don't need to separate them)
  
  
  # 1)	Number of groups - count the distinct combinations of ContestID and group label by party)
  Groups_label_RPP <- noms_extract %>% 
    
    ## Just take the RPP column
    filter(!(is.na(RPPNAME) | RPPNAME =='') & !(is.na(GROUPLABEL) | GROUPLABEL =='')) %>% 
    dplyr::select(CONTESTID, RPPNAME, GROUPLABEL) %>% 
    distinct() %>% 
    group_by(RPPNAME)  %>%
    summarise(`Group count` = n()) 
  
  
  # 2)	Number of candidates with no group - count the number of candidates with no group label by party 
  #     (this will include all mayoral candidates, but we don't need to separate them)
  ## Blan rows are the independents here
  Candidates_no_Groups_RPP <- noms_extract %>% 
    
    ## Just take the RPP column
    filter(!(is.na(RPPNAME) | RPPNAME =='') & (is.na(GROUPLABEL) | GROUPLABEL =='')) %>% 
    group_by(RPPNAME) %>%
    tally(., name = 'Ungrouped Candidates')  
  
  
  
  ## Create separate tables of RPP names to join to the RPP report
  RPP_names      <- RPP_Summary_Report %>% dplyr::select(PARTYABBREVIATEDNAME = 'Group Name') %>%
    dplyr::rename(HTVNAME   = PARTYABBREVIATEDNAME) %>% 
    mutate(HTV_Check = HTVNAME)
  
  
  ## This table needs to have 
  Party_name_ref <- EMA_Eligible_Parties %>% 
    left_join(RPP_names, by = c("PARTYABBREVIATEDNAME" = "HTVNAME")) %>% 
    left_join(RPP_names, by = c("PARTYNAME"            = "HTVNAME")) %>% 
    mutate(HTVNAME = ifelse(is.na(HTV_Check.x), HTV_Check.y, HTV_Check.x)) %>% 
    dplyr::select(-contains('HTV_Check')) %>% 
    
    ## Removing duplicated HTV name
    mutate(HTVNAME = ifelse(PARTYNAME == 'LAKE MAC INDEPENDENTS', 'LAKE MAC INDEPENDENTS', HTVNAME))
  
  
  ## Join RPP report 
  
  
  ## RPP table
  ## Count the material, but also the groups, and the candiates without a group
  Total_material_by_RPP_candidates <- RPP_Summary_Report %>% 
    
    ## Rename
    dplyr::rename(HTVNAME = `Group Name`) %>% 
    
    ## Total the items for both councillor and mayor
    mutate(`Lodged For Mayor`                  = as.numeric(`Lodged For Mayor`),
           `Lodged For Councillor`             = as.numeric(`Lodged For Councillor`),
           `Total pieces of material received` = `Lodged For Mayor` + `Lodged For Councillor`) %>% 
    
    ## Join and manage party abbreviated names DEBACLE!!! 
    ## There are items 
    full_join(Party_name_ref, by = "HTVNAME") %>%
    
    ## Try filling in Partyname with Party Abbreviated
    mutate(EMA_missing = ifelse(is.na(PARTYNAME), TRUE, FALSE),
           PARTYNAME   = ifelse(is.na(PARTYNAME), HTVNAME, PARTYNAME)) %>% 
    
    ##
    full_join(Groups_label_RPP,         by = c("PARTYNAME" = "RPPNAME")) %>% 
    full_join(Candidates_no_Groups_RPP, by = c("PARTYNAME" = "RPPNAME")) %>%  
    
    ## Replace the numeric NA values with 0
    mutate(`Group count` = ifelse(is.na(`Group count`) & EMA_missing, NA, `Group count`),
           `Ungrouped Candidates` = ifelse(is.na(`Ungrouped Candidates`) & EMA_missing, NA, `Ungrouped Candidates`),
           
           `Group count` = ifelse(is.na(`Group count`) & !EMA_missing, 0, `Group count`),
           `Ungrouped Candidates` = ifelse(is.na(`Ungrouped Candidates`) & !EMA_missing, 0, `Ungrouped Candidates`),
           
           `Total pieces of material expected` = `Group count` + `Ungrouped Candidates`,
           Difference = `Total pieces of material received` - `Total pieces of material expected`) %>%
    
    ## Order the columns
    dplyr::select(PARTYNAME, PARTYABBREVIATEDNAME, `Total pieces of material expected`, `Lodged For Mayor`, `Lodged For Councillor`, 
           `Total pieces of material received`, Difference)
  
  
 
  
  
  ## 7). CANDIDATES YET TO SUBMIT ==============================================================================
  
  
  ## Just aggregate the above join and count rows
  
  
  # Total Number of groups vs how many groups have submitted material, and how many haven't  
  # Total no. of mayor or ungrouped councillors vs. how many have submitted material, and how many haven't 
  
  
  # We want to determine an estimate what candidates or groups still haven't yet submitted an item of material. 
  # We expect each candidate or group to submit at least 1 item of material.
  
  
  ## Manipulate NOMs candidate data
  ## This is noms extract
  noms_candidates_count <- noms_extract %>% 
    
    ## Get the name on the ballot
    mutate(BALLOTSURNAME = str_to_lower(BALLOTSURNAME),
           BALLOTSURNAME = str_to_title(BALLOTSURNAME),
           `Full Name`   = paste0(BALLOTGIVENNAMES, ' ', BALLOTSURNAME),
           `Full Name`   = str_trim(`Full Name`)) %>% 
    
    ## Make sure we get the distinct names
    dplyr::select(`Full Name`) %>% group_by(`Full Name`) %>% tally(n = 'Count')
  
  
  ## Manipulate NOMs candidate data
  ## This is noms extract
  noms_candidates_type <- noms_extract %>% 
    
    ## Get the name on the ballot
    mutate(BALLOTSURNAME = str_to_lower(BALLOTSURNAME),
           BALLOTSURNAME = str_to_title(BALLOTSURNAME),
           `Full Name`   = paste0(BALLOTGIVENNAMES, ' ', BALLOTSURNAME),
           `Full Name`   = str_trim(`Full Name`)) %>% 
    
    ## Make sure we get the distinct names
    dplyr::select(`Full Name`,  CONTESTTYPECODE)    %>% 
    dplyr::rename(`Contest Type` = CONTESTTYPECODE) %>% 
    distinct()
  
  
  ## HTV Candidates
  Candidates_full_name <- Candidate_Summary_Report %>%
    
    ## Create a table of
    mutate(`Full Name` = paste0(`First Name`, ' ', `Last Name`),
           `Full Name` = str_to_lower(`Full Name`),
           `Full Name` = str_to_title(`Full Name`),
           
           `Last Name` = str_to_lower(`Last Name`),
           `Last Name` = str_to_title(`Last Name`))
  
  
  ## Count the candidate names
  HTV_candidates <- Candidates_full_name %>%  group_by(`Full Name`) %>% tally(n = 'Name Count') 
  
  
  ## Table of Candidates who haven't sumbitted any material
  Candidates_material <- Candidates_full_name  %>%
    
    ## Join on the Noms Candidates - need to be careful here.
    ## We can't match the Candidate summary report with the noms candidates if their are 
    ## Name duplicates, EG different names in different contests.
    dplyr::select(`Full Name`, `Last Name`,
           `Represent a Group`, `Lodged For Mayor`, `Lodged For Councillor`) %>% 
    
    ## Which Candidates haven't submitted any material at all?
    mutate(`Lodged For Mayor`         = as.numeric(`Lodged For Mayor`),
           `Lodged For Councillor`    = as.numeric(`Lodged For Councillor`),
           `Total pieces of material` = `Lodged For Mayor` + `Lodged For Councillor`) 
  
  
  ## Does this table of candidates need to match the EMA parties? 
  Candidates_not_submitted <- Candidates_material %>% 
    
    ## Filter
    filter(`Total pieces of material` == 0) %>% 
    arrange(desc(`Represent a Group`), `Last Name`)
  
  
  ## Calculate the % of submitted vs unsubmitted RPP
  Total_candidates             <- Candidates_material %>% nrow()
  Total_candidates_submitted   <- Candidates_material %>% filter(`Total pieces of material` > 0)  %>% nrow()
  Total_candidates_unsubmitted <- Candidates_material %>% filter(`Total pieces of material` == 0) %>% nrow()
  
  Total_mayor_submitted        <- Candidates_material %>% filter(`Lodged For Mayor` > 0)  %>% nrow()
  Total_mayor_unsubmitted      <- Candidates_material %>% filter(`Lodged For Mayor` == 0) %>% nrow()
  
  Total_council_submitted      <- Candidates_material %>% filter(`Represent a Group` == 'No' & `Lodged For Councillor` > 0)  %>% nrow()
  Total_council_unsubmitted    <- Candidates_material %>% filter(`Represent a Group` == 'No' & `Lodged For Councillor` == 0) %>% nrow()
  
  Total_submitted_percent      <- paste0(round(Total_candidates_submitted/ Total_candidates   * 100, digits = 0), "%")
  Total_ubsubmitted_percent    <- paste0(round(Total_candidates_unsubmitted/ Total_candidates * 100, digits = 0), "%")
  
  
  
  ## Calculate the % of submitted vs unsubmitted RPP 
  # Total_RPP               <- nrow(EMA_Eligible_Parties)
  # Total_RPP_submitted     <- Total_material_by_RPP_candidates %>% filter(`Total pieces of material` > 0)  %>% nrow()
  # Total_RPP_unsubmitted   <- Total_material_by_RPP_candidates %>% filter(`Total pieces of material` == 0) %>% nrow()
  # 
  # RPP_submitted_percent   <- paste0(round(Total_submitted/ Total_RPP * 100,   digits = 0), "%")
  # RPP_ubsubmitted_percent <- paste0(round(Total_unsubmitted/ Total_RPP * 100, digits = 0), "%")
  
  
  ## HTV RPP count table
  Total_material_by_RPP_count <- tibble(
    
    `Candidates` = c('Submitted Candidates',
                     'Un-submitted Candidates'),
    
    `Applications by candidates` = c(Total_candidates_submitted,
                                     Total_candidates_unsubmitted))
  
  
  
  
  
  ## CHECK DATA  ==========================================================
  
  
  ## Now find out which rows are in the endpoint, but not the webportal
  if(test_data) {
    
    ## Check the statuses
    file_list <- c('Materials_Export_status_convert',
                   'KPI_Detail_Report_date_time',
                   
                   'RPP_Summary_Report',
                   'Total_material_by_RPP_candidates',
                   
                   'Candidates_not_submitted')
    
    
    ## Add data to workbook
    HTV_workbook <- createWorkbook()
    
    ## file = file_list[1]
    for(file in file_list) {
      
      ## Get required columns.
      File_to_Write <- get(file)
      
      ## Add worksheet to the spread sheet
      message('writing ', file,  ' to check ballot track data')
      addWorksheet(HTV_workbook, file)
      
      ## Write the data to the corresponding worksheet
      writeDataTable(wb       = HTV_workbook, 
                     sheet    = file,
                     x        = get(file),
                     startCol = 1, 
                     startRow = 1, 
                     rowNames = FALSE,
                     tableStyle = "TableStyleMedium2")
      
    }
    
    ## Save the whole workbook
    saveWorkbook(HTV_workbook, 
                 paste0(HTV_server_data, "HTV_data_check.xlsx"),
                 overwrite = TRUE)
  }
  
  
  
  
  
  ## 8). HIGHLIGHTS TABLE ==============================================================================
  
  
  ## Create a flex table for the HTV Headlines
  How_to_Vote_Headline <- tibble(
    
    Names = c('How to Vote Summary',
              'Candidates with submitted materials',
              'Candidates yet to submitted materials',
              
              "Processing Times",
              
              'Minimum', 
              'Average',
              'Maximum',
              
              "Material by Status",
              
              'Accepted Materials', 
              'Processing Materials',
              
              'Rejected Materials', 
              'Withdrawn Materials',
              'Total Materials Lodged'),
    
    Values = c("Count",
               Total_candidates_submitted,
               Total_candidates_unsubmitted,
               
               "",
               
               Approved_min %>% round(digits = 2) %>% paste0(., ' Hours'),
               Approved_av  %>% round(digits = 2) %>% paste0(., ' Hours'),
               Approved_max %>% round(digits = 2) %>% paste0(., ' Hours'),
               
               "",
               
               Total_status_count %>% filter(Status == 'Accepted')   %>% .$`Total pieces of material`,
               Total_status_count %>% filter(Status == 'Processing') %>% .$`Total pieces of material`,
               
               Total_status_count %>% filter(Status == 'Rejected')   %>% .$`Total pieces of material`,
               Total_status_count %>% filter(Status == 'Withdrawn')  %>% .$`Total pieces of material`,
               Total_status_count %>%                                    .$`Total pieces of material` %>% sum())) %>% 
    
    flextable() %>% 
    
    ## Set background
    bg(bg = "deepskyblue", i =~ str_detect(Names, "Materials", negate = TRUE)) %>%
    bg(bg = "coral",       i =~ str_detect(Names, "Materials")) %>%
    bg(bg = "black",       i =~ Values == "Count")              %>%
    bg(bg = "black",       i=~ Values == "")                    %>%
    
    ## Fontsize, color and bold 
    fontsize(size = flex_headline_size) %>%
    bold(i = 1,  j = NULL, bold = TRUE, part = "body")      %>%
    bold(i = 4,  j = NULL, bold = TRUE, part = "body")      %>%
    bold(i = 8,  j = NULL, bold = TRUE, part = "body")      %>%
    
    color(color = "white", i=~ Values == "Count")           %>%
    color(color = "white", i=~ Names  == "Total Pre-Poll:") %>%
    color(color = "white", part = "all")                    %>%
    
    width(j =~ Names, width = 600) %>%
    width(j =~ Values,width = 100) %>%
    delete_part(part = "header")   %>%
    border(border = fp_border(color = "white", width = 3))  %>% 
    height_all(., height = 10)
  
  
} else {
  message('HTV data incomplete, create empty flextable')
  
  
  How_to_Vote_Headline <- tibble(
    
    Names = c('How to Vote Summary',
              'Candidates with submitted materials',
              'Candidates yet to submitted materials',
              
              "Processing Times",
              
              'Minimum', 
              'Average',
              'Maximum',
              
              "Material by Status",
              
              'Accepted Materials', 
              'Processing Materials',
              
              'Rejected Materials', 
              'Withdrawn Materials',
              'Total Materials Lodged'),
    
    Values = c("Count",
               0,
               0,
               
               "",
               
               0,
               0,
               0,
               
               "",
               
               0,
               0,
               
               0,
               0,
               0)) %>% 
    
    flextable() %>% 
    
    ## Set background
    bg(bg = "deepskyblue", i =~ str_detect(Names, "Materials", negate = TRUE)) %>%
    bg(bg = "coral",       i =~ str_detect(Names, "Materials")) %>%
    bg(bg = "black",       i =~ Values == "Count")              %>%
    bg(bg = "black",       i=~ Values == "")                    %>%
    
    ## Fontsize, color and bold 
    fontsize(size = flex_headline_size) %>%
    bold(i = 1,  j = NULL, bold = TRUE, part = "body")      %>%
    bold(i = 4,  j = NULL, bold = TRUE, part = "body")      %>%
    bold(i = 8,  j = NULL, bold = TRUE, part = "body")      %>%
    
    color(color = "white", i=~ Values == "Count")           %>%
    color(color = "white", i=~ Names  == "Total Pre-Poll:") %>%
    color(color = "white", part = "all")                    %>%
    
    width(j =~ Names, width = 600) %>%
    width(j =~ Values,width = 100) %>%
    delete_part(part = "header")   %>%
    border(border = fp_border(color = "white", width = 3))  %>% 
    height_all(., height = 10)
  
}


## 
message('All R Processing code run for How to Vote data')





################################## ---- TBC ---- ########################################
