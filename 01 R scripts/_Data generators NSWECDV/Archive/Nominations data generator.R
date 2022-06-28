####################################################################################################################
###################################### NOMINATIONS DATA GENERATOR ---- #############################################
####################################################################################################################





## This code pulls the data for the 'Candidate nominations' dashboard
message('Run R code to create Candidate Nominations data')


## Set global variables for candidate nominations
cashpayments   <- 36
chequepayments <- 154
visapayments   <- 626


## Try and remove everything that is hard coded from the code below.................................................





## 1). PULL NOMINATIONS DATA FROM DATABASE ================================================================================


## Query database
cand_reg_noms <- sqlExecute(hyperion_connection,
                            "SELECT * FROM [ElectionData].[Storage].[CandidateNomsReg]",
                            fetch = TRUE,
                            stringsAsFactors = FALSE)


cand_nom_totals_area_status <- sqlExecute(hyperion_connection,
                                          "SELECT * FROM [ElectionData].[Views].[CandidateNomWithStatusEMA] 
                              ORDER BY AREACODE",
                                          fetch = TRUE,
                                          stringsAsFactors = FALSE)


distinct_groups <- sqlExecute(hyperion_connection,
                              "SELECT DISTINCT(GROUPNUMBER) 
                              FROM [ElectionData].[Views].[CandidateNomWithStatusEMA]
                              WHERE AREACODE = 'NSW' AND GROUPNUMBER IS NOT NULL",
                              fetch = TRUE,
                              stringsAsFactors = FALSE)


cand_nom_totals_area_status <- cand_nom_totals_area_status %>% group_by(AREACODE, CandidateStatus) %>% tally()
cand_nom_totals_status      <- cand_nom_totals_area_status %>% group_by(CandidateStatus) %>% tally()
cand_nom_totals_status$n    <- as.numeric(as.character(cand_nom_totals_status$n))


cand_party_totals <- sqlExecute(hyperion_connection,
                                "SELECT ISNULL(p.PARTYNAME,' Independent') as PARTY, CandidateStatus
                                       FROM [ElectionData].[Views].[CandidateNomWithStatusEMA] e
                                       LEFT JOIN [ElectionData].[Staging].[EMA_PM_PARTY] p on e.ENDORSEDBYPARTYID = p.PARTYID
                                       WHERE e.AREACODE <> 'NSW'",
                                fetch = TRUE,
                                stringsAsFactors = FALSE)

colnames(cand_party_totals)[2] <- 'STATUS'
cand_party_totals <- updateStatus(cand_party_totals) %>% 
  group_by(PARTY, STATUS) %>% summarise(n = n())


## 
cand_cumsum_totals_date <- sqlExecute(hyperion_connection,
                                      "SELECT * FROM [ElectionData].[Views].[CandidateNomCumTotalsByDate]",
                                      fetch = TRUE,
                                      stringsAsFactors = FALSE)

cand_cumsum_totals_date$Total   <- as.numeric(as.character(cand_cumsum_totals_date$Total))
cand_cumsum_totals_date$NomDate <- as.Date(cand_cumsum_totals_date$NomDate)








####################################################################################################################
#################################################### TBC ###########################################################
####################################################################################################################