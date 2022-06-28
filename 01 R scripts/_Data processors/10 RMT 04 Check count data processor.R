
# EMA ---------------------------------------------------------------------

# gazetted name  of postal counts need work and figure out closed or not postal count in display_show.
# 

run_ref_pol <- FALSE

Contest_Councillor <- Contest_raw %>% filter(CONTESTTYPECODE == 'Councillor')


Contest_Mayor_rp <- Contest_raw %>% filter(CONTESTTYPECODE %in% c('Referendum','Poll','General Ward Poll','Mayor')) %>% left_join(Contest_Councillor %>% select(AREACODE
                                                                                                                                                            ,CONTESTAREACODE_c = CONTESTAREACODE
                                                                                                                                                            ,CONTESTID_c = CONTESTID), by=c('AREACODE')) %>% 
  filter(CONTESTSTATUSTYPECODE == 'Contested')	


Contest <- Contest_raw %>% filter(CONTESTSTATUSTYPECODE == 'Contested')	

divided_Mayor_rp <- Contest_Mayor_rp %>% filter(CONTESTAREACODE != CONTESTAREACODE_c)



PRCC_comments_distinct <- PRCC_comments %>% select(CONTESTID = ELECTION_CODE
                                                  # ,AREACODE = AREA_CODE
                                                   ,VENUEVOTETYPE  = POLLING_PLACE_TYPE
                                                   ,LOCATIONID = POLLING_PLACE_CODE
                                                   ,GAZETTEDNAME = POLLING_PLACE_NAME
                                                   ,COMMENTS = POLLING_PLACE_COMMENT
                                                  ,COUNT_NUMBER = PROGRESSIVECOUNT_ID
                                                 # ,POLLINGPLACE_ID
                                                   ) %>%
  mutate(VENUEVOTETYPE = ifelse(VENUEVOTETYPE == 'DV',LOCATIONID,VENUEVOTETYPE)) %>%
  mutate(VENUEVOTETYPE = ifelse(VENUEVOTETYPE == 'PR','Pre-Poll',VENUEVOTETYPE)) %>%
  
  group_by(CONTESTID
           #, AREACODE
           , VENUEVOTETYPE ,LOCATIONID
          # ,POLLINGPLACE_ID
           ,COUNT_NUMBER) %>%
  summarise(COMMENTS = paste(COMMENTS, collapse='| ')) %>% ungroup() %>%
  mutate(forceApproval = ifelse(grepl(RMT_override_checkcount,tolower(COMMENTS)),'Y','N'))



EMA_firstCount_raw <- EMA_ord %>% bind_rows(EMA_dec) %>% rename(VENUEVOTETYPE = VOTINGCENTRETYPECODE) %>%
  mutate(VENUEVOTETYPE   = ifelse(VENUEVOTETYPE == 'Pre-poll Ordinary','Pre-Poll',VENUEVOTETYPE)) %>%
  left_join(Contest %>% select(CONTESTID, AREACODE, CONTESTTYPECODE,CONTESTAREACODE)) %>% 
  #filter(!is.na(INITIALCOUNT) & !is.na(CONTESTTYPECODE)) %>%
  filter(!is.na(CONTESTTYPECODE)) %>%
  
  left_join(PRCC_comments_distinct %>%
              mutate(VENUEVOTETYPE = as.character(VENUEVOTETYPE)), by=c("CONTESTID", "VENUEVOTETYPE", "LOCATIONID",'COUNT_NUMBER')) %>%
  filter(!VENUEVOTETYPE  %in% c('NNOR','Silent')) %>%
  mutate(GAZETTEDNAME = ifelse(VENUEVOTETYPE  == 'Postal', paste0('Count ',ifelse(COUNT_NUMBER<10,paste0('0',COUNT_NUMBER),COUNT_NUMBER)),GAZETTEDNAME))






# EMA expected ------------------------------------------------------------


# postal

EMA_postal <- EMA_postal_raw %>% group_by(CONTESTID, COUNT) %>%
  summarise(ACCEPTED = sum(ACCEPTED))

# deal with markoff to contest level

EMA_dec_expected_from_markoff <- EMA_dec_markoff %>% mutate(LOCATIONID = as.character(ifelse(VOTINGCENTRETYPECODE == 'Pre-Poll',LOCATIONID,VOTINGCENTRETYPECODE))) %>%
  group_by(ISSUINGDISTAREACODE
           ,VOTINGCENTRETYPECODE
           ,VENUE_VOTE_TYPE
           ,LOCATIONID) %>%
  summarise(EXPECTED_BALLOTS = sum(as.numeric(DEC_VOTE_MARKOFFS))
            ,ACCOUNTED_BALLOTS_EN = NA) %>%
  left_join(Contest %>% select(CONTESTID
                               ,AREACODE
                               ,CONTESTAREACODE
                               ,CONTESTSTATUSTYPECODE
                               ,CONTESTTYPECODE
  ) %>% distinct(), by=c('ISSUINGDISTAREACODE'='CONTESTAREACODE'))


divided_Mayor_rp <- Contest_Mayor_rp %>% filter(CONTESTAREACODE != CONTESTAREACODE_c) %>% 
  select(CONTESTID,CONTESTAREACODE,CONTESTTYPECODE,CONTESTSTATUSTYPECODE,CONTESTID_c,CONTESTAREACODE_c) %>%
  left_join(EMA_dec_expected_from_markoff %>%
              select(-CONTESTID,-CONTESTTYPECODE,-CONTESTSTATUSTYPECODE), by=c('CONTESTAREACODE_c'='ISSUINGDISTAREACODE')) %>%
  group_by(CONTESTID
           ,CONTESTAREACODE
           ,CONTESTTYPECODE
           ,CONTESTSTATUSTYPECODE
           ,VOTINGCENTRETYPECODE
           ,VENUE_VOTE_TYPE
           ,LOCATIONID
  ) %>%
  
  summarise(EXPECTED_BALLOTS = sum(EXPECTED_BALLOTS ,na.rm = T)
            ,ACCOUNTED_BALLOTS_EN = sum(ACCOUNTED_BALLOTS_EN)) %>%
  filter(!is.na(VOTINGCENTRETYPECODE)) %>%
  mutate(AREACODE = CONTESTAREACODE)



EMA_dec_expected <- EMA_dec_expected_from_markoff %>% filter(CONTESTSTATUSTYPECODE == 'Contested') %>%
  filter(!CONTESTID %in% divided_Mayor_rp$CONTESTID) %>%
  bind_rows(divided_Mayor_rp %>% rename(ISSUINGDISTAREACODE = CONTESTAREACODE)) %>%
  mutate(COUNT_NUMBER = 1)  %>%
  
  bind_rows(EMA_postal %>% 
              select(CONTESTID
                     ,COUNT_NUMBER = COUNT
                     ,EXPECTED_BALLOTS = ACCEPTED
              ) %>% left_join(Contest%>% select(CONTESTID
                                                ,AREACODE
                                                ,CONTESTAREACODE
                                                ,CONTESTSTATUSTYPECODE
                                                ,CONTESTTYPECODE)) %>% filter(CONTESTSTATUSTYPECODE == 'Contested') %>%
              mutate(LOCATIONID = 'Postal'
                     ,VOTINGCENTRETYPECODE = 'Postal'
                     ,ACCOUNTED_BALLOTS_EN = NA
                     ,ISSUINGDISTAREACODE = CONTESTAREACODE
                     ,VENUE_VOTE_TYPE = paste0('Count ',ifelse(COUNT_NUMBER<10,paste0('0',COUNT_NUMBER),COUNT_NUMBER))) %>%
              select(-CONTESTAREACODE)
  ) 

EMA_ord_expected <- EMA_ord_expected_raw %>% left_join(Contest %>% select(CONTESTID
                                                                      ,AREACODE
                                                                      ,CONTESTAREACODE
                                                                      ,CONTESTSTATUSTYPECODE
                                                                      ,CONTESTTYPECODE), by=c('CONTESTID','AREACODE')) %>% filter(CONTESTSTATUSTYPECODE == 'Contested') %>%
  mutate(COUNT_NUMBER = 1)

EMA_expected <- bind_rows(EMA_dec_expected %>% select(
  CONTESTID
  ,AREACODE             
  ,CONTESTAREACODE = ISSUINGDISTAREACODE
  ,VOTINGCENTRETYPECODE
  ,LOCATIONID
  ,GAZETTEDNAME = VENUE_VOTE_TYPE
  ,COUNT_NUMBER
  ,EXPECTED_BALLOTS
  ,ACCOUNTED_BALLOTS_EN
  ,CONTESTTYPECODE)
  
  ,EMA_ord_expected %>% select(
    CONTESTID
    ,AREACODE
    ,CONTESTAREACODE    
    ,VOTINGCENTRETYPECODE
    ,LOCATIONID
    ,GAZETTEDNAME
    ,COUNT_NUMBER
    ,EXPECTED_BALLOTS
    ,ACCOUNTED_BALLOTS_EN
    ,CONTESTTYPECODE)) 


  


####



EMA_firstCount <- EMA_firstCount_raw %>% mutate(AREACODE = ifelse(CONTESTID %in% divided_Mayor_rp$CONTESTID, CONTESTAREACODE,AREACODE))

mayor_councilor_temp <- EMA_firstCount %>% full_join(EMA_expected, by = c("CONTESTID", "AREACODE", "LOCATIONID", "GAZETTEDNAME", "COUNT_NUMBER", "CONTESTTYPECODE", "CONTESTAREACODE")) %>%
  filter(CONTESTTYPECODE == 'Councillor')

# PRCC --------------------------------------------------------------------

if (load_PRCC) {
  
  
  PRCC_registered <- PRCC_raw %>% 
                           rename(CONTESTID = ELECTION_CODE) %>%
                left_join(Contest %>% select(CONTESTID, CONTESTAREACODE, AREACODE, CONTESTTYPECODE)
                          ,by = c("CONTESTAREACODE", 'CONTESTID')) %>%
               # rename(CheckCount = OTHER_TOTAL) %>%
                      filter(!is.na(CONTESTTYPECODE)) %>% 
    
    # rename to cope EMA
    select(CONTESTID
           ,AREACODE
           ,VENUEVOTETYPE = POLLING_PLACE_TYPE
           ,LOCATIONID = POLLING_PLACE_CODE
           ,GAZETTEDNAME = POLLING_PLACE_NAME
           ,OTHER_TOTAL
           ,CONTESTTYPECODE
           ,POLLINGPLACE_ID
           ,COUNT_NUMBER = PROGRESSIVECOUNT_ID
           ,MAYOR_CHECK_COUNT 
           ,COUNCILLOR_EN_COUNT
           ,REGISTER_APPROVED
          ) 
  
  if(nrow(PRCC_registered) >0) {
    
    PRCC_registered <- PRCC_registered %>%
    mutate(VENUEVOTETYPE = as.character(VENUEVOTETYPE)) %>%
    mutate(VENUEVOTETYPE = case_when(VENUEVOTETYPE == 'DI' ~ 'DI Ordinary',
                                     VENUEVOTETYPE == 'PR' ~ 'Pre-Poll',
                                     VENUEVOTETYPE == 'DV' ~ LOCATIONID,
                                     T ~ VENUEVOTETYPE)
           ,GAZETTEDNAME = ifelse(VENUEVOTETYPE  == 'Postal', paste0('Count ',ifelse(COUNT_NUMBER<10,paste0('0',COUNT_NUMBER),COUNT_NUMBER)),GAZETTEDNAME)
           ) 
  }
  
  PRCC_registered <- PRCC_registered %>%
    
    group_by(CONTESTID,AREACODE,VENUEVOTETYPE
             ,LOCATIONID
             ,GAZETTEDNAME
             ,CONTESTTYPECODE
             ,POLLINGPLACE_ID
             ,COUNT_NUMBER
             ,REGISTER_APPROVED
             ) %>%
    summarise(OTHER_TOTAL = sum(OTHER_TOTAL)
              ,MAYOR_CHECK_COUNT = sum(MAYOR_CHECK_COUNT)
              ,COUNCILLOR_EN_COUNT = sum(COUNCILLOR_EN_COUNT)
              ) 
  
  # %>%
  #   
  #     pivot_wider(names_from = CONTESTTYPECODE
  #               ,values_from = CheckCount)
 
  
  # Data Entered/Reconciled join PRCC_BPs here or at councilor part
  
  PRCC_checkCount <- PRCC_registered %>% left_join(PRCC_BPs %>% select(CONTESTID = ELECTION_CODE
                                                                      # ,AREACODE = AREA_CODE
                                                                       ,POLLINGPLACE_ID
                                                                      # ,VENUEVOTETYPE = POLLING_PLACE_TYPE 
                                                                       ,COUNT_NUMBER = PROGRESSIVECOUNT_ID
                                                                    #   ,Registered
                                                                       ,Entered
                                                                       ,incomplete_Batch)) %>% 
    mutate(Registered = ifelse(REGISTER_APPROVED == 'A', OTHER_TOTAL, NA)
           ,Entered = ifelse(REGISTER_APPROVED == 'A' & OTHER_TOTAL == 0, OTHER_TOTAL, Entered)
            ) %>%
    mutate(Entered = ifelse(incomplete_Batch == 0, Entered, NA))
       
                                                   
                                                             
  
} else {
  PRCC_checkCount <- data.frame(CONTESTID = NA
                                ,AREACODE = NA
                                ,VENUEVOTETYPE = NA
                                ,LOCATIONID = NA
                                ,GAZETTEDNAME = NA
                                ,CONTESTTYPECODE = NA
                                ,CheckCount = NA
                                ,COMMENTS = NA
                                ,COUNT_NUMBER = NA)
  
  
  Data_entry <- data.frame()
  
  
}

# combine -----------------------------------------------------------------

counts_that_count <- PRCC_registered %>% #filter(VENUEVOTETYPE == 'Postal') %>%
                            select(CONTESTID, VENUEVOTETYPE, COUNT_NUMBER)

Councillor <- EMA_firstCount %>% full_join(PRCC_checkCount %>%
                                             mutate(LOCATIONID = as.character(LOCATIONID)
                                                    ,GAZETTEDNAME = as.character(GAZETTEDNAME)), by = c("CONTESTID", "AREACODE", "VENUEVOTETYPE", "LOCATIONID", "GAZETTEDNAME", "CONTESTTYPECODE",'COUNT_NUMBER')) %>%
  filter(CONTESTTYPECODE == 'Councillor') %>%
  
  #excl sth
  filter(LOCATIONID != 'EMS.AddressID.0002967') %>% left_join(Contest %>% select(CONTESTID
                                                                                ,CONTESTSTATUSTYPECODE
  )) %>% filter(CONTESTSTATUSTYPECODE == 'Contested') %>%
  filter(paste0(CONTESTID, VENUEVOTETYPE,COUNT_NUMBER) %in% paste0(counts_that_count$CONTESTID, counts_that_count$VENUEVOTETYPE,counts_that_count$COUNT_NUMBER)) %>%
  filter(!is.na(GAZETTEDNAME))



Mayor <- EMA_firstCount %>% full_join(PRCC_checkCount %>%
                                        mutate(LOCATIONID = as.character(LOCATIONID)
                                               ,GAZETTEDNAME = as.character(GAZETTEDNAME)), by = c("CONTESTID", "AREACODE", "VENUEVOTETYPE", "LOCATIONID", "GAZETTEDNAME", "CONTESTTYPECODE",'COUNT_NUMBER')) %>%
  filter(CONTESTTYPECODE == 'Mayor') %>%
  filter(LOCATIONID != 'EMS.AddressID.0002967')  %>%
  left_join(mayor_councilor_temp %>%
              group_by(AREACODE,VENUEVOTETYPE,LOCATIONID,GAZETTEDNAME) %>%
              #       summarise(CouncillorBPs = sum(as.numeric(INITIALCOUNT),na.rm=T))) %>%
              summarise(CouncillorBPs = sum(as.numeric(INITIALCOUNT)))) %>%
  filter(paste0(CONTESTID, VENUEVOTETYPE,COUNT_NUMBER) %in% paste0(counts_that_count$CONTESTID,counts_that_count$VENUEVOTETYPE,counts_that_count$COUNT_NUMBER))%>%
  filter(!is.na(GAZETTEDNAME))


if(run_ref_pol) {
  
Ref_poll <- EMA_firstCount %>%  filter(CONTESTTYPECODE %in% c('Referendum','Poll')) %>%
  #excl sth
  filter(LOCATIONID != 'EMS.AddressID.0002967')

}
# presentation tables ----------------------------------------------------------------

# make nums
i_want_numeric <- c('INITIALCOUNT','OTHER_TOTAL','MAYOR_CHECK_COUNT','COUNCILLOR_EN_COUNT','Registered','Entered')

Councillor[,i_want_numeric] <- sapply(Councillor[,i_want_numeric], as.numeric)


Councillor_interim <- Councillor %>% left_join(Contest %>% select(CONTESTID,CONTESTTYPECODE,CONTESTAREACODE)) %>% 
  mutate(Diff = Registered - INITIALCOUNT
         ,Diff2 = Entered - Registered) %>%
  mutate(RequireChecking = 'No') %>%
  mutate(RequireChecking = case_when(abs(Diff/INITIALCOUNT) > 0.01 ~ 'Yes'
                                     ,abs(Diff2/Registered) > 0.01 ~ 'Yes'
                                     ,Diff==INITIALCOUNT ~ 'No'
                                     ,T ~ RequireChecking)) %>%
  mutate(RequireChecking = ifelse(!is.na(forceApproval) & forceApproval == 'Y','No',RequireChecking)) %>%
  
  mutate(RequireChecking = as.factor(RequireChecking)
         ,VENUEVOTETYPE = as.factor(VENUEVOTETYPE)
         ,CONTESTAREACODE = as.factor(CONTESTAREACODE)
         ,AREACODE = as.factor(AREACODE)
              ) %>%
  arrange(CONTESTID) %>%
  filter(!is.na(GAZETTEDNAME))


  Councillor_show <- Councillor_interim %>% select(
                                            `Requires Checking` = RequireChecking
                                            ,`LGAreaCode` = AREACODE
                                            ,`Area/Ward` = CONTESTAREACODE
                                            ,`Vote Type` = VENUEVOTETYPE
                                            ,Venue = GAZETTEDNAME
                                            ,`Initial Count (A)` = INITIALCOUNT
                                            ,`Registered/Approved Total (B)` = Registered
                                            ,`Diff1 (B-A)` = Diff
                                            ,`Data Entered/Reconciled Total (C)` = Entered
                                            ,`Diff2 (C-B)` = Diff2
                                            ,Comments = COMMENTS
                                          )


Mayor[,i_want_numeric] <- sapply(Mayor[,i_want_numeric], as.numeric)


Mayor_interim <- Mayor %>% 
  mutate(Diff = Registered - INITIALCOUNT
         ,Diff2 = Entered - Registered
         ,Diff3 = CouncillorBPs - Registered) %>%
  mutate(RequireChecking = 'No') %>%
  mutate(RequireChecking = case_when(abs(Diff/INITIALCOUNT) > 0.01 ~ 'Yes'
                                     ,abs(Diff2/Registered) > 0.01 ~ 'Yes'
                                     ,abs(Diff3/INITIALCOUNT) > 0.01 ~ 'Yes'
                                     
                                     ,Diff==INITIALCOUNT ~ 'No'
                                     ,T ~ RequireChecking)) %>%
  mutate(RequireChecking = ifelse(!is.na(forceApproval) & forceApproval == 'Y','No',RequireChecking)) %>%
  
  mutate(RequireChecking = as.factor(RequireChecking)
         ,VENUEVOTETYPE = as.factor(VENUEVOTETYPE)
         ,AREACODE = as.factor(AREACODE)
                        ) %>%
  arrange(CONTESTID) %>%
  filter(!is.na(GAZETTEDNAME))
  
  Mayor_show <- Mayor_interim %>% select(
                                      `Requires Checking` = RequireChecking
                                      ,`Area/Ward` = AREACODE
                                      ,`Vote Type` = VENUEVOTETYPE
                                      ,Venue = GAZETTEDNAME
                                      ,`Initial Count (A)` = INITIALCOUNT
                                      ,`Registered/Approved Total (B)` = Registered
                                      ,`Diff1 (B-A)` = Diff
                                      ,`Data Entered/Reconciled Total (C)` = Entered
                                      ,`Diff2 (C-B)` = Diff2
                                      ,`Councillor IC (D)` = CouncillorBPs
                                      ,`Diff3 (D-B)` = Diff3
                                      ,Comments = COMMENTS
                                      
                                    )

if(run_ref_pol) {
  
Ref_poll[,c('INITIALCOUNT')] <- sapply(Ref_poll[,c('INITIALCOUNT')], as.numeric)
Ref_poll_checkC[,c('CHECKCOUNT')] <- sapply(Ref_poll_checkC[,c('CHECKCOUNT')], as.numeric)

if (nrow(Ref_poll) >0) {
  
  Ref_poll_show <- Ref_poll %>% left_join(Ref_poll_checkC %>% select(-CONTESTTYPECODE, -CONTESTAREACODE)) %>% rename(CheckCount = CHECKCOUNT) %>%
    mutate(Diff = CheckCount - INITIALCOUNT) %>%
    mutate(RequireChecking = 'Yes') %>%
    mutate(RequireChecking = case_when(abs(Diff/INITIALCOUNT) < 0.01 ~ 'No'
                                       ,Diff==INITIALCOUNT ~ 'No'
                                       ,grepl('RMT stamped|accepted by ROSO',COMMENTS) ~ 'No'
                                       ,T ~ RequireChecking)) %>%
    mutate(RequireChecking = as.factor(RequireChecking)
           ,VENUEVOTETYPE = as.factor(VENUEVOTETYPE)) %>%  
    
    arrange(CONTESTID) %>%
    select(
      RequireChecking
      ,LGArea = AREACODE
      ,CONTESTTYPECODE
      ,VENUEVOTETYPE
      ,VenueName = GAZETTEDNAME
      ,InitialCount = INITIALCOUNT
      ,CheckCount
      ,Diff
      ,Comments = COMMENTS
    )
} else {
  
  Ref_poll_show <- data.frame()
  
}

}

# Knit related tables --------------------------------------------------------------------
 
summary_1 <- Mayor_interim
  
summary_2 <- Mayor_interim %>% filter(!is.na(INITIALCOUNT))

summary_3 <- Mayor_interim %>% filter(!is.na(Registered))
summary_4 <- Mayor_interim %>% filter(!is.na(Entered))
summary_5 <- Mayor_interim %>% filter(forceApproval == 'Y')
summary_6 <- Mayor_interim %>% filter(RequireChecking == 'Yes')


 if(nrow(summary_1) >0) {
   
 col1 <- SingleChoiceTable(summary_1, 'VENUEVOTETYPE') %>% rename(Total = Count)
 } else {
   col1 <- tibble(VENUEVOTETYPE = NA
                      ,Total = NA)
   
 }

if(nrow(summary_2) >0) {
  
col2 <- SingleChoiceTable(summary_2, 'VENUEVOTETYPE') %>% rename(`Initial Count Entered` = Count)
} else {
  col2 <- tibble(VENUEVOTETYPE = NA
                     ,`Initial Count Entered` = NA)
  
}
if(nrow(summary_3) >0) {
  
col3 <- SingleChoiceTable(summary_3, 'VENUEVOTETYPE') %>% rename(`Batch Registered/Approved` = Count)
} else {
  col3 <- tibble(VENUEVOTETYPE = NA
                     ,`Batch Registered/Approved` = NA)
  
}

if(nrow(summary_4) >0) {
  
col4 <- SingleChoiceTable(summary_4, 'VENUEVOTETYPE') %>% rename(`Data Entered/Reconciled` = Count)
} else {
  col4 <- tibble(VENUEVOTETYPE = NA
                     ,`Data Entered/Reconciled` = NA)
}
  
  if(nrow(summary_5) >0) {
    
 col5 <- SingleChoiceTable(summary_5, 'VENUEVOTETYPE') %>% rename(`RMT Approved` = Count)
  } else {
   
    col5 <- tibble(VENUEVOTETYPE = NA,
           `RMT Approved` = NA)
  }
  
if(nrow(summary_6) >0) {
  
col6 <- SingleChoiceTable(summary_6, 'VENUEVOTETYPE') %>% rename(`Requires RMT Checking` = Count)
} else {
  col6 <- tibble(VENUEVOTETYPE = NA
                     ,`Requires RMT Checking` = NA)
  
}

Mayor_summary <- col1 %>% 
  left_join(col2) %>% 
  left_join(col3) %>%
  left_join(col4) %>%
  left_join(col6) %>%
  left_join(col5) %>%
  rename(`Vote Type` = VENUEVOTETYPE)

# Mayor_summary[is.na(Mayor_summary)] <- 0

###

summary_1 <- Councillor_interim

summary_2 <- Councillor_interim %>% filter(!is.na(INITIALCOUNT))
summary_3 <- Councillor_interim %>% filter(!is.na(Registered))
summary_4 <- Councillor_interim %>% filter(!is.na(Entered))
summary_5 <- Councillor_interim %>% filter(forceApproval == 'Y')
summary_6 <- Councillor_interim %>% filter(RequireChecking == 'Yes')


 if(nrow(summary_1) >0) {
   
   col1 <- SingleChoiceTable(summary_1, 'VENUEVOTETYPE') %>% rename(Total = Count)
 } else {
   col1 <- tibble(VENUEVOTETYPE = NA
                      ,Total = NA)
   
 }
if(nrow(summary_2) >0) {
  
col2 <- SingleChoiceTable(summary_2, 'VENUEVOTETYPE') %>% rename(`Initial Count Entered` = Count)
} else {
  col2 <- tibble(VENUEVOTETYPE = NA
                     ,`Initial Count Entered` = NA)
  
}
if(nrow(summary_3) >0) {
  
col3 <- SingleChoiceTable(summary_3, 'VENUEVOTETYPE') %>% rename(`Batch Registered/Approved` = Count)
} else {
  col3 <- tibble(VENUEVOTETYPE = NA
                     ,`Batch Registered/Approved` = NA)
  
}
if(nrow(summary_4) >0) {
  
col4 <- SingleChoiceTable(summary_4, 'VENUEVOTETYPE') %>% rename(`Data Entered/Reconciled` = Count)
} else {
  col4 <- tibble(VENUEVOTETYPE = NA
                     ,`Data Entered/Reconciled` = NA)
  
}
if(nrow(summary_5) >0) {
  
col5 <- SingleChoiceTable(summary_5, 'VENUEVOTETYPE') %>% rename(`RMT Approved` = Count)
} else {
  col5 <- tibble(VENUEVOTETYPE = NA
                     ,`RMT Approved` = NA)
  
}

if(nrow(summary_6) >0) {
  
  col6 <- SingleChoiceTable(summary_6, 'VENUEVOTETYPE') %>% rename(`Requires RMT Checking` = Count)
} else {
  col6 <- tibble(VENUEVOTETYPE = NA
                     ,`Requires RMT Checking` = NA)
  
}

Councillor_summary <- col1 %>% 
  left_join(col2) %>% 
  left_join(col3) %>%
  left_join(col4) %>%
  left_join(col6) %>%
  left_join(col5) %>%
  
  rename(`Vote Type` = VENUEVOTETYPE)


# deal with exceptions in the show dataset


# save data for RO --------------------------------------------------------


setwd('//SVRANALYTICS1/AnalyticsData/LG2101/RMT')

write.csv(Councillor,'Counvillor check count.csv')
write.csv(Mayor,'Mayor check count.csv')




