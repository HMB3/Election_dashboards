
# EMA ---------------------------------------------------------------------


# for EMA initial count
# mayoral <- candidate + informal
# councilor <- group + btl(candidate) + other

# infomal_other <- VoteCountingPoint_informal_other %>% select(-INFORMALSCHECKCOUNT
#                                                              ,-TOTAL_RATL
#                                                              ,-TOTAL_BTL
#                                                              ,-EXPECTED_BALLOTS
#                                                              ,-SPOILT_BALLOTS) %>%
#                                             pivot_longer(names_to = 'CANDIDATEBNALLOTNAME'
#                                                          ,values_to = 'FIRSTPREFERENCEVOTECOUNT'
#                                                          ,!c(CONTESTID,AREACODE,VOTINGCENTRETYPECODE,LOCATIONID,GAZETTEDNAME)) %>%
#                                             filter(!is.na(FIRSTPREFERENCEVOTECOUNT))
# 
# 
# 
# 
# 
# 
# EMA_initial_count <- Candidate_results %>% bind_rows(infomal_other) %>% 
#                                             group_by(CONTESTID
#                                                     ,AREACODE
#                                                     ,VOTINGCENTRETYPECODE
#                                                     ,LOCATIONID
#                                                     ,GAZETTEDNAME) %>%
#                     # add group for councilor contest
#                                 bind_rows(Group_results %>% 
#                                              group_by(CONTESTID
#                                                       ,AREACODE
#                                                       ,VOTINGCENTRETYPECODE
#                                                       ,LOCATIONID
#                                                       ,GAZETTEDNAME) %>%
#                                              summarise(FIRSTPREFERENCEVOTECOUNT = sum(EN_RATL,na.rm = TRUE))) %>%
#                   # summing up to venue level
#                               summarise(InitialCount = sum(FIRSTPREFERENCEVOTECOUNT,na.rm = TRUE)) %>%
#                   # jopining contest for easier view by contest type
#                     left_join(Contest %>% select(CONTESTID,CONTESTTYPECODE), by = c("CONTESTID")) %>%
#                     ungroup() %>%
#                     arrange(AREACODE,GAZETTEDNAME)
# 

Contest <- Contest %>% filter(CONTESTSTATUSTYPECODE == 'Contested')	

# distinct comments


EMA_comments_distinct <- EMA_comments %>%
  arrange(CONTESTID, AREACODE, VOTINGCENTRETYPECODE,LOCATIONID,COMMENTDATE) %>%
  group_by(CONTESTID, AREACODE, VOTINGCENTRETYPECODE,LOCATIONID) %>%
  summarise(COMMENTS = paste(COMMENTS, collapse='| ')) %>% ungroup() %>%
  rename(VENUEVOTETYPE = VOTINGCENTRETYPECODE)



EMA_firstCount <- EMA_ord %>% bind_rows(EMA_dec) %>% filter(!is.na(INITIALCOUNT)) %>%
                   left_join(Contest %>% select(CONTESTID, AREACODE, CONTESTTYPECODE)) %>%
                   left_join(EMA_comments_distinct) %>%
  filter(!VOTINGCENTRETYPECODE %in% c('NNOR','Silent'))

# %>%
#                    pivot_wider(names_from = CONTESTTYPECODE
#                                ,values_from = INITIALCOUNT)





# PRCC --------------------------------------------------------------------

if (not_load_prcc) {
  
  PRCC_checkCount <- data.frame(CONTESTID = NA
                                ,AREACODE = NA
                                ,VENUEVOTETYPE = NA
                                ,LOCATIONID = NA
                                ,GAZETTEDNAME = NA
                                ,CONTESTTYPECODE = NA
                                ,CheckCount = NA)
  
} else {
  


PRCC_checkCount <- 
    bind_rows(PRCC_raw %>% select(-ELECTION_CODE, -MAYOR_CHECK_COUNT) %>% 
            left_join(Contest %>% select(CONTESTID, CONTESTAREACODE, AREACODE, CONTESTTYPECODE) %>% filter(CONTESTTYPECODE == 'Councillor'),by = "CONTESTAREACODE") %>%
            rename(CheckCount = CouncillorCheckCount)
          ,
          PRCC_raw %>% select(-ELECTION_CODE, -CouncillorCheckCount) %>% filter(!is.na(MAYOR_CHECK_COUNT)) %>%
            left_join(Contest %>% filter(CONTESTTYPECODE == 'Councillor') %>% select(CONTESTAREACODE, AREACODE),by = "CONTESTAREACODE") %>%
            select(-CONTESTAREACODE) %>%
            left_join(Contest %>% select(CONTESTID, CONTESTAREACODE, CONTESTTYPECODE) %>% filter(CONTESTTYPECODE == 'Mayor'),by = c('AREACODE' = "CONTESTAREACODE")) %>%
            distinct() %>%
            rename(CheckCount = MAYOR_CHECK_COUNT) 
          ) %>%
            filter(!is.na(CONTESTID)) %>%
          # rename to cope EMA
          select(CONTESTID,AREACODE,VENUEVOTETYPE = POLLING_PLACE_TYPE
                 ,LOCATIONID = POLLING_PLACE_CODE
                 ,GAZETTEDNAME = POLLING_PLACE_NAME
                 ,CheckCount
                 ,CONTESTTYPECODE) %>%
          mutate(VENUEVOTETYPE = case_when(VENUEVOTETYPE == 'DI' ~ 'DI Ordinary',
                                           VENUEVOTETYPE == 'PR' ~ 'Pre-poll Ordinary',
                                           VENUEVOTETYPE == 'DV' ~ LOCATIONID,
                                           T ~ VENUEVOTETYPE)) %>%
  group_by(CONTESTID,AREACODE,VENUEVOTETYPE
           ,LOCATIONID
           ,GAZETTEDNAME
           ,CONTESTTYPECODE) %>%
  summarise(CheckCount = sum(CheckCount))

# %>%
#   
#     pivot_wider(names_from = CONTESTTYPECODE
#               ,values_from = CheckCount)
  
}

# combine -----------------------------------------------------------------

Councillor <- EMA_firstCount %>% full_join(PRCC_checkCount, by = c("CONTESTID", "AREACODE", "VENUEVOTETYPE", "LOCATIONID", "GAZETTEDNAME", "CONTESTTYPECODE")) %>%
                                              filter(CONTESTTYPECODE == 'Councillor') %>%
              #excl sth
                              filter(LOCATIONID != 'EMS.AddressID.0002967')

Mayor <- EMA_firstCount %>% full_join(PRCC_checkCount, by = c("CONTESTID", "AREACODE", "VENUEVOTETYPE", "LOCATIONID", "GAZETTEDNAME", "CONTESTTYPECODE")) %>%
                        filter(CONTESTTYPECODE == 'Mayor') %>%
  #excl sth
  filter(LOCATIONID != 'EMS.AddressID.0002967')


Ref_poll <- EMA_firstCount %>%  filter(CONTESTTYPECODE %in% c('Referendum','Poll')) %>%
  #excl sth
  filter(LOCATIONID != 'EMS.AddressID.0002967')

# presentation tables ----------------------------------------------------------------

# remove unwanted columns

Councillor[,c('INITIALCOUNT','CheckCount')] <- sapply(Councillor[,c('INITIALCOUNT','CheckCount')], as.numeric)


Councillor_show <- Councillor %>% left_join(Contest %>% select(CONTESTID,CONTESTTYPECODE,CONTESTAREACODE)) %>% 
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
                                         ,CONTESTAREACODE
                                         ,VENUEVOTETYPE
                                         ,VenueName = GAZETTEDNAME
                                         ,InitialCount = INITIALCOUNT
                                         ,CheckCount
                                         ,Diff
                                         ,Comments = COMMENTS
                                         )
                                  

Mayor[,c('INITIALCOUNT','CheckCount')] <- sapply(Mayor[,c('INITIALCOUNT','CheckCount')], as.numeric)


Mayor_show <- Mayor %>% 
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
                                        ,VENUEVOTETYPE
                                        ,VenueName = GAZETTEDNAME
                                        ,InitialCount = INITIALCOUNT
                                        ,CheckCount
                                        ,Diff
                                        ,Comments = COMMENTS
                                  )
                    
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


              
# Knit related tables --------------------------------------------------------------------

Councillor_bp_counted <- tibble(InitialCount = sum(Councillor_show$InitialCount,na.rm = TRUE)
       ,CheckCount = sum(Councillor_show$CheckCount,na.rm = TRUE)
       ,Diff = sum(Councillor_show$CheckCount,na.rm = TRUE) - sum(Councillor_show$InitialCount,na.rm = TRUE)
       ,`Items/Venues to check` = nrow(Councillor_show %>% filter(RequireChecking == 'Yes')))


Mayor_bp_counted <- tibble(InitialCount = sum(Mayor_show$InitialCount,na.rm = TRUE)
       ,CheckCount = sum(Mayor_show$CheckCount,na.rm = TRUE)
       ,Diff = sum(Mayor_show$CheckCount,na.rm = TRUE) - sum(Mayor_show$InitialCount,na.rm = TRUE)
       ,`Items/Venues to check` = nrow(Mayor_show %>% filter(RequireChecking == 'Yes')))



if(nrow(Ref_poll_show) > 0) {
  
Ref_poll_bp_counted <- tibble(InitialCount = sum(Ref_poll_show$InitialCount,na.rm = TRUE)
                              ,CheckCount = sum(Ref_poll_show$CheckCount,na.rm = TRUE)
                              ,Diff = sum(Ref_poll_show$CheckCount,na.rm = TRUE) - sum(Ref_poll_show$InitialCount,na.rm = TRUE)
                              ,`Items/Venues to check` = nrow(Ref_poll_show %>% filter(RequireChecking == 'Yes')))

} else {
  
  Ref_poll_bp_counted <- tibble()
  
}





# % of two count match

head(Councillor_show)

nrow(Councillor_show %>% filter(abs(Councillor_show$Diff/Councillor_show$InitialCount) >0.01))
nrow(Councillor_show)



# deal with exceptions in the show dataset








