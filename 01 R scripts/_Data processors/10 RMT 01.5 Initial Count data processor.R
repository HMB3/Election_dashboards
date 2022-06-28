
# EMA ---------------------------------------------------------------------
Contest_Councillor <- Contest_raw %>% filter(CONTESTTYPECODE == 'Councillor')

Contest_Mayor_rp <- Contest_raw %>% filter(CONTESTTYPECODE %in% c('Referendum','Poll','General Ward Poll','Mayor')) %>% left_join(Contest_Councillor %>% select(AREACODE
                                                                                                            ,CONTESTAREACODE_c = CONTESTAREACODE
                                                                                                            ,CONTESTID_c = CONTESTID), by=c('AREACODE')) %>% 
                        filter(CONTESTSTATUSTYPECODE == 'Contested')	

Contest <- Contest_raw %>% filter(CONTESTSTATUSTYPECODE == 'Contested')	


# postal

EMA_postal <- EMA_postal_raw %>% group_by(CONTESTID, COUNT) %>%
                             summarise(ACCEPTED = sum(ACCEPTED))

# distinct comments

EMA_comments_distinct <- EMA_comments %>%
  arrange(CONTESTID, AREACODE, VOTINGCENTRETYPECODE,LOCATIONID,COMMENTDATE) %>%
  group_by(CONTESTID, AREACODE, VOTINGCENTRETYPECODE,LOCATIONID,COUNT_NUMBER) %>%
  summarise(COMMENTS = paste(COMMENTS, collapse='| ')) %>% ungroup() %>%
  rename(VENUEVOTETYPE = VOTINGCENTRETYPECODE) %>%
  mutate(COUNT_NUMBER = ifelse(is.na(COUNT_NUMBER),1,COUNT_NUMBER)) %>%
  mutate(forceApproval = ifelse(grepl(RMT_override_initial,tolower(COMMENTS)),'Y','N'))




EMA_firstCount_raw <- EMA_ord %>% bind_rows(EMA_dec) %>% 
                  mutate(VOTINGCENTRETYPECODE = ifelse(VOTINGCENTRETYPECODE == 'Pre-poll Ordinary','Pre-Poll',VOTINGCENTRETYPECODE)) %>%
                   left_join(Contest %>% select(CONTESTID, AREACODE, CONTESTTYPECODE,CONTESTAREACODE)) %>% 
                   #filter(!is.na(INITIALCOUNT) & !is.na(CONTESTTYPECODE)) %>%
                   filter(!is.na(CONTESTTYPECODE)) %>%
  
                  left_join(EMA_comments_distinct %>% select(-VENUEVOTETYPE), by = c("CONTESTID", "AREACODE", "LOCATIONID",'COUNT_NUMBER')) %>%
                  filter(!VOTINGCENTRETYPECODE %in% c('NNOR','Silent')) %>%
                  mutate(GAZETTEDNAME = ifelse(VOTINGCENTRETYPECODE == 'Postal', paste0('Count ',ifelse(COUNT_NUMBER<10,paste0('0',COUNT_NUMBER),COUNT_NUMBER)),GAZETTEDNAME))


# %>%
#                    pivot_wider(names_from = CONTESTTYPECODE
#                                ,values_from = INITIALCOUNT)


                              
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
  # here need to split the uncontested wards and add mayor bit
if (FALSE) {
  
Mayor_ref_contests <- Contest_details %>% filter(ContestTypeCode != 'Councillor') %>% select(CONTESTID = ContestID
                                                                                    ,LGArea
                                                                                    ,CONTESTTYPECODE = ContestTypeCode
                                                                                    ,CONTESTSTATUSTYPECODE = ContestStatusTypeCode) %>% 
                                       left_join(
                 
                 Contest_details %>% filter(ContestTypeCode == 'Councillor') %>% select(AreaCode
                                                                                        ,LGArea)
                                                                                        ,by = c('LGArea')
                                           ) %>%
                            filter(CONTESTTYPECODE == 'Mayor', CONTESTSTATUSTYPECODE == 'Contested')

Mayor_contest_with_uncontessted_wards <- EMA_dec_expected_from_markoff %>% filter(is.na(CONTESTSTATUSTYPECODE)) %>% select(ISSUINGDISTAREACODE
                                                                                                                           ,VOTINGCENTRETYPECODE
                                                                                                                           ,VENUE_VOTE_TYPE
                                                                                                                           ,LOCATIONID
                                                                                                                           ,EXPECTED_BALLOTS
                                                                                                                           ,ACCOUNTED_BALLOTS_EN) %>%
                                              left_join(Mayor_ref_contests, by=c('ISSUINGDISTAREACODE'='AreaCode')) %>% 
                                              mutate(ISSUINGDISTAREACODE = LGArea) %>%
                                              rename(AREACODE = LGArea)
  
  


 

}



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



EMA_firstCount <- EMA_firstCount_raw %>% mutate(AREACODE = ifelse(CONTESTID %in% divided_Mayor_rp$CONTESTID, CONTESTAREACODE,AREACODE))

# combine -----------------------------------------------------------------

Councillor_postal_interim <- EMA_firstCount %>% full_join(EMA_expected) %>%
                                              filter(CONTESTTYPECODE == 'Councillor') %>%
                                  left_join(EMA_allocated) %>%
              #excl sth
                              filter(LOCATIONID != 'EMS.AddressID.0002967') %>% left_join(Contest%>% select(CONTESTID
                                                                                                            ,CONTESTSTATUSTYPECODE
                              )) %>% filter(CONTESTSTATUSTYPECODE == 'Contested') 
                          

Councillor <- bind_rows(Councillor_postal_interim %>% filter(VOTINGCENTRETYPECODE == 'Postal') %>%
                              filter(paste0(CONTESTID,COUNT_NUMBER) %in% paste0(EMA_postal$CONTESTID,EMA_postal$COUNT))
                       ,Councillor_postal_interim %>% filter(VOTINGCENTRETYPECODE != 'Postal')
                       ) %>%
  filter(!is.na(GAZETTEDNAME))


Mayor_postal_interim <- EMA_firstCount %>% full_join(EMA_expected) %>%
                        filter(CONTESTTYPECODE == 'Mayor') %>% 
  # join by aggregated up councilor numbers
                        left_join(Councillor %>% group_by(AREACODE,VOTINGCENTRETYPECODE,LOCATIONID,GAZETTEDNAME) %>%
                             #       summarise(CouncillorBPs = sum(as.numeric(INITIALCOUNT),na.rm=T))) %>%
                                    summarise(CouncillorBPs = sum(as.numeric(INITIALCOUNT)))) %>%
  
       left_join(EMA_allocated) %>%
  #excl sth 
  filter(LOCATIONID != 'EMS.AddressID.0002967') %>% left_join(Contest%>% select(CONTESTID
                                                                                ,CONTESTSTATUSTYPECODE
  )) %>% filter(CONTESTSTATUSTYPECODE == 'Contested') 



Mayor <- bind_rows(Mayor_postal_interim %>% filter(VOTINGCENTRETYPECODE == 'Postal') %>%
                                       filter(paste0(CONTESTID,COUNT_NUMBER) %in% paste0(EMA_postal$CONTESTID,EMA_postal$COUNT))
                                     ,Mayor_postal_interim %>% filter(VOTINGCENTRETYPECODE != 'Postal')) %>%
               filter(!is.na(GAZETTEDNAME))



Ref_poll_postal_interim <- EMA_firstCount %>% 
  
   full_join(EMA_expected) %>%
  filter(CONTESTTYPECODE %in% c('Referendum','Poll','General Ward Poll')) %>%
  # join by aggregated up councilor numbers
  left_join(Councillor %>% group_by(AREACODE,VOTINGCENTRETYPECODE,LOCATIONID,GAZETTEDNAME) %>%
              #  summarise(CouncillorBPs = sum(as.numeric(INITIALCOUNT),na.rm=T))) %>%
              summarise(CouncillorBPs = sum(as.numeric(INITIALCOUNT)))) %>%
  
  left_join(EMA_allocated) %>%
  #  filter(!is.na(ALLOCATED)) %>%
  #excl sth
                      filter(LOCATIONID != 'EMS.AddressID.0002967') %>% left_join(Contest%>% select(CONTESTID
                                                                                                    ,CONTESTAREACODE
                                                                                                    ,CONTESTSTATUSTYPECODE
                                                                                                    ,QUESTIONLABEL
                      )) %>% filter(CONTESTSTATUSTYPECODE == 'Contested') 
  
  
  Ref_poll <- bind_rows(Ref_poll_postal_interim %>% filter(VOTINGCENTRETYPECODE == 'Postal') %>%
                            filter(paste0(CONTESTID,COUNT_NUMBER) %in% paste0(EMA_postal$CONTESTID,EMA_postal$COUNT))
                        ,Ref_poll_postal_interim %>% filter(VOTINGCENTRETYPECODE != 'Postal')
                        ) %>%
    filter(!is.na(GAZETTEDNAME))

# presentation tables ----------------------------------------------------------------



# remove unwanted columns
Councillor[,c('INITIALCOUNT','EXPECTED_BALLOTS','ALLOCATED','ACCOUNTED_BALLOTS_EN')] <- sapply(Councillor[,c('INITIALCOUNT','EXPECTED_BALLOTS','ALLOCATED','ACCOUNTED_BALLOTS_EN')], as.numeric)

 Councillor_interim <- Councillor %>% 
                                  mutate(Diff = INITIALCOUNT - ifelse(is.na(EXPECTED_BALLOTS),0,EXPECTED_BALLOTS)
                                        ,Diff2 = ACCOUNTED_BALLOTS_EN - ALLOCATED) %>%
                                  mutate(RequireChecking = 'No') %>%
                                  mutate(RequireChecking = case_when(abs(Diff/INITIALCOUNT) > 0.01 ~ 'Yes'
                                                                     ,abs(Diff2/ACCOUNTED_BALLOTS_EN) >0.01 ~ 'Yes'
                                                                     ,T ~ RequireChecking)) %>% 
                                  mutate(RequireChecking = case_when(!VOTINGCENTRETYPECODE %in% c('PP','Pre-Poll') & abs(Diff/INITIALCOUNT) > 0.01 ~'Yes'
                                                                     ,!VOTINGCENTRETYPECODE %in% c('PP','Pre-Poll') & Diff == 0 ~'No'
                                                                     ,T ~ RequireChecking)) %>%
                                  mutate(RequireChecking = ifelse(!is.na(forceApproval) & forceApproval == 'Y','No',RequireChecking)) %>%
                                  mutate(RequireChecking = as.factor(RequireChecking)
                                         ,VOTINGCENTRETYPECODE = as.factor(VOTINGCENTRETYPECODE)
                                         ,CONTESTAREACODE = as.factor(CONTESTAREACODE)
                                         ,AREACODE = as.factor(AREACODE)
                                         ) %>%
                                     arrange(CONTESTID) %>%
                            filter(!is.na(GAZETTEDNAME))
                           #       mutate(dontshow = ifelse(VOTINGCENTRETYPECODE == 'Postal' & is.na(EXPECTED_BALLOTS),T,F)) %>%
                           #       filter(dontshow == F)

  Councillor_show <- Councillor_interim %>% select(
                                          `Requires Checking` = RequireChecking
                                         ,LGArea = AREACODE
                                         ,`Area/Ward` = CONTESTAREACODE
                                         ,`Vote Type` = VOTINGCENTRETYPECODE
                                         ,Venue = GAZETTEDNAME
                                         ,`Expected (A)` = EXPECTED_BALLOTS
                                         ,`Initial Count (B)` = INITIALCOUNT
                                         ,`Diff1 (B-A)` = Diff
                                         ,`Allocated (C)` = ALLOCATED
                                         ,`Accounted (D)` = ACCOUNTED_BALLOTS_EN
                                         ,`Diff2 (D-C)` = Diff2
                                         ,Comments = COMMENTS
                                         ) %>%
                                      arrange(LGArea
                                              ,`Area/Ward`
                                              ,`Vote Type`
                                              ,Venue)#%>% filter(!is.na(Initial))
                                  
Mayor[,c('INITIALCOUNT','EXPECTED_BALLOTS','ALLOCATED','ACCOUNTED_BALLOTS_EN')] <- sapply(Mayor[,c('INITIALCOUNT','EXPECTED_BALLOTS','ALLOCATED','ACCOUNTED_BALLOTS_EN')], as.numeric)


 Mayor_interim <- Mayor %>% 
                                 mutate(Diff = INITIALCOUNT - ifelse(is.na(EXPECTED_BALLOTS) & !is.na(INITIALCOUNT),0,EXPECTED_BALLOTS)
                                        ,Diff2 = ACCOUNTED_BALLOTS_EN - ALLOCATED
                                        ,CouncillorDiff = CouncillorBPs - INITIALCOUNT
                                         ) %>%
                                 mutate(RequireChecking = 'No') %>%
                                 mutate(RequireChecking = case_when(abs(Diff/INITIALCOUNT) > 0.01 ~ 'Yes' 
                                                                    ,abs(Diff2/ACCOUNTED_BALLOTS_EN) > 0.01 ~ 'Yes' 
                                                                    ,abs(CouncillorDiff/INITIALCOUNT) > 0.01 ~ 'Yes'
                                                                    ,is.na(CouncillorDiff) & abs(Diff/INITIALCOUNT) < 0.01 & abs(Diff2/ACCOUNTED_BALLOTS_EN) <0.01~ 'No'
                                                                    ,T ~ RequireChecking)) %>%
              mutate(RequireChecking = case_when(!VOTINGCENTRETYPECODE %in% c('PP','Pre-Poll') & abs(Diff/INITIALCOUNT) > 0.01 ~'Yes'
                                                 ,!VOTINGCENTRETYPECODE %in% c('PP','Pre-Poll') & Diff == 0 ~'No'
                                                 ,T ~ RequireChecking)) %>%
              mutate(RequireChecking = ifelse(!is.na(forceApproval) & forceApproval == 'Y','No',RequireChecking)) %>%   
                                             mutate(RequireChecking = as.factor(RequireChecking)
                                                    ,VOTINGCENTRETYPECODE = as.factor(VOTINGCENTRETYPECODE)
                                                    ,AREACODE = as.factor(AREACODE)) %>%
                                             arrange(CONTESTID) %>%
                                          filter(!is.na(GAZETTEDNAME)) #%>% 
  # mutate(dontshow = ifelse(VOTINGCENTRETYPECODE == 'Postal' & is.na(EXPECTED_BALLOTS),T,F)) %>%
  # filter(dontshow == F) 

  Mayor_show <- Mayor_interim %>% select(
                                          `Requires Checking` = RequireChecking
                                          ,`Area/Ward` = AREACODE
                                          ,`Vote Type` = VOTINGCENTRETYPECODE
                                          ,Venue = GAZETTEDNAME
                                          ,`Expected (A)` = EXPECTED_BALLOTS
                                          ,`Initial Count (B)` = INITIALCOUNT
                                          ,`Diff1 (B-A)` = Diff
                                          ,`Allocated (C)` = ALLOCATED
                                          ,`Accounted (D)` = ACCOUNTED_BALLOTS_EN
                                          ,`Diff2 (D-C)` = Diff2
                                          ,`Councillor (E)` = CouncillorBPs
                                          ,`Diff3 (E-B)`= CouncillorDiff
                                          ,Comments = COMMENTS
                                        )  %>%
                                      arrange( `Area/Ward`
                                              ,`Vote Type`
                                              ,Venue)#%>% filter(!is.na(Initial))
                    

Ref_poll[,c('INITIALCOUNT','EXPECTED_BALLOTS','ALLOCATED','ACCOUNTED_BALLOTS_EN')] <- sapply(Ref_poll[,c('INITIALCOUNT','EXPECTED_BALLOTS','ALLOCATED','ACCOUNTED_BALLOTS_EN')], as.numeric)

 Ref_poll_interim <- Ref_poll %>% 
                                 mutate(Diff = INITIALCOUNT - ifelse(is.na(EXPECTED_BALLOTS),0,EXPECTED_BALLOTS)
                                        ,Diff2 = ACCOUNTED_BALLOTS_EN - ALLOCATED
                                        ,CouncillorDiff = CouncillorBPs - INITIALCOUNT) %>%
                                 mutate(RequireChecking = 'No') %>%
                                 mutate(RequireChecking = case_when(abs(Diff/INITIALCOUNT) > 0.01 ~ 'Yes' 
                                                                    ,abs(Diff2/ACCOUNTED_BALLOTS_EN) > 0.01 ~ 'Yes' 
                                                                    ,abs(CouncillorDiff/INITIALCOUNT) > 0.01 ~ 'Yes'
                                                                    ,is.na(CouncillorDiff) & abs(Diff/INITIALCOUNT) < 0.01 & abs(Diff2/ACCOUNTED_BALLOTS_EN) <0.01~ 'No'
                                                                    ,T ~ RequireChecking)) %>%
  mutate(RequireChecking = case_when(!VOTINGCENTRETYPECODE %in% c('PP','Pre-Poll') & abs(Diff/INITIALCOUNT) > 0.01 ~'Yes'
                                     ,!VOTINGCENTRETYPECODE %in% c('PP','Pre-Poll') & Diff == 0 ~'No'
                                     ,T ~ RequireChecking)) %>%
   mutate(RequireChecking = ifelse(!is.na(forceApproval) & forceApproval == 'Y','No',RequireChecking)) %>%   
                                 mutate(RequireChecking = as.factor(RequireChecking)
                                        ,VOTINGCENTRETYPECODE = as.factor(VOTINGCENTRETYPECODE)
                                        ,CONTESTAREACODE = as.factor(CONTESTAREACODE)) %>%
                                 arrange(CONTESTID) %>%
                              filter(!is.na(GAZETTEDNAME)) #%>% 
 #  mutate(dontshow = ifelse(VOTINGCENTRETYPECODE == 'Postal' & is.na(EXPECTED_BALLOTS),T,F)) %>%
 #  filter(dontshow == F)

  Ref_poll_show <- Ref_poll_interim %>% select(
                                           `Requires Checking` = RequireChecking
                                           ,`Area/Ward` = CONTESTAREACODE
                                           ,`Vote Type` = VOTINGCENTRETYPECODE
                                           ,Venue = GAZETTEDNAME
                                           ,`Expected (A)` = EXPECTED_BALLOTS
                                           ,`Initial Count (B)` = INITIALCOUNT
                                           ,`Diff1 (B-A)` = Diff
                                           ,`Allocated (C)` = ALLOCATED
                                           ,`Accounted (D)` = ACCOUNTED_BALLOTS_EN
                                           ,`Diff2 (D-C)` = Diff2
                                           ,`Councillor (E)` = CouncillorBPs
                                           ,`Diff3 (E-B)`= CouncillorDiff
                                           ,Comments = COMMENTS
                                           ,`Contest Type` = CONTESTTYPECODE
                                           ,Comments = COMMENTS
                                           ,Question = QUESTIONLABEL
                                         ) %>%
                                        arrange( `Area/Ward`
                                                ,`Vote Type`
                                                ,Venue)
  # %>% filter(!is.na(Initial)) %>% distinct()
              
# Knit related tables --------------------------------------------------------------------

summary_1 <- bind_rows(Mayor %>% filter(VOTINGCENTRETYPECODE == 'Postal' & EXPECTED_BALLOTS > 0)
                       ,Mayor %>% filter(VOTINGCENTRETYPECODE == 'Postal' & is.na(EXPECTED_BALLOTS))
                       ,Mayor %>% filter(VOTINGCENTRETYPECODE != 'Postal'))
  
summary_2 <- Mayor %>% filter(INITIALCOUNT>0)

summary_3 <- bind_rows(Mayor %>% filter(VOTINGCENTRETYPECODE != 'Postal', ACCOUNTED_BALLOTS_EN > 0),
                       Mayor %>% filter(VOTINGCENTRETYPECODE == 'Postal', EXPECTED_BALLOTS > 0)
)
summary_5 <- Mayor_interim %>% filter(!is.na(INITIALCOUNT)) %>% filter(RequireChecking == 'Yes')
summary_4 <- Mayor_interim %>% filter(!is.na(INITIALCOUNT)) %>% filter(forceApproval == 'Y')


if(nrow(summary_1) >0) {
  
col1 <- SingleChoiceTable(summary_1, 'VOTINGCENTRETYPECODE') %>% rename(Total = Count)
} else {
  
  col1 <- tibble(VOTINGCENTRETYPECODE = NA
                     ,Total = NA)
  
}

if(nrow(summary_2) >0) {
  
col2 <- SingleChoiceTable(summary_2, 'VOTINGCENTRETYPECODE') %>% rename(`Initial Count Entered` = Count)
} else {
  
  col2 <- tibble(VOTINGCENTRETYPECODE = NA
                     ,`Initial Count Entered` = NA)
  
}

if(nrow(summary_3) >0) {
  
col3 <- SingleChoiceTable(summary_3, 'VOTINGCENTRETYPECODE') %>% rename(`Allocated/Expected` = Count)
} else {
  
  col3 <- tibble(VOTINGCENTRETYPECODE = NA
                     ,`Allocated/Expected` = NA)
}

if(nrow(summary_4) >0) {
  
col4 <- SingleChoiceTable(summary_4, 'VOTINGCENTRETYPECODE') %>% rename(`RMT Approved` = Count)
} else {
  col4 <- tibble(VOTINGCENTRETYPECODE = NA
                     ,`RMT Approved` = NA)
  
}
if(nrow(summary_5) >0) {
  
col5 <- SingleChoiceTable(summary_5, 'VOTINGCENTRETYPECODE') %>% rename(`Requires RMT Checking` = Count)

} else {
  
  col5 <- tibble(VOTINGCENTRETYPECODE = NA
                     ,`Requires RMT Checking` = NA)
  
}

Mayor_summary <- col1 %>% left_join(col2,by = "VOTINGCENTRETYPECODE") %>% 
                          left_join(col3) %>%
                          left_join(col5) %>%
                          left_join(col4) %>%
                    rename(`Vote Type` = VOTINGCENTRETYPECODE)

# Mayor_summary[is.na(Mayor_summary)] <- 0

###

summary_1 <- bind_rows(Councillor %>% filter(VOTINGCENTRETYPECODE == 'Postal' & EXPECTED_BALLOTS > 0)
                       ,Councillor %>% filter(VOTINGCENTRETYPECODE == 'Postal' & is.na(EXPECTED_BALLOTS))
                       
                      ,Councillor %>% filter(VOTINGCENTRETYPECODE != 'Postal'))
summary_2 <- Councillor %>% filter(INITIALCOUNT>0)
summary_3 <- bind_rows(Councillor %>% filter(VOTINGCENTRETYPECODE != 'Postal', ACCOUNTED_BALLOTS_EN > 0),
                       Councillor %>% filter(VOTINGCENTRETYPECODE == 'Postal', EXPECTED_BALLOTS > 0)
                       )
summary_5 <- Councillor_interim %>% filter(!is.na(INITIALCOUNT)) %>% filter(RequireChecking == 'Yes')
summary_4 <- Councillor_interim %>% filter(!is.na(INITIALCOUNT)) %>% filter(forceApproval == 'Y')

if(nrow(summary_1) >0) {
  
col1 <- SingleChoiceTable(summary_1, 'VOTINGCENTRETYPECODE') %>% rename(Total = Count)
} else {
  
  col1 <- tibble(VOTINGCENTRETYPECODE = NA
                     ,Total = NA)
  
  
}
if(nrow(summary_2) >0) {
  
col2 <- SingleChoiceTable(summary_2, 'VOTINGCENTRETYPECODE') %>% rename(`Initial Count Entered` = Count)

} else {
  
  col2 <- tibble(VOTINGCENTRETYPECODE = NA
                     ,`Initial Count Entered` = NA)
  
}
if(nrow(summary_3) >0) {
  
col3 <- SingleChoiceTable(summary_3, 'VOTINGCENTRETYPECODE') %>% rename(`Allocated/Expected` = Count)
} else {
  
  col3 <- tibble(VOTINGCENTRETYPECODE = NA
                     ,`Allocated/Expected` = NA)
  
  
}
if(nrow(summary_4) >0) {
  
col4 <- SingleChoiceTable(summary_4, 'VOTINGCENTRETYPECODE') %>% rename(`RMT Approved` = Count)
} else {
  
  col4 <- tibble(VOTINGCENTRETYPECODE = NA
                     ,`RMT Approved` = NA)
  
}
if(nrow(summary_5) >0) {
  
col5 <- SingleChoiceTable(summary_5, 'VOTINGCENTRETYPECODE') %>% rename(`Requires RMT Checking` = Count)
} else{
  col5 <- tibble(VOTINGCENTRETYPECODE = NA
                     ,`Requires RMT Checking` = NA)
  
}
Councillor_summary <- col1 %>% left_join(col2) %>% 
  left_join(col3) %>%
  left_join(col5) %>%
  left_join(col4) %>%
  rename(`Vote Type` = VOTINGCENTRETYPECODE)

# Councillor_summary[is.na(Councillor_summary)] <- 0


###


summary_1 <- bind_rows(Ref_poll %>% filter(VOTINGCENTRETYPECODE == 'Postal' & EXPECTED_BALLOTS > 0)
                       ,Ref_poll %>% filter(VOTINGCENTRETYPECODE == 'Postal' & is.na(EXPECTED_BALLOTS))
                       
                       ,Ref_poll %>% filter(VOTINGCENTRETYPECODE != 'Postal'))
summary_2 <- Ref_poll %>% filter(INITIALCOUNT>0)
summary_3 <- bind_rows(Ref_poll %>% filter(VOTINGCENTRETYPECODE != 'Postal', ACCOUNTED_BALLOTS_EN > 0),
                       Ref_poll %>% filter(VOTINGCENTRETYPECODE == 'Postal', EXPECTED_BALLOTS > 0)
)
summary_4 <- Ref_poll_interim %>% filter(!is.na(INITIALCOUNT)) %>% filter(forceApproval == 'Y')
summary_5 <- Ref_poll_interim %>% filter(!is.na(INITIALCOUNT)) %>% filter(RequireChecking == 'Yes')

if(nrow(summary_1) >0) {
  
col1 <- SingleChoiceTable(summary_1, 'VOTINGCENTRETYPECODE') %>% rename(Total = Count)
} else {
  col1 <- tibble(VOTINGCENTRETYPECODE = NA
                     ,Total = NA)
  
}
if(nrow(summary_2) >0) {
  
col2 <- SingleChoiceTable(summary_2, 'VOTINGCENTRETYPECODE') %>% rename(`Initial Count Entered` = Count)
} else {
  col2 <- tibble(VOTINGCENTRETYPECODE = NA
                     ,`Initial Count Entered` = NA)
}

if(nrow(summary_3) >0) {
  
col3 <- SingleChoiceTable(summary_3, 'VOTINGCENTRETYPECODE') %>% rename(`Allocated/Expected` = Count)
} else{
  col3 <- tibble(VOTINGCENTRETYPECODE = NA
                     ,`Allocated/Expected` = NA)
}

if(nrow(summary_4) >0) {
  
col4 <- SingleChoiceTable(summary_4, 'VOTINGCENTRETYPECODE') %>% rename(`RMT Approved` = Count)
} else {
  
  col4 <- tibble(VOTINGCENTRETYPECODE = NA
                     ,`RMT Approved` = NA)
  
}

if(nrow(summary_5) >0) {
  
col5 <- SingleChoiceTable(summary_5, 'VOTINGCENTRETYPECODE') %>% rename(`Requires RMT Checking` = Count)
} else {
  
  col5 <- tibble(VOTINGCENTRETYPECODE = NA
                     ,`Requires RMT Checking` = NA)
  
}
Ref_poll_summary <- col1 %>% left_join(col2) %>% 
  left_join(col3) %>%
  left_join(col5) %>%
  left_join(col4) %>%
  rename(`Vote Type` = VOTINGCENTRETYPECODE)

# Ref_poll_summary[is.na(Ref_poll_summary)] <- 0





# deal with exceptions in the show dataset


# save data for RO --------------------------------------------------------

setwd('//SVRANALYTICS1/AnalyticsData/LG2101/RMT')

write.csv(Councillor,'Counvillor initial count.csv')
write.csv(Mayor,'Mayor initial count.csv')
write.csv(Ref_poll,'Ref Poll initial count.csv')








