
# this had to be a temporary solution as days marked off and days counted does not match in testing
# Markoff only occured in 3 days and count happend on 11 different days



Days_AoBP <- AoBP %>% distinct(VenueName,ReconciliationDate) %>% group_by(VenueName) %>%
                     mutate(Day = order(ReconciliationDate)) %>%
                      select(VenueName,ReconciliationDate,Day)

  
Days_Markoff <- PrePoll_Markoff %>% distinct(VENUENAME,DAY) %>% group_by(VENUENAME) %>%
                   mutate(Day = order(DAY)) %>%
                   select(VENUENAME,DAY,Day)


AoBP_ram1 <- AoBP %>% 
             left_join(Days_AoBP, by = c("ReconciliationDate", "VenueName"))


MarkOff_ram1 <- PrePoll_Markoff %>% 
             left_join(Days_Markoff, by =c("DAY",'VENUENAME' = 'VENUENAME'))



# summing up markoff to location level

# decv is grouped together as RMT wanted decvote to show as separate to prepoll ord as a column but decv as a whole.

MarkOff_ram1 <- MarkOff_ram1 %>% mutate(DecVotes = ifelse(DECLARATIONEXCUSEVOTETYPECODE == 'Pre-poll Ordinary','Pre-poll','DecVote'))


Location_Daily_MarkOff_Summary <- MarkOff_ram1 %>% group_by(ELECTIONEVENTID,DAY,Day,VOTINGCENTRETYPECODE,VENUENAME,LOCATIONID,DecVotes) %>%
                           summarise(MarkOff = sum(TOTAL)) %>%
                        arrange(VENUENAME,DAY) %>%
                        pivot_wider(names_from = DecVotes,values_from = MarkOff)
                                                  


Total_Allocations <- Allocations %>% group_by(ContestID,ContestName,AreaCode,LocationID,VenueName,LocationType) %>%
  summarise(Quantity = sum(Quantity)) %>%
  filter(LocationType == 'Pre-Poll') 



#joining AoBP with markoff and allocations


PrePoll_reconcilliation <- AoBP_ram1 %>%
  full_join(Location_Daily_MarkOff_Summary, by=c('VenueName'='VENUENAME'
                                                 ,'LocationType'='VOTINGCENTRETYPECODE'
                                                 ,'Day')) %>%
  select(ContestID
         ,ContestName
         ,AreaCode
         ,VenueName
         ,LocationType
         ,ReconciliationDate
         ,MarkOffDay = Day
         ,`Pre-poll`
         ,DecVote
         ,Spoilt
         ,Discarded
         ,Unused
  ) %>%
  arrange(ContestID,VenueName,MarkOffDay) %>%
  group_by(ContestID
           ,ContestName
           ,AreaCode
           ,VenueName
           ,LocationType
           ) %>%
  mutate(CumulativeDecVote = cumsum(DecVote)
         ,`CumulativePre-poll` = cumsum(`Pre-poll`)
         ,CumulativeSpoilt = cumsum(Spoilt)
         ,CumulativeDiscarded = cumsum(Discarded)
         )


PrePoll_reconcilliation_allocations <- PrePoll_reconcilliation %>%

  full_join(Total_Allocations, by = c("ContestID", "ContestName", "AreaCode", "VenueName", "LocationType")) %>%
  filter(!is.na(ContestID) & !is.na(ReconciliationDate))
  
PrePoll_reconcilliation_allocations[is.na(PrePoll_reconcilliation_allocations)] <- 0

PrePoll_reconcilliation_allocations <- PrePoll_reconcilliation_allocations %>%
  mutate(Q_M_S = Quantity - `CumulativePre-poll` - CumulativeDecVote - CumulativeSpoilt) %>%
  mutate(VarianceAcceptable = ifelse(Q_M_S/Unused > 1.01 | Q_M_S/Unused < 0.99,'No','Yes' )) %>%
  
 dplyr::select(ContestID
                ,ContestName
                ,AreaCode
                ,VenueName
                ,LocationType
                ,ReconciliationDate
                ,MarkOffDay
                ,`Pre-poll`
                ,DecVote
                ,Spoilt
                ,Discarded
                ,CumulativeDecVote
                ,`CumulativePre-poll`
                ,CumulativeSpoilt
                ,CumulativeDiscarded
                ,LocationID
                ,Quantity
                ,Unused
                ,Q_M_S
                ,VarianceAcceptable)




# find out missing AoBP
missing_AoBP <- Total_Allocations %>% anti_join(AoBP)

currentday_AoBP <- AoBP %>% filter(ReconciliationDate == max(ReconciliationDate))

current_missing_PP <- nrow(Total_Allocations %>% ungroup() %>% distinct(LocationID)) - nrow(currentday_AoBP %>% ungroup() %>% distinct(LocationID))

current_missing_LGA <- nrow(Total_Allocations %>% ungroup() %>% distinct(AreaCode)) - nrow(currentday_AoBP %>% ungroup() %>% distinct(AreaCode))

Currentday_MarkOffs <- MarkOff_ram1 %>% filter(Day == max(Day))


# markoff line

Daily_prepoll <- MarkOff_ram1 %>% group_by(DAY) %>%
          summarise(Prepoll = sum(TOTAL))





  

