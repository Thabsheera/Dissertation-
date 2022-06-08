# Example preprocessing script.


# create the generation groups 

## converting the age & iyear to numeric 
uktus15_diary_ep_long$DVAge<- as.numeric(as.character(uktus15_diary_ep_long$DVAge))
uktus15_diary_ep_long$IYear<- as.numeric(as.character(uktus15_diary_ep_long$IYear))
# creating generations

uktus15_diary_ep_long<- uktus15_diary_ep_long%>%
  mutate(birth_year=IYear-DVAge)%>%
  mutate(generation= ifelse(birth_year<1928,"others",
                            ifelse(birth_year<=1945,"Silent Generation",
                                   ifelse(birth_year<=1964,"Baby Boomers",
                                          ifelse(birth_year<=1980,"Gen X",
                                                 ifelse(birth_year<=1996,"Millennials",
                                                        ifelse(birth_year<=2012,"Gen Z","Unknown Generation"
                                                               
                            )))))))%>%
  mutate(Psychosocial_Stages= ifelse(DVAge<10,"0-9",
                            ifelse(DVAge<=18,"10-18",
                                   ifelse(DVAge<=25,"19-25",
                                          ifelse(DVAge<=39,"26-39",
                                                 ifelse(DVAge<=49,"40-49",
                                                        ifelse(DVAge<=57,"50-57",
                                                               ifelse(DVAge<=67,"58-67",
                                                                      ifelse(DVAge<=74,"68-74",
                                                                             ifelse(DVAge<=84,"75-84",
                                                                                    ifelse(DVAge<=94,"85-94",
                                                                                           "95+"
                                                               
                                                        )))))))))))%>%
  mutate(dow=ifelse(ddayw %in% c('Saturday','Sunday') , 'Sat-Sun','Mon-Fri'))
  
  



# mapping the activity category to big table 


diary_long_df<- uktus15_diary_ep_long%>%
  left_join(activity_freq.activity_freq)


# creating an hour column

# for that selecting the first two characters in the tid column

diary_long_df$hour<- as.numeric(substr(diary_long_df$tid,start = 1,stop = 2))

# joining the household data to diary table

# filtering the fields from households data 

household_filtered<- uktus15_household%>%
  select(-HhOut,-strata,-psu,-IMonth,-IYear)

# View(head(diary_long_df%>%left_join(household_filtered)))


# joining the individual data to diary table

# filtering the fields from individual data 

individual_filtered<- uktus15_individual%>%
  select(-strata,-psu,-HhOut,-IndOut,-IMonth,-IYear,-DMFlag,-DVAge)
individual_filtered$dhhtype<- as.character(individual_filtered$dhhtype)
# View(head(diary_long_df%>%left_join(individual_filtered)))

diary_ind_mapped<-diary_long_df%>%
  left_join(individual_filtered)%>%
  mutate(hhtype=ifelse(is.na(dhhtype),NA,ifelse(grepl(x = dhhtype,pattern = 'with children',ignore.case = TRUE),"Household With Children",
                       ifelse(dhhtype=='Single person household','Single person household',
                              'Household Without Children'))))


# since household data already in individual data, considering only individual data




# joining the week schedule data to diary table

# filtering the fields from week schedule data 

wksched_filtered<- uktus15_wksched%>%
  select(-strata,-psu)

# View(head(diary_long_df%>%left_join(wksched_filtered)))





#-------------------------------------------------------

# creating the table with most frequent activity 
#  at that hour
# --------------------------------------------------


frequent_activity_hourly<- diary_ind_mapped%>%
  group_by(serial,pnum,ddayw,hour,whatdoing,Macro.Group)%>%
  summarise(freq=n())%>%
  arrange(desc(freq))%>% 
  group_by(serial,pnum,ddayw,hour)%>%
  mutate(rank = rank(desc(freq), ties.method = "first"))%>%
  filter(rank==1)


# inner joining with initial table 

diary_ind_mapped_simplified<- diary_ind_mapped%>%
  inner_join(frequent_activity_hourly)

