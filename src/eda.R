library('ProjectTemplate')
load.project()

# for (dataset in project.info$data)
# {
#   message(paste('Showing top 5 rows of', dataset))
#   print(head(get(dataset)))
# }


# checking the distribution of variables 

# activity count across time 

activity_freq<- uktus15_diary_ep_long%>%
  group_by(whatdoing)%>%
  summarise(freq= n(),
            ratio=round((freq/587632)*100,2))%>%
  arrange(desc(freq))

# groups classic group by generation
# grouping by interest group for us
# 






# activity except sleep/eating 
activity_freq_wo_slp_eat<- uktus15_diary_ep_long%>%
  filter(!whatdoing %in% c('Eating','Sleep'))%>%
  group_by(whatdoing)%>%
  summarise(freq= n(),
            ratio=round((freq/470855)*100,2))%>%
  arrange(desc(freq))

ggplot(activity_freq,aes(x="",y=freq,fill=whatdoing))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+theme_void()+
  #theme(legend.position="none") +
  theme(legend.text=element_text(size=4))
  # geom_text(aes(y=freq,label=whatdoing),size=2,
  #           position = position_stack(vjust = 0.5))
hist(uktus15_diary_ep_long$whatdoing)

ggplot(activity_freq,aes(x=tid,y=whatdoing,size=freq))+
  geom_point()
library(data.table)
p<-ggplot(uktus15_diary_ep_long%>%
            filter(tid %like% "^10:10"), aes(whatdoing)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
ggplotly(p)

View(uktus15_diary_ep_long%>%
       filter(tid %like% "^10")
     )


summary(uktus15_diary_ep_long)

# ***********************************************

# Hypothesis _____________________________________

# *************************************************




# questions

# Q1 - What is the most common activities 
# Q2 - How's the activity usage varies across 
#      household type 
# Q3 - the activities by different age group on 
#       different day of the week?
# Q4 - where do they do those activities from
# Q5 - level of enjoyment for each activity
#       for each age group and day


# -----------------------------------------
# ------------------Q1
#------------------------------------------

# using the table diary_ind_mapped

macro_behavior_freq<- diary_ind_mapped%>%
  mutate(dow=ifelse(ddayw %in% c('Saturday','Sunday') , 'Sat-Sun','Mon-Fri'))%>%
  group_by(Macro.Group,dow)%>%
  summarise(Freq=n())%>%
  arrange(desc(Freq))

ggplot(macro_behavior_freq,aes(x=reorder(Macro.Group,-Freq),y=Freq))+
  geom_col()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 7))+
  facet_grid(. ~ dow)

# during weekend tv, video has more frequency 
# compared to personal care 
# also weekends account for more travel 
# most likey for work or school than eating 




# ---------------------------------------------
# --------- Q2 
#----------------------------------------------

# Q2 - How's the activity usage varies across 
#      household type 

View(data.frame(colnames(diary_ind_mapped)))

unique(diary_ind_mapped$dhhtype)
sum(is.na(diary_ind_mapped$dhhtype))

macro_behavior_freq_hhtype<- diary_ind_mapped%>%
  filter(!is.na(dhhtype))%>%
  mutate(hhtype=ifelse(grepl(x = dhhtype,pattern = 'with children',ignore.case = TRUE),"Household With Children",
                       ifelse(dhhtype=='Single person household','Single person household',
                              'Household Without Children'))
                       )%>%
  group_by(Macro.Group,hhtype)%>%
  summarise(Freq=n(),
            hh_count=n_distinct(serial),
            Freq_per_hh=Freq/hh_count)%>%
  arrange(desc(Freq))

ggplot(macro_behavior_freq_hhtype,aes(x=reorder(Macro.Group,-Freq_per_hh),y=Freq_per_hh))+
  geom_col()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 7))+
  facet_grid(. ~ hhtype)




#----------------------------------------------
# -----------------Q3
#---------------------------------------------

# Q3 - the activities by different age group on 
#       different day of the week?

diary_ind_mapped$DVAge<- as.numeric(as.character(diary_ind_mapped$DVAge))
macro_behavior_freq<- diary_ind_mapped%>%
  mutate(dow=ifelse(ddayw %in% c('Saturday','Sunday') , 'Sat-Sun','Mon-Fri'))%>%
  mutate(age_group=ifelse(DVAge<=14,'Children',
                          ifelse(DVAge<=24 ,'Youth',
                                 ifelse(DVAge<=64,'Adults',
                                        'Seniors'))))%>%
  group_by(Macro.Group,age_group)%>%
  summarise(Freq=n())%>%
  arrange(desc(Freq))

ggplot(macro_behavior_freq,aes(x=reorder(Macro.Group,-Freq),y=Freq))+
  geom_col()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 7))+
  facet_wrap(. ~ age_group)



# ----------------------------------------------
# ------------ Q4
#--------------------------------------------------

# Q4 - where do they do those activities from

macro_behavior_freq<- diary_ind_mapped%>%
  mutate(dow=ifelse(ddayw %in% c('Saturday','Sunday') , 'Sat-Sun','Mon-Fri'))%>%
  group_by(Macro.Group,WhereWhen)%>%
  summarise(Freq=n())%>%
  arrange(desc(Freq))

ggplot(macro_behavior_freq%>%
         filter(Macro.Group=='social life and entertainment'),
       aes(x=reorder(WhereWhen,-Freq),y=Freq))+
  geom_col()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 7))+
  facet_wrap(. ~ Macro.Group)




# ---------------------------------------------
# --------------------Q5
#----------------------------------------------
# Q5 - level of enjoyment for each activity
#       for each age group and day

macro_behavior_freq<- diary_ind_mapped%>%
  filter(!Enjoy %in% c('NA','not reported'))%>%
  mutate(dow=ifelse(ddayw %in% c('Saturday','Sunday') , 'Sat-Sun','Mon-Fri'))%>%
  mutate(age_group=ifelse(DVAge<=14,'Children',
                          ifelse(DVAge<=24 ,'Youth',
                                 ifelse(DVAge<=64,'Adults',
                                        'Seniors'))))%>%
  group_by(Macro.Group,Enjoy)%>%
  summarise(Freq=n())%>%
  arrange(desc(Freq))

ggplot(macro_behavior_freq,
       aes(x=reorder(Macro.Group,-Freq),y=Freq))+
  geom_col()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 7))+
  facet_wrap(. ~ Enjoy)

plot_fn <- function(x) {
  p<-ggplot(macro_behavior_freq%>%
              filter(Enjoy==x),
            aes(x=reorder(Macro.Group,-Freq),y=Freq))+
    geom_col()+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 7))+
    facet_wrap(. ~ Enjoy)
  return(p)  
}

enjoy_1<-plot_fn('not at all')
enjoy_2<-plot_fn('2')
enjoy_3<-plot_fn('3')
enjoy_4<-plot_fn('4')
enjoy_5<-plot_fn('5')
enjoy_6<-plot_fn('6')
enjoy_7<-plot_fn('very much')









