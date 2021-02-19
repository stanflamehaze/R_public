library(tidyverse)
getwd()
setwd('\\\\Mac/Home/Desktop')
#gc165ab
gc165ab <- read.csv('gc165ab.csv', sep = ';')

#Average Throughout the 2 months per day
gc165ab %>% 
  group_by(Day, Hour) %>% 
  summarise(Avg_hour = mean(Sat.Count)) %>% 
  ggplot()+geom_line(aes(Hour, Avg_hour, color=Day))+
  labs(title = "Average saturation per day throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )

gc165ab %>% 
  group_by(Day, Hour) %>% 
  summarise(Avg_hour = mean(Sat.Count)) %>% 
  ggplot()+geom_line(aes(Hour, Avg_hour, color=Day))+facet_wrap('Day')+
  labs(title = "Average saturation per day throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )

#Mondays
gc165ab %>% 
  filter(Day=='MO') %>% 
  group_by(Date, Hour) %>% 
  summarise(mean_hour=mean(Sat.Count)) %>% 
  arrange(desc(Date)) %>% 
  ggplot()+geom_line(aes(Hour, mean_hour, color=Date))+facet_wrap('Date')+
  labs(title = "Average saturation on Mondays throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )

gc165ab %>% 
  filter(Day=='MO') %>% 
  group_by(Date, Hour) %>% 
  summarise(mean_hour=mean(Sat.Count)) %>% 
  arrange(desc(Date)) %>% 
  ggplot()+geom_line(aes(Hour, mean_hour, color=Date))+
  labs(title = "Average saturation on Mondays throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )

#Tuesdays
gc165ab %>% 
  filter(Day=='TU') %>% 
  group_by(Date, Hour) %>% 
  summarise(mean_hour=mean(Sat.Count)) %>% 
  arrange(desc(Date)) %>% 
  ggplot()+geom_line(aes(Hour, mean_hour, color=Date))+facet_wrap('Date')+
  labs(title = "Average saturation on Tuesdays throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )

gc165ab %>% 
  filter(Day=='TU') %>% 
  group_by(Date, Hour) %>% 
  summarise(mean_hour=mean(Sat.Count)) %>% 
  arrange(desc(Date)) %>% 
  ggplot()+geom_line(aes(Hour, mean_hour, color=Date))+
  labs(title = "Average saturation on Tuesdays throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )

#Wednesdays
gc165ab %>% 
  filter(Day=='WE') %>% 
  group_by(Date, Hour) %>% 
  summarise(mean_hour=mean(Sat.Count)) %>% 
  arrange(desc(Date)) %>% 
  ggplot()+geom_line(aes(Hour, mean_hour, color=Date))+facet_wrap('Date')+
  labs(title = "Average saturation on Wednesdays throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )

gc165ab %>% 
  filter(Day=='WE') %>% 
  group_by(Date, Hour) %>% 
  summarise(mean_hour=mean(Sat.Count)) %>% 
  arrange(desc(Date)) %>% 
  ggplot()+geom_line(aes(Hour, mean_hour, color=Date))+
  labs(title = "Average saturation on Wednesdays throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )

#Thursdays
gc165ab %>% 
  filter(Day=='TH') %>% 
  group_by(Date, Hour) %>% 
  summarise(mean_hour=mean(Sat.Count)) %>% 
  arrange(desc(Date)) %>% 
  ggplot()+geom_line(aes(Hour, mean_hour, color=Date))+
  labs(title = "Average saturation on Thursdays throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )

gc165ab %>% 
  filter(Day=='TH') %>% 
  group_by(Date, Hour) %>% 
  summarise(mean_hour=mean(Sat.Count)) %>% 
  arrange(desc(Date)) %>% 
  ggplot()+geom_line(aes(Hour, mean_hour, color=Date))+facet_wrap('Date')+
  labs(title = "Average saturation on Thursdays throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )


#Fridays
gc165ab %>% 
  filter(Day=='FR') %>% 
  group_by(Date, Hour) %>% 
  summarise(mean_hour=mean(Sat.Count)) %>% 
  arrange(desc(Date)) %>% 
  ggplot()+geom_line(aes(Hour, mean_hour, color=Date))+
  labs(title = "Average saturation on Fridays throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )

gc165ab %>% 
  filter(Day=='FR') %>% 
  group_by(Date, Hour) %>% 
  summarise(mean_hour=mean(Sat.Count)) %>% 
  arrange(desc(Date)) %>% 
  ggplot()+geom_line(aes(Hour, mean_hour, color=Date))+facet_wrap('Date')+
  labs(title = "Average saturation on Fridays throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )

#Saturdays
gc165ab %>% 
  filter(Day=='SA') %>% 
  group_by(Date, Hour) %>% 
  summarise(mean_hour=mean(Sat.Count)) %>% 
  arrange(desc(Date)) %>% 
  ggplot()+geom_line(aes(Hour, mean_hour, color=Date))+
  labs(title = "Average saturation on Saturdays throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )

gc165ab %>% 
  filter(Day=='SA') %>% 
  group_by(Date, Hour) %>% 
  summarise(mean_hour=mean(Sat.Count)) %>% 
  arrange(desc(Date)) %>% 
  ggplot()+geom_line(aes(Hour, mean_hour, color=Date))+facet_wrap('Date')+
  labs(title = "Average saturation on Saturdays throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )

#sundays
gc165ab %>% 
  filter(Day=='SU') %>% 
  group_by(Date, Hour) %>% 
  summarise(mean_hour=mean(Sat.Count)) %>% 
  arrange(desc(Date)) %>% 
  ggplot()+geom_line(aes(Hour, mean_hour, color=Date))+
  labs(title = "Average saturation on Sundays throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )

gc165ab %>% 
  filter(Day=='SU') %>% 
  group_by(Date, Hour) %>% 
  summarise(mean_hour=mean(Sat.Count)) %>% 
  arrange(desc(Date)) %>% 
  ggplot()+geom_line(aes(Hour, mean_hour, color=Date))+facet_wrap('Date')+
  labs(title = "Average saturation on Sundays throughout July and August - gc165ab", x= "Hour", y="Mean Saturation Count" )

