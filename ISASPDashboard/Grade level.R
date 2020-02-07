### read in the data

library(tidyverse)


proficencies <- readr::read_csv("~/PVCSD Annual Data Analysis Packet 2019-2020/data/tidy state proficencies.csv")



###### GRADE LEVEL PROFICENCIES ######
grade = 3

proficencies %>%
  filter(Grade == grade ) %>%
  group_by(Year)%>%
  mutate(Score = str_remove(Score, pattern = "%")) %>%
  summarise(Score2 = mean(as.numeric(Score), na.rm = T), 
            testName = `Test Name`[1])%>%
  ggplot(aes(x=Year, y=Score2, fill=testName, color=testName)) +
  geom_bar(stat='identity')+
  ylim(0,100)+
  labs(x="", y="", 
       title = paste0("Grade ", grade, " Percent Proficient By Year" ),
       fill="Test", color= "Test")+
  theme_pleasval()+
  theme(legend.position = "bottom")+
  scale_fill_manual( values = c("#26547C", "#011638"))+
  scale_color_manual( values = c("#000000", "#F6F6F6"))


###### GRADE LEVEL PROFICENCIES BY SCHOOL ######
grade = 3
school = 'BRDV'

proficencies %>%
  filter(Grade == grade ) %>%
  group_by(Year, School)%>%
  mutate(Score = str_remove(Score, pattern = "%")) %>%
  summarise(Score2 = mean(as.numeric(Score), na.rm = T), 
            testName = `Test Name`[1])%>%
  ggplot(aes(x=Year, y=Score2, fill=testName, color=testName)) +
  geom_bar(stat='identity')+
  ylim(0,100)+
  labs(x="", y="", 
       title = paste0(" Grade ", grade, " Percent Proficient By Year" ), 
       fill="Test", color= "Test")+
  theme_pleasval()+
  theme(legend.position = "bottom")+
  scale_fill_manual( values = c("#26547C", "#011638"))+
  scale_color_manual( values = c("#000000", "#F6F6F6"))+
  facet_wrap(vars(School), nrow = 3)
