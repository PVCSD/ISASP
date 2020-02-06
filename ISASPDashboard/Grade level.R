### read in the data

library(tidyverse)


proficencies <- readr::read_csv("~/PVCSD Annual Data Analysis Packet 2019-2020/data/tidy state proficencies.csv")

grade = 3

proficencies %>%
  filter(Grade == grade ) %>%
  group_by(Year)%>%
  mutate(Score = str_remove(Score, pattern = "%")) %>%
  summarise(Score2 = mean(as.numeric(Score), na.rm = T), 
            testName = `Test Name`[1])%>%
  ggplot(aes(x=Year, y=Score2, fill=testName)) +
  geom_bar(stat='identity')+
  labs(x="", y="", title = paste0("Grade ", grade, " Percent Proficient By Year" ))+
  theme_pleasval()+
  theme(legend.position = "bottom")+
  scale_fill_manual( values = c("#26547C", "#011638"))
