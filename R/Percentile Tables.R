#### Percentile table

percentileTable <- read_csv("ISASPDashboard/Data/IowaPercentileRanksfor2019.csv")


lookup_percentile <- function(studentgrade=3, score=410, type="ELA"){

  #read the percentiles
  percentileTable <- read.csv("ISASPDashboard/Data/IowaPercentileRanksfor2019.csv")

  percentileTable %>%
    filter(Grade == studentgrade) %>%
    filter(ScaleScore==score) -> percentileRow

  if(type=="ELA"){
    percentileRow %>%
      select(ELA)-> percentile
  }
  else if(type=="Reading"){
    percentileRow %>%
      select(Reading)-> percentile
  }
  else if(type=="LW"){
    percentileRow %>%
      select(Language.Writing)-> percentile
  }
  else if(type=="Science"){
    percentileRow %>%
      select(Science)-> percentile
  }
  else if(type=="Math"){
    percentileRow %>%
      select(Mathematics)-> percentile
  }
  else{
    percentile <- NULL
  }

    return(percentile[1,1])


}



lookup_percentile(3, 410, "Math")-> test



#### THIS IS VERY SLOW NEED TO FIGURE OUT HOW TO OPTIMISE IT
scale %>%
  mutate(ELAlable="ELA")%>%
  mutate(ELAPercentile=mapply(lookup_percentile, Grade, ELAScaleScore,ELAlable )) %>%
  mutate(Readinglable="Reading")%>%
  mutate(ReadPercentile=mapply(lookup_percentile, Grade, ReadScaleScore,Readinglable )) %>%
  mutate(LWlable="LW")%>%
  mutate(LWAPercentile=mapply(lookup_percentile, Grade, LWScaleScore,LWlable )) %>%
  mutate(Mathlable="Math")%>%
  mutate(MathPercentile=mapply(lookup_percentile, Grade, MathScaleScore,Mathlable )) %>%
  mutate(Scilable="Science")%>%
  mutate(SciPercentile=mapply(lookup_percentile, Grade, SciScaleScore,Scilable )) %>%
  select(StateID, Grade,
         ELAScaleScore, ELAPercentile,
         ReadScaleScore, ReadPercentile,
         LWScaleScore, LWAPercentile,
         MathScaleScore, MathPercentile,
         SciScaleScore, SciPercentile)-> percentiles

