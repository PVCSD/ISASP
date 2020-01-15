########### ISASP DASHBOARD ##################
library(tidyr)
library(dplyr)
library(ggplot2)



subgroups <- c("AmericanIndianorAlaskan","Asian", "AfricanAmerican", "HispanicLatino","HawaiianPacificIslander","White",
               "MilitaryConnected","SE","504","FRL","GT","ELL","T1L","T1M","Homeless")


df <- ISASP_DATA



df %>%
  select(DistrictName, LastName, FirstName, Gender, StateID, DistrictID, Grade,
         AmericanIndianorAlaskan,Asian, AfricanAmerican,HispanicLatino,HawaiianPacificIslander,
         White, MilitaryConnected,SE, "plan504"=`504.0`, FRL, GT,ELL,T1L,T1M, Homeless) -> studentDemo



###### GET MAIN SCORES INTO THEIR OWN TABLES
df %>%
  select(StateID, Grade, #identifer and grade
         ELAScaleScore, ELAAchLvl, ELARawScore, #main ELA Scores
         ReadScaleScore, ReadRawScore, #main Reading Scores
         LWScaleScore, LWRawScore #main Writing Scores
         ) -> scoresELA
df %>%
  select(StateID, Grade, #identifer and grade
         MathScaleScore, MathAchLvl, MathRawScore
  ) -> scoresMath

df %>%
  select(StateID, Grade, #identifer and grade
       SciScaleScore, SciAchLvl, SciRawScore
  ) -> scoresScience



###### SUBTESTSCORES ########


#### Reading ####

### KID ###
df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=KIDLabel, "pctCorrect"=KIDPctCorrect, "pointsPossible"=KIDPntPoss, "subScore"=KIDRawScore
         ) -> scoresKID
### CS ###
df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=CSLabel, "pctCorrect"=CSPctCorrect, "pointsPossible"=CSPntPoss, "subScore"=CSRawScore
  ) -> scoresCS

### IKI ###
df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=IKILabel, "pctCorrect"=IKIPctCorrect, "pointsPossible"=IKIPntPoss, "subScore"=IKIRawScore
  ) -> scoresIKI



#### Langauge/Writing ####

df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=RPKLabel, "pctCorrect"=RPKPctCorrect, "pointsPossible"=RPKPntPoss, "subScore"=RPKRawScore
  ) -> scoresRPK
### PDW ###
df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=PDWLabel, "pctCorrect"=PDWPctCorrect, "pointsPossible"=PDWPntPoss, "subScore"=PDWRawScore
  ) -> scoresPDW

### TTP ###
df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=TTPLabel, "pctCorrect"=TTPPctCorrect, "pointsPossible"=TTPPntPoss, "subScore"=TTPRawScore
  ) -> scoresTTP

### COSE-KOL
df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=COSEKLPLabel, "pctCorrect"=COSEKLPPctCorrect, "pointsPossible"=COSEKLPPntPoss, "subScore"=COSEKLPRawScore
  ) -> scoresCOSE


### VAU ###
df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=VAULabel, "pctCorrect"=VAUPctCorrect, "pointsPossible"=VAUPntPoss, "subScore"=VAURawScore
  ) -> scoresVAU




#### Science ####

### LS ###
df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=LSLabel, "pctCorrect"=LSPctCorrect, "pointsPossible"=LSPntPoss, "subScore"=LSRawScore
  ) -> scoresLS
### PS ###
df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=PSLabel, "pctCorrect"=PSPctCorrect, "pointsPossible"=PSPntPoss, "subScore"=PSRawScore
  ) -> scoresPS

### ES ###
df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=ESLabel, "pctCorrect"=ESPctCorrect, "pointsPossible"=ESPntPoss, "subScore"=ESRawScore
  ) -> scoresES


#### Math ####


### MathD1 ###
df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=MathD1Label, "pctCorrect"=MathD1PctCorrect, "pointsPossible"=MathD1PntPoss, "subScore"=MathD1RawScore
  ) -> scoresMathD1

### MathD2 ###
df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=MathD2Label, "pctCorrect"=MathD2PctCorrect, "pointsPossible"=MathD2PntPoss, "subScore"=MathD2RawScore
  ) -> scoresMathD2

### MathD3 ###
df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=MathD3Label, "pctCorrect"=MathD3PctCorrect, "pointsPossible"=MathD3PntPoss, "subScore"=MathD3RawScore
  ) -> scoresMathD3

### MathD4 ###
df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=MathD4Label, "pctCorrect"=MathD4PctCorrect, "pointsPossible"=MathD4PntPoss, "subScore"=MathD4RawScore
  ) -> scoresMathD4

### MathD5 ###
df %>%
  select(StateID, Grade,  #identifer and grade
         "testLable"=MathD5Label, "pctCorrect"=MathD5PctCorrect, "pointsPossible"=MathD5PntPoss, "subScore"=MathD5RawScore
  ) -> scoresMathD5


##### CREATING TIDY DATA FRAMES#####
tidyReadingScores <- rbind(scoresKID, scoresCS, scoresIKI)%>%
  mutate("testDomain"="Reading")
rm(scoresKID, scoresCS, scoresIKI)

tidyLWScores <- rbind(scoresRPK, scoresPDW, scoresTTP, scoresCOSE, scoresVAU)%>%
  mutate("testDomain"="Language/Writing")
rm(scoresRPK, scoresPDW, scoresTTP, scoresCOSE, scoresVAU)

tidySciScores <- rbind(scoresLS, scoresPS, scoresES)%>%
  mutate("testDomain"="Science")
rm(scoresLS, scoresPS, scoresES)

tidyMathScores <- rbind(scoresMathD1, scoresMathD2, scoresMathD3, scoresMathD4, scoresMathD5)%>%
  mutate("testDomain"="Math")
rm(scoresMathD1, scoresMathD2, scoresMathD3, scoresMathD4, scoresMathD5)

tidySubTests <- rbind(tidyReadingScores,tidyLWScores, tidyMathScores, tidySciScores)


tidySubTests %>%
  group_by(testLable, Grade) %>%
  summarise(numberofpoints=mean(pointsPossible))%>%
  na.omit()-> questions


tidySubTests %>%
  group_by(testLable, Grade)%>%
  summarise(medianScore= median(pctCorrect)) %>%
  pivot_wider(names_from = testLable, values_from = medianScore) %>%
  arrange(Grade) %>%
  select(Grade,
         KID, CS, IKI,
         RPK, PDW, TTP, `COSE-KOL`, VAU,
         LS, PS, ES,
         OA, NBT, NF, MD, G, RP, NS, EE, SP, `F`, S, A, N)-> medianSubScores

tidySubTests %>%
  group_by(testLable, Grade)%>%
  summarise(meanScore= mean(pctCorrect)) %>%
  pivot_wider(names_from = testLable, values_from = meanScore) %>%
  arrange(Grade) %>%
  select(Grade,
         KID, CS, IKI,
         RPK, PDW, TTP, `COSE-KOL`, VAU,
         LS, PS, ES,
         OA, NBT, NF, MD, G, RP, NS, EE, SP, `F`, S, A, N)-> meanSubScores






# COSE-KOL,PDW,RPK,TTP,VAU  ##Langauge/writing
# A,F,G,MD,N,NBT,NS	,OA	,RP	,S,SP,
# CS,IKI,KID ##Reading
# ES, LS, PS	Science


t(testdf)->test2
scoresELA %>% ggplot(aes(x=ELAScaleScore))+
  geom_histogram(aes(fill=ELAAchLvl), binwidth = 4)+
  facet_wrap(vars(Grade))+
  theme_minimal()
