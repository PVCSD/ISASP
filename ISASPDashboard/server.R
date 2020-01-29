library(readxl)
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)
library(shinydashboard)
library(readr)


shinyServer(function(input, output) {
  grades_possible <- function(df) {
    df %>%
      select(Grade) %>%
      distinct() -> values

    return(values)
  }


  ##### Data Operations #####

  ## read in the data
  filedata <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }

    df <- read_excel(infile$datapath)
    return(df)
  })


  ## read in the data
  percentiledf <- reactive({
    infile <- input$file2
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }

    df <- read_csv(infile$datapath)
    return(df)
  })

  #### Seperate the student Data for joining later ####
  StudentData <- reactive({
    df <- filedata()
    df %>%
      select(DistrictName, SchoolName, LastName, FirstName, Gender, StateID, DistrictID, Grade,
        AmericanIndianorAlaskan, Asian, AfricanAmerican, HispanicLatino, HawaiianPacificIslander,
        White, MilitaryConnected, SE,
        "plan504" = `504.0`, FRL, GT, ELL, T1L, T1M, Homeless
      ) -> studentDemo
    return(studentDemo)
  })

  # get Domain Scores
  DomainScores <- reactive({
    df <- filedata()
    df %>%
      select(
        StateID, Grade,
        ELAAchLvl, ELAScaleScore,
        ReadScaleScore, LWScaleScore,
        MathAchLvl, MathScaleScore,
        SciScaleScore
      ) -> studenDomainScores
    return(studenDomainScores)
  })


  # Make the data TIDY
  TidyData <- reactive({
    df <- filedata()

    ###### SUBTESTSCORES ########


    #### Reading ####

    ### KID ###
    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = KIDLabel, "pctCorrect" = KIDPctCorrect, "pointsPossible" = KIDPntPoss, "subScore" = KIDRawScore
      ) -> scoresKID
    ### CS ###
    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = CSLabel, "pctCorrect" = CSPctCorrect, "pointsPossible" = CSPntPoss, "subScore" = CSRawScore
      ) -> scoresCS

    ### IKI ###
    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = IKILabel, "pctCorrect" = IKIPctCorrect, "pointsPossible" = IKIPntPoss, "subScore" = IKIRawScore
      ) -> scoresIKI



    #### Langauge/Writing ####

    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = RPKLabel, "pctCorrect" = RPKPctCorrect, "pointsPossible" = RPKPntPoss, "subScore" = RPKRawScore
      ) -> scoresRPK
    ### PDW ###
    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = PDWLabel, "pctCorrect" = PDWPctCorrect, "pointsPossible" = PDWPntPoss, "subScore" = PDWRawScore
      ) -> scoresPDW

    ### TTP ###
    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = TTPLabel, "pctCorrect" = TTPPctCorrect, "pointsPossible" = TTPPntPoss, "subScore" = TTPRawScore
      ) -> scoresTTP

    ### COSE-KOL
    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = COSEKLPLabel, "pctCorrect" = COSEKLPPctCorrect, "pointsPossible" = COSEKLPPntPoss, "subScore" = COSEKLPRawScore
      ) -> scoresCOSE


    ### VAU ###
    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = VAULabel, "pctCorrect" = VAUPctCorrect, "pointsPossible" = VAUPntPoss, "subScore" = VAURawScore
      ) -> scoresVAU




    #### Science ####

    ### LS ###
    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = LSLabel, "pctCorrect" = LSPctCorrect, "pointsPossible" = LSPntPoss, "subScore" = LSRawScore
      ) -> scoresLS
    ### PS ###
    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = PSLabel, "pctCorrect" = PSPctCorrect, "pointsPossible" = PSPntPoss, "subScore" = PSRawScore
      ) -> scoresPS

    ### ES ###
    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = ESLabel, "pctCorrect" = ESPctCorrect, "pointsPossible" = ESPntPoss, "subScore" = ESRawScore
      ) -> scoresES


    #### Math ####


    ### MathD1 ###
    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = MathD1Label, "pctCorrect" = MathD1PctCorrect, "pointsPossible" = MathD1PntPoss, "subScore" = MathD1RawScore
      ) -> scoresMathD1

    ### MathD2 ###
    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = MathD2Label, "pctCorrect" = MathD2PctCorrect, "pointsPossible" = MathD2PntPoss, "subScore" = MathD2RawScore
      ) -> scoresMathD2

    ### MathD3 ###
    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = MathD3Label, "pctCorrect" = MathD3PctCorrect, "pointsPossible" = MathD3PntPoss, "subScore" = MathD3RawScore
      ) -> scoresMathD3

    ### MathD4 ###
    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = MathD4Label, "pctCorrect" = MathD4PctCorrect, "pointsPossible" = MathD4PntPoss, "subScore" = MathD4RawScore
      ) -> scoresMathD4

    ### MathD5 ###
    df %>%
      select(StateID, Grade, # identifer and grade
        "testLable" = MathD5Label, "pctCorrect" = MathD5PctCorrect, "pointsPossible" = MathD5PntPoss, "subScore" = MathD5RawScore
      ) -> scoresMathD5


    ##### CREATING TIDY DATA FRAMES#####

    #### Join Reading ####
    tidyReadingScores <- rbind(scoresKID, scoresCS, scoresIKI) %>%
      mutate("testDomain" = "Reading")
    rm(scoresKID, scoresCS, scoresIKI)

    #### Join Langague & Writing ####
    tidyLWScores <- rbind(scoresRPK, scoresPDW, scoresTTP, scoresCOSE, scoresVAU) %>%
      mutate("testDomain" = "Language/Writing")
    rm(scoresRPK, scoresPDW, scoresTTP, scoresCOSE, scoresVAU)

    #### Join Science ####
    tidySciScores <- rbind(scoresLS, scoresPS, scoresES) %>%
      mutate("testDomain" = "Science")
    rm(scoresLS, scoresPS, scoresES)

    #### Join Math ####
    tidyMathScores <- rbind(scoresMathD1, scoresMathD2, scoresMathD3, scoresMathD4, scoresMathD5) %>%
      mutate("testDomain" = "Math")
    rm(scoresMathD1, scoresMathD2, scoresMathD3, scoresMathD4, scoresMathD5)


    #### Bind them All together ####
    tidySubTests <- rbind(tidyReadingScores, tidyLWScores, tidyMathScores, tidySciScores)

    ### data out ###
    return(tidySubTests)
  })



  #### Domain SCORES ####

  output$countELAProficent <- renderUI({
    dfDomainScores <- DomainScores()


    FilteredData() %>%
      left_join(dfDomainScores) %>%
      filter(ELAAchLvl %in% c("P")) %>%
      tally() -> value1

    infoBox("Number Proficient", value = value1, color = "yellow", width = 12)
  })

  output$countELAAdvanced <- renderUI({
    dfDomainScores <- DomainScores()


    FilteredData() %>%
      left_join(dfDomainScores) %>%
      filter(ELAAchLvl %in% c("A")) %>%
      tally() -> value1

    infoBox("Number Advanced", value = value1, color = "olive", width = 12)
  })

  output$countELAANotPro <- renderUI({
    dfDomainScores <- DomainScores()


    FilteredData() %>%
      left_join(dfDomainScores) %>%
      filter(ELAAchLvl %in% c("N")) %>%
      tally() -> value1

    infoBox("Number Advanced", value = value1, color = "red", width = 12)
  })



  ##### Data Operations #####

  ###### Mean Domain Scores ######

  ###### Mean Domain Scores ######
  MeanDomain <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }

    df <- FilteredData()
    scores <- DomainScores()

    # if no students return Nothing
    if (length(df) < 1) {
      return(NULL)
    }
    df %>%
      left_join(scores)%>%
      group_by(Grade)%>%
      summarise("ELA" = mean(ELAScaleScore, na.rm = T),
                "Reading"=mean(ReadScaleScore, na.rm = T),
                "language/Writing"=mean(LWScaleScore, na.rm = T),
                "Science"=mean(SciScaleScore, na.rm = T),
                "Math"=mean(MathScaleScore, na.rm = T)) %>%
      arrange(Grade) %>%
      select(
        "Grade",
        one_of("ELA", "Reading", "language/Writing", "Science", "Math")
      ) -> meanDomainScores

    return(meanDomainScores)
  })
  ###### Median Domain Scores ######
  MedianDomain <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }

    df <- FilteredData()
    scores <- DomainScores()

    # if no students return Nothing
    if (length(df) < 1) {
      return(NULL)
    }
    df %>%
      left_join(scores)%>%
      group_by(Grade)%>%
      summarise("ELA" = median(ELAScaleScore, na.rm = T),
                "Reading"=median(ReadScaleScore, na.rm = T),
                "language/Writing"=mean(LWScaleScore, na.rm = T),
                "Science"=median(SciScaleScore, na.rm = T),
                "Math"=median(MathScaleScore, na.rm = T)) %>%
      arrange(Grade) %>%
      select(
        "Grade",
        one_of("ELA", "Reading", "language/Writing", "Science", "Math")
      ) -> medianDomainScores

    return(medianDomainScores)
  })

  ###### Filter Domain Scores ######
  FilteredDomainScores <- reactive({

  })

  ##### UI Set Up #####

  output$tableOutputDomain <- renderUI({
    if (fileReady() == F) {
      return(NULL)
    }
    if (input$tableType == "Number of Questions") {
      output$domain <- renderDataTable(datatable(MeanDomain(), rownames = FALSE, options = list(dom = "t")) %>%
        formatRound(columns = c(2:7), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(1), backgroundColor = "#656565", color = "#A5A5A5") %>%
        formatStyle(columns = c(2:7), border = "1px solid #ddd"))
      dataTableOutput("domain")
    }
    else if (input$tableType == "Mean Scores") {
      output$domain <- renderDataTable(datatable(MeanDomain(),
        rownames = FALSE,
        options = list(
          dom = "t"
        )
      ) %>%
        formatRound(columns = c(2:7), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(1), backgroundColor = "#656565", color = "#A5A5A5") %>%
        formatStyle(columns = c(2:7), border = "1px solid #ddd"))
      dataTableOutput("domain")
    }
    else if (input$tableType == "Median Scores") {
      output$domain <- renderDataTable(datatable(MedianDomain(),
        rownames = FALSE,
        options = list(
          dom = "t"
        )
      ) %>%
        formatRound(columns = c(2:7), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(1), backgroundColor = "#656565", color = "#A5A5A5") %>%
        formatStyle(columns = c(2:7), border = "1px solid #ddd"))
      dataTableOutput("domain")
    }
    else {
      return(NULL)
    }
  })


  #### Subtests ####


  ##### Subtests with Student Data #####
  filteredSubScores <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }
    sdf <- FilteredData()
    subScores <- TidyData()

    sdf %>%
      left_join(subScores) %>%
      group_by(testLable, Grade) -> dfJoined

    return(dfJoined)
  })

  ##### Number of Questions  By Grade #####

  ###### Number of Questions: Reading ######
  NumQuestionsReading <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }
    TidyData() %>%
      group_by(testLable, Grade) %>%
      summarise(numberofpoints = mean(round(pointsPossible, 1))) %>%
      na.omit() %>%
      pivot_wider(names_from = testLable, values_from = numberofpoints) %>%
      arrange(Grade) %>%
      select(
        Grade,
        KID, CS, IKI
      ) -> questions
    return(questions)
  })
  ###### Number of Questions: Langauge/Writing ######
  NumQuestionsLW <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }
    TidyData() %>%
      group_by(testLable, Grade) %>%
      summarise(numberofpoints = mean(round(pointsPossible, 1))) %>%
      na.omit() %>%
      pivot_wider(names_from = testLable, values_from = numberofpoints) %>%
      arrange(Grade) %>%
      select(
        Grade,
        RPK, PDW, TTP, `COSE-KOL`, VAU
      ) -> questions
    return(questions)
  })
  ###### Number of Questions: Math ######
  NumQuestionsMath <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }
    TidyData() %>%
      group_by(testLable, Grade) %>%
      summarise(numberofpoints = mean(round(pointsPossible, 1))) %>%
      na.omit() %>%
      pivot_wider(names_from = testLable, values_from = numberofpoints) %>%
      arrange(Grade) %>%
      select(
        Grade,
        OA, NBT, NF, MD, G, RP, NS, EE, SP, `F`, S, A, N
      ) -> questions
    return(questions)
  })
  ###### Number of Questions: Science ######
  NumQuestionsScience <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }
    TidyData() %>%
      group_by(testLable, Grade) %>%
      summarise(numberofpoints = mean(round(pointsPossible, 1))) %>%
      na.omit() %>%
      pivot_wider(names_from = testLable, values_from = numberofpoints) %>%
      arrange(Grade) %>%
      select(
        Grade,
        LS, PS, ES
      ) -> questions
    return(questions)
  })

  #### Mean Score for subtests by grade ####

  ###### Mean Score: Reading ######
  MeanSubTestScoresReading <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }

    df <- filteredSubScores()

    df %>%
      summarise(meanScore = mean(pctCorrect)) %>%
      pivot_wider(names_from = testLable, values_from = meanScore) %>%
      arrange(Grade) %>%
      select(
        "Grade",
        one_of("KID", "CS", "IKI")
      ) -> meanSubScores

    return(meanSubScores)
  })
  ###### Mean Score: Language/Writing ######
  MeanSubTestScoresLW <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }

    df <- filteredSubScores()

    # if no students return Nothing
    if (length(df) < 1) {
      return(NULL)
    }

    df %>%
      summarise(meanScore = mean(pctCorrect)) %>%
      pivot_wider(names_from = testLable, values_from = meanScore) %>%
      arrange(Grade) %>%
      select(
        "Grade",
        one_of("RPK", "PDW", "TTP", "COSE-KOL", "VAU")
      ) -> meanSubScores

    return(meanSubScores)
  })
  ###### Mean Score: Math ######
  MeanSubTestScoresMath <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }

    df <- filteredSubScores()

    # if no students return Nothing
    if (length(df) < 1) {
      return(NULL)
    }
    df %>%
      summarise(meanScore = mean(pctCorrect)) %>%
      pivot_wider(names_from = testLable, values_from = meanScore) %>%
      arrange(Grade) %>%
      select(
        "Grade",
        one_of("OA", "NBT", "NF", "MD", "G", "RP", "NS", "EE", "SP", "F", "S", "A", "N")
      ) -> meanSubScores

    return(meanSubScores)
  })
  ###### Mean Score: Science ######
  MeanSubTestScoresScience <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }

    df <- filteredSubScores()

    # if no students return Nothing
    if (length(df) < 1) {
      return(NULL)
    }
    df %>%
      summarise(meanScore = mean(pctCorrect)) %>%
      pivot_wider(names_from = testLable, values_from = meanScore) %>%
      arrange(Grade) %>%
      select(
        "Grade",
        one_of("LS", "PS", "ES")
      ) -> meanSubScores

    return(meanSubScores)
  })


  #### Median Score for subtests by grade ####

  ###### Median Score: Reading ######
  MedianSubTestScoresReading <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }

    df <- filteredSubScores()

    df %>%
      summarise(medianScore = median(pctCorrect)) %>%
      pivot_wider(names_from = testLable, values_from = medianScore) %>%
      arrange(Grade) %>%
      select(
        "Grade",
        one_of("KID", "CS", "IKI")
      ) -> medianSubScores

    return(medianSubScores)
  })
  ###### median Score: Language/Writing ######
  MedianSubTestScoresLW <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }

    df <- filteredSubScores()

    # if no students return Nothing
    if (length(df) < 1) {
      return(NULL)
    }

    df %>%
      summarise(medianScore = median(pctCorrect)) %>%
      pivot_wider(names_from = testLable, values_from = medianScore) %>%
      arrange(Grade) %>%
      select(
        "Grade",
        one_of("RPK", "PDW", "TTP", "COSE-KOL", "VAU")
      ) -> medianSubScores

    return(medianSubScores)
  })
  ###### median Score: Math ######
  MedianSubTestScoresMath <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }

    df <- filteredSubScores()

    # if no students return Nothing
    if (length(df) < 1) {
      return(NULL)
    }
    df %>%
      summarise(medianScore = median(pctCorrect)) %>%
      pivot_wider(names_from = testLable, values_from = medianScore) %>%
      arrange(Grade) %>%
      select(
        "Grade",
        one_of("OA", "NBT", "NF", "MD", "G", "RP", "NS", "EE", "SP", "F", "S", "A", "N")
      ) -> medianSubScores

    return(medianSubScores)
  })
  ###### median Score: Science ######
  MedianSubTestScoresScience <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }

    df <- filteredSubScores()

    # if no students return Nothing
    if (length(df) < 1) {
      return(NULL)
    }
    df %>%
      summarise(medianScore = median(pctCorrect)) %>%
      pivot_wider(names_from = testLable, values_from = medianScore) %>%
      arrange(Grade) %>%
      select(
        "Grade",
        one_of("LS", "PS", "ES")
      ) -> medianSubScores

    return(medianSubScores)
  })





  ######### Reporting / Cuttofs?
  PotentialHonorsSci <- reactive({
    StudentData() %>%
      left_join(percentiledf()) %>%
      filter(Grade == input$cuttoffGrade) %>%
      filter(MathPercentile >= input$minMathPercentile |
        ELAPercentile >= input$minELAPercentile) %>%
      select(StateID, FirstName, LastName, MathScaleScore, MathPercentile, ELAScaleScore, ELAPercentile)
  })

  output$honorsScience <- renderDataTable({
    datatable(PotentialHonorsSci())
  })


  #### Get list of schools ####
  SchoolList <- reactive({
    filedata() %>%
      select(SchoolName) %>%
      distinct() -> list_of_schools

    return(list_of_schools)
  })


  #### Join Tidy Data with Student data ####
  TidyDataWithDemos <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }
    sdf <- StudentData()
    subScores <- TidyData()

    sdf %>%
      left_join(subScores) %>%
      group_by(testLable, Grade) -> dfJoined

    return(dfJoined)
  })

  #### Filtered Data ####
  FilteredData <- reactive({
    df <- ProgramsFilter()

    return(df)
  })

  ### Filter By Building
  BuildingFilter <- reactive({
    if (CheckFilterBuildings() == T) {
      selected_buildings <- input$buildingSelect
      df <- StudentData() %>%
        filter(SchoolName %in% selected_buildings)
    }
    else {
      df <- StudentData()
    }

    return(df)
  })

  ### Filter By Gender
  GenderFilter <- reactive({
    if (CheckFilterGender() == T) {
      selected_gender <- input$gender
      df <- BuildingFilter() %>%
        filter(Gender %in% selected_gender)
    }
    else {
      df <- BuildingFilter()
    }

    return(df)
  })

  DemoFilter <- reactive({
    if (CheckFilterDemos() == T) {
      selected_races <- input$demos
      df <- GenderFilter() %>%
        filter_at(selected_races, any_vars(. != 0))
    }
    else {
      df <- GenderFilter()
    }

    return(df)
  })

  ProgramsFilter <- reactive({
    if (CheckFilterPrograms() == T) {
      selected_programs <- input$programs
      df <- DemoFilter() %>%
        filter_at(selected_programs, any_vars(. != 0))
    }
    else {
      df <- DemoFilter()
    }

    return(df)
  })



  ##### UI OPTIONS #####

  # What table to show

  ## Reading
  output$tableOutputReading <- renderUI({
    if (fileReady() == F) {
      return(NULL)
    }
    if (input$tableType == "Number of Questions") {
      output$reading <- renderDataTable(datatable(NumQuestionsReading(), rownames = FALSE, options = list(dom = "t")) %>%
        formatRound(columns = c(2:4), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(1), border = "1px solid #ddd") %>%
        formatStyle(columns = c(2:5), border = "1px solid #ddd"))
      dataTableOutput("reading")
    }
    else if (input$tableType == "Mean Scores") {
      output$reading <- renderDataTable(datatable(MeanSubTestScoresReading(),
        rownames = FALSE,
        options = list(
          dom = "t"
        )
      ) %>%
        formatRound(columns = c(2:4), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(2:4), backgroundColor = styleInterval(
          cuts = column_cond_format(col_max = 100, col_min = 0)$brks,
          values = column_cond_format(col_max = 100, col_min = 0)$clrs
        )) %>%
        formatString(columns = c(2:4), suffix = "%") %>%
        formatStyle(columns = c(1), backgroundColor = "#656565", color = "#A5A5A5"))
      dataTableOutput("reading")
    }
    else if (input$tableType == "Median Scores") {
      output$reading <- renderDataTable(datatable(MedianSubTestScoresReading(),
        rownames = FALSE,
        options = list(
          dom = "t"
        )
      ) %>%
        formatRound(columns = c(2:4), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(2:4), backgroundColor = styleInterval(
          cuts = column_cond_format(col_max = 100, col_min = 0)$brks,
          values = column_cond_format(col_max = 100, col_min = 0)$clrs
        )) %>%
        formatString(columns = c(2:4), suffix = "%") %>%
        formatStyle(columns = c(1), backgroundColor = "#656565", color = "#A5A5A5"))
      dataTableOutput("reading")
    }
    else {
      return(NULL)
    }
  })

  ## Language/Writing
  output$tableOutputLW <- renderUI({
    if (fileReady() == F) {
      return(NULL)
    }
    if (input$tableType == "Number of Questions") {
      output$LW <- renderDataTable(datatable(NumQuestionsLW(), rownames = FALSE, options = list(dom = "t")) %>%
        formatRound(columns = c(2:10), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(1), border = "1px solid #ddd") %>%
        formatStyle(columns = c(2:10), border = "1px solid #ddd"))
      dataTableOutput("LW")
    }
    else if (input$tableType == "Mean Scores") {
      output$LW <- renderDataTable(datatable(MeanSubTestScoresLW(),
        rownames = FALSE,
        options = list(
          dom = "t"
        )
      ) %>%
        formatRound(columns = c(2:10), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(2:10), backgroundColor = styleInterval(
          cuts = column_cond_format(col_max = 100, col_min = 0)$brks,
          values = column_cond_format(col_max = 100, col_min = 0)$clrs
        )) %>%
        formatString(columns = c(2:10), suffix = "%") %>%
        formatStyle(columns = c(1), backgroundColor = "#656565", color = "#A5A5A5"))
      dataTableOutput("LW")
    }
    else if (input$tableType == "Median Scores") {
      output$LW <- renderDataTable(datatable(MedianSubTestScoresLW(),
        rownames = FALSE,
        options = list(
          dom = "t"
        )
      ) %>%
        formatRound(columns = c(2:10), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(2:10), backgroundColor = styleInterval(
          cuts = column_cond_format(col_max = 100, col_min = 0)$brks,
          values = column_cond_format(col_max = 100, col_min = 0)$clrs
        )) %>%
        formatString(columns = c(2:10), suffix = "%") %>%
        formatStyle(columns = c(1), backgroundColor = "#656565", color = "#A5A5A5"))
      dataTableOutput("LW")
    }
    else {
      return(NULL)
    }
  })

  ## Math
  output$tableOutputMath <- renderUI({
    if (fileReady() == F) {
      return(NULL)
    }
    if (input$tableType == "Number of Questions") {
      output$Math <- renderDataTable(datatable(NumQuestionsMath(), rownames = FALSE, options = list(dom = "t")) %>%
        formatRound(columns = c(2:25), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(1), border = "1px solid #ddd") %>%
        formatStyle(columns = c(2:25), border = "1px solid #ddd"))
      dataTableOutput("Math")
    }
    else if (input$tableType == "Mean Scores") {
      output$Math <- renderDataTable(datatable(MeanSubTestScoresMath(),
        rownames = FALSE,
        options = list(
          dom = "t"
        )
      ) %>%
        formatRound(columns = c(2:25), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(2:25), backgroundColor = styleInterval(
          cuts = column_cond_format(col_max = 100, col_min = 0)$brks,
          values = column_cond_format(col_max = 100, col_min = 0)$clrs
        )) %>%
        formatString(columns = c(2:25), suffix = "%") %>%
        formatStyle(columns = c(1), backgroundColor = "#656565", color = "#A5A5A5"))
      dataTableOutput("Math")
    }
    else if (input$tableType == "Median Scores") {
      output$Math <- renderDataTable(datatable(MedianSubTestScoresMath(),
        rownames = FALSE,
        options = list(
          dom = "t"
        )
      ) %>%
        formatRound(columns = c(2:25), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(2:25), backgroundColor = styleInterval(
          cuts = column_cond_format(col_max = 100, col_min = 0)$brks,
          values = column_cond_format(col_max = 100, col_min = 0)$clrs
        )) %>%
        formatString(columns = c(2:25), suffix = "%") %>%
        formatStyle(columns = c(1), backgroundColor = "#656565", color = "#A5A5A5"))
      dataTableOutput("Math")
    }
    else {
      return(NULL)
    }
  })

  ## Science
  output$tableOutputScience <- renderUI({
    if (fileReady() == F) {
      return(NULL)
    }
    if (input$tableType == "Number of Questions") {
      output$Science <- renderDataTable(datatable(NumQuestionsScience(), rownames = FALSE, options = list(dom = "t")) %>%
        formatRound(columns = c(2:25), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(1), border = "1px solid #ddd") %>%
        formatStyle(columns = c(2:25), border = "1px solid #ddd"))
      dataTableOutput("Science")
    }
    else if (input$tableType == "Mean Scores") {
      output$Science <- renderDataTable(datatable(MeanSubTestScoresScience(),
        rownames = FALSE,
        options = list(
          dom = "t"
        )
      ) %>%
        formatRound(columns = c(2:25), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(2:25), backgroundColor = styleInterval(
          cuts = column_cond_format(col_max = 100, col_min = 0)$brks,
          values = column_cond_format(col_max = 100, col_min = 0)$clrs
        )) %>%
        formatString(columns = c(2:25), suffix = "%") %>%
        formatStyle(columns = c(1), backgroundColor = "#656565", color = "#A5A5A5"))
      dataTableOutput("Science")
    }
    else if (input$tableType == "Median Scores") {
      output$Science <- renderDataTable(datatable(MedianSubTestScoresScience(),
        rownames = FALSE,
        options = list(
          dom = "t"
        )
      ) %>%
        formatRound(columns = c(2:25), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(2:25), backgroundColor = styleInterval(
          cuts = column_cond_format(col_max = 100, col_min = 0)$brks,
          values = column_cond_format(col_max = 100, col_min = 0)$clrs
        )) %>%
        formatString(columns = c(2:25), suffix = "%") %>%
        formatStyle(columns = c(1), backgroundColor = "#656565", color = "#A5A5A5"))
      dataTableOutput("Science")
    }
    else {
      return(NULL)
    }
  })

  output$tableOutput <- renderUI({
    if (fileReady() == F) {
      return(NULL)
    }
    if (input$tableType == "Number of Questions") {
      output$aa <- renderDataTable(datatable(NumQuestionsReading(), rownames = FALSE, options = list(dom = "t")) %>%
        formatRound(columns = c(2:24), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(1), border = "1px solid #ddd") %>%
        formatStyle(columns = c(2:5), border = "1px solid #ddd"))
      dataTableOutput("aa")
    }
    else if (input$tableType == "Mean Scores") {
      output$aa <- renderDataTable(datatable(MeanSubTestScoresReading(),
        rownames = FALSE,
        options = list(
          dom = "t",
          columnDefs = list(list(className = "dt-center", targets = 0:4))
        )
      ) %>%
        formatRound(columns = c(2:25), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(2:25), backgroundColor = styleInterval(
          cuts = column_cond_format(col_max = 100, col_min = 0)$brks,
          values = column_cond_format(col_max = 100, col_min = 0)$clrs
        )) %>%
        formatString(columns = c(2:25), suffix = "%") %>%
        formatStyle(columns = c(1), backgroundColor = "#656565", color = "#A5A5A5"))
      dataTableOutput("aa")
    }
    else if (input$tableType == "Median Scores") {
      output$aa <- renderDataTable(datatable(MedianSubTestScoresReading(),
        rownames = FALSE,
        options = list(
          dom = "t",
          columnDefs = list(list(className = "dt-center", targets = 0:4))
        )
      ) %>%
        formatRound(columns = c(2:25), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(2:25), backgroundColor = styleInterval(
          cuts = column_cond_format(col_max = 100, col_min = 0)$brks,
          values = column_cond_format(col_max = 100, col_min = 0)$clrs
        )) %>%
        formatString(columns = c(2:25), suffix = "%") %>%
        formatStyle(columns = c(1), backgroundColor = "#656565", color = "#A5A5A5"))
      dataTableOutput("aa")
    }
    else {
      return(NULL)
    }
  })


  output$demo <- renderTable({
    FilteredData() -> df

    df %>%
      summarise(meanScore = mean(pctCorrect)) %>%
      pivot_wider(names_from = testLable, values_from = meanScore) %>%
      arrange(Grade) %>%
      select(
        "Grade",
        "KID", "CS", "IKI"
      ) -> test

    return(test)
  })

  # filter by school
  output$buildingFilter <- renderUI({
    if (fileReady() == F) {

    }
    else {
      selectizeInput("buildingSelect",
        label = "Filter By Building",
        choices = c(SchoolList()),
        multiple = T
      )
    }
  })


  # conditional formatting
  column_cond_format <- function(col_max, col_min, pal = c("#ea4335", "white", "#34a853")) {
    stop_color_max <- 100 ## set as max(column)
    stop_color_min <- 0
    brks <- quantile(c(0, stop_color_max), probs = seq(.05, .95, .05), na.rm = TRUE) ## set max/min
    myPal <- shades::gradient(pal, 18, space = "Lab")
    clrs <- c(myPal[1], myPal, myPal[length(myPal)])

    return(list(brks = brks, clrs = clrs))
  }


  ##### Sanity checks #####

  # check if the file has been read in.
  fileReady <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(FALSE)
    }
    return(TRUE)
  })


  # check if a demographic has been selected
  CheckFilterDemos <- reactive({
    if (length(input$demos) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  # programs selected?
  CheckFilterPrograms <- reactive({
    if (length(input$programs) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  CheckFilterGender <- reactive({
    if (length(input$gender) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  CheckFilterBuildings <- reactive({
    if (length(input$buildingSelect) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })


  # debugging tools
  # output$demotext <- renderPrint({
  #     return(SelectedRaces())
  #     })
  #
  # output$demotext2 <- renderPrint({
  #     return(length(input$demos))
  # })
})
