library(readxl)
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)
library(shinydashboard)

shinyServer(function(input, output) {


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

  #### Seperate the student Data for joining later ####
  StudentData <- reactive({
    df <- filedata()
    df %>%
      select(DistrictName, LastName, FirstName, Gender, StateID, DistrictID, Grade,
        AmericanIndianorAlaskan, Asian, AfricanAmerican, HispanicLatino, HawaiianPacificIslander,
        White, MilitaryConnected, SE,
        "plan504" = `504.0`, FRL, GT, ELL, T1L, T1M, Homeless
      ) -> studentDemo
    return(studentDemo)
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




    #### data out ####
    return(tidySubTests)
  })


  #### Number of Questions  By Grade ####


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
  NumQuestions <- reactive({
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
  NumQuestions <- reactive({
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


  ###### Number of Questions: ALL ######
  NumQuestions <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }
    TidyData() %>%
      group_by(testLable, Grade) %>%
      summarise(numberofpoints = mean(round(pointsPossible, 1))) %>%
      pivot_wider(names_from = testLable, values_from = numberofpoints) %>%
      arrange(Grade) %>%
      select(
        Grade,
        KID, CS, IKI,
        RPK, PDW, TTP, `COSE-KOL`, VAU,
        LS, PS, ES,
        OA, NBT, NF, MD, G, RP, NS, EE, SP, `F`, S, A, N
      ) -> questions
    return(questions)
  })


  #### Mean Score for subtests by grade ####
  ###### Mean Score: Reading ######
  ###### Mean Score: Language/Writing ######
  ###### Mean Score: Math ######
  ###### Mean Score: Science ######



  MeanSubTestScores <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }
    sdf <- StudentData()

    df <- FilteredData()

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
        "KID", "CS", "IKI",
        "RPK", "PDW", "TTP", "COSE-KOL", "VAU",
        "LS", "PS", "ES",
        "OA", "NBT", "NF", "MD", "G", "RP", "NS", "EE", "SP", "F", "S", "A", "N"
      ) -> meanSubScores

    return(meanSubScores)

  })

  #### Median Score for subtests by grade ####
  MedianSubTestScores <- reactive({
    if (fileReady() == F) {
      return(NULL)
    }
    if (filterDemo() == T & filterPrograms() == T) {
      sdf <- StudentData()

      selected_races <- input$demos
      selected_programs <- input$programs

      TidyData() %>%
        left_join(sdf) %>%
        group_by(testLable, Grade) %>%
        filter_at(selected_races, any_vars(. != 0)) %>%
        filter_at(selected_programs, any_vars(. != 0)) -> df

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
          "KID", "CS", "IKI",
          "RPK", "PDW", "TTP", "COSE-KOL", "VAU",
          "LS", "PS", "ES",
          "OA", "NBT", "NF", "MD", "G", "RP", "NS", "EE", "SP", "F", "S", "A", "N"
        ) -> medianSubScores2

      return(medianSubScores2)
    } else if (filterDemo() == T) {
      sdf <- StudentData()
      selected_races <- input$demos

      TidyData() %>%
        left_join(sdf) %>%
        group_by(testLable, Grade) %>%
        filter_at(selected_races, any_vars(. != 0)) %>%
        filter_at(selected_programs, any_vars(. != 0)) -> df

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
          "KID", "CS", "IKI",
          "RPK", "PDW", "TTP", "COSE-KOL", "VAU",
          "LS", "PS", "ES",
          "OA", "NBT", "NF", "MD", "G", "RP", "NS", "EE", "SP", "F", "S", "A", "N"
        ) -> medianSubScores2

      return(medianSubScores2)
    } else if (filterPrograms() == T) {
      sdf <- StudentData()
      selected_programs <- input$programs

      TidyData() %>%
        left_join(sdf) %>%
        group_by(testLable, Grade) %>%
        filter_at(selected_programs, any_vars(. != 0)) -> df

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
          "KID", "CS", "IKI",
          "RPK", "PDW", "TTP", "COSE-KOL", "VAU",
          "LS", "PS", "ES",
          "OA", "NBT", "NF", "MD", "G", "RP", "NS", "EE", "SP", "F", "S", "A", "N"
        ) -> medianSubScores2
      return(medianSubScores2)
    } else {
      TidyData() %>%
        group_by(testLable, Grade) %>%
        summarise(medianScore = median(pctCorrect)) %>%
        pivot_wider(names_from = testLable, values_from = medianScore) %>%
        arrange(Grade) %>%
        select(
          Grade,
          KID, CS, IKI,
          RPK, PDW, TTP, `COSE-KOL`, VAU,
          LS, PS, ES,
          OA, NBT, NF, MD, G, RP, NS, EE, SP, `F`, S, A, N
        ) -> medianSubScores
      return(medianSubScores)
    }
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

    TidyData() %>%
      left_join(sdf) %>%
      group_by(testLable, Grade)-> dfJoined

    return(dfJoined)
  })

  #### Filtered Data ####
  FilteredData <- reactive({


    selected_races <- input$demos
    selected_programs <- input$programs

    if (filterDemo() == T & filterPrograms() == T) {
      TidyDataWithDemos() %>%
        filter_at(selected_races, any_vars(. != 0)) %>%
        filter_at(selected_programs, any_vars(. != 0)) -> df
    }
    else if(filterDemo() == T){
      TidyDataWithDemos() %>%
        filter_at(selected_races, any_vars(. != 0)) -> df
    }
    else if(filterPrograms() == T){
      TidyDataWithDemos() %>%
        filter_at(selected_programs, any_vars(. != 0)) -> df
    }
    else{
      TidyDataWithDemos() -> df
    }

    return(df)

  })

  ##### UI OPTIONS #####

  # What table to show
  output$tableOutput <- renderUI({
    if (fileReady() == F) {
      return(NULL)
    }
    if (input$tableType == "Number of Questions") {
      output$aa <- renderDataTable(datatable(NumQuestions(), rownames = FALSE, options = list(dom = "t")) %>%
        formatRound(columns = c(2:24), digits = 0) %>%
        formatStyle(columns = c(1), fontWeight = "bold") %>%
        formatStyle(columns = c(1), border = "1px solid #ddd") %>%
        formatStyle(columns = c(2:5), border = "1px solid #ddd"))
      dataTableOutput("aa")
    }
    else if (input$tableType == "Mean Scores") {
      output$aa <- renderDataTable(datatable(MeanSubTestScores(),
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
      output$aa <- renderDataTable(datatable(MedianSubTestScores(),
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


  # filter by school
  output$buildingFilter <- renderUI({
    if (fileReady() == F) {

    }
    else {
      selectInput("buildingSelect",
        label = "Filter By Building",
        choices = c("All", SchoolList())
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
  filterDemo <- reactive({
    if (length(input$demos) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    return(FALSE)
  })

  # programs selected?
  filterPrograms <- reactive({
    if (length(input$programs) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    return(FALSE)
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
