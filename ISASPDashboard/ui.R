
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "ISASP Dashboard"),

    ## Sidebar
    dashboardSidebar(
      sidebarMenu(
        menuItem("Upload", tabName = "upload", icon = icon("upload")),
        menuItem("Building Report", tabName = "buildingReport", icon = icon("school") ),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Honors Science", tabName = "cutoffs", icon = icon("flask"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "upload",
          box(
            "upload",
            fileInput("file1", "Upload the ISASP Data File",
              multiple = TRUE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".txt"
              )
            ),

            fileInput("file2", "Upload the Percentiles",
              multiple = TRUE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".txt"
              )
            )
          )
        ),
        tabItem(
          tabName = "buildingReport",
          fluidPage(

            box("ELA",
              width = 4,
              plotOutput("schoolELA", height = "200px"),
              uiOutput("countELAAdvanced"),
              uiOutput("countELAProficent"),
              uiOutput("countELAANotPro")

            ),

            box("Math",
                width = 4,
                uiOutput("countMathAdvanced"),
                uiOutput("countMathProficent"),
                uiOutput("countMathNotPro")
            ),
            box("Science",
                width = 4,
                uiOutput("countSciAdvanced"),
                uiOutput("countSciProficent"),
                uiOutput("countSciNotPro")
            ),



          )

        ),
        tabItem(
          tabName = "dashboard",
          fluidPage(box("Controls",
                        width = 3,
                        selectInput(
                          "tableType",
                          label = "Table Type",
                          choices = c(
                            "Number of Questions",
                            "Mean Scores",
                            "Median Scores"
                          )
                        ),
                        uiOutput("buildingFilter"),
                        selectizeInput("gender",
                                       label = "Choose Gender",
                                       choices = c(
                                         "Female" = "F",
                                         "Male" = "M"
                                       ), multiple = T
                        ),

                        selectizeInput("demos",
                                       label = "Filter By Race",
                                       choices = c(
                                         "American Indian or Alaskan" = "AmericanIndianorAlaskan",
                                         "Asian" = "Asian", "African American" = "AfricanAmerican",
                                         "Hispanic" = "HispanicLatino", "Hawaiian / Pacific Islander" = "HawaiianPacificIslander",
                                         "white" = "White"
                                       ), multiple = T
                        ),

                        selectizeInput("programs",
                                       label = "Filter By Group",
                                       choices = c(
                                         "Military Connected" = "MilitaryConnected", "Special Ed" = "SE",
                                         "504 Plan" = "plan504", "Free/Reduced Lunch" = "FRL",
                                         "GT" = "GT", "ELL" = "ELL", "T1L" = "T1L",
                                         "T1M" = "T1M", "Homeless" = "Homeless"
                                       ), multiple = T
                        )
          ),


            tabBox(
              width = 9,
              title = "Sub Scores",
              tabPanel(
                id = "tabReading",
                title = "Reading",
                uiOutput("tableOutputReading")
              ),
              tabPanel(
                id = "tabLW",
                title = "Language/Writing",
                uiOutput("tableOutputLW")
              ),
              tabPanel(
                id = "tabScience",
                title = "Science",
                uiOutput("tableOutputScience")
              ),
              tabPanel(
                id = "tabMath",
                title = "Math",
                uiOutput("tableOutputMath")
              ),
              tabPanel(
                id = "tabDomainScores",
                title = "Domain",
                uiOutput("tableOutputDomain")
              )
            )
          )
        ),
        tabItem(
          tabName = "cutoffs",
          fluidPage(
            box(
              width = 3,
              title = "Controls",
              radioButtons("cuttoffGrade", "Grade", choices = c("7", "8")),
              numericInput("minELAPercentile", "Min ELA Percentile", value = "90"),
              numericInput("minMathPercentile", "Min Math Percentile", value = "90")
            ),
            box(
              width = 9,
              dataTableOutput("honorsScience")
            )
          )
        )
      )
    ),
    title = "ISASP Dashboard"
  )
)
