
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "ISASP Dashboard"),

    ## Sidebar with controls
    dashboardSidebar(
      sidebarMenu(
        menuItem("Upload", tabName = "upload", icon = icon("Upload")),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
      ),

      selectInput(
        "tableType",
        label = "Table Type",
        choices = c(
          "Number of Questions",
          "Mean Scores",
          "Median Scores"
        )
      ),

      h4("Filter By Demo"),
      selectizeInput("demos",
        label = "Race",
        choices = c(
          "American Indian or Alaskan"="AmericanIndianorAlaskan",
          "Asian"="Asian", "African American"="AfricanAmerican",
          "Hispanic"="HispanicLatino", "Hawaiian / Pacific Islander"="HawaiianPacificIslander",
          "white"="White"
        ), multiple = T

      ),

      selectizeInput("programs",
        label = "Group",
        choices = c(
          "Military Connected" = "MilitaryConnected", "Special Ed" = "SE",
          "504 Plan"="plan504","Free/Reduced Lunch" ="FRL",
          "GT" = "GT", "ELL" = "ELL", "T1L" =  "T1L",
          "T1M"= "T1M", "Homeless" = "Homeless"
        ), multiple=T
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "upload",
                fluidRow(
                  fileInput("file1", "Upload the ISASP Data File",
                            multiple = TRUE,
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".txt"
                            )
                  ),
                )

        ),
        tabItem(tabName = "dashboard",
          fluidRow(
            box(
              title = "Table",
              uiOutput("tableOutput")
            )
          )
        )
      )


    ),
    title = "ISASP Dashboard"
  )
)

#     # Application title
#     title = "ISASP Dashboard",
#
#     br(),br(),
#     fluidRow(
#         column(3,
#                h4("ISASP DASHBOARD"),
#                fileInput("file1", "Upload the ISASP Data File",
#                          multiple = TRUE,
#                          accept = c("text/csv",
#                                     "text/comma-separated-values,text/plain",
#                                     ".txt")
#                ),
#                selectInput(
#                    "tableType", label = "Table Type",
#                    choices = c("Number of Questions",
#                                "Mean Scores",
#                                "Median Scores")
#                )
#
#     ),
#     column(3, offest=1,
#            h4("Filter By Demo"),
#            checkboxGroupInput("demos", label = "Race",
#                               choiceNames =  c("American Indian or Alaskan", "Asian", "African American",
#                                                "Hispanic", "Hawaiian / Pacific Islander", "white"),
#                               choiceValues = c("AmericanIndianorAlaskan","Asian", "AfricanAmerican",
#                                                "HispanicLatino", "HawaiianPacificIslander", "White"))
#     ),
#     column(3,
#            checkboxGroupInput("programs", label = "Group",
#                               choiceNames =  c("Military Connected", "Special Ed", "504 Plan",
#                                                "Free/Reduced Lunch", "GT", "ELL", "T1L", "T1M" , "Homeless"),
#                               choiceValues = c("MilitaryConnected","SE", "plan504",
#                                                "FRL", "GT", "ELL", "T1L","T1M", "Homeless" ))
#            )
#
#
#
#     ),
#
#     column(3,
#            uiOutput("buildingFilter")
#            ),
#     verbatimTextOutput("demotext"), verbatimTextOutput("demotext2"),
#
#         column(12, align="center", uiOutput("tableOutput"))
#
#
#
#
#
#
#
# ))
