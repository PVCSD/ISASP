
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)

shinyUI(fluidPage(

    # Application title
    title = "ISASP Dashboard",

    br(),br(),
    fluidRow(
        column(3,
               h4("ISASP DASHBOARD"),
               fileInput("file1", "Upload the ISASP Data File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".txt")
               ),
               selectInput(
                   "tableType", label = "Table Type",
                   choices = c("Number of Questions",
                               "Mean Scores",
                               "Median Scores")
               )

    ),
    column(3, offest=1,
           h4("Filter By Demo"),
           checkboxGroupInput("demos", label = "Race",
                              choiceNames =  c("American Indian or Alaskan", "Asian", "African American",
                                               "Hispanic", "Hawaiian / Pacific Islander", "white"),
                              choiceValues = c("AmericanIndianorAlaskan","Asian", "AfricanAmerican",
                                               "HispanicLatino", "HawaiianPacificIslander", "White"))
    ),
    column(3,
           checkboxGroupInput("programs", label = "Group",
                              choiceNames =  c("Military Connected", "Special Ed", "504 Plan",
                                               "Free/Reduced Lunch", "GT", "ELL", "T1L", "T1M" , "Homeless"),
                              choiceValues = c("MilitaryConnected","SE", "plan504",
                                               "FRL", "GT", "ELL", "T1L","T1M", "Homeless" ))
           )



    ),

    column(3,
           uiOutput("buildingFilter")
           ),
    verbatimTextOutput("demotext"), verbatimTextOutput("demotext2"),

        column(12, align="center", uiOutput("tableOutput"))







))
