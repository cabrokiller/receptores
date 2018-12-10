library(shiny)
library(dplyr)
library(readr)



# Define UI for application that draws a histogram
shinyUI(fluidPage(# Application title
    titlePanel("Receptors"),
    fluidRow(
        column(3,
               checkboxGroupInput("checkGroup",
                                  h3("Checkbox group"),
                                  choices = list(
                                      "Antipsychotic" = "Antipsychotic",
                                      "Antidepressant" = "Antidepressant",
                                      "Other" = "Other"),
                                  selected = c("Other")),
               selectInput("select_1",
                           label = h4("Drug"),
                           choices = c("a","b"),
                           selected = "a")),
        
        column(6,
               plotOutput("drugPlot",
                          height = "900px",
                          width = "1200px")))))