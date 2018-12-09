library(shiny)
library(dplyr)
library(readr)


drugs <- 
    read_csv("https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugs.csv") %>%
    arrange(name) %>%
    pull(name)

# Define UI for application that draws a histogram
shinyUI(fluidPage(# Application title
    titlePanel("Receptors"),
    fluidRow(
        column(3,
            selectInput(
                "select_1",
                label = h4("Drug"),
                choices = drugs,
                selected = 1
            ),
            selectInput(
                "select_2",
                label = h4("Drug"),
                choices = drugs,
                selected = 2
            ),
            selectInput(
                "select_3",
                label = h4("Drug"),
                choices = drugs,
                selected = 3
            )
        ),
        column(3,
               plotOutput(
                   "drugPlot", height = "900px", width = "1200px"
               ))
    )))