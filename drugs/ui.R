library(shiny)
library(dplyr)
library(readr)


drugs <- 
    read_csv("https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugs.csv") %>%
    arrange(name) %>%
    pull(name)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Receptors"),
    fluidRow(
        column(5,
               h4("Select a molecule"),
               selectInput("select", label = h3("Drug"), 
                           choices = drugs, 
                           selected = 1)
        ),
        column(4,
               h4("Molecule"),
               radioButtons("radio", label = h3("Display type"),
                            choices = list("Target" = 1, "Enzymes" = 2), 
                            selected = 1))
    ),
    hr(),
    plotOutput("drugPlot", height = "600px", width = "800px")
))