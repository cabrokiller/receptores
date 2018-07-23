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
               #h4("Select a molecule"),
               selectInput("select", label = h4("Drug"), 
                           choices = drugs, 
                           selected = 1)
        ),
        column(4,
               #h5("Molecule"),
               radioButtons("radio", label = h4("Display type"),
                            choices = list("Target" = 1, "Enzymes" = 2), 
                            selected = 1, inline = T))
    ),
    hr(),
    plotOutput("drugPlot", height = "600px", width = "1200px")
))