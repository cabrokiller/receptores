library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    title = "dkwkjkjdkw",
    drugs <- read_csv("../data/drugbank_target_parse.csv") %>%
        arrange(Drug) %>%
        pull(Drug),
    
    fluidRow(
        column(5,
               h4("select drug"),
               selectInput("select", label = h3("Drug"), 
                           choices = drugs, 
                           selected = 1)
        ),
        column(4,
               h4("display"),
               radioButtons("radio", label = h3("Display type"),
                            choices = list("Target" = 1, "Enzymes" = 2), 
                            selected = 1))
    ),
    hr(),
    plotOutput("drugPlot", height = "600px", width = "800px")
))