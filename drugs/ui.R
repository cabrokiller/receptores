#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Drugs"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
        # Copy the line below to make a set of radio buttons
        drugs <- read_csv("../data/drugbank_target_parse.csv") %>%
            arrange(Drug) %>%
            pull(Drug),
        selectInput("select", label = h3("Drug"), 
                    choices = drugs, 
                    selected = 1),
        radioButtons("radio", label = h3("Display type"),
                     choices = list("Target" = 1, "Enzymes" = 2), 
                     selected = 1),
        hr(),
        fluidRow(column(1, verbatimTextOutput("value")))
        ),
    
    mainPanel(
        plotOutput("drugPlot") 
    ))
))

