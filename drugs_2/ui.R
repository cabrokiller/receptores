library(shiny)
library(dplyr)
library(readr)

fams <- 
    read_csv("https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugs.csv") %>%
    distinct(fam) %>%
    pull(fam)

# Define UI for application that draws a histogram
shinyUI(fluidPage(# Application title
    titlePanel("Receptors"),
    fluidRow(
        column(3,
               checkboxGroupInput("checkGroup",
                                  h4("Select drug categories"),
                                  choices = as.list(fams),
                                  selected = fams),
               h3('select Drugs'),
               selectInput("select_1",
                           label = h4("Drug 1"),
                           choices = c("")),
               selectInput("select_2",
                           label = h4("Drug 2"),
                           choices = c("a","b"),
                           selected = "a"),
               selectInput("select_3",
                           label = h4("Drug 3"),
                           choices = c("a","b"),
                           selected = "a")
               ),
        
        column(6,
               plotOutput("drugPlot",
                          height = "800px",
                          width = "100%")))))
