library(shiny)
library(dplyr)
library(readr)

fams <- 
    read_csv("https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugs.csv") %>%
    distinct(fam) %>%
    pull(fam)

# Define UI for application that draws a histogram
shinyUI(fluidPage(# Application title
    titlePanel("Perfiles farmacodinámicos"),
    fluidRow(
        h5("Aplicación para visualizar de manera simple el perfil receptorial
          de hasta 3 fármacos. Es posible seleccionar la familia de fármacos a desplegar en las listas"),
        column(3,
               checkboxGroupInput("checkGroup",
                                  h4("Familia de fármacos"),
                                  choices = as.list(fams),
                                  selected = fams),
               h3('Selección de fármacos'),
               selectInput("select_1",
                           label = h4("Fármaco 1"),
                           choices = c("")),
               selectInput("select_2",
                           label = h4("Fármaco 2"),
                           choices = c("a","b"),
                           selected = "a"),
               selectInput("select_3",
                           label = h4("Fármaco 3"),
                           choices = c("a","b"),
                           selected = "a")
               ),
        
        column(6,
               plotOutput("drugPlot",
                          height = "700px",
                          width = "900px")))))
