library(shiny)
library(dplyr)
library(readr)

drugs <- 
    read_csv("https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugs.csv") %>%
    group_by(fam) %>%
    arrange(fam, name)

# Define UI for application that draws a histogram
shinyUI(fluidPage(# Application title
    titlePanel("Perfiles farmacodinámicos"),
    fluidRow(
        h5("Aplicación para visualizar de manera simple el perfil receptorial
          de hasta 3 fármacos. Es posible seleccionar la familia de fármacos a desplegar en las listas"),
        column(3,
               h3('Selección de fármacos'),
               checkboxGroupInput("checkGroup",
                                  h4("Familia de fármacos"),
                                  choices = as.list(drugs$name),
                                  selected = NULL),
                ),
        column(6,
               plotOutput("drugPlot",
                          height = "700px",
                          width = "900px")))))
