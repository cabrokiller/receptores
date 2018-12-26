library(shiny)
library(dplyr)
library(readr)
library(plotly)

drugs <-
    read_csv(
        "https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugs.csv"
    )


get_fam <- function(df, family) {
    df %>%
        filter(fam %in% family) %>%
        arrange(name)
}

ad <- get_fam(drugs, c("Antidepressant", "Mood stabilizer"))
ap <- get_fam(drugs, "Antipsychotic")
ot <-
    get_fam(drugs, c("Other", "Stimulant", "Opioid", "Depressants"))



# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = "bootstrap.css",
    titlePanel("Perfiles farmacodinámicos"),
    h5(
        "Aplicación para visualizar de manera simple el perfil receptorial
        de hasta 3 fármacos. Es posible seleccionar la familia de fármacos a desplegar en las listas"
    ),
    fluidRow(column(
        2,
        selectInput(
            'drugs',
            "select drugs",
            choices = as.list(drugs$name),
            selected = c("Aripiprazole", "Olanzapine", "Sertindole"),
            multiple = T
            
        ),
        textOutput("click")
    ),
    column(10,
           tabsetPanel(
               tabPanel(
                   "Targets",
                   plotlyOutput("plot_target",
                                height = '900px',
                                width = "100%")
               ),
               tabPanel(
                   "Enzymes",
                   plotlyOutput("plot_enzyme",
                                height = '900px',
                                width = "100%")
               )
           )))
    ))
