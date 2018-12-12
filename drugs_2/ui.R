library(shiny)
library(dplyr)
library(readr)

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
        tabsetPanel(
            tabPanel(
                "Antipsicóticos",
                checkboxGroupInput(
                    "drugs_2",
                    NULL,
                    choices = as.list(ap$name),
                    selected = c("Aripiprazole", "Olanzapine", "Sertindole")
                )
            ),
            tabPanel(
                "Antidepresivos/eutimizantes",
                checkboxGroupInput("drugs_1",
                                   NULL,
                                   choices = as.list(ad$name))
            ),
            tabPanel(
                "Otros",
                checkboxGroupInput("drugs_3",
                                   NULL,
                                   choices = as.list(ot$name))
            )
        )
    ),
    column(
        10,
        plotOutput("drugPlot",
                   height = '900px',
                   width = "100%")
    ))
))
