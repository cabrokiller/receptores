library(shiny)
library(tidyverse)
library(plotly)
library(viridisLite)
library(shinyWidgets)


# source("src/preproc.R")

clean_data <- 
    read_csv("https://raw.githubusercontent.com/cabrokiller/receptores/master/data/clean.csv")

my_family <-
    c("DRD", "HTR")


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
ot <- get_fam(drugs, c("Other", "Stimulant", "Opioid", "Depressants"))



ui <- fluidPage(
    #theme = "bootstrap.css",
    # titlePanel("Perfiles farmacodinámicos"),
    # h5(
    #     "Aplicación para visualizar de manera simple el perfil receptorial
    #     de hasta 3 fármacos. Es posible seleccionar la familia de fármacos a desplegar en las listas"
    # ),
    fluidRow(
        column(
            2,
            
            "Antipsicóticos",
            pickerInput(
                inputId = "drugs_2",
                label = "Select/deselect all + format selected",
                choices = as.list(ap$name),
                options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 1"
                ),
                multiple = TRUE
            ),
            "Antidepresivos/eutimizantes",
            pickerInput(
                inputId = "drugs_1",
                label = "Select/deselect all + format selected",
                choices = as.list(ad$name),
                options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 1"
                ),
                multiple = TRUE
            ),
            "Otros",
            pickerInput(
                inputId = "drugs_3",
                label = "Select/deselect all + format selected",
                choices = as.list(ot$name),
                options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 1"
                ),
                multiple = TRUE
            ),
            checkboxGroupInput(
                inputId = "pharma",
                label = "Acción farmacológica",
                choices = c("Si" = "Yes",
                            "No" = "No",
                            "Desconocida" = "Unknown"),
                selected = c("Yes", "No", "Unknown"),
                inline = T
            ),
            p(strong("Receptor family")),
            uiOutput("families")
    ),
    column(
        10,
        plotOutput("drugPlot",
                     height = '100%',
                     width = "100%", 
                     inline = T)
    ))
)

server <- function(input, output) {
    output$families <- renderUI({
        
        families <- 
            clean_data %>%
            filter(drug_name %in% c(input$drugs_1, input$drugs_2, input$drugs_3),
                   `Pharmacological action` %in% input$pharma) %>%
            dplyr::distinct(family) %>%
            pull(family) %>%
            as.list()
        
        checkboxGroupInput(
            "families",
            NULL,
            choices = families,
            selected = families,
            )
    })
    
    output$drugPlot <- renderPlot({
        molecule <- c(input$drugs_1, input$drugs_2, input$drugs_3)
        families <- c(input$families)
        for_plot <-
            clean_data %>%
            filter(
                drug_name %in% molecule,
                family %in% families,
                `Pharmacological action` %in% input$pharma
            )
        
        my_symbols <-
            distinct(for_plot, symbol_2, .keep_all = T) %>%
            arrange(Actions) %>%
            pull(symbol)
        
        
        for_plot %>%
            ggplot(aes(x = drug_name, y = receptor, shape = Actions, color = log(Ki))) +
            geom_point(size = 8) +
            scale_shape_manual(values = my_symbols) +
            scale_color_viridis_c(direction = -1, na.value = "gray40", option = "B") +
            theme_minimal()
    })
}

# Run the application
shinyApp(ui = ui, server = server)

