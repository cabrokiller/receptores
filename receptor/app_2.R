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
    theme = "bootstrap.css",
    titlePanel("Perfiles farmacodinámicos"),
    h5(
        "Aplicación para visualizar de manera simple el perfil receptorial
        de hasta 3 fármacos. Es posible seleccionar la familia de fármacos a desplegar en las listas"
    ),
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
                    size = 15,
                    `selected-text-format` = "count > 3"
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
                    size = 15,
                    `selected-text-format` = "count > 3"
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
                    size = 15,
                    `selected-text-format` = "count > 3"
                ),
                multiple = TRUE
            ),
            uiOutput("families")
    ),
    column(
        10,
        plotlyOutput("drugPlot",
                     height = '1200px',
                     width = "100%")
    ))
)

server <- function(input, output) {
    output$families <- renderUI({
        
        families <- 
            clean_data %>%
            filter(drug_name %in% c(input$drugs_1, input$drugs_2, input$drugs_3),
                   `Pharmacological action` %in% c("Yes", "Unknown")) %>%
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
    
    output$drugPlot <- renderPlotly({
        molecule <- c(input$drugs_1, input$drugs_2, input$drugs_3)
        families <- c(input$families)
        for_plot <-
            clean_data %>%
            filter(
                drug_name %in% molecule,
                family %in% families,
                `Pharmacological action` %in% c("Yes", "Unknown")
            )
        
        my_symbols <-
            distinct(for_plot, symbol, .keep_all = T) %>%
            arrange(Actions) %>%
            pull(symbol)
        
        plot_ly(
            data = for_plot,
            x = ~ drug_name,
            y = ~ receptor,
            symbol =  ~ Actions,
            symbols = my_symbols,
            sizes = c(3, 25)
        ) %>%
            add_fun(function(plot) {
                #add non NA points
                plot %>%
                    filter(!is.na(potency)) %>%
                    add_markers(
                        color = ~ potency,
                        colors = plasma(length(for_plot)),
                        size = ~ potency,
                        legendgroup = ~ Actions,
                        hoverinfo = 'text',
                        text = ~ paste0(
                            '<b>Receptor:</b> ',
                            Name,
                            '</br><b>Action:</b> ',
                            Actions,
                            '</br><b>Function:</b> ',
                            show_text
                        ),
                        marker = list(
                            sizemode = "diameter",
                            opacity = .85,
                            line = list(color = "black",
                                        width = 2)
                        )
                    )
            }) %>%
            add_fun(function(plot) {
                #add NA points
                plot %>%
                    filter(is.na(potency)) %>%
                    add_markers(
                        legendgroup = ~ Actions,
                        hoverinfo = 'text',
                        text = ~ paste0(
                            '<b>Receptor:</b> ',
                            Name,
                            '</br><b>Action:</b> ',
                            Actions,
                            '</br><b>Function:</b> ',
                            show_text
                        ),
                        name = ~ paste(Actions, ", (when <i>Ki</i> is unknown)"),
                        marker = list(
                            size = 15,
                            color = "lightgray",
                            opacity = .85,
                            line = list(color = "black",
                                        width = 2)
                        )
                    )
            }) %>%
            layout(
                autosize = T,
                legend = list(tracegroupgap = 20),
                xaxis = list(
                    title = 'Drugs',
                    side = "top",
                    tickfont = list(size = 18)
                ),
                yaxis = list(title = '',
                             tickfont = list(size = 12)),
                margin = list(t = 80),
                barmode = list(orientation = 'v')
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)

