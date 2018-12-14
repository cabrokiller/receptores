library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(grid)
library(stringr)
library(plotly)

shinyServer(function(input, output, session) {
    data_full <- read_csv('https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugbank_target_parse.csv')
    output$drugPlot <- renderPlotly({
        
        molecule <- c(input$drugs_1,input$drugs_2, input$drugs_3)
        
        for_plot <-
            data_full %>%
            filter(Drug %in% molecule) %>%
            mutate(
                Actions = case_when(
                    Actions == "AntagonistAgonist" ~ "Antagonist",
                    Actions %in% c("Ligand") ~ "Agonist",
                    Actions %in% c("Other", "Unknown", "AntagonistOther/unknown")  ~ "Other/unknown",
                    Actions %in% c("Blocker", "Inhibitor") ~ "Blocker/inhibitor",
                    Actions == "AntagonistPartial agonist" ~ "Partial agonist",
                    is.na(Actions) ~ "Other/unknown",
                    T ~ Actions
                ),
                receptor = str_remove(.$`Name`, pattern = "receptor"),
                family = str_extract(.$`Gene Name`, pattern = "[:upper:]+"),
                family = ifelse(is.na(family), "NE", family),
                symbol = case_when(
                    Actions == "Agonist" ~ "star-triangle-up",
                    Actions == "Antagonist" ~ "star-triangle-down",
                    Actions == "Blocker/inhibitor" ~ "square-x",
                    Actions == "Other/unknown" ~ "circle-dot",
                    Actions == "Partial agonist" ~ "hexagram",
                    Actions == "Inverse agonist" ~ "star-triangle-down-open-dot",
                    Actions == "Binder" ~ "diamond-wide",
                    Actions == "Potentiator" ~ "triangle-up",
                    Actions == "Positive allosteric modulator" ~ "triangle-nw",
                    TRUE ~ "circle-open"
                ),
                potency = -log10(`Ki (nM)_med`)
            )
        
        symbols <- 
            distinct(for_plot, symbol, .keep_all = T) %>%
            arrange(Actions) %>%
            pull(symbol)
        
        
        plot_ly(data = for_plot,
                type = 'scatter',
                mode = 'markers',
                x = ~ Drug,
                y = ~ receptor, 
                symbol = ~ Actions,
                symbols = symbols) %>%
            add_trace(
                size = ~ potency,
                sizes = c(10,100))
    })
})

