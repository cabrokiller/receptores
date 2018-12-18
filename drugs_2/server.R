library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(grid)
library(stringr)
library(plotly)
library(viridisLite)

shinyServer(function(input, output, session) {
    data_full <- read_csv('https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugbank_target_parse.csv')
    output$drugPlot <- renderPlotly({
        
        molecule <- c(input$drugs)
        
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
                    Actions == "Positive allosteric modulator" ~ "Allosteric mod (+)",
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
                    Actions == "Inverse agonist" ~ "star-triangle-down-dot",
                    Actions == "Binder" ~ "diamond-wide",
                    Actions == "Potentiator" ~ "triangle-up-dot",
                    Actions == "Allosteric mod (+)" ~ "triangle-up",
                    TRUE ~ "circle-open"
                ),
                potency = 10-log10(`Ki (nM)_med`)
            )
        
        my_symbols <- 
            distinct(for_plot, symbol, .keep_all = T) %>%
            arrange(Actions) %>%
            pull(symbol)
    
        
 
        plot_ly(data = for_plot,
                x = ~ Drug,
                y = ~ receptor,
                key = ~ `Specific Function`,
                symbol =  ~Actions,
                text = ~ paste0("Ki: ", `Ki (nM)_min`, " - ", `Ki (nM)_max`,
                                "<br>Kd: ", `Kd (nM)_min`, " - ", `Kd (nM)_max`),
                hoverinfo = 'text',
                symbols = my_symbols,
                sizes = c(3,25)
                ) %>%
            add_fun(function(plot){   #add non NA points
                plot %>%
                    filter(!is.na(potency)) %>%
                    add_markers(
                        color = ~ potency,
                        colors = plasma(length(for_plot)),
                        size = ~potency,
                        legendgroup = ~Actions,
                        marker = list(sizemode = "diameter",
                                      opacity = .85,
                                      line = list(color = "black",
                                                  width = 2)))
            }) %>%
            add_fun(function(plot){  #add NA points
                plot %>%
                    filter(is.na(potency)) %>%
                    add_markers(
                        legendgroup = ~Actions,
                        name = ~paste(Actions, ", (Ki = NA)"),
                        marker = list(size = 13, color = "lightgray",
                                      opacity = .85,
                                      line = list(color = "black",
                                                  width = 2)))
            })%>%
            layout(
                autosize = T,
                legend = list(tracegroupgap = 20),
                xaxis = list(
                    title = 'Drugs',
                    side = "top",
                    tickfont = list(size = 18)),
                yaxis = list(
                    title='',
                    tickfont = list(size = 10)),
                margin = list(t=80),
                barmode = list(orientation = 'v')
            )
    })
    
    output$click <- renderText({
        d <- event_data("plotly_click")
        if (is.null(d)) "Receptor function" else paste0(d$y, ":", d$key)
    })
})


