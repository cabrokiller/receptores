library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(rsvg)
library(magick)
library(grid)
library(stringr)

shinyServer(function(input, output, session) {
    data_full <- read_csv('https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugbank_target_parse.csv')
    output$drugPlot <- renderPlot({
        
        molecule <- c(input$drugs_1,input$drugs_2, input$drugs_3)
        
        for_plot <-
            data_full %>%
            filter(Drug %in% molecule) %>%
            mutate(Actions = case_when(
                Actions == "AntagonistAgonist" ~ "Antagonist",
                Actions %in% c("Ligand") ~ "Agonist",
                Actions %in% c("Other", "Unknown", "AntagonistOther/unknown")  ~ "Other/unknown",
                Actions %in% c("Blocker", "Inhibitor") ~ "Blocker/inhibitor",
                Actions == "AntagonistPartial agonist" ~ "Partial agonist",
                is.na(Actions) ~ "Other/unknown",
                T ~ Actions
            )) %>%
            mutate(receptor = str_remove(.$`Name`, pattern = "receptor")) %>%
            mutate(family = str_extract(.$`Gene Name`, pattern = "[:upper:]+")) %>%
            mutate(family = ifelse(is.na(family), "Other", family))
        
        plot <- 
            for_plot %>%
            ggplot(aes(y = reorder(`receptor`, desc(Name)), x=0)) +
            geom_point(aes(shape = Actions, color = log10(`Ki (nM)_med`)), size = 5, stroke = 1.4) +
            scale_color_viridis_c(option = "B", direction = -1, begin = .1, end = .9, na.value = "gray30") +
            scale_shape_manual(values =  c("Agonist" = 2,
                                           "Antagonist" = 6,
                                           "Blocker/inhibitor" = 7,
                                           "Other/unknown" = 8,
                                           "Partial agonist" = 11,
                                           "Inverse agonist" = 13,
                                           "Binder" = 0,
                                           "Potentiator" = 14,
                                           "Positive allosteric modulator" = 5
            )) +
            scale_x_continuous(breaks = NULL) +
            scale_y_discrete(position = "right") +
            labs(x = '', y = '', color = 'log(Ki)') +
            theme_minimal(base_size = 14) +
            facet_grid(cols = vars(Drug), rows = vars(`family`),
                       scales = "free_y", space = "free", switch = "y") +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  strip.text.y = element_text(angle = 180),
                  strip.text.x = element_text(size = 16))
    plot
    })
})

