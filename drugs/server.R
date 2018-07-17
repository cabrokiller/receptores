library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(ggthemes)
library(magick)
library(grid)
library(stringr)


shinyServer(function(input, output) {
    data_enzyme <- read_csv('https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugbank_enzyme_parse.csv')
    data_target <- read_csv('https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugbank_target_parse.csv')
    
    output$drugPlot <- renderPlot({
        molecule <- input$select
        ifelse(input$radio == 1,
               type <- "target",
               type <- "enzyme")
        
        data_full <- 
            data_target %>%
            mutate(Type = "target") %>%
            bind_rows(data_enzyme %>%
                          mutate(Type = "enzyme"))
        
        for_plot <-   
            data_full %>%
            filter(Drug == molecule,
                   Type == type) %>%
            mutate(Actions = str_replace_all(.$Actions, pattern = c('([:upper:][:lower:]+)' = "\\1 -"))) %>%
            separate(Actions, into = c("action1","action2", "action3"), extra = "drop", fill = "left") %>%
            mutate(action3 = ifelse(action3 == "" | action3 == "unknown", NA, action3)) %>%
            gather(key,Actions, -c(Name, `Gene Name`:Type), na.rm = T) %>%
            select(-key) %>%
            mutate(Actions = str_to_title(Actions),
                   known_ki = ifelse(is.na(`Ki (nM)_med`), F, T),
                   max_ki = ifelse(is.infinite(max(`Ki (nM)_med`, na.rm = T)), 1, max(`Ki (nM)_med`, na.rm = T))) %>%
            arrange(`Pharmacological action`, `Ki (nM)_med`) %>%
            mutate(`Name` = factor(`Name`, levels = unique(.$Name)))
        
        
        mol_img <-
            for_plot %>%
            pull(drugbank_id) %>%
            unique() %>%
            paste0('https://www.drugbank.ca/structures/', ., '/image.svg') %>%
            image_read_svg() %>%
            image_colorize(opacity = 100, color = '#002b36')
        
        
        for_plot %>%
            ggplot(aes(y = `Name`,
                       x = Actions)) +
            annotation_custom(rasterGrob(mol_img)) +
            geom_tile(aes(fill = -log10(`Ki (nM)_med`), color = `Pharmacological action`),
                        interpolate = F, alpha = .8, size = 1) +
            geom_tile(aes(y = `Pharmacological action`))+
            
            scale_fill_viridis_c(option = "D", na.value="#93a1a1", direction = -1) +
            labs(title = unique(for_plot$Drug), x = '', y = "", fill = '-log10(Ki)') +
            
            scale_shape_manual(values = c(1,8, 5, 13)) +
            theme_minimal(base_size = 18) +
            theme(legend.position="bottom")
        
        
    })
    
})
