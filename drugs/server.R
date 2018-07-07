#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    pacman::p_load(rvest, tidyverse, ggthemes, png, RCurl, grid, magick, rsvg, RColorBrewer)
    output$drugPlot <- renderPlot({
        molecule <- input$select
        ifelse(input$radio == 1,
                data <- read_csv("../data/drugbank_target_parse.csv"),
                data <- read_csv("../data/drugbank_enzyme_parse.csv"))
        df <- 
            data %>%
            filter(Drug == molecule)
        # get number of `General Function` to plot
        n <-
            length(unique(df$`General Function`))
        
        # get molecule image
        mol_img <-
            df %>%
            pull(drugbank_id) %>%
            unique() %>%
            paste0('https://www.drugbank.ca/structures/', ., '/image.svg') %>%
            image_read_svg() %>%
            image_colorize(opacity = 100, color = '#839496')
        
        # plot
        df %>%
            select(Name, `Gene Name`, Actions, `General Function`) %>%
            mutate(Actions = str_replace_all(.$Actions, pattern = c('([:upper:][:lower:]+)' = "\\1 -"))) %>%
            separate(Actions, into = c("action1","action2", "action3"), extra = "drop", fill = "left") %>%
            mutate(action3 = ifelse(action3 == "", NA, action3)) %>%
            gather(key,Actions, -c(Name, `General Function`, `Gene Name`), na.rm = T) %>%
            select(-key) %>%
            # plot
            ggplot(aes(x=`Gene Name`, y=Actions, fill=str_remove(`General Function`, ',.+'))) +
            annotation_custom(rasterGrob(mol_img)) +
            geom_bin2d(alpha = .6) +
            # geom_point(size=10, shape=15, alpha = .8) +
            coord_flip() +
            scale_fill_manual(values = colorRampPalette(brewer.pal(n = 7, name = 'BrBG'))(n)) +
            labs(title = str_to_title(molecule), x = "", y="", fill = "Function") +
            theme_minimal()
  })
  
})
