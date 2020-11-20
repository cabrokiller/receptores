library(shiny)
library(tidyverse)
library(plotly)
library(viridisLite)
library(shinyWidgets)
library(ggthemes)
library(shinythemes)



# source("src/preproc.R")

clean_data <-
    read_csv("https://raw.githubusercontent.com/cabrokiller/receptores/master/data/clean.csv") %>%
    #read_csv("../data/clean.csv") %>%
    mutate(`Pharmacological action` = factor(`Pharmacological action`, levels = c("Yes", "Unknown", "No")))



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



ui <-
    fluidPage(theme = shinytheme("simplex"),
              #titlePanel("Perfiles farmacodinámicos"),
              # h5(
              #     "Aplicación para visualizar de manera simple el perfil receptorial
              #     de hasta 3 fármacos. Es posible seleccionar la familia de fármacos a desplegar en las listas"
              # ),
              fluidRow(
                  column(
                      2,
                      align = "left", 
                      wellPanel(
                          h3("Pharma profiles"),
                          p(
                              "This app is designed to show the pharmacological receptor
                            profiles of different drugs used in Psychiatry"
                          ),
                          p(
                              "You may choose different molecules to display their receptor binding profile.
                            You can hover over the different receptor activities to get more information displayed in the right pane"
                          ),
                          HTML("<hr>"),
                          h4("Drugs"),
                          pickerInput(
                              inputId = "drugs_2",
                              label = "Select/deselect - Antipsychotics",
                              choices = as.list(ap$name),
                              selected = c("Aripiprazole", "Clozapine"),
                              options = list(
                                  `actions-box` = TRUE,
                                  size = 10,
                                  `selected-text-format` = "count > 1"
                              ),
                              multiple = TRUE
                          ),
                          pickerInput(
                              inputId = "drugs_1",
                              label = "Select/deselect  - Mood/AD",
                              choices = as.list(ad$name),
                              options = list(
                                  `actions-box` = TRUE,
                                  size = 10,
                                  `selected-text-format` = "count > 1"
                              ),
                              multiple = TRUE
                          ),
                          pickerInput(
                              inputId = "drugs_3",
                              label = "Select/deselect all - Other Meds",
                              choices = as.list(ot$name),
                              options = list(
                                  `actions-box` = TRUE,
                                  size = 10,
                                  `selected-text-format` = "count > 1"
                              ),
                              multiple = TRUE
                          ),
                          HTML("<hr>"),
                          h4("Pharmacological action"),
                          checkboxGroupInput(
                              inputId = "pharma",
                              label = NULL,
                              choices = c(
                                  "Yes" = "Yes",
                                  "No" = "No",
                                  "Unknown" = "Unknown"
                              ),
                              selected = c("Yes", "No", "Unknown"),
                              inline = F
                          ),
                          HTML("<hr>"),
                          h4("Receptor Families"),
                          uiOutput("families"),
                      ),
                      #p(strong("Information")),
                      
                      #uiOutput("txt")
                  ),
                  
                  # PLOT
                  column(
                      7,
                      align = "center",
                      plotOutput(
                          "drugPlot",
                          height = "900px",
                          width = "1000px",
                          hover = hoverOpts("hover_txt", delay = 300, delayType = "debounce")
                      )
                  ),
                  column(
                      3,
                      align = "left",
                      wellPanel(h3("Information"),
                                uiOutput("txt")))
                  
              ))





server <- function(input, output) {
    output$families <- renderUI({
        families <-
            clean_data %>%
            filter(
                drug_name %in% c(input$drugs_1, input$drugs_2, input$drugs_3),
                `Pharmacological action` %in% input$pharma
            ) %>%
            dplyr::distinct(family) %>%
            pull(family) %>%
            as.list()
        
        pickerInput(
            "families",
            label = NULL,
            choices = families,
            selected = families,
            options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 1"
            ),
            multiple = TRUE
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
            pull(symbol_2)
        
        q <-
            for_plot %>%
            ggplot(aes(
                x = `Pharmacological action`,
                y = receptor,
                shape = Actions,
                color = log(Ki)
            )) +
            geom_point(size = 8) +
            #geom_text(size = 10, aes(label = symbol_2, family="Arial Unicode MS", color = log(Ki))) +
            scale_shape_manual(values = my_symbols,
                               guide = guide_legend(override.aes = list(color = "gray60"))) +
            scale_color_viridis_c(direction = -1,
                                  na.value = "gray60",
                                  option = "B") +
            facet_grid(
                family ~ drug_name,
                scales = "free_y",
                space = "free_y",
                shrink = T
            ) +
            theme_solarized_2(light = F, base_size = 14) +
            theme(axis.title.y = element_blank(),
                  strip.text.x = element_text(size = 20),
                  text=element_text(family="Arial Black"))
        
        
        
        q
        
    })
    
    output$txt <-
        renderUI({
            my_row <-
                nearPoints(
                    clean_data,
                    input$hover_txt,
                    threshold = 10,
                    maxpoints = 1,
                    addDist = TRUE
                )
            texto <-
                    paste0(
                        "<b>Ki min:</b> ",
                        my_row$Ki_min,
                        "<b>  -  Ki max:</b>",
                        my_row$Ki_max,
                        "<br> <b>INFO</b>: ",
                        my_row$show_text
                    
                )
            
            # render html
            if(nrow(my_row)==0){
                HTML("Select a point in the plot for more information")
                
            } else {
                
                id <- 
                    drugs %>%
                    filter(name == my_row$drug_name) %>%
                    pull(drugbank_id)
                
                img_url <- 
                    paste0("https://go.drugbank.com/structures/", id,"/image.png")
                
                img <- paste0('<img src=', img_url,' style="width:300px;height:300px">')
                
            HTML(paste0(
                h2(my_row$drug_name), '<br>',
                img,'<br>',
                h4(paste0(my_row$receptor, ' ', my_row$Actions, ' ', my_row$symbol_2)), '<br>',
                texto))}
        })

}

# Run the application
shinyApp(ui = ui, server = server)