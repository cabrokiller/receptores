#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(
                "unic",
                "unicode",
                
            )),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        data(iris)
        
        shapes <- "\u2BC2"
        
        iris %>%
            mutate(shap = input$unic) %>%
            ggplot(aes(Sepal.Length, Sepal.Width, color = Species, shape = shap)) +
            geom_point(size = 10) +
            scale_shape_manual(values = shapes)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
