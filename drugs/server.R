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
    source("../src/testing.R")
    output$drugPlot <- renderPlot({
        molecule <- input$select
        ifelse(input$radio == 1,
                type <- "target",
                type <- "enzyme")
        
        do_plot_3(molecule = molecule, type = type)
        
        
  })
  
})
