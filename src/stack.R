library(plotly)
library(dplyr)

set.seed(1234)
my_data <- 
data_frame(
    cat = sample(LETTERS[1:3], 30, replace=TRUE, prob=c(0.4, 0.5, 0.1)),
    num1 = runif(30,1,6),
    num2 = runif(30,100,200),
    )  %>%
    mutate(num2 = ifelse(row_number() == 14, NA, num2))
    
plot_ly(data = my_data,
        type = "scatter",
        mode = "markers",
        x = ~ cat,
        y = ~ num1,
        symbol =  ~ cat,
        marker = list(size = 20, color = "grey")) %>%
    add_fun(function(plot){
        plot %>%
            filter(!is.na(num2)) %>%
            add_trace(
                inherit = F,
                type = "scatter",
                mode = "markers",
                x = ~ cat,
                y = ~ num1,
                color = ~ num2,
                colors = "Reds",
                symbol =  ~ cat,
                marker = list(size = 20),
                text = ~ num2,
                showlegend=F)
    }) %>%
    add_fun(function(plot){
        plot %>%
            filter(is.na(num2)) %>%
            add_trace(
                type = "scatter",
                mode = "markers",
                x = ~ cat,
                y = ~ num1,
                symbol =  ~ cat,
                marker = list(size = 20,
                              color = I("grey")),
                text = ~ num2,
                showlegend=F)
    })


        