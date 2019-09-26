pacman::p_load(tidyverse, grid, plotly)

# source("src/preproc.R")
load("data/clean")

molecule <-
    c("Aripiprazole", "Lurasidone", "Cariprazine")

my_family <- 
    c("DRD", "HTR")


for_plot <- 
    clean_data %>%
    filter(
        drug_name %in% molecule,
        family %in% my_family,
        `Pharmacological action` %in% c("Yes", "Unknown")
    )

my_symbols <- 
    distinct(for_plot, symbol, .keep_all = T) %>%
    arrange(Actions) %>%
    pull(symbol)


# library(radarchart)

plot_ly(data = for_plot,
        x = ~ drug_name,
        y = ~ receptor,
        symbol =  ~ Actions,
        symbols = my_symbols,
        sizes = c(3,25)
) %>%
    add_fun(function(plot){   #add non NA points
        plot %>%
            filter(!is.na(potency)) %>%
            add_markers(
                color = ~ potency,
                colors = plasma(length(for_plot)),
                size = ~ potency,
                legendgroup = ~ Actions,
                hoverinfo = 'text',
                text= ~ paste0(
                    '<b>Receptor:</b> ', Name,
                    '</br><b>Action:</b> ', Actions,
                    '</br><b>Function:</b> ', show_text
                ),
                marker = list(sizemode = "diameter",
                              opacity = .85,
                              line = list(color = "black",
                                          width = 2)))
    }) %>%
    add_fun(function(plot){  #add NA points
        plot %>%
            filter(is.na(potency)) %>%
            add_markers(
                legendgroup = ~ Actions,
                hoverinfo = 'text',
                text= ~ paste0(
                    '<b>Receptor:</b> ', Name,
                    '</br><b>Action:</b> ', Actions,
                    '</br><b>Function:</b> ', show_text
                    ),
                name = ~ paste(Actions, ", (when <i>Ki</i> is unknown)"),
                marker = list(size = 15, color = "lightgray",
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
            tickfont = list(size = 12)),
        margin = list(t=80),
        barmode = list(orientation = 'v')
    )




