pacman::p_load(tidyverse, plotly)

data_full <-
    read_csv(
        'https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugbank_target_parse.csv'
    )

molecule <-
    c("Aripiprazole", "Lorazepam", "Diazepam")

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
            Actions == "Other/unknown" ~ "cross-dot",
            Actions == "Partial agonist" ~ "hexagram",
            Actions == "Inverse agonist" ~ "star-triangle-down-dot",
            Actions == "Binder" ~ "diamond-wide",
            Actions == "Potentiator" ~ "triangle-up",
            Actions == "Allosteric mod (+)" ~ "triangle-up-dot",
            TRUE ~ "circle-open"
        ),
        potency = 10-log10(`Ki (nM)_med`)
    )

my_symbols <- 
    distinct(for_plot, symbol, .keep_all = T) %>%
    arrange(Actions) %>%
    pull(symbol)



plot_ly(data = for_plot,
        type = "scatter",
        mode = "markers",
        x = ~ Drug,
        y = ~ receptor,
        symbol =  ~ Actions,
        symbols = my_symbols,
        visible = "legendonly",
        opacity = 1,
        #colors = "Viridis",
        sizes = c(.1,100),
        marker = list(size = 12, color = I("black"))) %>%
    add_fun(function(plot){
        plot %>%
            filter(!is.na(potency)) %>%
            add_markers(
                inherit = F,
                x = ~ Drug,
                y = ~ receptor,
                color = ~ potency,
                symbol =  ~ Actions,
                size = ~potency*2,
                marker = list(sizemode = "area", opacity =.5),
                showlegend=F,
                text = ~potency)
    }) %>%
    add_fun(function(plot){
        plot %>%
            filter(is.na(potency)) %>%
            add_markers(
                inherit = F,
                x = ~ Drug,
                y = ~ receptor,
                symbol =  ~ Actions,
                marker = list(size = 15, color = "gray"),
                showlegend=F)
    })




