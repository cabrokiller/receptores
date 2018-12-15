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
            Actions == "Agonist" ~ "star-triangle-up-open",
            Actions == "Antagonist" ~ "star-triangle-down-open",
            Actions == "Blocker/inhibitor" ~ "square-x",
            Actions == "Other/unknown" ~ "circle-dot",
            Actions == "Partial agonist" ~ "hexagram",
            Actions == "Inverse agonist" ~ "star-triangle-down-open-dot",
            Actions == "Binder" ~ "diamond-wide",
            Actions == "Potentiator" ~ "triangle-up",
            Actions == "Allosteric mod (+)" ~ "triangle-up",
            TRUE ~ "circle-open"
        ),
        potency = log10(`Ki (nM)_med`)
    ) %>%
    select(Drug, receptor, Actions, symbol, potency)

symbols <- 
    distinct(for_plot, symbol, .keep_all = T) %>%
    arrange(Actions) %>%
    pull(symbol)




for_plot %>%
    ggplot(aes(x = Drug, y = receptor, color = potency, shape = Actions))+
    geom_point() +
    #scale_shape_manual(values = symbols) +
    scale_color_viridis_c(option="B", direction = 1)+
    theme_minimal() +
    labs(x="", y="")




