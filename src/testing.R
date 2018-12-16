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


for_plot %>%
plot_ly(
    type   = 'scatter',
    mode   = 'markers',
    y = ~ receptor,
    x = ~ Drug,
    color = ~ potency,
    colors = "Reds",
    size = ~replace(potency, is.na(potency),8),
    sizes = c(6,20),
    marker = list(sizemode = 'diameter',
                  symbol = ~symbol,
                  line = list(width = 2, color = '#000000')),
    text = ~paste('pot', potency))
z







