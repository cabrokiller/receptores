pacman::p_load(tidyverse, grid)

data_full <-
    read_csv(
        'https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugbank_target_parse.csv'
    )

molecule <-
    c("Aripirazole", "Diazepam", "Olanzapine")

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
            is.na(Actions) ~ "Other/unknown",
            T ~ Actions,
        )
    ) %>%
    mutate(receptor = str_remove(.$`Name`, pattern = "receptor")) %>%
    mutate(family = str_extract(.$`Gene Name`, pattern = "[:upper:]+")) %>%
    mutate(family = ifelse(is.na(family), "Other", family))

p <- 
for_plot %>%
    ggplot(aes(y = receptor, x = Drug)) +
    geom_point(aes(shape = Actions, color = log(`Ki (nM)_med`)),
               size = 4,
               stroke = 1.4) +
    scale_color_viridis_c(
        option = "B",
        direction = -1,
        begin = .1,
        end = .9,
        na.value = "gray30"
    ) +
    scale_shape_manual(
        values =  c(
            "Agonist" = 2,
            "Antagonist" = 6,
            "Blocker/inhibitor" = 7,
            "Other/unknown" = 8,
            "Partial agonist" = 11,
            "Inverse agonist" = 13,
            "Binder" = 0,
            "Potentiator" = 14,
            "Positive allosteric modulator" = 5
        )
    ) +
    labs(x = '', y = '', color = 'log(Ki)') +
    theme_minimal(base_size = 14) +
    theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
    )

ggplotly(p)
