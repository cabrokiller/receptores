pacman::p_load(tidyverse, ggthemes, magick, grid, scales, ggimage)

data_enzyme <- read_csv('https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugbank_enzyme_parse.csv')
data_target <- read_csv('https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugbank_target_parse.csv')



data_full <- 
    data_target %>%
    mutate(Type = "target") %>%
    bind_rows(data_enzyme %>%
                  mutate(Type = "enzyme"))

molecule = c("Asenapine", "Olanzapine")
type = "target"


for_plot <-
    data_full %>%
    filter(Drug %in% molecule) %>%
    mutate(Actions = case_when(
        Actions == "AntagonistAgonist" ~ "Antagonist",
        Actions %in% c("Ligand") ~ "Agonist",
        Actions %in% c("Other", "Unknown", "AntagonistOther/unknown")  ~ "Other/unknown",
        Actions %in% c("Blocker", "Inhibitor") ~ "Blocker/inhibitor",
        Actions == "AntagonistPartial agonist" ~ "Partial agonist",
        is.na(Actions) ~ "Other/unknown",
        T ~ Actions,
    )) %>%
    mutate(receptor = str_remove(.$`Uniprot Name`, pattern = "receptor")) %>%
    mutate(family = str_extract(.$`Gene Name`, pattern = "[:upper:]+")) %>%
    filter(!is.na(family))

tt <-
for_plot %>%
    distinct(drugbank_id, .keep_all = T) %>%
    select(drugbank_id) %>%
    purrr::map(function(x) {
        paste0('https://www.drugbank.ca/structures/', .$drugbank_id, '/image.svg') %>%
            magick::image_read_svg() %>%
            magick::image_colorize(opacity = 100, color = '#002b36')}
    )



tt[["drugbank_id"]][1]


for_plot %>%
    ggplot(aes(y = reorder(`receptor`, desc(Name)), x=0)) +
    geom_point(aes(shape = Actions, color = log(`Ki (nM)_med`)), size = 2, stroke = 1.4) +
    scale_color_viridis_c(option = "B", direction = -1, begin = .1, end = .9, na.value = "gray30") +
    scale_shape_manual(values =  c("Agonist" = 2,
                                   "Antagonist" = 6,
                                   "Blocker/inhibitor" = 7,
                                   "Other/unknown" = 8,
                                   "Partial agonist" = 11,
                                   "Inverse agonist" = 13,
                                   "Binder" = 0,
                                   "Potentiator" = 14
    )) +
    annotation_custom(rasterGrob(tt[["drugbank_id"]][2])) +
    scale_x_continuous(breaks = NULL) +
    scale_y_discrete(position = "right") +
    labs(x = '', y = '', color = 'log(Ki)') +
    theme_minimal(base_size = 12) +
    facet_grid(cols = vars(Drug), rows = vars(`family`),
               scales = "free", space = "free", switch = "y") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
    
   


