pacman::p_load(tidyverse, ggthemes, magick, grid, scales)

data_enzyme <- read_csv('https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugbank_enzyme_parse.csv')
data_target <- read_csv('https://raw.githubusercontent.com/cabrokiller/receptores/master/data/drugbank_target_parse.csv')



data_full <- 
    data_target %>%
    mutate(Type = "target") %>%
    bind_rows(data_enzyme %>%
                  mutate(Type = "enzyme"))

molecule = c("Aripiprazole", "Haloperidol")
type = "target"


for_plot <-
    data_full %>%
    filter(Drug %in% molecule,
           Type == type) %>%
    mutate(Actions = str_replace_all(.$Actions, pattern = c('([:upper:][:lower:]+)' = "\\1 -"))) %>%
    separate(Actions, into = c("action1","action2", "action3"), extra = "drop", fill = "left") %>%
    mutate(action3 = ifelse(action3 == "", NA, action3)) %>%
    gather(key,Actions, -c(Name, `Gene Name`:Type), na.rm = T) %>%
    select(-key) %>%
    mutate(Pot = ifelse(is.na(`Ki (nM)_med`), NA, -log10(`Ki (nM)_med`))) %>%
    arrange(Name)


for_plot %>%
    ggplot(aes(y = Name)) +
    geom_point(aes(color = `Ki (nM)_med`, shape = Actions, x = 0, group = Actions), position = position_dodge(1), size = 3) +
    facet_grid(cols = vars(Drug)) +
    scale_color_viridis_c(option = "B", direction = -1, end = .85)



