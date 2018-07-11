pacman::p_load(rvest, tidyverse, ggthemes, png, RCurl, grid, magick, rsvg, RColorBrewer)


data_target <- read_csv("data/drugbank_target_parse.csv")
data_enzyme <- read_csv("data/drugbank_enzyme_parse.csv")


do_plot_1 <- function(molecule = "Clozapine", type = "target"){
    ifelse(type == "target",
           data <- data_target,
           data <- data_enzyme)
    df <- 
        data %>%
        filter(Drug == molecule)
    # get number of `General Function` to plot
    n <-
        length(unique(df$`General Function`))

    # get molecule image
    mol_img <-
        df %>%
        pull(drugbank_id) %>%
        unique() %>%
        paste0('https://www.drugbank.ca/structures/', ., '/image.svg') %>%
        image_read_svg() %>%
        image_colorize(opacity = 100, color = '#839496')

    # plot
    df %>%
        select(Name, Actions, `General Function`) %>%
        mutate(Actions = str_replace_all(.$Actions, pattern = c('([:upper:][:lower:]+)' = "\\1 -"))) %>%
        separate(Actions, into = c("action1","action2", "action3"), extra = "drop", fill = "left") %>%
        mutate(action3 = ifelse(action3 == "", NA, action3)) %>%
        gather(key,Actions, -c(Name, `General Function`), na.rm = T) %>%
        select(-key) %>%
        # plot
        ggplot(aes(y=`Name`, fill=Actions, x=Actions))+
        annotation_custom(rasterGrob(mol_img)) +
        geom_bin2d(alpha = .65) +
        scale_fill_manual(values = colorRampPalette(solarized_pal()(8))(n))+
        labs(title = str_to_title(molecule), x = "", y="", fill = "Function") +
        theme_minimal()
}

do_plot_1()

data_full <- 
    data_target %>%
    mutate(Type = "target") %>%
    bind_rows(data_enzyme %>%
                  mutate(Type = "enzyme"))

n = 4

data_full %>%
    filter(Drug == "Clozapine",
           Type == "target") %>%
    replace_na(list(Actions = "Other/unknown")) %>%
    # plot
    ggplot(aes(y = `Gene Name`, x=1, color = Actions)) +
    geom_point(size = 3) +
    geom_point(aes(size = 1/log(`Ki (nM)_med`))) +
    scale_size_continuous(range = c(3,15)) +
    scale_color(type = 'qual', palette = 2) +
    coord_fixed(.75) +
    theme_minimal()
    
for_plot <-   
    data_full %>%
    filter(Drug == "Clozapine",
           Type == "target") %>%
    mutate(Actions = str_replace_all(.$Actions, pattern = c('([:upper:][:lower:]+)' = "\\1 -"))) %>%
    separate(Actions, into = c("action1","action2", "action3"), extra = "drop", fill = "left") %>%
    mutate(action3 = ifelse(action3 == "", NA, action3)) %>%
    gather(key,Actions, -c(Name, `Gene Name`:Type), na.rm = T) %>%
    select(-key)

mean_Ki <- 
    for_plot %>%
    summarise(mean = mean(`Ki (nM)_med`, na.rm = T)) %>%
    pull(mean)
    
for_plot %>%
    ggplot(aes(y = fct_reorder(`Gene Name`, `Ki (nM)_med`), x = 1, color = Actions, group = Actions)) +
    geom_point(aes(size = ifelse(is.na(`Ki (nM)_med`), -log(mean_Ki), -log(`Ki (nM)_med`)),
                   shape = ifelse(is.na(`Ki (nM)_med`),  "Ki unknown", "size = -log(Ki)")),
               position = position_dodge(1)) +
    scale_size_continuous(range = c(3,10), guide = F) +
    scale_shape_manual(values= c(18,19)) +
    scale_color_viridis_d(option = "D") +
    labs(title = unique(for_plot$Drug), x = '', y = "Targets", shape = '') +
    coord_fixed(.5) +
    theme_minimal()


for_plot %>%
    ggplot(aes(x = fct_reorder(`Gene Name`, `Ki (nM)_med`),
               y = ifelse(is.na(`Ki (nM)_med`), -log(mean_Ki), -log(`Ki (nM)_med`)),
               group = Actions)) +
    geom_line() +
    geom_dotplot(aes(fill = Actions), binaxis = 'x', stackgroups = T, 
                 stackdir = "centerwhole", binpositions = "all") +
    scale_color_viridis_d(option = "D") +
    labs(title = unique(for_plot$Drug), x = '', y = "Targets", shape = '') +
    theme_minimal()
    

    
    
