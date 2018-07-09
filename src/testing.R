pacman::p_load(rvest, tidyverse, ggthemes, png, RCurl, grid, magick, rsvg, RColorBrewer)


data_target <- read_csv("../data/drugbank_target_parse.csv")
data_enzyme <- read_csv("../data/drugbank_enzyme_parse.csv")




molecule <- "Haloperidol"
data <- data_enzyme




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
    select(Name, Actions, `General Function`, `Ki (nM)_min`, `Ki (nM)_max`, `Ki (nM)_med`) %>%
    mutate(Actions = str_replace_all(.$Actions, pattern = c('([:upper:][:lower:]+)' = "\\1 -"))) %>%
    separate(Actions, into = c("action1","action2", "action3"), extra = "drop", fill = "left") %>%
    mutate(action3 = ifelse(action3 == "", NA, action3)) %>%
    gather(key,Actions, -c(Name, `General Function`, `Ki (nM)_min`, `Ki (nM)_max`, `Ki (nM)_med`), na.rm = T) %>%
    select(-key) %>%
    mutate(Actions = str_to_title(Actions)) %>%
    # plot
    ggplot(aes(x=`Name`, color=Actions, y = `Ki (nM)_med`))+
    annotation_custom(rasterGrob(mol_img)) +
    geom_errorbar(aes(ymin = `Ki (nM)_min`, ymax = `Ki (nM)_max`), fatten = 10, 
                  alpha = .8, width = .6, position = position_dodge(.6), size = 1.2)+
    geom_point(aes(y = `Ki (nM)_med`), position = position_dodge(.6),
               size = 6, alpha = .8, shape = 19) +
    scale_color_manual(values = colorRampPalette(brewer.pal(12, 'Set3'))(n))+
    labs(title = str_to_title(molecule), x = "", y="", color = "Function") +
    ggplot2::scale_y_log10() +
    coord_flip() +
    theme_minimal()
    