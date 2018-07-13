pacman::p_load(rvest, tidyverse, ggthemes, png, RCurl, grid, magick, rsvg, RColorBrewer, scales)


data_target <- read_csv("../data/drugbank_target_parse.csv")
data_enzyme <- read_csv("../data/drugbank_enzyme_parse.csv")


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

do_plot_2 <- function(molecule = "Clozapine", type = "target"){
    data_full <- 
        data_target %>%
        mutate(Type = "target") %>%
        bind_rows(data_enzyme %>%
                      mutate(Type = "enzyme"))

    for_plot <-   
        data_full %>%
        filter(Drug == molecule,
               Type == type) %>%
        mutate(Actions = str_replace_all(.$Actions, pattern = c('([:upper:][:lower:]+)' = "\\1 -"))) %>%
        separate(Actions, into = c("action1","action2", "action3"), extra = "drop", fill = "left") %>%
        mutate(action3 = ifelse(action3 == "", NA, action3)) %>%
        gather(key,Actions, -c(Name, `Gene Name`:Type), na.rm = T) %>%
        select(-key) %>%
        mutate(Pot = ifelse(is.na(`Ki (nM)_med`), 0, -log10(`Ki (nM)_med`)))

    min_pki <- 
        for_plot %>%
        pull(Pot) %>%
        min(., na.rm = T)


    for_plot %>%
        ggplot(aes(y = fct_reorder(`Gene Name`, `Ki (nM)_med`),
                   x = ifelse(is.na(`Ki (nM)_med`), 0, log10(`Ki (nM)_med`)))) +
        
        geom_line(aes(group = ifelse(is.na(`Ki (nM)_med`), 'a', 'b'),
                      linetype = ifelse(is.na(`Ki (nM)_med`), 'a', 'b')),
                  size = .5) +
        
        geom_point(aes(shape = ifelse(is.na(`Ki (nM)_med`), 'a', 'b'))) + 
    
        geom_dotplot(aes(fill = Actions, group = Actions, x= 0), binaxis = 'y', stackgroups = T, 
                     stackdir = "centerwhole", binpositions = 'all')+

        scale_fill_viridis_d(option = "D") +
        scale_linetype_manual(values = c(4,1)) +
        labs(title = unique(for_plot$Drug), x = '', y = "Targets", shape = '') +
        scale_x_reverse() +
        theme_minimal()
}

do_plot_3 <- function(molecule = "Clozapine", type = "target"){
    data_full <- 
        data_target %>%
        mutate(Type = "target") %>%
        bind_rows(data_enzyme %>%
                      mutate(Type = "enzyme"))
    
    for_plot <-   
        data_full %>%
        filter(Drug == molecule,
               Type == type) %>%
        mutate(Actions = str_replace_all(.$Actions, pattern = c('([:upper:][:lower:]+)' = "\\1 -"))) %>%
        separate(Actions, into = c("action1","action2", "action3"), extra = "drop", fill = "left") %>%
        mutate(action3 = ifelse(action3 == "", NA, action3)) %>%
        gather(key,Actions, -c(Name, `Gene Name`:Type), na.rm = T) %>%
        select(-key) %>%
        mutate(Pot = ifelse(is.na(`Ki (nM)_med`), 0, -log10(`Ki (nM)_med`)))
    
    n_names <- 
        for_plot %>%
        tally() %>%
        pull(n)
    
    mol_img <-
        for_plot %>%
        pull(drugbank_id) %>%
        unique() %>%
        paste0('https://www.drugbank.ca/structures/', ., '/image.svg') %>%
        image_read_svg() %>%
        image_colorize(opacity = 100, color = '#839496')
    
    
    for_plot %>%
        ggplot(aes(y = fct_reorder(`Name`, -log(`Ki (nM)_med`)),
                   x = Actions)) +
        annotation_custom(rasterGrob(mol_img)) +
        geom_bin2d(aes(fill = log10(`Ki (nM)_med`)), alpha = .75) +
        scale_fill_viridis_c(option = "C") +
        labs(title = unique(for_plot$Drug), x = '', y = "Targets", shape = 'Potence') +
        theme_minimal()
    

    }

