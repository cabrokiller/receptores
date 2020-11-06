pacman::p_load(tidyverse, grid, plotly)

source("src/preproc.R")
clean <- read_csv("data/clean.csv")

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

plot_ly(
    data = for_plot,
    x = ~ drug_name,
    y = ~ receptor,
    symbol =  ~ Actions,
    symbols = my_symbols,
    sizes = c(3, 25)
) %>%
    add_markers(
        color = ~ Actions,
        colors = ggthemes::solarized_pal()(5),
        #size = ~ potency,
        legendgroup = ~ Actions,
        hoverinfo = 'text',
        text = ~ paste0(
            '<b>Receptor:</b> ',
            Name,
            '</br><b>Action:</b> ',
            Actions,
            '</br><b>Function:</b> ',
            show_text
        ),
        marker = list(
            sizemode = "diameter",
            opacity = .85,
            line = list(color = "black",
                        width = 2)
        )
    )


#########################
 
for_plot_2 <- 
for_plot %>%
    mutate(
        symbol_2 =
            case_when(
                Actions == "Agonist" ~ '\u2bc5',
                Actions == "Antagonist" ~ '\u2bc6',
                Actions == "Blocker/inhibitor" ~ '\u2bbf',
                Actions == "Other/unknown" ~ '\u2b57',
                Actions == "Partial agonist" ~ '\u2b1f',
                Actions == "Partial antagonist" ~ '\u2bc2',
                Actions == "Inverse agonist" ~ '\u2b9f',
                Actions == "Binder" ~ '\u2b2c',
                Actions == "Potentiator" ~ '\u2bed',
                Actions == "Allosteric mod (+)" ~ '\u2b99',
                TRUE ~ '\u2bc0'
            )
    )

my_symbols <- 
    distinct(for_plot_2, symbol_2, .keep_all = T) %>%
    arrange(Actions) %>%
    pull(symbol_2)

my_symbols

############## ggplot
for_plot_2 %>%
    ggplot(aes(x = drug_name, y = receptor, shape = Actions, color = log(Ki), size =  -Ki)) +
    geom_point() +
    scale_shape_manual(values = my_symbols) +
    scale_size(range = c(10,20)) +
    scale_color_viridis_c(direction = -1, na.value = "gray40", option = "B") +
    facet_grid(family ~ `Pharmacological action`, scales = "free_y", space = "free_y") +
    theme_minimal_grid()


######## 



