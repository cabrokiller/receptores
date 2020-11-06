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
 


############################# single

pacman::p_load(ggthemes, cowplot, png, RCurl)
ax <- 7
my_name = "Vortioxetine"

for_plot <- 
clean %>%
    mutate(
        action_plot = case_when(
            Actions == "Antagonist" ~ -5,
            Actions == "Agonist" ~ 5,
            Actions == "Other/unknown" ~ 0,
            Actions == "Blocker/inhibitor" ~ -3,
            Actions == "Partial agonist" ~ 3,
            Actions == "Partial agonist" ~ 3,
            Actions == "Allosteric mod (+)" ~ 3
        )
    ) %>%
    filter(drug_name == my_name,
           `Pharmacological action` %in% c("Yes", "Unknown", "No"))
    


img <- 
    drugs %>%
    filter(name == my_name) %>%
    pull(drugbank_id) %>%
    paste0('https://go.drugbank.com/structures/', ., '/image.png') %>%
    getURLContent() %>%
    readPNG() %>%
    rasterGrob(interpolate = T)



for_plot %>%    
    ggplot(aes(x = `Gene Name`, y = action_plot, group = `Gene Name`, color = Actions)) +
    geom_point(aes(shape = Actions), size = 5) +
    geom_segment(aes(xend = `Gene Name`, yend = 0), size = 5) +
    #geom_text(aes(label = `Gene Name`), nudge_y = 1) +
    geom_hline(yintercept = 0, size = 2) +
    annotation_custom(img, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    ylim(-5,4)




######## por receptores
clean %>%
    filter(family == "HTR", 
           `Pharmacological action` %in% c("Yes", "Unknown")) %>%
    ggplot(aes(y = drug_name, x = 0, color = Actions, shape = Actions)) +
    geom_point(size = 3) +
    facet_grid(~ `Gene Name`) +
    scale_colour_viridis_d() +
    theme_cowplot()
    

