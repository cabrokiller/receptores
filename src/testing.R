pacman::p_load(tidyverse)

#source("src/preproc.R")
clean <- read_csv("data/clean.csv")

molecule <-
    c("Aripiprazole", "Lurasidone", "Cariprazine")

my_family <- 
    c("DRD", "HTR")


for_plot <- 
    clean %>%
    filter(
        drug_name %in% molecule,
        family %in% my_family,
        `Pharmacological action` %in% c("Yes", "Unknown")
    )


my_symbols <- 
    distinct(for_plot, symbol_2, .keep_all = T) %>%
    arrange(Actions) %>%
    pull(symbol_2)




ggplot(for_plot) +
geom_point(aes(x = drug_name, y = receptor)) +
    

scale_shape_manual(values = my_symbols) +
    scale_size(range = c(10,20)) +
    scale_color_viridis_c(direction = -1, na.value = "gray40", option = "B") +
    facet_grid(family ~ `Pharmacological action`, scales = "free_y", space = "free_y") +
    theme_minimal_grid()


######## 



