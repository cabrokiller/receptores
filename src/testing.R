pacman::p_load(tidyverse)

#source("src/preproc.R")
clean <- read_csv("data/clean.csv")

molecule <-
    c("Aripiprazole", "Lurasidone", "Clozapine")

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


### test for fa
my_symbols <- 
fontawesome(c("fa-caret-up", "fa-caret-down", "fa-caret-square-o-up", "fa-caret-square-o-down"))






ggplot(for_plot, aes(x = `Pharmacological action`, y = receptor, shape = symbol_2, color = log(Ki))) +
geom_point(size = 
               8) +
scale_shape_manual(values = my_symbols) +
    scale_size(range = c(10,20)) +
    scale_color_viridis_c(direction = -1, na.value = "gray40", option = "B") +
    facet_grid(family ~ drug_name, scales = "free_y", space = "free_y") +
    ggthemes::theme_solarized_2(light = F) +
    theme(text = element_text(family = "serif"))




######## 



