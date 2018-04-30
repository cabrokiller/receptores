pacman::p_load(tidyverse)

data <-
    read_csv("data/part.csv", 
             col_names = c("name", "half", "receptor", "gene", "action", "known"))
