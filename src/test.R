pacman::p_load(tidyverse)

mydata <- 
    read.csv("data/KiDatabase.csv")



# list of drugs of interest
ap <- c("OLANZAPINE", "ARIPIPRAZOLE")

tt <- 
mydata %>%
    filter(Ligand.Name %in% ap,
           species == "HUMAN",
           ki.Note != ">") %>%
    select(Name, Ligand.Name, ki.Val, source) %>%
    unique() %>%
    group_by(Ligand.Name, Name) %>%
    summarise(Ki_mean = mean(ki.Val)) %>%
    mutate(pKi = -log10(Ki_mean) + 9)




tt$Name %>%
    stringr::str_replace_all(pattern = ".*5*-HT", replacement = "5HT")




