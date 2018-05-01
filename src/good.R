pacman::p_load(tidyverse)

# Drugbank_converted.csv is a query of `data/fulldatabase.xml`
# The query is 'data/basex/to_csv.xq"
drugbank <-
    read_delim("data/drugbank_converted.csv", delim = ";",
               col_names = c("name", "SMILES", "half", "atc_code", "receptor", "gene", "action", "known"))


# Selected drug is a manual list of psychiatric drugs"
selected_drugs <- 
    read_csv("data/clean_data/drugs.csv")

diccionary <- 
    read.csv("data/clean_data/receptor_dic.csv")

binding_data <- 
    read_csv("data/clean_data/bind.csv") %>%
    mutate(target = str_to_lower(target)) %>%
    left_join(diccionary)

drugs <- 
drugbank %>%
    filter(name %in% selected_drugs$name) %>%
    mutate(receptor = str_to_lower(receptor))


receptors <- 
drugs %>%
    select(receptor,gene) %>%
    distinct()



###############
tt <-
    binding_data %>%
    filter(str_detect(species, "Homo"),
           type == "Ki")






