pacman::p_load(tidyverse)

# Drugbank_converted.csv is a query of `data/fulldatabase.xml`
# The query is 'data/basex/to_csv.xq"
# Drugbank_converted.csv is a query of `data/fulldatabase.xml`
# The query is 'data/basex/to_csv.xq"
drugbank <-
    read_delim("data/drugbank_converted.csv", delim = ";",
               col_names = c("name", "inchi", "receptor", "gene", "action", "known"))

# Selected drug is a manual list of psychiatric drugs"
selected_drugs <- 
    read_csv("data/clean_data/drugs.csv")

drugs <- 
    drugbank %>%
    filter(name %in% selected_drugs$name) %>%
    mutate(receptor = str_to_lower(receptor))

drugs %>%
    filter(name == "Amphetamine") %>%
    ggplot(aes(gene, known, group = name, fill = known)) +
    geom_bar(stat = "Identity") + 
    coord_flip()

