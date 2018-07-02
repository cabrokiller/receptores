pacman::p_load_gh("rajarshi/chemblr/package")
pacman::p_load(tidyverse, RCurl, rJava, RJSONIO)


library(rJava)
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


tt <- 
drugs %>%
    select(inchi) %>%
    distinct() %>%
    mutate(chembId = chemblr::get.compounds(inchi, type = "stdinchi")["chemblId"])





####

chemblr::get.compounds(drugs$inchi[1], type="stdinchi")["chemblId"]

