pacman::p_load(rvest, tidyverse)

drugs <- 
    read_csv("data/drug links.csv") %>%
    mutate(Name = tolower(Name))

get_activity <- function(DB_id){
    url <- paste0("https://www.drugbank.ca/drugs/", DB_id)
    url %>%
        read_html() %>%
        html_node(xpath = '//*[@id="drug-moa-target-table"]') %>%
        html_table() %>%
        separate(Target, into = c("Activity", "Target"), sep = 1)
    }


get_DB_id <- function(query){
    match <- agrep(pattern = query, x = drugs$Name, value = T)[1]
    drugs %>%
        filter(Name == match) %>%
        select(`DrugBank ID`, Name)
        #pull(`DrugBank ID`)
    }


get_DB_id("Aripiprazole")




