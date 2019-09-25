pacman::p_load(rvest, tidyverse)

drugs <- read_csv("data/drugs.csv")

target_node <- function(drug) {
    paste0("https://www.drugbank.ca/drugs/", pull(drug, drugbank_id)) %>%
        html_session() %>%
        read_html() %>%
        html_node(css = '.bond-list-container.targets') %>%
        html_nodes(css = '.bond-list')
}

get_target <- function(child_node, y){
    drug <- 
        xml_child(child_node[[1]], y) %>%
        html_nodes('strong') %>%
        html_text()
    
    vals <- 
        xml_child(child_node[[1]], y) %>%
        html_nodes('dd') %>%
        html_text()
    
    cols <-
        xml_child(child_node[[1]], y) %>%
        html_nodes('dt') %>%
        html_text()
    
    binding <-
        try(
            xml_child(child_node[[1]], y) %>%
                html_nodes('.table-responsive table') %>%
                html_table() %>%
                .[[1]] %>%
                select(col = 1, value = 2) %>%
                mutate(value = str_remove(value, pattern = '>'),
                       value = as.numeric(value)) %>%
                group_by(col) %>%
                summarise(min = min(value),
                          max = max(value),
                          med = median(value)) %>%
                gather(key, value, -col) %>%
                unite(key, col, key) %>%
                arrange(key) %>%
                spread(key, value),
            silent = T)

    df <- 
        tibble(cols, vals, Name = drug) %>%
        spread(cols, vals)
    df

    if(class(binding)[1] != "try-error"){
        bind_cols(df, binding)} else {
            df
        }
}



get_targets <- function(df) {
    
    # catch error
    t <- try(xml_contents(df[[1]]), silent = T)
    if("try-error" %in% class(t)){
        d <- tibble(NA)
        return(d)
    }
    # get targets
    map_df(1:length(xml_contents(df[[1]])), function(x)
        get_target(df, y = x)) %>%
        separate(Name, into = c("on", "Name"), extra = "merge") %>%
        select(-on)
}



process <- function(name){
    get_selection <- function(match){
        selection <- 
            drugs %>%
            filter(name == match)
        return(selection)
    }
    aa <- get_selection(name)
    aa %>%
        target_node() %>%
        get_targets() %>%
        mutate(drug_name = aa$name,
               drugbank_id = aa$drugbank_id)}


get_all_drugs <- function(df){
    df %>%
        pull(name) %>%
        map_df(function(x) process(x))
}



# do scraping !!
targets <- 
drugs %>%
    get_all_drugs()

write_csv(targets, "data/drugbank_target_parse.csv")


