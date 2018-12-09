pacman::p_load(rvest, tidyverse)

drugs <- read_csv("data/drugs.csv")


get_selection <- function(query){
    match <- agrep(pattern = query, x = drugs$name, value = T)[1]
    selection <- 
        drugs %>%
        filter(name == match)
    return(selection)
}
get_nodes <- function(drug){
    df <- 
        url(paste0("https://www.drugbank.ca/drugs/", pull(drug, drugbank_id))) %>%
        read_html() %>%
        html_nodes(css ='.bond-list')
    return(df)
}
get_targets_df <- function(df, section) {
    get_target <- function(node, x, y){
        drug <- 
            xml_child(node[[x]], y) %>%
            html_nodes('strong') %>%
            html_text()
        vals <- 
            xml_child(node[[x]], y) %>%
            html_nodes('dd') %>%
            html_text()
        cols <-
            xml_child(node[[x]], y) %>%
            html_nodes('dt') %>%
            html_text()
        binding <- 
            try(
                xml_child(node[[x]], y) %>%
                    html_nodes('.table.table-sm.table-responsive') %>%
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
        if(class(binding)[1] != "try-error"){
            bind_cols(df, binding)} else {
                df
            }
    }
    
    # catch error
    t <- try(xml_contents(df[[section]]), silent = T)
    if("try-error" %in% class(t)){
        d <- tibble(NA)
        return(d)
    }
    # get targets
    map_df(1:length(xml_contents(df[[section]])), function(x)
        get_target(df, x = section, y = x)) %>%
        separate(Name, into = c("on", "Name"), extra = "merge") %>%
        select(-on)
}
do_all <- function(query, section = 1){
    sel <- get_selection(query)
    sel %>%
        get_nodes() %>%
        get_targets_df(section) %>%
        mutate(Drug = sel$name,
               drugbank_id = sel$drugbank_id)
}

get_all_drugs <- function(df, section){
    df %>%
        pull(name) %>%
        map_df(function(x) do_all(x, section))
}

targets <-
    drugs %>%
    slice(5) %>%
    get_all_drugs(section = 1)







    
    
drugs[1:2,] %>%
    map_df(function(x) {x %>% 
        get_nodes() %>%
        get_targets_df(1)
        mutate(Drug = .$name,
               drugbank_id = .$drugbank_id)})










write_csv(targets, "data/drugbank_target_parse.csv")

enzymes <-
    drugs %>%
    get_all_drugs(section = 2)

enzymes %>%
    select(-'NA') %>%
    write_csv("data/drugbank_enzyme_parse.csv")

