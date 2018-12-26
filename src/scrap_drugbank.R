pacman::p_load(rvest, tidyverse)

drugs <- read_csv("data/drugs.csv")

get_selection <- function(query){
    match <- agrep(pattern = query, x = drugs$name, value = T)[1]
    selection <- 
        drugs %>%
        filter(name == match)
    return(selection)
}
get_nodes <- function(drugbank_id){
    df <- 
        paste0("https://www.drugbank.ca/drugs/", drugbank_id) %>%
        html_session() %>%
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
        
        link <- 
            xml_child(node[[x]], y) %>%
            html_nodes('strong a') %>%
            html_attr('href')
        
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
            tibble(cols, vals, Name = drug, Link = ifelse(is.null(link), NA, link)) %>%
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

do_all <- function(query, section){
    sel <- get_selection(query)
    sel %>%
        pull(drugbank_id) %>%
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


# Scrap!!
targets <-
    drugs %>%
    get_all_drugs(section = 1)

enzymes <-
    drugs %>%
    get_all_drugs(section = 2)



get_func <- function(link) {
    if(is.na(link)) {
        return(NA)
    } else {
        nom <-
            paste0("https://www.drugbank.ca", link) %>%
            html_session() %>%
            read_html()
        
        func <-
            bind_cols(
                key =
                    nom %>%
                    html_nodes(css = "dt") %>%
                    html_text(),
                value =
                    nom %>%
                    html_nodes(css = "dd") %>%
                    html_text()
            ) %>%
            as.data.frame() %>%
            filter(key == "Specific Function") %>%
            pull(value)
        
        ifelse(is.null(func), NA, func)
    }
}


new_targets <- 
targets %>%
    rowwise() %>%
    mutate(full_function = get_func(Link))


new_enzymes <- 
    enzymes %>%
    rowwise() %>%
    mutate(full_function = get_func(Link))



write_csv(new_targets, "data/drugbank_target_parse.csv")

enzymes %>%
    select(-'NA') %>%
write_csv("data/drugbank_enzyme_parse.csv")


