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
    map_df(1:length(xml_contents(df[[section]])), function(x)
        get_target(df, x = section, y = x)) %>%
        separate(Name, into = c("on", "Name"), extra = "merge") %>%
        select(-on) %>%
        mutate(
            Actions = str_replace(.$Actions, pattern = '([:upper:][:lower:]+)([:upper:][:lower:]+)',
                                  '\\1 - \\2')
        )
}

do_all <- function(query, section = 1){
    sel <- get_selection(query)
    sel %>%
        get_nodes() %>%
        get_targets_df(section) %>%
        mutate(Drug = sel$name,
               drugbank_id = sel$drugbank_id)
}



targets <-
    pull(drugs, name) %>%
    map_df(function(x) do_all(x,1))

write_csv(targets, "data/drugbank_target_parse.csv")



enzymes <-
    drugs %>%
    filter(!(name %in% c("Paliperidone", "Varenicline", "Lithium", "Fludiazepam", "Lormetazepam"))) %>%
    pull(name) %>%
    map_df(function(x) do_all_enzyme(x,2))



write_csv(enzymes, "data/drugbank_enzyme_parse.csv")


