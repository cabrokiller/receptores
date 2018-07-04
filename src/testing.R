pacman::p_load(rvest, tidyverse, ggthemes, png, RCurl, grid, magick, rsvg)


get_enzymes <- function(query){
    drugs <- 
        read_csv("data/joined.csv") %>%
        filter(!str_detect(`Common name`, pattern = '.*[0-9].*')) %>%
        mutate(Name = tolower(`Common name`)) %>%
        select(Name, `DrugBank ID`, chembl_id)
    match <- agrep(pattern = query, x = drugs$Name, value = T)[1]
    
    selection <- 
        drugs %>%
        filter(Name == match)
    url <- paste0("https://www.drugbank.ca/drugs/", pull(selection, `DrugBank ID`))
    
    DB_df <- 
        url %>%
        read_html() %>%
        html_node(xpath = '//*[@id="drug-moa-target-table"]') %>%
        html_table() %>%
        separate(Target, into = c("Activity", "Target"), sep = 1) %>%
        mutate(Target = factor(Target, levels = .$Target),
               Activity = factor(Activity, levels = c("A", "U", "N")),
               Name = selection$Name,
               DrugBank_ID = selection$`DrugBank ID`,
               Chembl_ID = selection$chembl_id)
    
    return(DB_df)
}




# download olanzapine html
test <- 
    url("https://www.drugbank.ca/drugs/DB00334") %>%
    read_html() %>%
    html_nodes(css ='.bond-list')

x = 1 #1 = target, 2 = enzymes
y = 1


name <- 
    xml_child(test[[x]], y) %>%
    html_nodes('strong') %>%
    html_text()


keys <-
    xml_child(test[[x]], y) %>%
    html_nodes('dt') %>%
    html_text() %>%
    tibble(keys = .) %>%
    mutate(values = c(
        xml_child(test[[x]], y) %>%
        html_nodes('dd') %>%
        html_text()))

# try
binding <- 
xml_child(test[[x]], y) %>%
    html_nodes('.table') %>%
    html_table() %>%
    .[[1]]


binding %>%
    select(col = 1, value = 2) %>%
    group_by(col) %>%
    summarise(min = min(value),
              max = max(value),
              med = median(value)) %>%
    gather(key, value, -col)










