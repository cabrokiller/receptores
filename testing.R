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


tt <- 
'https://www.drugbank.ca/drugs/DB00334' %>%
    url %>%
    read_html() %>%
    html_node(xpath = '/html/body/main/div/div[4]/div[2]/div')
    
    
    
    
    separate(Target, into = c("Activity", "Target"), sep = 1) %>%
    mutate(Target = factor(Target, levels = .$Target),
           Activity = factor(Activity, levels = c("A", "U", "N")))



# download html
html <- getURL("https://www.drugbank.ca/drugs/DB00334")

# parse html
doc = XML::htmlParse(html, asText=TRUE)

plain.text <- XML::xpathSApply(doc, "/html/body/main/div/div[4]/div[2]/div", XML::xmlValue)

cat(paste(plain.text, collapse = " "))

//*[@id="BE0002433"]/div[2]
