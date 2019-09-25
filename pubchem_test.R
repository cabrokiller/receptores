pacman::p_load(tidyverse, rvest, V8)
drugs <- read_csv("data/drugs.csv")

# get binding info
cid <- '135398745'
url_csv <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/", cid,"/assaysummary/CSV")



x <- read_csv(url_csv)


y <-
x %>%
    filter(`Bioactivity Outcome` == "Active",
           `Activity Name` == "Ki")



read_csv(paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/", cid, "/CSV"))



inchi_key <- 
    drugs$inchi[1]


# GET CID FROM INCHI
read.csv(paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/inchikey/", inchi_key, "/cids/TXT")) -> z


get_cid <- function(inchi_key){
    read_csv(paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/inchikey/", inchi_key, "/cids/TXT"))
        
}

############

get_inchi <- function(db_id){
    inchi <- 
        get_general_info(db_id) %>%
        filter(key == "InChI Key") %>%
        pull(value)
    return(inchi)
}


database <- 
    drugs %>%
    rowwise() %>%
    mutate(inchi = get_inchi(drugbank_id))




get_general_info <- function(db_id) {
    url_did <- paste0("https://www.drugbank.ca/drugs/", db_id)
    nodes <-
        url_did %>%
        html_session() %>%
        read_html() %>%
        html_node('dl')
    key <-
        nodes %>%
        html_nodes('dt') %>%
        html_text()
    value <-
        nodes %>%
        html_nodes('dd') %>%
        html_text()
    tibble(key, value)
}
get_actions <- function(db_id) {
    # Get actions table from db_id
    url_did <- paste0("https://www.drugbank.ca/drugs/", db_id)
    url_did %>%
        html_session() %>%
        read_html() %>%
        html_node(xpath = '//*[@id="drug-moa-target-table"]') %>%
        html_table()
}




