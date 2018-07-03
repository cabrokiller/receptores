pacman::p_load(rvest, tidyverse, ggthemes, png, RCurl, grid)


get_activity <- function(query){
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
    

do_activity_plot <- function(DB_df){
    mol_img <- 
        DB_df %>%
        distinct(Chembl_ID) %>%
        pull(Chembl_ID) %>%
        paste0('https://www.ebi.ac.uk/chembl/api/data/image/', .) %>%
        getURLContent() %>%
        readPNG()
    
    DB_df %>%
        ggplot(aes(x=fct_rev(Target), y=Actions, color=Activity)) + 
        geom_point(size=5, shape=15) +
        coord_flip() +
        scale_color_manual(values = solarized_pal("red")(3)) +
        labs(title = DB_df$Name, x = "") +
        theme_solarized_2(light = F) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
        annotation_custom(rasterGrob(mol_img))
}

"haloperidol" %>%
    get_activity(.) %>%
    do_activity_plot(.)


