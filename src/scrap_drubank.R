pacman::p_load(rvest, tidyverse, ggthemes, png, RCurl, grid, magick, rsvg)


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
        distinct(DrugBank_ID) %>%
        pull(DrugBank_ID) %>%
        paste0('https://www.drugbank.ca/structures/', ., '/image.svg') %>%
        image_read_svg() %>%
        image_colorize(opacity = 100, color = '#839496')
    
    DB_df %>%
        ggplot(aes(x=fct_rev(Target), y=Actions, color=Activity)) + 
        annotation_custom(rasterGrob(mol_img)) +
        geom_point(size=5, shape=15, alpha = .8) +
        coord_flip() +
        scale_color_manual(values = solarized_pal("red")(3)) +
        labs(title = str_to_title(DB_df$Name), x = "")
        }


get_activity("vortioxetin") %>%
    do_activity_plot()




ozp %>%
    ggplot(aes(x=fct_rev(Target), y=Activity, fill=Actions)) + 
    annotation_custom(rasterGrob(mol_img)) +
    geom_bin2d(alpha = .6) +
    coord_flip() + 
    #geom_point(size=5, shape=15, alpha = .8) +
    #geom_line(aes(group = Activity), size = 2, alpha = .8) +
    scale_fill_manual(values = solarized_pal("red")(3)) +
    labs(title = str_to_title(ozp$Name), x = "", y = "")

