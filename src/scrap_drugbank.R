require(pacman)
pacman::p_load(rvest, tidyverse, ggthemes)

drugs <- 
    read_csv("data/drug links.csv") %>%
    mutate(Name = tolower(Name))

get_activity <- function(name){
    match <- agrep(pattern = name, x = drugs$Name, value = T)[1]
    drugs %>%
        filter(Name == match) %>%
        pull(`DrugBank ID`) %>%
        paste0("https://www.drugbank.ca/drugs/", .) %>%
        read_html() %>%
        html_node(xpath = '//*[@id="drug-moa-target-table"]') %>%
        html_table() %>%
        separate(Target, into = c("Activity", "Target"), sep = 1) %>%
        mutate(Target = factor(Target, levels = .$Target),
               Name = match)
    }


make_plot <- function(drugbank_df){
    drugbank_df %>%
        ggplot(aes(x=fct_rev(Target), y = Actions, color = Activity, group = Activity))+
        geom_line() +
        geom_point(size=3) +
        coord_flip() +
        theme_solarized_2(light = F) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
        labs(x="Receptor", title = drugbank_df$Name[1])
    }


    
get_activity("fluoxwt") %>%
    make_plot()
