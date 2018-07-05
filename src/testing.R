pacman::p_load(rvest, tidyverse, ggthemes, png, RCurl, grid, magick, rsvg)



drug <- "DB00334"
df <- 
    url(paste0("https://www.drugbank.ca/drugs/", drug)) %>%
    read_html() %>%
    html_nodes(css ='.bond-list')



tt <- 
    map_df(1:length(xml_contents(df[[1]])), function(x) get_target(df, x=1, y= x))



get_target <- function(node, x = 1, y = 1){
    name <- 
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
            group_by(col) %>%
            summarise(min = min(value),
                      max = max(value),
                      med = median(value)) %>%
            gather(key, value, -col) %>%
            unite(key, col, key) %>%
            arrange(key) %>%
            spread(key, value)
        )

    df <- 
        tibble(cols, vals, Name = name) %>%
        spread(cols, vals)

    if(class(binding)[1] != "try-error"){
        bind_cols(df, binding)} else {
            df
    }
}
