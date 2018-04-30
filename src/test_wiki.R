pacman::p_load(tidyverse, rvest, scales)

OZP <- 
    "https://en.wikipedia.org/wiki/Olanzapine" %>%
    read_html() %>%
    html_nodes(css='.wikitable') %>%
    .[[1]] %>%
    html_table(trim = T, fill = T, dec = ".")%>%
    head(-1) %>% select(1:3) %>%
    filter(!stringr::str_detect(string = `Ki (nM)`, pattern = ">|≥")) %>%
    mutate_all(funs(replace(., . == "ND", NA))) %>%
    mutate(`Ki (nM)` = stringr::str_replace_all(string = `Ki (nM)`, pattern = ",", replacement = "")) %>%
    filter(Site != "SERT") %>%                                                  ## ONLY IN WINDOWS
    separate(`Ki (nM)`, into = c("Ki_inf", "Ki_sup"), sep = "–", convert = T) %>%
    mutate(Ki_mean = rowMeans(.[2:3], na.rm = T)) %>%
    mutate(pKi = -log10(`Ki_mean`) + 9)
    

AZP <- 
    "https://en.wikipedia.org/wiki/Asenapine" %>%
    read_html() %>%
    html_nodes(css='.wikitable') %>%
    .[[1]] %>%
    html_table(trim = T, fill = T, dec = ".") %>%
    head(-1)


RSP <- 
    "https://en.wikipedia.org/wiki/Risperidone" %>%
    read_html() %>%
    html_nodes(css='.wikitable') %>%
    .[[3]] %>%
    html_table(trim = T, fill = T, dec = ".") %>%
    mutate(`Ki (nM)` = stringr::str_replace_all(string = `Ki (nM)`, pattern = ",", replacement = "")) %>%
    head(-1) %>%
    mutate(`Ki (nM)` = as.numeric(`Ki (nM)`),
           pKi2 = -log10(`Ki (nM)`) + 9)


QTP <- 
    "https://en.wikipedia.org/wiki/Quetiapine" %>%
    read_html() %>%
    html_nodes(css='.wikitable') %>%
    .[[3]] %>%
    html_table(trim = T, fill = T, dec = ".") %>%
    head(-1) %>% select(c(1:2,4)) %>%
    filter(!stringr::str_detect(string = QTP, pattern = ">|≥")) %>%
    mutate_all(funs(replace(., . == "ND", NA))) %>%
    mutate(QTP = stringr::str_replace_all(string = QTP, pattern = ",", replacement = "")) %>%
    separate(QTP, into = c("Ki_inf", "Ki_sup"), sep = "–", convert = T) %>%
    mutate(Ki_mean = rowMeans(.[2:3], na.rm = T)) %>%
    mutate(pKi = -log10(`Ki_mean`) + 9)


ZPD <- 
    "https://en.wikipedia.org/wiki/Ziprasidone" %>%
    read_html() %>%
    html_nodes(css='.wikitable') %>%
    .[[1]] %>%
    html_table(trim = T, fill = T, dec = ".") %>%
    head(-1) %>% select((1:3)) %>%
    arrange(`Ki (nM)`) %>%
    tail(32) %>%
    mutate_all(funs(replace(., . == "ND", NA))) %>%
    mutate(`Ki (nM)` = stringr::str_replace_all(string = `Ki (nM)`, pattern = ",", replacement = "")) %>%
    separate(`Ki (nM)`, into = c("Ki_inf", "Ki_sup"), sep = "–", convert = T) %>%
    mutate(Ki_mean = rowMeans(.[2:3], na.rm = T)) %>%
    mutate(pKi = -log10(`Ki_mean`) + 9)


ARP <- 
    "https://en.wikipedia.org/wiki/Aripiprazole" %>%
    read_html() %>%
    html_nodes(css='.wikitable') %>%
    .[[3]] %>%
    html_table(trim = T, fill = T, dec = ".") %>%
    head(-1) %>% select((1:3)) %>%
    arrange(`Ki (nM)`) %>%
    tail(35) %>%
    mutate_all(funs(replace(., . == "ND", NA))) %>%
    mutate(`Ki (nM)` = stringr::str_replace_all(string = `Ki (nM)`, pattern = ",", replacement = "")) %>%
    separate(`Ki (nM)`, into = c("Ki_inf", "Ki_sup"), sep = "–", convert = T) %>%
    mutate(Ki_mean = rowMeans(.[2:3], na.rm = T)) %>%
    mutate(pKi = -log10(`Ki_mean`) + 9)


LSD <- 
    "https://en.wikipedia.org/wiki/Lurasidone" %>%
    read_html() %>%
    html_nodes(css='.wikitable') %>%
    .[[1]] %>%
    html_table(trim = T, fill = T, dec = ".") %>%
    head(-1) %>% select((1:3)) %>%
    arrange(`Ki (nM)`) %>%
    tail(15) %>%
    mutate_all(funs(replace(., . == "ND", NA))) %>%
    mutate(`Ki (nM)` = stringr::str_replace_all(string = `Ki (nM)`, pattern = ",", replacement = "")) %>%
    mutate(`Ki (nM)` = as.numeric(`Ki (nM)`),
           pKi2 = -log10(`Ki (nM)`) + 9)







# plot
    ggplot(aes(x = fct_reorder(Site, potency, desc = T), y = potency, fill = Action)) +
    geom_bar(stat = "Identity", width = 1) +
    scale_fill_brewer(type = "qual", palette = 2) +
    coord_flip() +
    labs(x="Receptor", y = "Potencia relativa") +
    scale_y_continuous(labels = percent)

              