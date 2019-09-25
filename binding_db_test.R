pacman::p_load(tidyverse)


drugs <- read_csv("data/drugs.csv")

x <- 
    read_tsv("data/BindingDB_All.tsv")


y <- 
x %>%
    filter(`DrugBank ID of Ligand` %in% drugs$drugbank_id) %>%
    filter(`Target Source Organism According to Curator or DataSource` == 'Homo sapiens',
           !is.na(`Ki (nM)`)) %>%
    left_join(drugs, by = c("DrugBank ID of Ligand" = "drugbank_id"))


y %>%
    filter(name == "Aripiprazole") %>%
    select(name,
           target = `Target Name Assigned by Curator or DataSource`,
           ki = `Ki (nM)`, 
           target2 =  `UniProt (SwissProt) Recommended Name of Target Chain`) %>%
    mutate(potency = 100-log(as.numeric(ki))) %>%
    group_by(target2) %>%
    
    # plot
    ggplot(aes(target2, potency, group = 1)) +
    geom_line() +
    coord_polar()
    
