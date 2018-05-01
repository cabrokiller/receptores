pacman::p_load(tidyverse, xml2, httr)

selected_drugs <- 
    read_csv("data/clean_data/drugs.csv")

# get info from binddb
# Save username as variable
# http://bindingdb.org/axis2/services/BDBService/getTargetByCompound?smiles={SMILES}&cutoff={similarity_cutoff}

url <- 'http://bindingdb.org/axis2/services/BDBService/getTargetByCompound'

binding_data <- 
    lapply(selected_drugs$SMILES, function(x){ 
        paste0(url,'?smiles=',x,'&cutoff=0.9') %>%
            GET() %>%
            content() %>%
            xml_find_all("//bdb:affinities") %>%
            lapply(function(y) data.frame(
                SMILES = x,
                target  = xml_text(xml_find_all(y, ".//bdb:target")),
                species = xml_text(xml_find_all(y, ".//bdb:species")),
                type = xml_text(xml_find_all(y, ".//bdb:affinity_type")),
                value = xml_text(xml_find_all(y, ".//bdb:affinity")))) %>%
            do.call("rbind", .)
       }
    ) %>%
    do.call("rbind", .)

binding_data %>%
    merge(selected_drugs) %>%
    select(name, SMILES, target, species, type, value) %>%
    write_csv("data/clean_data/bind.csv")
