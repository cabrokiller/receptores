pacman::p_load(tidyverse)

data_full <-read_csv("data/drugbank_target_parse.csv")

clean_data <-
    data_full %>%
    mutate(
        Actions = case_when(
            Actions == "AntagonistAgonist" ~ "Antagonist",
            Actions %in% c("AntagonistLigand", "AntagonistBinder", "Inverse agonist") ~ "Antagonist",
            Actions %in% c("Ligand", "Activator") ~ "Agonist",
            Actions %in% c("Other", "Unknown", "AntagonistOther/unknown")  ~ "Other/unknown",
            Actions %in% c("Blocker", "Inhibitor", "InhibitorInducer", "Negative modulator", "Binder") ~ "Blocker/inhibitor",
            Actions == "AntagonistPartial agonist" ~ "Partial antagonist",
            Actions == "AgonistPartial agonist" ~ "Partial agonist",
            Actions == "AntagonistPartial agonistLigand" ~ "Partial antagonist",
            Actions == "Positive allosteric modulator" ~ "Allosteric mod (+)",
            is.na(Actions) ~ "Other/unknown",
            T ~ Actions
        ),
        receptor = str_remove(.$`Name`, pattern = "receptor"),
        family = str_extract(.$`Gene Name`, pattern = "[:upper:]+"),
        family = ifelse(is.na(family), "NE", family),
        symbol = case_when(
            Actions == "Agonist" ~ "triangle-up",
            Actions == "Antagonist" ~ "triangle-down",
            Actions == "Blocker/inhibitor" ~ "square-x",
            Actions == "Other/unknown" ~ "circle-dot",
            Actions == "Partial agonist" ~ "hexagram",
            Actions == "Partial antagonist" ~ "hexagram-dot",
            Actions == "Inverse agonist" ~ "triangle-down-dot",
            Actions == "Binder" ~ "diamond-wide",
            Actions == "Potentiator" ~ "triangle-up-dot",
            Actions == "Allosteric mod (+)" ~ "triangle-up",
            TRUE ~ "circle-open"
        ),
        potency = 10 - log10(`Ki (nM)_med`),
        show_text = str_wrap(.$full_function, width = 60),
    ) %>%
    select(drug_name, Name, receptor, Actions, `Gene Name`, `Pharmacological action`, potency, family, symbol, show_text,
           Ki = `Ki (nM)_med`, Ki_min = `Ki (nM)_min`, Ki_max = `Ki (nM)_max`)

write_csv(clean_data, "data/clean.csv")
