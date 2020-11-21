pacman::p_load(tidyverse)

data_full <-read_csv("data/drugbank_target_parse.csv")

clean_data <- 
    data_full %>%
    mutate(Actions_mod = str_replace(Actions, '\\B([[:upper:]])', '_\\1')) %>%
    separate(Actions_mod, c("A", "B", "C"), sep = '_', fill = "right") %>%
    mutate(
        A = ifelse(Actions == "Other/unknown", "Unknown", A),
        symbol = case_when(
            A == "Agonist" ~ '\u2bc5',
            A == "Antagonist" ~ '\u2bc6',
            A == "Inhibitor" ~ '\u2bbd',
            A == "Binder" ~ '\u2b2c',
            A == "Blocker" ~ '\u2b59',
            A == "Negative modulator" ~ '\u2b8b',
            A == "Stimulator" ~ '\u2b99',
            A == "Inverse agonist" ~ '\u2b9f',
            A == "Ligand" ~ '\u2b58',
            A == "Activator" ~ '\u2b06',
            A == "Other" ~ '\u2b57',
            A == "Unknown" ~ '\u2047',
            A == "Partial agonist" ~ '\u2b1f',
            A == "Partial antagonist" ~ '\u2bc2',
            A == "Inverse agonist" ~ '\u2b9f',
            A == "Inducer" ~ '\u25c6',
            A == "Potentiator" ~ '\u2bed',
            A == "Positive allosteric modulator" ~ '\u2b89',
            A == "Modulator" ~ '\u2b14',
            is.na(A) ~ '\u2cc'
        ),
        receptor = str_remove(.$`Name`, pattern = "receptor"),
        family = str_extract(.$`Gene Name`, pattern = "[:upper:]+"),
        family = ifelse(is.na(family), "NE", family),
        potency = 10 - log10(`Ki (nM)_med`),
        show_text = str_wrap(.$full_function, width = 60),
    ) %>%
    select(drug_name, Name, receptor, Actions = A, `Gene Name`, `Pharmacological action`,
           potency, family, symbol, show_text,
           Ki = `Ki (nM)_med`, Ki_min = `Ki (nM)_min`, Ki_max = `Ki (nM)_max`,
           Kind, weight = `Molecular Weight`, comment = `Curator comments`)

write_csv(clean_data, "data/clean.csv")


