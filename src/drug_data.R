pacman::p_load(tidyverse, xml2)



test <- read_xml("data/test.xml")


xml_structure(test)
xml_find_all(test, "//drug")

xml_find_all(xml_child(test), ".//name")

 ll <- as_list(xx)

 
xml_find_all(xx, "*")
