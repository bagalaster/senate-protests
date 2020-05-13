# A script to get senate classes

# Author: Mac Bagwell
# Version: 2020-02-26

# Libraries
library(tidyverse)
library(rvest)

# Parameters

url <- "https://en.wikipedia.org/wiki/115th_United_States_Congress#Senate"
file_out <- here::here("c01-own/data/senate-class.rds")

last_name_recode <- 
  c(
    "Smith" = "Hyde-Smith",
    ""
  )

get_last_names_from_ul <- function(node) {
  node %>% 
    html_children() %>% 
    map(get_last_names_from_li) %>% 
    unlist(recursive = FALSE)
}

get_last_names_from_li <- function(node) {
  node %>% 
    html_text() %>% 
    str_extract_all("\\w+(?=( Jr.)? \\(.*\\))")
}

get_classes_from_ul <- function(node) {
  node %>% 
    html_children() %>% 
    html_text() %>% 
    str_extract("^\\d") %>% 
    as.integer()
}
#===============================================================================

nodes <- 
  url %>% 
  read_html() %>% 
  html_node(".mw-parser-output > div:nth-child(62) > table:nth-child(1)")
  
# list(
#   last_name = 
#     list(nodes = nodes %>% html_nodes("td > ul"), fun = get_last_names_from_ul),
#   state = 
#     list(
#       nodes = 
#         nodes %>% 
#         html_nodes("h4 > span.mw-headline") %>% 
#         html_attr("id") %>% 
#         str_replace_all("_", " "), 
#       fun = identity
#     ),
#   class = 
#     list(nodes = nodes %>% html_nodes("td > ul"), fun = get_classes_from_ul)
# ) %>% 
#   map2_dfc(~ map_chr(.$nodes, .$fun)) %>% 
#   drop_na() %>% 
#   write_rds(file_out)

nodes %>% 
  html_nodes("td > ul") %>% 
  map(get_classes_from_ul) %>% 
  set_names(state.abb) %>% 
  transpose() %>% 
  unlist(recursive = FALSE) %>% 
  enframe(name = "state", value = "class") %>% 
  mutate(
    class = map_int(class, unlist),
    last_name = 
      nodes %>% 
      html_nodes("td > ul") %>% 
      map(get_last_names_from_ul) %>% 
      transpose() %>% 
      unlist(recursive = FALSE) 
  ) %>% 
  unnest(cols = c(last_name)) %>% 
  mutate(
    last_name = 
      case_when(
        state == "MS" & last_name == "Smith" ~ "Hyde-Smith",
        state == "MD" & last_name == "Hollen" ~ "Van Hollen",
        state == "NV" & last_name == "Masto" ~ "Cortez Masto",
        TRUE ~ last_name
      )
  ) %>% 
  write_rds(file_out)
   
