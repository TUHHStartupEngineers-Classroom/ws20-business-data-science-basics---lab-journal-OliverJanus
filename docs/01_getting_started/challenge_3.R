library(tidyverse)
library(rvest)
library(glue)
library(stringr)

home_url <- "https://www.radon-bikes.de"
html <- home_url %>% 
  read_html()

# fetch the links to all the availabe categories and store it in a tibble with family and category
category_url_tbl <- html %>%
  html_nodes(css = ".megamenu__item > a") %>%
  html_attr('href') %>%
  discard(.p = ~stringr::str_detect(.x,"wear")) %>%
  enframe(name = "position", value = "category_path") %>%
  mutate(category_url = glue(home_url, "{category_path}bikegrid/")) %>% # display all models: add "/bikegrid/" to category url
  mutate(category_path = str_remove_all(.$category_path,"(?<!.)/|/(?!.)")) %>%
  separate(col = category_path, into = c("family","category"), sep = "/")

# fetch the links to all available bikes and store it in a tibble with respective family, category and category-link
bike_url_tbl <- category_url_tbl$category_url %>%
  map(read_html) %>%
  map(html_nodes, css = ".m-bikegrid__grid > div a") %>%
  map(html_attr, 'href') %>%
  map(discard, .p = ~stringr::str_detect(.x,"frame")) %>%
  map(enframe, name = "position", value = "bike_path") %>%
  enframe(name = "position", value = "category_tbl") %>%
  right_join(category_url_tbl, by = "position") %>%
  select(-c("position")) %>%
  unnest(cols = c("category_tbl")) %>%
  select(-c("position")) %>%
  distinct() %>%
  mutate(bike_url = map(.$bike_path, ~str_glue(home_url, "{.}")))


bike_info_lst <- bike_url_tbl$bike_url %>%
  map(read_html) %>%
  map(html_nodes, css = ".m-bikedetail__breadcrumb > ul > li > span > a") %>%
  map(html_text)

bike_price_lst <- bike_url_tbl$bike_url %>%
  map(read_html) %>%
  map(html_nodes, css = ".m-bikedetail__price--active") %>%
  map(html_text) %>%
  map(str_extract, "[0-9]+") %>%
  map(na.omit) %>%
  map(~str_glue("{.} €"))


bike_info_tbl <- tibble(Info = bike_info_lst, Price = bike_price_lst, Url = bike_url_tbl$bike_url) %>%
  mutate(Info_str = map(.$Info, toString) %>% str_remove_all("[\"c()]")) %>%
  separate(col = Info_str, into = c("Family", "Category", "Series", "Name"), sep = ", ") %>%
  select(Name, Price, Series, Category, Family, Url)