library(rvest)
library(tidyverse)
library(jsonlite)
library(purrr)
library(furrr)     # Parallel Processing using purrr (iteration)
library(xopen) 
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

scrap_imdb_charts <- function(){
  url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
  html <- url %>% 
    read_html()
  
  
  rank <- html %>%
    html_nodes(css = ".titleColumn") %>%
    html_text() %>%
    stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)") %>%
    as.numeric()
  
  title <- html %>%
    html_nodes(css = ".titleColumn > a") %>%
    html_text()
  
  year <- html %>% 
    html_nodes(css = ".titleColumn > span") %>%
    html_text() %>%
    stringr::str_extract("[0-9]+") %>%
    as.numeric()
  
  people <- html %>% 
    html_nodes(css = ".titleColumn > a") %>%
    html_attr("title")
  
  rating <- html %>% 
    html_nodes(css = ".imdbRating > strong") %>%
    html_text() %>%
    as.numeric()
  
  num_ratings <-  html %>% 
    html_nodes(css = ".imdbRating > strong") %>%
    html_attr("title") %>%
    stringr::str_extract("(?<=based on ).*(?= user ratings)") %>% 
    stringr::str_replace_all(pattern = ",", replacement = "") %>%
    as.numeric()
  
  imbd_tibble <- tibble(rank, title, year, people, rating, num_ratings)
}


extract_color_attribute <- function(){
  bike_data_lst <- fromJSON("01_getting_started/exercise_datasets/bike_data.json") %>%
    purrr::pluck("productDetail", "variationAttributes", "values", 1, "displayValue")
  # same as: bike_data_lst[["productDetail"]][["variationAttributes"]][["values"]][[1]][["displayValue"]]
}


#==============================================================================================================================#

# Step 1

url  <- "https://www.canyon.com/en-de/"
html <- url %>% 
  read_html()

bike_family_tbl <- html %>%
  html_nodes(css = ".js-navigationDrawer__list--secondary") %>%
  html_attr('id') %>%
  discard(.p = ~stringr::str_detect(.x,"WMN|WOMEN|GEAR|OUTLET")) %>%
  enframe(name = "position", value = "family_class") %>%
  mutate(family_id = str_glue("#{family_class}"))

family_id_css <- bike_family_tbl$family_id %>%
  stringr::str_c(collapse = ", ")

bike_category_tbl <- html %>%
  html_nodes(css = family_id_css) %>%
  html_nodes(css = ".js-ridestyles") %>%
  html_attr('href') %>%
  enframe(name = "position", value = "subdirectory") %>%
  mutate(url = glue("https://www.canyon.com{subdirectory}")) %>%
  distinct(url)


# Step 2

# 2.2
scrap_bike_data <- function(url){

  # 2.1
  bike_category_html <- url %>%
    read_html()

  bike_url_tbl <- bike_category_html %>%
    html_nodes(css = ".productTile__link") %>%
    html_attr('href') %>%
    str_remove(pattern = "\\?.*") %>%
    # discard(.p = ~stringr::str_detect(.x,"frameset")) %>%
    enframe(name = "position", value = "url")

  bike_desc_tbl <- bike_category_html %>%
    html_nodes('.productTile__productSummaryLeft > meta[itemprop="description"]') %>%
    html_attr("content") %>%
    enframe(name = "position", value = "description")


  bike_json_tbl <- bike_category_html %>%
    html_nodes(css = ".productGrid__listItem > div") %>%
    html_attr("data-gtm-impression") %>%
    map(fromJSON) %>%
    map(purrr::pluck, 2, "impressions") %>%
    map(na_if, "not defined") %>%
    map(na_if, "") %>%
    map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
    bind_rows() %>%
    as_tibble() %>%
    rowid_to_column(var='position') %>%
    left_join(bike_desc_tbl) %>%
    left_join(bike_url_tbl)
}


# 2.3
scrap_all_bikes <- function(bike_category_tbl){
  bike_data_tbl <- tibble()
  for(url in  bike_category_tbl$url)
    bike_data_tbl <- bind_rows(bike_data_tbl, scrap_bike_data(url))
  # saveRDS(bike_data_tbl, "01_getting_started/data/bike_data_tbl.rds")
  return(bike_data_tbl)
}

# bike_data_tbl <- scrap_all_bikes(bike_category_tbl)
bike_data_tbl <- readRDS("01_getting_started/data/bike_data_tbl.rds")


# 2.4
clean_bike_data <- function(bike_data_tbl){
  bike_data_cleaned_tbl <- bike_data_tbl %>%
    filter(nchar(.$id) == 4) %>%
    filter(!(name %>% str_detect("Frameset"))) %>%
    distinct(id, .keep_all = T) %>%
    separate(col = category, into = c("category_1", "category_2", "category_3"), sep = "/") %>%
    rename("year"       = "dimension50") %>%
    rename("model"      = "name") %>%
    rename("gender"     = "dimension63") %>%
    rename("price_euro" = "metric4") %>%
    mutate(year = replace_na(year, 2021)) %>%
    mutate(frame_material = case_when(
      model %>% str_detect(" CF ") ~ "carbon",
      model %>% str_detect(" CFR ") ~ "carbon",
      TRUE ~ "aluminium")) %>%
    select(-c(position, brand, variant, starts_with("dim"), quantity, feedProductId, price, metric5)) %>%
    select(id, model, year, frame_material, price_euro, everything())
  # saveRDS(bike_data_cleaned_tbl, "01_getting_started/data/bike_data_cleaned_tbl.rds")
  return(bike_data_cleaned_tbl)
}

# bike_data_cleaned_tbl <- clean_bike_data(bike_data_tbl)
bike_data_cleaned_tbl <- readRDS("01_getting_started/data/bike_data_cleaned_tbl.rds")


# Step 3

# 3.1
bike_url_vec <- bike_data_cleaned_tbl$url

get_colors <- function(url) {
  url %>%
    read_html() %>%
    html_nodes(css = "script") %>%
    as.character() %>%
    str_subset(pattern = "window.deptsfra") %>%
    str_replace("^[^\\{]+", "") %>%
    str_replace("[^\\}]+$", "") %>%
    fromJSON() %>%
    purrr::pluck("productDetail", "variationAttributes", "values", 1, "value")
}

# plan("multiprocess")
# bike_data_colors_tbl <- bike_data_cleaned_tbl %>% 
#   mutate(colors = future_map(bike_url_vec, get_colors))
# saveRDS(bike_data_colors_tbl, "01_getting_started/data/bike_data_colors_tbl.rds")

bike_data_colors_tbl <- readRDS("01_getting_started/data/bike_data_colors_tbl.rds")


# 3.2 Create the urls for each variation

create_color_urls <- function(bike_data_colors_tbl){
  bike_data_colors_tbl <- bike_data_colors_tbl %>%
    unnest(colors) %>%
    mutate(url_color = glue("{url}?dwvar_{id}_pv_rahmenfarbe={colors}")) %>%
    select(-url) %>%
    mutate(url_color = ifelse(str_detect(colors, pattern = "/"), stringi::stri_replace_last_fixed(url_color, "/", "%2F"), url_color))
  return(bike_data_colors_tbl)
}

bike_data_colors_tbl <- create_color_urls(bike_data_colors_tbl)
