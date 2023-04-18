library(httr)
library(tidyverse)
library(spotifyr)
library(jsonlite)
library(purrr)
library(stringr)
library(dplyr)

# get access token and create header for api requests
access_token <- get_spotify_access_token(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                         client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"))
header <- c('Authorization' = paste("'Bearer ", access_token, "'"), 
            'Content-Type' = 'application/json', 
            'Accept' = 'application/json')
base_url <- "https://api.spotify.com"


# a function to extract data of a top 50 playlist from a given link
scrap_charts_data <- function(playlist_url){
  # print(playlist_url)
  charts_json <- GET(url = playlist_url, add_headers(header)) %>%
    content(as = "parsed")
    
  charts_tbl <- tibble()
  for(track in charts_json$tracks$items){
    name <- track$track$name
    artist <- track$track$artist[[1]]$name
    album <- track$track$album$name
    id <- track$track$id
    track_url <- paste("https://open.spotify.com/track/", id, sep = "")
    api_url <- track$track$href
    song <- tibble(name = name, 
                   artist = artist, 
                   album = album, 
                   id = id, 
                   track_url = track_url, 
                   api_track_url = api_url)
    charts_tbl <- bind_rows(charts_tbl, song)
  }
  charts_tbl <- charts_tbl %>%
    mutate(position = 1:50) %>%
    select(position, name, artist, album, id, track_url, api_track_url)
  return(charts_tbl)
}


# search for top 50 playlists and extract names and api-links to the offiial playlists
search_url <- paste(base_url, "/v1/search?q=top+50&type=playlist&limit=50",sep = "")
response_json <- httr::GET(url = search_url, add_headers(header)) %>%
  content(as = "parsed")

playlist_tbl <- tibble(playlist_name = character(), api_playlist_url = character())
for(playlist in response_json$playlists$items){
  if(playlist$owner$display_name == 'spotifycharts' && str_detect(playlist$name, " 50")){
    playlist_tbl <- add_row(playlist_tbl, 
                            playlist_name = playlist$name, 
                            api_playlist_url = playlist$href)
  }
}


# get information for all playlists and combine them into one tibble
all_charts_tbl <- playlist_tbl %>%
  mutate(country = map(.$playlist_name, str_remove," Top 50")) %>%
  mutate(playlist_info = map(.$api_playlist_url, scrap_charts_data)) %>%
  bind_rows() %>%
  unnest(cols = playlist_info) %>%
  select(playlist_name, position, name, artist, album, country, track_url)

# create comparison tibble for all top 50 chart songs
chart_comp_tbl <- all_charts_tbl %>%
  pivot_wider(names_from = country, values_from = name) %>%
  group_by(position) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-c(position, playlist_name, artist, album, track_url))
chart_comp_tbl %>% print(n = 10)
