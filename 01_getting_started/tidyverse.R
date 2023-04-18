library(tidyverse)

load_datasets <- function(){
  diamonds2 <- readRDS("01_getting_started/exercise_datasets/diamonds2.rds")
  diamonds3 <- readRDS("01_getting_started/exercise_datasets/diamonds3.rds")
  diamonds4 <- readRDS("01_getting_started/exercise_datasets/diamonds4.rds")
  diamonds5 <- readRDS("01_getting_started/exercise_datasets/diamonds5.rds")
  return(list("diamonds2" = diamonds2, "diamonds3" = diamonds3, "diamonds4" = diamonds4, "diamonds5" = diamonds5))
}

tidy_diamond2 <- function(dataset){
  dataset %>% pivot_longer(cols = c("2008", "2009"), names_to = "year", values_to = "price") %>% head(n = 5)
}

tidy_diamond3 <- function(dataset){
  dataset %>% pivot_wider(names_from = "dimension", values_from = "measurement") %>% head(n = 5)
}

tidy_diamond4 <- function(dataset){
  dataset %>% separate(col = "dim", into = c("x", "y", "z"), sep = "/", remove = TRUE, convert = TRUE) %>% head(n = 5)
}

tidy_diamond5 <- function(dataset){
  dataset %>% unite(col = "clarity", clarity_prefix, clarity_suffix, sep = "") %>% head(n = 5)
}




dataset_list <- load_datasets()


dataset <- dataset_list$diamonds2
tidy_diamond2(dataset)

dataset <- dataset_list$diamonds3
tidy_diamond3(dataset)

dataset <- dataset_list$diamonds4
tidy_diamond4(dataset)

dataset <- dataset_list$diamonds5
tidy_diamond5(dataset)


