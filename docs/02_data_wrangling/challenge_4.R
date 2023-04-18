library(vroom)
library(tidyverse)
library(glue)
library(data.table)
library(lubridate)


# define col_types and load in patents ----

# data dir-path
path <- "A:/Olli/Datasets/04_patent_data/"

#define col_types
assignee_col_types <- list(
  id = col_character(),
  type = col_skip(),
  name_first = col_skip(),
  name_last = col_skip(),
  organization = col_character()
)

patent_col_types <- list(
  id = col_character(),
  type = col_skip(),
  number = col_skip(),
  country = col_skip(),
  date = col_date("%Y-%m-%d"),
  abstract = col_skip(),
  title = col_skip(),
  kind = col_skip(),
  num_claims = col_double(),
  filename = col_skip(),
  withdrawn = col_skip()
)

patent_assignee_col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_skip()
)

uspc_col_types <- list(
  uuid = col_skip(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_skip(),
  sequence = col_skip()
)


# read data from files
assignee_tbl <- vroom(
  file       = glue("{path}dataset/assignee.tsv"), 
  delim      = "\t", 
  col_types  = assignee_col_types,
  na         = c("", "NA", "NULL")
)

patent_tbl <- vroom(
  file       = glue("{path}dataset/patent.tsv.zip"),
  delim      = "\t",
  col_types  = patent_col_types,
  na         = c("", "NA", "NULL")
)

patent_assignee_tbl <- vroom(
  file       = glue("{path}dataset/patent_assignee.tsv"), 
  delim      = "\t", 
  col_types  = patent_assignee_col_types,
  na         = c("", "NA", "NULL")
)

uspc_tbl <- vroom(
  file       = glue("{path}dataset/uspc.tsv"), 
  delim      = "\t",
  col_types  = uspc_col_types,
  na         = c("", "NA", "NULL")
)

setDT(assignee_tbl)
setDT(patent_tbl)
setDT(patent_assignee_tbl)
setDT(uspc_tbl)



# 1. Patent Dominance ----

merged_1_dt <- merge(assignee_tbl, patent_assignee_tbl, by.x = "id", by.y = "assignee_id")
patent_dominance <- merged_1_dt[!is.na(organization), .(n_patents = .N), by = "organization"][
  order(n_patents, decreasing = TRUE)][
    1:10]
saveRDS(patent_dominance, glue("{path}data/patent_dominance.RDS"))

# 2. Recent patent acitivity ----

merged_2_dt <- merge(merged_1_dt, patent_tbl, by.x = "patent_id", by.y = "id")
patent_activity <- merged_2_dt[year(date) == 2019, .(n_patents =.N), by = "organization"][
  order(n_patents, decreasing = TRUE)][
    1:10]
saveRDS(patent_activity, glue("{path}data/patent_activity.RDS"))

# 3. Innovation in Tech ----

merged_3_dt <- merge(merged_2_dt, uspc_tbl, by = "patent_id")
tech_innovation <- merged_3_dt[organization %in% patent_dominance$organization, .(n_patents = .N, mainclass_id), by = "organization"][
  !is.na(mainclass_id), .(occurence = .N), by = "mainclass_id"][
    order(occurence, decreasing = TRUE)][
      1:5]
saveRDS(tech_innovation, glue("{path}data/tech_innovation.RDS"))

