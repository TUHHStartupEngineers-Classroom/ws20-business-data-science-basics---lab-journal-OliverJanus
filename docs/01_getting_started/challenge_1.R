library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)

# Part 1

bike_orderlines_wrangled_tbl_old <- read_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds") # read wrangled dataset from .rds file


bike_orderlines_wrangled_tbl <- bike_orderlines_wrangled_tbl_old %>% 
  mutate(year = year(order_date)) %>% # extract the year from date
  separate(col = "location", into = c("city", "state"), sep = ", ") # separate location into city and state

sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>%
  select(state, year, total_price) %>% # only use required columns
  group_by(state) %>%
  summarise(sales = sum(total_price)) %>% # show sales per state
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €")) %>%
  arrange(desc(sales)) # sort by descending revenue
sales_by_state_tbl %>% select(state, sales_text)


sales_by_state_tbl %>% ggplot(mapping = aes(x = reorder(state, -sales), y = sales)) + 
  geom_bar(stat="identity", fill = "#2DC6D6") + 
  geom_label(aes(label = sales_text), size = 2.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # format x-axis labels
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by state",
    subtitle = "bla bla bla",
    x = "",
    y = "Revenue"
  )


# Part 2
sales_by_state_and_year_tbl <- bike_orderlines_wrangled_tbl %>%
  select(state, year, total_price) %>% # only use required columns
  group_by(state, year) %>%
  summarise(sales = sum(total_price)) %>% # show sales per state per year
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €")) %>%
  arrange(year, desc(sales)) # sort by year, then descending revenue



sales_by_state_and_year_tbl %>% ggplot(mapping = aes(x = year, y = sales, fill = state)) + 
  geom_col() + 
  facet_wrap(~ state) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " €")) +
  labs(
    title = "Revenue by state and year",
    subtitle = "bla bla bla",
    x = "",
    y = "Revenue"
  )
                                
