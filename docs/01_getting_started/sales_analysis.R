# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(writexl)


rm(list = ls(all.names = TRUE))

# 2.0 Importing Files ----
bikes_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/bikes.xlsx")
bikeshops_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
orderlines_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")

# 3.0 Examining Data ----


# 4.0 Joining Data ----
orderline_joined_tbl <- orderlines_tbl %>% 
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>% 
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- orderline_joined_tbl %>% 
  separate(col = "category", into = c("category.1", "category.2", "category.3"), sep = " - ") %>% 
  mutate(total.price = price * quantity) %>% 
  select(-...1, -gender) %>%
  select(-customer.id, -product.id) %>%
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))


# 6.0 Business Insights ----



# 6.1 Sales by Year ----

# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>% 
  select(order_date, total_price) %>% 
  mutate(year = year(order_date)) %>%
  group_by(year) %>%
  summarise(sales = sum(total_price)) %>% 
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Step 2 - Visualize
sales_by_year_tbl %>% ggplot(mapping = aes(x = year, y = sales)) +
  geom_bar(stat="identity", fill = "#2DC6D6") + 
  geom_label(aes(label = sales_text)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) + 
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "Year", # Override defaults for x and y
    y = "Revenue"
  )

# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate
sales_by_year_and_category_tbl <- bike_orderlines_wrangled_tbl %>% 
  select(order_date, category_1, total_price) %>% 
  mutate(year = year(order_date)) %>%
  group_by(year, category_1) %>%
  summarise(sales = sum(total_price)) %>% 
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
# Step 2 - Visualize
sales_by_year_and_category_tbl %>% ggplot(mapping = aes(x = year, y = sales, fill = category_1)) +
  geom_col() + 
  facet_wrap(~ category_1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )


# 7.0 Writing Files ----

# 7.1 Excel ----
bike_orderlines_wrangled_tbl %>%
  write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>% 
  write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")
