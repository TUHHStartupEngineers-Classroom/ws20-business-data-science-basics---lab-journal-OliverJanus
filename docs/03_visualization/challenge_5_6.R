library(tidyverse)
library(data.table)
library(ggplot2)
library(scales)
library(maps)

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv") %>% 
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories))

covid_data_dt <- covid_data_tbl %>% as.data.table()

# Challenge 5 ----

covid_data_cum_dt <- covid_data_dt[, .(dateRep, day, month, countriesAndTerritories, cases)][
  order(month, day)][
    , cumCases := cumsum(cases), by = "countriesAndTerritories"][
      , .(dateRep, countriesAndTerritories, cumCases)] %>% 
  pivot_wider(names_from = countriesAndTerritories, values_from = cumCases) %>%
  .[, c("dateRep", "Germany", "Spain", "UK", "USA", "France")] %>% 
  na.omit()

ylab <- c(2.5, 5.0, 7.5, 10, 12.5, 15)
covid_data_cum_dt %>%
  ggplot(aes(x = as.Date(dateRep, format="%d/%m/%Y"))) +
  geom_line(aes(y = Germany, color = "Germany"), size = 0.5, linetype = 1) +
  geom_line(aes(y = France, color = "France"), size = 0.5, linetype = 1) +
  geom_line(aes(y = UK, color = "UK"), size = 0.5, linetype = 1)  +
  geom_line(aes(y = Spain, color = "Spain"), size = 0.5, linetype = 1) +
  geom_line(aes(y = USA, color = "USA"), size = 0.5, linetype = 1) +
  scale_y_continuous(labels = paste0(ylab, "M"),
                     breaks = 10^6 * ylab) +
  scale_x_date(date_breaks = "1 month", 
             labels=date_format("%B"),
             limits = as.Date(c('2020-01-01','2020-12-31'))) + 
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "grey10"),
    legend.text = element_text(color = 'white'),
    legend.key.size = unit(2, "line"),
    legend.key = element_rect(fill = "grey10"),
    axis.text = element_text(color = 'white', size = 7.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 15),
    title = element_text(colour = 'white', size = 9),
    panel.background = element_rect(fill = 'grey10'),
    panel.grid.major = element_line(color = 'grey20', size = 0.5),
    panel.grid.minor = element_line(color = 'grey30', size = 0.5), 
    plot.background = element_rect(fill = 'grey10')) +
  labs(title    = "COVID-19 confirmed cases worldwide",
       subtitle = "As of 06/12/2020 USA is the leading country in cumulated cases",
       x = "Year 2020",
       y = "Cumulative Cases",
       color = "")

# Challenge 6 ----


world <- map_data("world")

covid_data_deaths_tbl <- covid_data_tbl %>% 
  group_by(countriesAndTerritories) %>%
  summarise(mortality = sum(deaths/popData2019), .groups = 'keep') %>% 
  merge(world, by.x = "countriesAndTerritories", by.y = "region") %>% 
  rename(region = countriesAndTerritories)

covid_data_deaths_tbl %>% ggplot() +
  geom_map(aes(fill = mortality, x = long, y = lat, map_id = region), map = world, color = 'grey10') +
  theme(
    legend.background = element_rect(fill = "grey10"),
    legend.text = element_text(color = 'white'),
    legend.key = element_rect(fill = "grey10"),
    panel.background = element_rect(fill = 'grey10'),
    panel.grid.major = element_line(color = 'grey20', size = 0.5),
    panel.grid.minor = element_line(color = 'grey30', size = 0.5), 
    plot.background = element_rect(fill = 'grey10'),
    text = element_text(color = 'white')
  ) +
  labs(title    = "Confirmed COVID-19 deaths relative to the size of the population",
       subtitle = "More than 1.2 Million confirmed COVID-19 deaths worldwide",
       x = "",
       y = "",
       color = "") + 
  guides(fill = guide_colorbar()) +
  scale_fill_continuous(name = "Mortality Rate", low = "#a81e1e", high = "black", labels = percent)
