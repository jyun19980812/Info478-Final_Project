library(dplyr)
library(tidyr)
library(ggplot2)

raw_covid_data <- read.csv("owid-covid-data.csv")
raw_covid_data$date = as.character(raw_covid_data$date)

deaths_and_vaccinations <- raw_covid_data %>%
  filter(location != "World") %>% 
  select(location, date, new_deaths, total_vaccinations) %>%
  na.omit() %>%
  filter(!location %in% continents) %>%
  group_by(location) %>% 
  filter(date == max(date, na.rm = TRUE)) %>%
  ungroup(location) %>% 
  arrange(-new_deaths) %>% 
  slice_head(n = 10)

continents <- c("South America", "Europe", "North America", "European Union")

deaths_vs_vaccinations_plot <- ggplot(deaths_and_vaccinations) +
  geom_point(mapping = aes(x = total_vaccinations, y = new_deaths)) +
  labs(title = "Top 10 Total Deaths vs. Total Vaccinations",
       x = "Total Vaccinations",
       y = "Total Deaths") +
  geom_text(aes(x = total_vaccinations, y = new_deaths, label=location), hjust=0, vjust=0)