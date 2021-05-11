library(tidyverse)
library(dplyr)
library(ggplot2)

vaccination_data <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
vaccination_data[is.na(vaccination_data)] <- 0

avg_vaccinations_per_month <- vaccination_data %>%
  filter(location == "World") %>%
  select(location, date, daily_vaccinations) %>%
  mutate(date = as.character(date)) %>%
  mutate(year_month = substring(date, 0, 7)) %>%
  group_by(year_month) %>%
  summarise(avg_vaccinations = mean(daily_vaccinations))

avg_vaccinations_month_plot <- ggplot(avg_vaccinations_per_month) +
  geom_col(mapping = aes(x = year_month, y = avg_vaccinations)) +
  labs(
    title = "Average Number of Vaccinations Per Month",
    x = "Year-Month",
    y = "Average Number of Vaccinations"
  )