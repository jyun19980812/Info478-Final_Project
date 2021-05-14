# Load packages and data
library(tidyverse)

covid_data <- read.csv("./data/owid-covid-data.csv")
covid_data$date = as.character(covid_data$date)

vaccination_data <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
vaccination_data[is.na(vaccination_data)] <- 0

world_data <- vaccination_data %>%
  filter(location == "World")

# Distribution of Variables -----------------------------------------------

# Select Top 10 countries that have highest average deaths and compare through
# bar graphs
high_avg_deaths <- covid_data %>%
  group_by(location) %>%
  filter(date == max(date, na.rm = T)) %>%
  filter(!grepl("OWID", iso_code)) %>%
  summarize(distb_of_deaths = mean(total_deaths)) %>%
  top_n(10, distb_of_deaths)

plot_avg_deaths <- ggplot(data = high_avg_deaths) +
  geom_col(mapping = aes(x = location, y = distb_of_deaths, fill = location)) +
  coord_flip() +
  labs(
    title = "Top 10 Countries With Highest Average Deaths",
    x = "Country",
    y = "Average Deaths of COVID"
  )

# Calculate average vaccinations per month and visualize through bar graphs
avg_vaccinations_per_month <- world_data %>% 
  select(location, date, daily_vaccinations) %>%
  mutate(date = as.character(date)) %>%
  mutate(year_month = substring(date, 0, 7)) %>%
  group_by(year_month) %>%
  summarise(avg_vaccinations = mean(daily_vaccinations))

avg_vaccinations_month_plot <- ggplot(avg_vaccinations_per_month) +
  geom_col(mapping = aes(x = year_month, y = avg_vaccinations), fill = "red") +
  labs(
    title = "Average Number of Vaccinations Per Month",
    x = "Year-Month",
    y = "Average Number of Vaccinations"
  )

# Relationships between variables -----------------------------------------

# Select Top 10 countries that have highest deaths and the vaccination numbers
# and make plot (deaths vs. vaccination)
raw_covid_data <- covid_data
raw_covid_data$date <- as.character(raw_covid_data$date)

continents <- c(
  "South America", "Europe", "North America", "European Union",
  "Asia", "Africa"
)

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

deaths_vs_vaccinations_plot <- ggplot(deaths_and_vaccinations) +
  geom_point(mapping = aes(x = total_vaccinations, y = new_deaths)) +
  labs(
    title = "Top 10 New Deaths vs. Total Vaccinations",
    x = "Total Vaccinations",
    y = "New Deaths"
  ) +
  geom_text(aes(x = total_vaccinations, y = new_deaths, label = location), hjust = 0, vjust = 0)

# Filter by each months to compare how many deaths have been recorded in
# each month (Deaths vs. Months)
dates <- c(
  "2020-01-30", "2020-02-28", "2020-03-31", "2020-04-30", "2020-05-31", "2020-06-30",
  "2020-07-31", "2020-08-31", "2020-09-30", "2020-10-31", "2020-11-30", "2020-12-31",
  "2021-01-31", "2021-02-28", "2021-03-31", "2021-04-30"
)

months_data <- covid_data %>%
  filter(location == "World") %>%
  filter(date %in% dates) %>%
  mutate(date = substr(date, 1, 7))

deaths_vs_months <-  ggplot(months_data) +
  geom_bar(mapping = aes(x = date, y = new_deaths), stat = 'identity', fill = "light green") +
  labs(
    title = "Covid Deaths from 01/20 to 04/21",
    x = "Months",
    y = "New Deaths"
  )

