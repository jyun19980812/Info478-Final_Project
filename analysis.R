library(tidyverse)

covid_data <- read.csv("owid-covid-data.csv") %>% 
  group_by(location) %>% 
  filter(date == max(date, na.rm = T))

vaccination_data <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv") %>% 
  group_by(location) %>% 
  filter(date == max(date, na.rm = T))

world_data <- vaccination_data %>% 
  filter(location == "World")

# Select Top 10 countries that have highest deaths and the vaccination numbers
# and make plot (deaths vs. vaccination)

# Pull total vaccination from each month of world data and compare it with months
# (months vs vaccination)

