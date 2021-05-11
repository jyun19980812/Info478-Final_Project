covid_data <- read.csv("owid-covid-data.csv") %>% 
  group_by(location) %>% 
  filter(date == max(date, na.rm = T))

# Select Top 10 countries that have highest average deaths 
high_avg_deaths <- covid_data %>% 
  filter(!grepl('OWID', iso_code)) %>% 
  group_by(location) %>% 
  summarize(distb_of_deaths = mean(total_deaths)) %>% 
  top_n(10, distb_of_deaths)

plot_avg_deaths <- ggplot(data = high_avg_deaths) +
  geom_col(mapping =  aes(x = location, y = distb_of_deaths, fill = location)) +
  coord_flip() +
  labs(
    title = "Top 10 Countries With Highest Average Deaths",
    x = "Country",
    y = "Average Deaths of COVID"
  )