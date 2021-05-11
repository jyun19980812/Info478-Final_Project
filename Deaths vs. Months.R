library(tidyr)
library(dplyr)
library(ggplot2)
#x= months
#y = deaths

world_data <- covid_data %>% 
  filter(location == 'World') %>% 
  filter(date %in% dates) %>% 
  mutate(date = substr(date, 1, 7))
  
  
dates <- c('2020-01-30', '2020-02-28', '2020-03-31', '2020-04-30', '2020-05-31', '2020-06-30',
            '2020-07-31', '2020-08-31', '2020-09-30', '2020-10-31', '2020-11-30', '2020-12-31',
           '2021-01-31', '2021-02-28', '2021-03-31', '2021-04-30')



deaths_vs_months <-  ggplot(world_data) +
  geom_bar(mapping = aes(x = date, y = total_deaths), stat = 'identity', fill = 'green') +
  labs(
    title = "Covid Deaths from 01/20 to 04/21",
    x = "Months",
    y = "Total Deaths"
  )
  