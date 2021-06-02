# This is app_server.R file
covid_data <- read.csv("./data/owid-covid-data.csv")

vaccination_data <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
vaccination_data[is.na(vaccination_data)] <- 0

world_data <- vaccination_data %>%
  filter(location == "World")

# Creating Country shapefile.
shapefile <- map_data("world") %>%
  rename(location = region)

# Modify the shapefile location of USA into United States
shapefile$location[shapefile$location == "USA"] <- "United States"

# Look through the COVID-19 data to calculate the average rate of death in 
# each country.
avg_deaths <- covid_data %>% 
  filter(!grepl("OWID", iso_code)) %>%
  group_by(location) %>%
  filter(date == max(date, na.rm = T)) %>% 
  summarize (distb_of_deaths = mean(total_deaths, na.rm = T)) %>%
  mutate(avg_rate_deaths = distb_of_deaths / sum(distb_of_deaths, na.rm = T))

# Setting na values to zero
avg_deaths[is.na(avg_deaths)] <- 0

# Define a server function for a Shiny app
server <- function(input, output) {
  
  # Filter by each months to compare how many deaths have been recorded in
  # each month (Deaths vs. Months)
  output$deaths_months_plot <- renderPlot({
    dates <- c(
      "2020-01-30", "2020-02-28", "2020-03-31", "2020-04-30", "2020-05-31", "2020-06-30",
      "2020-07-31", "2020-08-31", "2020-09-30", "2020-10-31", "2020-11-30", "2020-12-31",
      "2021-01-31", "2021-02-28", "2021-03-31", "2021-04-30"
    )
    
    continents <- covid_data %>% 
      mutate(date = substr(date, 1, 7)) %>% 
      select(c("iso_code", "continent", "location", "date", "new_deaths"))
    
    all_con <-  c("Africa", 'Asia', 'Europe', 'North America', 'Oceania', 'South America')
    if(input$select_con == "All") {
      new_continents <- continents[continents$new_deaths > 0, ] %>% 
        na.omit(continents) %>% 
        select(c("iso_code", "continent", "date", "new_deaths"))
      
      new_continents<- new_continents[new_continents$continent != "", ] %>% 
        group_by(date) %>% 
        group_by(continent)
      
      
     ggplot(new_continents, aes(fill=continent, y=new_deaths, x=date)) + 
        geom_bar(position="stack", stat="identity") +
        labs(
          title = "Covid Deaths from 01/20 to 05/21",
          x = "Months",
          y = "New Deaths"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else {
      new_continents <- continents[continents$new_deaths > 0, ] %>% 
        na.omit(continents) %>% 
        select(c("iso_code", "continent", "date", "new_deaths"))
      
      new_continents<- new_continents[new_continents$continent != "", ] %>% 
        group_by(date) %>% 
        group_by(continent) %>% 
        filter(continent %in% input$select_con)
      
      ggplot(new_continents, aes(fill=continent, y=new_deaths, x=date)) + 
        geom_bar(position="stack", stat="identity") +
        labs(
          title = "Covid Deaths from 01/20 to 04/21",
          x = "Months",
          y = "New Deaths"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Calculate average vaccinations per month and visualize through bar graphs
  output$avg_vaccinations <- renderPlot({
    avg_vaccinations_per_month <- vaccination_data %>%
    filter(location == input$income_group) %>%
    select(location, date, daily_vaccinations) %>%
      mutate(date = as.character(date)) %>%
      mutate(year_month = substring(date, 0, 7)) %>%
      group_by(year_month) %>%
      summarise(avg_vaccinations = mean(daily_vaccinations))
    
    avg_vaccinations_month_plot <- ggplot(avg_vaccinations_per_month, 
                                          aes(x = year_month, 
                                              y = avg_vaccinations, 
                                              label = round(avg_vaccinations, 3), 
                                              fill = "red")
    ) +
      geom_col() +
      geom_text(vjust=-0.25) +
      labs(
        title = "Average Number of Vaccinations Per Month",
        x = "Year-Month",
        y = "Average Number of Vaccinations"
      )
    
    avg_vaccinations_month_plot
  })
  
  # Vaccinations vs Deaths
  output$vacc_death_plot <- renderPlot({
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
      filter(total_vaccinations >= input$vacc_range[1],
             total_vaccinations <= input$vacc_range[2]) %>%
      ungroup(location) %>%
      arrange(-new_deaths) %>%
      slice_head(n = 10)
    
    deaths_vs_vaccinations_plot <- ggplot(deaths_and_vaccinations) +
      geom_point(mapping = aes(x = total_vaccinations, y = new_deaths)) +
      labs(
        title = "New Deaths vs. Total Vaccinations",
        x = "Total Vaccinations",
        y = "New Deaths"
      ) +
      geom_text(aes(x = total_vaccinations, y = new_deaths, label = location), hjust = 0, vjust = 0)
    
    deaths_vs_vaccinations_plot
  })
  
  # Creating map of average death caused by COVID-19
  output$map <- renderPlot({
    # Selecting country to see the country of interest
    country_input <- input$world_name
    
    # Filtering country to country of interest, and modify to merge with world's
    # shapefile.
    avg_rate <- avg_deaths %>%
      filter(location == country_input) %>%
      select(location, avg_rate_deaths) 
    
    avg_rate_interest <- shapefile %>%   
      left_join(avg_rate, by = "location")
    
    # Creating a map that shows the average death rate of COVID-19 varied by 
    # each countries
    avg_death_map <- ggplot(data = avg_rate_interest) +
      geom_polygon(
        mapping = aes(x = long, y = lat, group = group, fill = avg_rate_deaths),
        color = "gray"
      ) +
      labs(
        title = paste("Average Death Rate from COVID-19 in ", country_input),
        x = "", y = "", fill = "Average Rate of Death"
      ) +
      scale_fill_continuous(
        limits = c(0, max(avg_deaths$avg_rate_deaths, na.rm = T)),
        na.value = "white", low = input$color_low, high = input$color_high
      ) +
      coord_quickmap() +
      theme_void()
    return(avg_death_map)
  })

}

