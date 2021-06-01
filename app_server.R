# This is app_server.R file
covid_data <- read.csv("./data/owid-covid-data.csv")

vaccination_data <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
vaccination_data[is.na(vaccination_data)] <- 0

world_data <- vaccination_data %>%
  filter(location == "World")


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
                                              label = avg_vaccinations, 
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
}

