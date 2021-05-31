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
      filter(date %in% dates) %>%
      mutate(date = substr(date, 1, 7)) %>% 
      select(c("iso_code", "continent", "location", "date", "new_deaths"))
    
    all_con <-  c("Africa", 'Asia', 'Europe', 'North America', 'Oceania', 'South America')
    if(input$select_con == "All") {
      
      continents_edited <- continents[continents$continent != "", ] %>% 
        group_by(continent)
      
      ggplot(continents_edited, aes(fill=continent, y=new_deaths, x=date)) + 
        geom_bar(position="stack", stat="identity") +
        labs(
          title = "Covid Deaths from 01/20 to 04/21",
          x = "Months",
          y = "New Deaths"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      continents_edited <- continents[continents$continent != "", ] %>% 
        group_by(continent) %>% 
        filter(continent %in% input$select_con)
      
      ggplot(continents_edited, aes(fill=continent, y=new_deaths, x=date)) + 
        geom_bar(position="stack", stat="identity") +
        labs(
          title = "Covid Deaths from 01/20 to 04/21",
          x = "Months",
          y = "New Deaths"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })    
}

