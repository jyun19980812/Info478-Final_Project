# This is app_server.R file
covid_data <- read.csv("owid-covid-data.csv")

vaccination_data <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
vaccination_data[is.na(vaccination_data)] <- 0

world_data <- vaccination_data %>%
  filter(location == "World")


# Define a server function for a Shiny app
server <- function(input, output) {

}

