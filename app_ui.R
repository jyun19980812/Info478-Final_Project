# This is app_ui.R file.
# Creating UI interface.

page_two <- tabPanel(
  "New Covid Deaths vs Months",
  sidebarLayout(
    sidebarPanel(
      selectInput("select_con", "Select Continent", c("All" = "All",
                                                         "Africa" = "Africa",
                                                         "Asia" = "Asia",
                                                         "Europe" = "Europe",
                                                         "North America" = "North America",
                                                         "Oceania" = "Oceania",
                                                         "South America" = "South America"))
    ),
    mainPanel(
      plotOutput(
        outputId = "deaths_months_plot"
      )
    ) 
  ) 
) 

ui <- navbarPage(
  "COVID-19 Vaccinations and Deaths",
  page_two
)
