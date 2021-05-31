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

page_three <- tabPanel(
  "Average Vaccinations Per Month",
  sidebarLayout(
    sidebarPanel(
      inf_input <- selectInput(
        "income_group",
        label = "Select Country Income Group",
        choices = list("World" = "World",
                       "Low Income" = "Low income",
                       "Lower Middle Income" = "Lower middle income",
                       "Upper Middle Income" = "Upper middle income",
                       "High Income" = "High income"
        ),
        selected = "World"
      ),
    ),
    mainPanel(
      plotOutput(
        outputId = "avg_vaccinations"
      )
    )
  )
)

ui <- navbarPage(
  "COVID-19 Vaccinations and Deaths",
  page_two,
  page_three
)
