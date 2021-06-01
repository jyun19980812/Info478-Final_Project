# This is app_ui.R file.

# Creating Introduction Page for the assignment
introduction_layout <- sidebarLayout(
  sidebarPanel(
    em("Purpose of the project"),
    p("The purpose of this project is to create a resource about COVID-19
      vaccination distribution and deaths around the world."),
    em("Data sources"),
    p("Data was collected Data was collected from ", 
      strong("Johns Hopkins University (JHU)"), " and the JHU dataset is 
      maintained by a team at its ", strong("Center for Science and Engineering 
                                            (CSSE)")),
    p("The data for the vaccinations were collected by the ", strong("Our World 
    in Data"), " team from official reports")
  ),
  mainPanel(
    h2("Introduction To the Disease Modeling"),
    p("Throughout the pandemic, many of us experienced pain and familiarized 
    ourselves to new type of lifestyle due to ", strong("COVID-19"), "We have  
    finally produced vaccinations, yet there are growing concerns to these 
    vaccines. Thorough this project, we aim to: "), 
    p("1. Look through the data of ", em("Covid-19 Vaccinations"), " and ", 
      em("COVID-19 Deaths"), " to see if there is any correlation between them."),
    p("2. We wish to find out about the ", em("effectiveness of the COVID-19 
      vaccination"), " and how it is differentiated throughout the ", 
      em("world.")),
    p("3. We aim to visualize through ", em("effective graphs"), ", for us and 
      our users to easily compare.")
  )
)

# Creating Panel that shows introduction of this project.
introduction_panel <- tabPanel("Introduction", introduction_layout)


# Creating UI interface.
country_df <- covid_data %>%
  distinct(location) %>%
  inner_join(shapefile, by = "location") %>%
  distinct(location)

# Create Sidebar that shows dropdown menu which user can select.
map_layout <- sidebarLayout(
  sidebarPanel(
    h2("Map on Average Death Rate in Country of Interest"),
    selectInput(
      inputId = "world_name",
      label = "Country of Interest",
      choices = country_df
    ),
    h2("Changing Color of Lower Legend"),
    p("Lets the user choose the color of lower legend"),
    selectInput(
      inputId = "color_low",
      label = "Color of Low Legend",
      choices = c("Green", "White", "Yellow", "Brown")
    ),
    h2("Changing Color of Higher Legend"),
    p("Lets the user choose the color of higher legend"),
    selectInput(
      inputId = "color_high",
      label = "Color of High Legend",
      choices = c("Black", "Blue", "Red", "Orange")
    )
  ),
  mainPanel(
    p("In this plot, we observe the average death rate of each countries caused 
      by COVID-19, selected by user's interest. It is pertinent to find out the 
      average rate of deaths in each of these countries, since in order to find
      the correlation with vaccination and deaths, we must look through the data
      of how much death is caused in each of the countries at first. Through the
      graph, we are able to learn that", strong("United States"), " have the 
      highest rate with ", strong("0.18 or 18%.")),
    plotOutput(outputId = "map"),
  )
)

# Creating Panel for Average COVID-19 Death Rate Map.
page_one <- tabPanel("Map of Average Deaths", map_layout)

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

## Total Vaccines vs New Deaths
new_covid_data <- read.csv("data/owid-covid-data.csv")
new_covid_data <- new_covid_data %>%
  select(total_vaccinations) %>%
  na.omit()

range <- c(min(new_covid_data$total_vaccinations),
           max(new_covid_data$total_vaccinations))

page_four <- tabPanel(
  "Total Vaccinations vs. New Covid Deaths",
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "vacc_range",
        label = "Select Country Vaccine Range",
        min = range[1],
        max = range[2],
        value = range
      ),
    ),
    mainPanel(
      plotOutput(
        outputId = "vacc_death_plot"
      )
    )
  )
)

ui <- navbarPage(
  "COVID-19 Vaccinations and Deaths",
  introduction_panel,
  page_one,
  page_two,
  page_three,
  page_four
)
