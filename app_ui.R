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
    ourselves to a new type of lifestyle due to ", strong("COVID-19"), ". We have  
    finally produced vaccinations, yet there are growing concerns about these 
    vaccines. Through this project, we aim to: "), 
    p("1. Look through the data of ", em("Covid-19 Vaccinations"), " and ", 
      em("COVID-19 Deaths"), " to see if there are any correlations between them."),
    p("2. We hope to find out more about the ", em("effectiveness of the COVID-19 
      vaccinations"), " and how it is distributed throughout the ", 
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
      plotOutput(outputId = "deaths_months_plot"),
      p(strong("Analysis: ")),
      br(),
      p("The World data (¡°All¡±) shows a positive trend which suggests that covid deaths are generally 
     increasing as time goes on with a few exceptions for certain months. The exceptions in the 
     increasing trend in covid related deaths can be due to lockdowns, restrictions, and other health 
     interventions intended to slow the spread of COVID-19. Even though we saw a decline after 
     January 2021 to March 2021 the number of deaths are rising again. The peak number of deaths 
     was reported in April 2021. There is not enough data for May 2021 to
     make any conclusions."),
      p("When we look at the different continents"),
      tags$li("Africa: There was a peak in January 2021 and the new deaths number have been declining since."),
      tags$li("Asia: Shows a general bell curve with the exception of March 2021 in which new deaths had a sharp increase."),
      tags$li("Europe: New deaths declined from April 2020 and was on the rise from September 2020 to January 2021. Numbers have been steady since then."),
      tags$li("North America: We see a general increase till January 2021 and then the number has been steadily decreasing since then."),
      tags$li("Oceania: Has had the lowest number of deaths in general and had their peak in August 2020."),
      tags$li("South America: Had a peak in July- August of 2020 although numbers are on the rise again."),
      p("Overall Europe, North America and South America have experienced the highest peaks of new deaths each month excluding the incomplete data from May 2021")
    ) )
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
      ),

      p(""),
      p("This plot shows the average number of vaccinations per month. 
        Inittially, the chart just showed the average vaccinations for the
        world, but we decided to explore what this would look like when we split
        it up into different income groups. The different income groups in the
        data set were low income, lower middle income, upper middle income, and
        high income."),
      p("We can see that there is a positive trend, where the a
        verage number of vaccinations is increasing over time. This applies to
        all income groups. When we look at the low income and lower middle 
        income groups, we can see that the data about average vaccinations
        starts in January or February, but for the upper middle income and high
        income groups, the data starts in December 2020. We also noticed that
        each groups shows that the average vaccination numbers increase more
        than the previous month except for the lower middle income group, where
        April 2021 has a higher average of vaccinations than May 2021."),
      p(""),
      br(),s
      p("Something that should be considered is that this trend may not be
        reflected when looking at different countries individually around the
        world. While this plot shows the overall average vaccination numbers for
        the world, different countries may show different numbers and trends
        regarding the average number of vaccinations.")
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

conclusion_panel <- tabpanel(
  "Conclusion of the Project",
  mainPanel(
    h2("Conclusion"),
    p(""),
    h2("Limitation"),
    p("Some of the limitations of this project are that the data we used is 
    continuously being updated, and it is self-reported. Our findings and 
    visualizations in this project can change.")
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
