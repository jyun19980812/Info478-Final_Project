# This is app_ui.R file.

# Creating Introduction Page for the assignment
introduction_layout <- sidebarLayout(
  sidebarPanel(
    em("Purpose of the project"),
    p("The purpose of this project is to create a resource about COVID-19
      vaccination distribution and deaths around the world."),
    em("Data sources"),
    p(
      "Data was collected Data was collected from",
      strong("Johns Hopkins University (JHU)"), " and the JHU dataset is 
      maintained by a team at its ", strong("Center for Science and Engineering 
                                            (CSSE)")
    ),
    p("The data for the vaccinations were collected by the ", strong(a("Our World 
    in Data", href="https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations")), " team from official reports")
  ),
  mainPanel(
    h2("Introduction To the Disease Modeling"),
    p("Throughout the pandemic, many of us experienced pain and familiarized 
    ourselves to a new type of lifestyle due to ", strong("COVID-19"), ". We have  
    finally produced vaccinations, yet there are growing concerns about these 
    vaccines. Through this project, we aim to: "),
    p(
      "1. Look through the data of ", em("Covid-19 Vaccinations"), " and ",
      em("COVID-19 Deaths"), " to see if there are any correlations between them."
    ),
    p(
      "2. We hope to find out more about the ", em("effectiveness of the COVID-19 
      vaccinations"), " and how it is distributed throughout the ",
      em("world.")
    ),
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
    plotOutput(outputId = "mapping"),
  )
)

# Creating Panel for Average COVID-19 Death Rate Map.
page_one <- tabPanel("Map of Average Deaths", map_layout)

page_two <- tabPanel(
  "New Covid Deaths vs Months",
  sidebarLayout(
    sidebarPanel(
      selectInput("select_con", "Select Continent", c(
        "All" = "All",
        "Africa" = "Africa",
        "Asia" = "Asia",
        "Europe" = "Europe",
        "North America" = "North America",
        "Oceania" = "Oceania",
        "South America" = "South America"
      ))
    ),
    mainPanel(
      plotOutput(outputId = "deaths_months_plot"),
      p(strong("Analysis: ")),
      br(),
      p("The World data ", strong("All"), " shows a positive trend which 
      suggests that covid deaths are generally increasing as time goes on with a
      few exceptions for certain months. The exceptions in the increasing trend 
      in covid related deaths can be due to lockdowns, restrictions, and other 
      health interventions intended to slow the spread of COVID-19. Even though 
      we saw a decline after January 2021 to March 2021 the number of deaths are
      rising again. The peak number of deaths was reported in April 2021. 
      There is not enough data for May 2021 to make any conclusions."),
      p("When we look at the different continents"),
      tags$li("Africa: There was a peak in January 2021 and the new deaths 
              number have been declining since."),
      tags$li("Asia: Shows a general bell curve with the exception of March 2021
              in which new deaths had a sharp increase."),
      tags$li("Europe: New deaths declined from April 2020 and was on the rise 
              from September 2020 to January 2021. Numbers have been steady 
              since then."),
      tags$li("North America: We see a general increase till January 2021 and 
              then the number has been steadily decreasing since then."),
      tags$li("Oceania: Has had the lowest number of deaths in general and had 
              their peak in August 2020."),
      tags$li("South America: Had a peak in July- August of 2020 although 
              numbers are on the rise again."),
      p("Overall Europe, North America and South America have experienced the 
        highest peaks of new deaths each month excluding the incomplete data 
        from May 2021")
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
        choices = list(
          "World" = "World",
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
        Initially, the chart just showed the average vaccinations for the
        world, but we decided to explore what this would look like when we split
        it up into different income groups."),
      p("The different income groups in the data set were low income, lower
        middle income, upper middle income, and high income. The way different
        countries are classified by income groups can be found",
        strong(a("here", 
                 href="https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups")),
        ", but here is a list of some of the countries in the different income groups:"),
      tags$li("Low Income: Afghanistan, Burkina Faso, Congo, Ethiopia,
              Madagascar, Rwanda, Liberia, Uganda, Yemen"),
      tags$li("Lower Middle Income: Angola, Bangladesh, Cambodia, Egypt, India,
              Kenya, Lesotho, Mongolia, Nicaragua, Ukraine, Vietnam"),
      tags$li("Upper Middle Income: Argentina, Belarus, Brazil, China, Cuba,
              Fiji, Guatemala, Jamaica, Mexico, Peru, Samoa, Thailand,
              Turkey"),
      tags$li("High Income: Australia, Belgium, Canada, Finland, France,
              Italy, Kuwait, New Zealand, Portugal, Singapore, Sweden,
              United Kingdom, United States"),
      p(""),
      p("We can see that there is a positive trend, where the average
        number of vaccinations is increasing over time. This applies to
        all income groups. When we look at the low income and lower middle 
        income groups, we can see that the data about average vaccinations
        starts in January or February, but for the upper middle income and high
        income groups, the data starts in December 2020. We also noticed that
        each group shows that the average vaccination numbers increase more
        than the previous month except for the lower middle income group, where
        April 2021 has a higher average of vaccinations than May 2021. These
        differences could be due to how vaccines are being distributed in
        different countries as well as how data is being reported."),
      p(""),
      p("Something that should be considered is that this trend may not be
        reflected when looking at different countries individually around the
        world. While this plot shows the overall average vaccination numbers for
        the world, different countries may show different numbers and trends
        regarding the average number of vaccinations.")
    )
  )
)

## Total Vaccines vs New Deaths
range <- c(0, 100)

page_four <- tabPanel(
  "Proportion Vaccinated vs. Proportion of New Covid Deaths",
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "vacc_range",
        label = "Select % of Population Vaccinated",
        min = 0,
        max = 100,
        value = c(0, 100)
      ),
    ),
    mainPanel(
      plotOutput(
        outputId = "vacc_death_plot"
      ),
      tableOutput(
        outputId = "countries_in_range"
      ),
      br(),
      p("This plot shows the proportion (as a percentage) of
        people in country who are fully vaccinated in
        comparison to the proprotion of deaths (as a percentage) for that
        respective country. You can also filter it to only show countries
        where their proportion of fully vaccinated people are in a specific
        range you specify."),
      p("Just looking at the plot there doesn't seem to be any trend between
        the proportion of peopel fully vaccinated and the proportion of new
        deaths in the country. However, we can see that the majority of 
        countries have between 0% to 20% of their population fully vaccinated. We can
        also see that Israel is the country with the highest proportion of their
        population being fully vaccinated at about", em("58.463%"), ". Urugay
        is the country with the highest proportion of deaths at about ", em("0.0023%"), ".")
    )
  )
)

conclusion_panel <- tabPanel(
  "Conclusion of the Project",
  h2("Conclusion"),
  p("Looking through the ", em("Map of Average Death"), " we were able to 
      observe that the proportion of average death caused from COVID-19 was 
      was higher from the countries of North America, Asia, and Europe. The plot
      enables us to discover how certain countries had striking death rate 
      compared to other countries, and indicates the need for the vaccinations 
      to decrease the death rate was imminent."),
  p("Using the ", em("New Covid Deaths vs Months graph"), " we found that 
    new deaths were increasing worldwide and while some continents have 
    decreasing numbers Covid deaths is still a pertinent issue. We hope this 
    data can highlight the importance of this issue and the need for COVID-19 
    vaccinations in order to protect vulnerable populations and decrease 
    Covid related deaths."),
  p("Our results from the ", em("Average Vaccinations Per Month"), " 
    visualization showed that even though the average number of vaccinations is 
    generally increasing over time, the average number of vaccinations that
    higher income countries receive is a lot higher than lower income countries.
    Higher income countries also received the vaccines earlier than lower income 
    countries. As seen in other visualizations, this also indicates that there
    is a need for better vaccine distribution."),
  p(
    "Looking at the ", em("Proportion Vaccinated vs. Proportion of New Covid Deaths"),
    " plot we can't see any obvious trends 
    between the proportion of people fully vaccinated in a country and the 
    proportion of new deaths in that country. This is likely due to the fact 
    that none of these countries have reached herd immunity yet, which is when 
    such a large proportion of a community becomes immune to a disease making, 
    the spread of the disease from person to person very unlikely. However, 
    since most countries will still take quite a while to reach herd immunity 
    vaccines are currently mostly effective for the individual who has gotten 
    the vaccine. The only exception seems to be Israel which seems to be
    the country closest to reaching herd immunity, if not having already reached it,
    with ", em("58.463%"), " of their population being fully vaccinated and ",
    em("0%"), "of their population having any new deaths."
  ),
  h2("Limitations"),
  p("A limitation of this project is that the data we used is continuously
    being updated, so our findings and visualizations in this project can
    change. Another limitation is that the data is self-reported, which means
    that the data we use and findings may not be entirely accurate or correct.")
)

ui <- navbarPage(
  "COVID-19 Vaccinations and Deaths",
  introduction_panel,
  page_one,
  page_two,
  page_three,
  page_four,
  conclusion_panel
)
