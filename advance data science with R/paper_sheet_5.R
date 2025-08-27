# Exercise Sheet 5
# Authors: Akram Helal, 
#          Seyed Ramtin Hosseini 

library(shiny)                   # For creating Shiny web app
library(ggplot2)                 # For creating plots
library(tidyverse)               # For data manipulation
#install.packages("shinythemes") # installing shinythemes
library(shinythemes)             # For better themes for the UI

# Set working directory to the location of the current script/file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#-------------------------------------------------------------------------------
# Exercise 0_c) load_and_prepare_data function
# The function reads a population dataset from a CSV file and cleans the 
# age_group variable by removing prefixes and replacing underscores with hyphens.
# It then converts age_group into an ordered factor based on the numeric age 
# start values and sets year as a factor with levels ordered by their appearance.
# This prepares the data for correct ordering and labeling in plots and analyses. 
#-------------------------------------------------------------------------------
load_and_prepare_data <- function(path_to_file) {
  # read csv file into a dataframe pop_data
  pop_data <- read_csv(path_to_file)
  
  pop_data <- pop_data %>%
    # remove "pop_" prefix from age_group
    mutate(age_group = gsub("pop_", "", age_group),
           # replace underscores with hyphens (example 20_24 -> 20-24)
           age_group = gsub("_", "-", age_group),
           # convert age_group to an ordered factor based on starting age
           age_group = factor(age_group, levels = unique(age_group[order(as.numeric(str_extract(age_group, "^[0-9]+")))]))) %>%
    # convert year to factor with levels ordered by apperanace
    mutate(year = factor(year, levels = unique(year)))
}

#-------------------------------------------------------------------------------
# Exercise 0_a) UI
# The UI code generates a User Interface with the title "What do I know?". 
# It includes a dynamic UI element (uiOutput("source")) and a tab called 
# "Histogram" that displays a plot (plotOutput("histogramPlot")).
#-------------------------------------------------------------------------------
ui <- fluidPage(
  # Exercise 3); Adding theme superhero from package shinythemes
  theme = shinytheme("superhero"),
  # app title
  titlePanel("Population Structures of Germany 1950-2021"), # Exercise 1_a); changing the title
  
  # p() adds a new paragraph
  p("Authors: Seyed Ramtin Hosseini and Akram Helal"), # Exercise 1_b), adding authors
  
  # dynamic UI output for source link
  uiOutput("source"),
  
  # Tabset panel with a multiple tabs
  tabsetPanel(
    # Tab Panel caleld Histogram
    tabPanel(
      "Histogram", 
      
      # Exercise 1_c), adding sliders
      sliderInput("selected_year", "Selected Year:", min = 1950, max = 2021, value = 1950, step = 1, sep = ""),
      
      # Exercise 1_d), adding choices for the user, female, male and total
      selectInput("gender", "Select Population Group: ", 
                  choices = c("Total" = "total", "Female" = "female", "Male" = "male"),
                  selected = "total"), # total is default
      
      # Exercise 3); choices for histogram colour
      selectInput("plot_color", "Select Plot Color:",
                  choices = c("Skyblue" = "skyblue", "Tomato" = "tomato", "Darkgreen" = "darkgreen", 
                              "Purple" = "purple", "Black" = "black", "Yellow" = "yellow"),
                  selected = "skyblue"), # default: skyblue
      # Output for the histogram plot
      plotOutput("histogramPlot"),
      
      # placeholder for heading, since it has to change dynamically
      uiOutput("population_header"),
      # output of the sum of total population 
      verbatimTextOutput("total_population")
    ),
    
    # Exercise 2; Binned histogramm
    tabPanel(
      "Binned Histogram",
      # input for bin size
      numericInput(
        inputId = "bin_size",
        label = "Set Bin Size (years)",
        value = 5, # default bin size
        min = 1,
        max = 100 # since the datasat only goes to 99, max is 100
      ),
      # show selected year from histogram tab
      p("Selected Year:"),
      verbatimTextOutput("selected_year_text"),
      
      # plot output for binned histogram
      plotOutput("binnedHistogramPlot")
    ),
    # Exercise 1_e); adding new TabPanel Manual
    tabPanel(
      "Manual",
      # h1, h2 and h3 for different headings
      h2("How to Use this Shiny R App"),
      # p for paragraphs
      p("This app allows you to explore the population structure of Germany between the years 1950 and 2021"),
      h3("You can:"),
      tags$ul( # ul -> unordered list
        # li -> add list item
        tags$li("Use the slider to select the year you want to see or analyze."),
        tags$li("Choose between total, female or male population using the dropdown menu in the histogram panel."),
        tags$li("Select your preferred colour for the plots in both the histogram and binned histogram panels. The colours are set trough the dropdown menu in the histogram panel."),
        tags$li("View how the population is distributed across different age groups for the chosen year in both panels."),
        tags$li("At the bottom of the histogram panel you can also see the total sum of the population count for your selected year and population group."),
        tags$li("In the binned histogram panel you can set the bin size. Minimum is 1, maximum is 100 and default is 5 years."),
        tags$li("Below the input for the bin size, you can see the selected year. The year is selected trough the histogram panel with the slider.")
      ),
      h2("Trends and Observations"),
      p("Over time, you may notice that the total population is having fewer children while people are living longer."),
      p("When comparing the female and male populations (for the year 1990), you will notice that there are more elderly females than males, partly due to the consequences of World War II."),
      p("These trends highlight demographic shifts such as increased life expectancy and lower birth rates.")
    ),
    # Exercise 3
    tabPanel(
      "Free Modification",
      h3("Added Modification:"),
      tags$ul( # ul -> unordered list
        # li -> add list item
        tags$li("In the histogram panel the user can now decide which colour he wants to have for the plot. Affects also the binned histogramm panel."),
        tags$li("Added total count of population for the selected year at the bottom of the histogram panel. It is affected by the choice of the user for female, male and total."),
        tags$li("Added shinythemes package for the superhero theme for a darker theme with strong contrasts.")
      ),
    )
  )
)

#-------------------------------------------------------------------------------
# Exercise 0_b) server
# The server function loads and prepares pop_data and displays the data source 
# as a clickable link.
# It filters the data for the year 1950 and uses this filtered data to render 
# a histogram plot showing the population by age groups.
# Since the data is static, the plot does not update dynamically.
#-------------------------------------------------------------------------------
server <- function(input, output) {
  
  # looad and prepare the population data from the local csv file
  pop_data <- load_and_prepare_data("data/pop_data_1950-2021.csv")
  
  # create a clickable link for the data source
  url <- a("Federal Statistical Office of Germany", href="https://service.destatis.de/bevoelkerungspyramide/index.html#!")
  # render the source link in the UI
  output$source <- renderUI({
    tagList("Data source:", url)
  })
  
  # Exercise 1_c)
  pop_year <- reactive({
    # filter data based on selected year from slider input
    pop_data %>% filter(year == input$selected_year) # was set to 1950 in app.R
  })
  
  # Render the histogram plot
  output$histogramPlot <- renderPlot({
    ggplot(pop_year(), aes(x = age_group, y = .data[[input$gender]])) + # Exercise 1_d); changing gender to user choice for y-axis
      geom_bar(stat = "identity", fill = input$plot_color) + # Exercise 3); draw bars with colour choice
      labs(
        title = paste("Population by Age Groups in", input$selected_year), # Exercise 1_c); show selected year
        x = "Age Groups",
        y = "Total population count"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # rotate x-axis labels
  })
  
  # Exercise 3)
  # show heading 3 with selected gender from user
  output$population_header <- renderUI({
    h3("Total population count for selected population group:",input$gender)
  })
  
  # Exercise 3)
  # show total population count for selection
  output$total_population <- renderText({
    format(sum(pop_year()[[input$gender]], na.rm = TRUE) * 1000, big.mark = ".") # *1000 to get correct count, and big mark for better view
  })
  
  # Exercise 2)
  # Show selected year text in binned panel from the histogram panel
  output$selected_year_text <- renderText({
    input$selected_year
  })
  
  # Exercise 2
  # Reactive binned data
  binned_data <- reactive({
    df_binned <- pop_year() # Use filtered data for selected year
    bin_size <- input$bin_size # Get bin size from numeric input
    
    df_binned <- df_binned %>%
      mutate(
        # Extract numeric age start
        age_start = as.numeric(str_extract(age_group, "^[0-9]+")),
        # Calculate lower bin boundary
        age_bin = floor(age_start / bin_size) * bin_size,
        # Create bin labels
        age_bin_label = paste(age_bin, age_bin + bin_size - 1, sep = "-")
      ) %>%
      group_by(age_bin_label, age_bin) %>%  # Keep age_bin for ordering
      summarise(
        # Sum the population for each bin and selected gender
        population = sum(.data[[input$gender]])
      ) %>%
      ungroup() %>%
      arrange(age_bin) %>% # Arrange by numeric bin
      mutate(
        age_bin_label = factor(age_bin_label, levels = unique(age_bin_label))
      )
    
    df_binned
  })
  
  # Exercise 2
  # Render binned histogram plot
  output$binnedHistogramPlot <- renderPlot({
    ggplot(binned_data(), aes(x = age_bin_label, y = population)) +
      geom_bar(stat = "identity", fill = input$plot_color) + # Exercise 3); set user colour
      labs(
        title = paste(input$gender, "Population by Binned Age Groups in", input$selected_year), # Exercise 1_c); set user year
        x = "Binned Age Groups",
        y = "Population Count"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Rotate x-axis labels
  })
}

# Run the application
shinyApp(ui = ui, server = server)
