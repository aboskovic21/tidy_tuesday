library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)

tuesdata <- tidytuesdayR::tt_load('2019-01-29')

cheese <- tuesdata$clean_cheese %>%
  select("Year", "Total American Chese", "Mozzarella", "Swiss", "Brick", "Muenster", "Blue", "Cheddar") %>%
  rename("American" = "Total American Chese")
cheese_tidy <- gather(cheese, key = "cheese_type", value = "amount", -Year)
cheese_choices <- c("American", "Mozzarella", "Swiss", "Brick", "Muenster", "Blue", "Cheddar")

fluid_milk <- tuesdata$fluid_milk_sales %>%
  mutate(year = as.Date(as.character(year), format = "%Y"),
         year = year(year))

min_year_value <- year(as.Date(as.character(1999), format = "%Y"))
max_year_value <- year(as.Date(as.character(2003), format = "%Y"))

`%not_in%` <- purrr::negate(`%in%`)

# Define UI for dataset viewer app ----
ui <- navbarPage(
  
  title="Dairy in the US",
  
  tabPanel(
    title = "Consumption by Cheese Type",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(inputId = "type"
                           , label = "Choose a cheese type to remove from the plot:"
                           , choices = cheese_choices
                           , selected = NULL
                           , inline = TRUE)
      ),
      mainPanel(
        plotOutput(outputId = "line")
      )
    )
  ),
  
  tabPanel(
    title = "Most Popular Milk",
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "milk_year"
                           , label = "Choose years of interest"
                           , min = min(fluid_milk$year)
                           , max = max(fluid_milk$year)
                           , value = c(min_year_value, max_year_value)
                           , format= "####"
                    )
      ),
      mainPanel(
        DT::dataTableOutput(outputId = "milk_popularity_table")
      )
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  data_for_cheese <- reactive ({
    cheese_tidy %>% 
      filter(cheese_type %not_in% input$type)
  })

  
  # Cheese popularity over years
  output$line <- renderPlot({
    ggplot(data_for_cheese(), aes(x = Year, y = amount, color = cheese_type)) + 
      geom_line() +
      labs(x = "Year",
            y = "Consumption in lbs per Person",
           title = "Cheese Consumption over Time in the US",
           color = " Cheese Type"
           ) +
      theme_classic()
  })
  
  data_for_table <- reactive ({
    milk <- fluid_milk %>%
      filter(milk_type != "Total Production") %>%
      filter(year >= min(input$milk_year), year <= max(input$milk_year)) %>%
      group_by(year) %>%
      summarise(most_popular = max(pounds)) %>%
      inner_join(fluid_milk, by = c("year", "most_popular" = "pounds")) %>%
      # mutate()
      rename("Year" = "year",
             "Most Popular" = "most_popular",
             "Milk Type" = "milk_type") %>%
      arrange(Year)
  })
  
  output$milk_popularity_table <- DT::renderDataTable({
    data_for_table()
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)