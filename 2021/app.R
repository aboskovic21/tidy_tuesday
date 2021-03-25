library(shiny)
library(shinythemes)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2019-01-29')
cheese <- tuesdata$clean_cheese
cheese_tidy <- gather(cheese, key = "cheese_type", value = "amount", -Year)


# Define UI for dataset viewer app ----
ui <- navbarPage(
  
  title="Cheese",
  
  tabPanel(
    title = "Line Graph",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(inputId = "type"
                           , label = "Choose a cheese type"
                           , choices = unique(cheese_tidy$cheese_type)
                           , selected = "Cheddar"
                           , inline = TRUE)
      ),
      mainPanel(
        plotOutput(outputId = "line")
      )
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  data <- reactive ({
    cheese_tidy %>% filter(cheese_type %in% input$type)
  })

  
  # Generate a summary of the dataset ----
  output$line <- renderPlot({
    ggplot(data(), aes(x = Year, y = amount, color = input$type)) + 
      geom_line()
  })
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)