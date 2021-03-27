# Load packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)

# Load dataset
tuesdata <- tidytuesdayR::tt_load('2019-01-29')

# Cheese Data
cheese <- tuesdata$clean_cheese %>%
  select("Year", "Total American Chese", "Mozzarella", "Swiss", "Brick", "Muenster", "Blue", "Cheddar") %>%
  rename("American" = "Total American Chese")
cheese_tidy <- gather(cheese, key = "cheese_type", value = "amount", -Year)
cheese_choices <- c("American", "Mozzarella", "Swiss", "Brick", "Muenster", "Blue", "Cheddar")

# Fluid Milk Data
fluid_milk <- tuesdata$fluid_milk_sales %>%
  mutate(year = as.Date(as.character(year), format = "%Y"),
         year = year(year))
for_fluid_milk <- fluid_milk %>%
  filter(milk_type != "Total Production")
milk_choices <- unique(for_milk_choices$milk_type)

fluid_milk <- fluid_milk %>%
  mutate(year = strptime(year, "%Y"),
         year = format(year, "%Y"),
         year = as.Date(year, "%Y"))
#max_year_value <- year(as.Date(as.character(2003), format = "%Y"))


# Milk Products Data
milk_prod_tidy <- gather(milk_prod, key = "product", value = "amount", -year) %>%
  filter(product != "fluid_milk") %>%
  mutate(is_yogurt = str_detect(product, 'yogurt'),
         is_cheese = str_detect(product, 'cheese'),
         is_butter = str_detect(product, 'butter'),
         is_dry = str_detect(product, 'dry'),
         is_evap = str_detect(product, 'evap_cnd'),
         is_frozen = str_detect(product, 'frozen'))

milk_prod_small <- milk_prod_tidy %>%
  mutate(product_general = ifelse(is_yogurt == TRUE, "yogurt",
                                  ifelse(is_cheese == TRUE, "cheese",
                                         ifelse(is_butter == TRUE, "butter",
                                                ifelse(is_dry == TRUE, "dry",
                                                       ifelse(is_evap == TRUE, "evap", "frozen")
                                                )
                                         )
                                  )
  )) %>%
  select(year, amount, product_general) %>%
  group_by(year, product_general) %>%
  summarise(total_amount = sum(amount))

prod_choice_values <- c("butter", "cheese", "dry", "evap", "frozen", "yogurt")
prod_choice_names <- c("Butter","Cheese","Dry", "Evaporated", "Frozen", "Yogurt")
names(prod_choice_values) <- prod_choice_names

# Miscellaneous
`%not_in%` <- purrr::negate(`%in%`)



# Define UI for dataset viewer app ----
ui <- navbarPage(
  theme = shinytheme("sandstone"),
  
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
        # sliderInput(inputId = "milk_year"
        #                    , label = "Choose years of interest"
        #                    , min = min(fluid_milk$year)
        #                    , max = max(fluid_milk$year)
        #                    , value = c(fluid_milk$year[15], fluid_milk$year[20])
        #             ),
        dateRangeInput(inputId = "milk_year",
                       label = "Choose years of interest",
                       start = fluid_milk$year[15],
                       end = fluid_milk$year[20],
                       min = min(fluid_milk$year),
                       max = max(fluid_milk$year),
                       format = "yyyy"
                       ),
        checkboxGroupInput(inputId = "milk_type"
                           , label = "Choose a milk type to remove from the plot:"
                           , choices = milk_choices
                           , selected = NULL
                           , inline = TRUE)
      ),
      mainPanel(
        DT::dataTableOutput(outputId = "milk_popularity_table"),
        plotOutput(outputId = "milk_line")
      )
    )
  ),
  tabPanel(
    title = "All Dairy Products",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(inputId = "product"
                           , label = "Choose a product to remove from the plot:"
                           , choices = prod_choice_values
                           , selected = NULL
                           , inline = TRUE)
      ),
      mainPanel(
        plotOutput(outputId = "dairy_general")
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
      rename("Year" = "year",
             "Most Popular" = "most_popular",
             "Milk Type" = "milk_type") %>%
      mutate(Year = format(Year, format="%Y")) %>%
      arrange(Year)
  })
  
  output$milk_popularity_table <- DT::renderDataTable({
    data_for_table()
  })
  
  data_for_milkline <- reactive ({
    for_fluid_milk %>% 
      filter(milk_type %not_in% input$milk_type)
  }) 
  
  output$milk_line <- renderPlot ({
    ggplot(data_for_milkline(), aes(x = year, y = pounds, color = milk_type)) + 
      geom_line() +
      labs(x = "Year",
           y = "Pounds of Milk Product",
           title = "Milk Production over Time in the US",
           color = "Milk Type"
      ) +
      theme_classic()
  })
  
  dairy_data <- reactive ({
    milk_prod_small %>%
      filter(product_general %not_in% input$product)
  })
  
  output$dairy_general <- renderPlot ({
    ggplot(dairy_data(), aes(x = year, y = total_amount, color = product_general)) +
      geom_line() +
      labs(x= "Year", y = "Average Comsumption per Person",
           title = "Average Dairy Product Comsuption per Person over Time",
           color = "Product") + 
      scale_color_manual(labels=c("Butter","Cheese","Dry", "Evaporated", "Frozen", "Yogurt"), 
                         values = c("#f8766d", "#afa100", "#00ba42", "#00b4ef", "#ac88ff", "#fc61d5")) + 
      theme_classic()
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)