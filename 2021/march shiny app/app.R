library(tidyverse)
library(lubridate)
# library(shiny)
library(shinythemes)

# For icons: https://fontawesome.com/icons?d=gallery&p=2&q=glass&m=free

# Load dataset
tuesdata <- tidytuesdayR::tt_load('2019-01-29')
cheese <- tuesdata$clean_cheese
fluid_milk <- tuesdata$fluid_milk_sales
milk_prod <- tuesdata$milk_products_facts
milkcow <- tuesdata$milkcow_facts
state_milk <- tuesdata$state_milk_production

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
milk_choices <- unique(for_fluid_milk$milk_type)

fluid_milk <- fluid_milk %>%
  mutate(year = strptime(year, "%Y"),
         year = format(year, "%Y"),
         year = as.Date(year, "%Y"))


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

# Milkcow
milkcow_clean <- gather(milkcow, key = "cow_stat", value = "amount", -year)
cow_choice_values <- colnames(milkcow)[2:11]
cow_choice_names <- c("Avg. Number of Milk Cows","Avg. Milk Production", "Total Milk Production", 
                      "Avg. Price of Milk", "Avg. Price of Dairy Cow Rations", "Milk Price:Cow Ration Ratio",
                      "Avg. Cost of Milk Cow", "Milk Vol. to Buy Cow", "Alfalfa Hay Price",
                      "Slaughter Cow Price"
                      )
names(cow_choice_values) <- cow_choice_names

# Dataset info
dataset_choice_values <- c("clean_cheese","fluid_milk","milk_products_facts", "milkcow_facts")
dataset_choice_names <- c("Cheese", "Milk", "Dairy Products", "Cow")
names(dataset_choice_values) <- dataset_choice_names

# Miscellaneous
`%not_in%` <- purrr::negate(`%in%`)

milkcow <- "cow.png"
dairy <- "dairy_products.png"
milk <- "milk.png" 
cheese <- "cheeses.png"

######## Define UI for dataset viewer app #######
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
                           , inline = TRUE),
        actionButton(inputId = "cheese_comp", label = "Say Cheese!", 
                     icon = icon("cheese"))
      ),
      mainPanel(
        br(),
        conditionalPanel(
          condition = "input.cheese_comp == 0",
          column(
            width = 12,
            img(src = cheese, width = "50%", 
                style="display: block; margin-left: auto; margin-right: auto;")
          )),
        plotOutput(outputId = "line")
      )
    )
  ),
  
  tabPanel(
    title = "Most Popular Milk",
    sidebarLayout(
      sidebarPanel(
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
                           , inline = TRUE),
        actionButton(inputId = "milk_comp", label = "Compare Milk!", 
                     icon = icon("glass-whiskey"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            br(),
            title = "Table",
            br(),
            conditionalPanel(
              condition = "input.milk_comp == 0",
              column(
                width = 12,
                img(src = milk, width = "50%", height = "10%",
                    style="display: block; margin-left: auto; margin-right: auto;")
              )),
            DT::dataTableOutput(outputId = "milk_popularity_table")
          ),
          tabPanel(
            br(),
            title = "Line Graph",
            br(),
            conditionalPanel(
              condition = "input.milk_comp == 0",
              column(
                width = 12,
                img(src = milk, width = "50%", height = "10%",
                    style="display: block; margin-left: auto; margin-right: auto;")
              )),
            plotOutput(outputId = "milk_line")
          )
        )
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
                           , inline = TRUE),
        actionButton(inputId = "dairy_comp", label = "Plot!", 
                     icon = icon("ice-cream"))
      ),
      mainPanel(
        br(),
        conditionalPanel(
          condition = "input.dairy_comp == 0",
          column(
            width = 12,
            img(src = dairy, width = "50%", 
                style="display: block; margin-left: auto; margin-right: auto;")
          )),
        plotOutput(outputId = "dairy_general")
      )
    )
  ),
  
  tabPanel(
    title = "Cow Facts",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "y_var"
                    , label = "Choose a variable of interest to plot:"
                    , choices = cow_choice_values
                    , selected = "avg_milk_cow_number"),
        actionButton(inputId = "compare", label = "Show me the cow facts!", 
                     icon = icon("chart-line"))
      ),
      mainPanel(
        br(),
        conditionalPanel(
          condition = "input.compare == 0",
          column(
            width = 12,
            img(src = milkcow, width = "50%", 
                style="display: block; margin-left: auto; margin-right: auto;")
          )),
        plotOutput(outputId = "cowplot")
      )
    )
  ),
  
  tabPanel(
  title = "About the Data",
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "dataset_info",
        label = "Choose a dataset to learn about",
        choices = dataset_choice_values,
        selected = "clean_cheese"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Variables",
          br(),
          imageOutput(outputId = "data_info_img")),
        tabPanel(
          title = "General Infomration",
          br(),
          tags$a(href="https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-01-29", 
                 "The data for this project came from the R4DS Tidy Tuesday Project: 01/29/2019."))
      )
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

  line <- eventReactive (input$cheese_comp, {
    ggplot(data_for_cheese(), aes(x = Year, y = amount, color = cheese_type)) + 
      geom_line() +
      labs(x = "Year",
           y = "Consumption in lbs per Person",
           title = "Cheese Consumption over Time in the US",
           color = " Cheese Type"
      ) +
      theme_classic() 
  })
  
  # Cheese popularity over years
  output$line <- renderPlot({
      line()
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
  
  milk_pop_table <- eventReactive (input$milk_comp, {
    data_for_table()
  })
  
  output$milk_popularity_table <- DT::renderDataTable({
    milk_pop_table()
  })
  
  data_for_milkline <- reactive ({
    for_fluid_milk %>% 
      filter(milk_type %not_in% input$milk_type)
  }) 
  
  milk_line <- eventReactive (input$milk_comp, {
    ggplot(data_for_milkline(), aes(x = year, y = pounds, color = milk_type)) + 
      geom_line() +
      labs(x = "Year",
           y = "Pounds of Milk Product",
           title = "Milk Production over Time in the US",
           color = "Milk Type"
      ) +
      theme_classic() +
      geom_vline(xintercept = 1993, linetype = "dashed") +
      geom_text(aes(x = 1997, label="got milk?\nCampaign Launched", y=15000000000), color = "black", text=element_text(size=12))
  })
  
  output$milk_line <- renderPlot ({
    milk_line()
  })
  
  dairy_data <- reactive ({
    milk_prod_small %>%
      filter(product_general %not_in% input$product)
  })
  
  dairy_general <- eventReactive (input$dairy_comp, {
    ggplot(dairy_data(), aes(x = year, y = total_amount, color = product_general)) +
      geom_line() +
      labs(x= "Year", y = "Average Comsumption per Person",
           title = "Average Dairy Product Comsuption per Person over Time",
           color = "Product") + 
      scale_color_manual(labels=c("Butter","Cheese","Dry", "Evaporated", "Frozen", "Yogurt"), 
                         values = c("#f8766d", "#afa100", "#00ba42", "#00b4ef", "#ac88ff", "#fc61d5")) + 
      theme_classic()
  })
  
  output$dairy_general <- renderPlot ({
    dairy_general()
  })
  
  cowdata <- reactive ({
    milkcow_clean %>%
      filter(cow_stat %in% input$y_var)
  })
  
  cowplot <- eventReactive (input$compare, {
    ggplot(data = cowdata(), aes(x = year, y = amount)) +
      geom_line() +
      labs(x = "Year", 
           y = cow_choice_names[cow_choice_values == input$y_var],
           title = paste0(cow_choice_names[cow_choice_values == input$y_var], " over Time")
      ) + 
      theme(plot.title = element_text(size = 20)) +
      theme_classic()
  })
  
  output$cowplot <- renderPlot ({
    cowplot()
  })
  
  
  output$data_info_img <- renderImage({
    
    image_file <- paste0("www/", input$dataset_info,".png")
    
    if(image_file == "www/clean_cheese.png") {
    return(list(
      src = image_file,
      style="display: block; margin-left: auto; margin-right: auto;",
      height = 520,
      width = 650
    ))
    }
    else if (image_file == "www/milk_products_facts.png"){
      return(list(
        src = image_file,
        style="display: block; margin-left: auto; margin-right: auto;",
        height = 520,
        width = 650
      ))
    }
    else if (image_file == "www/fluid_milk.png"){
      return(list(
        src = image_file,
        style="display: block; margin-left: auto; margin-right: auto;",
        height = 150,
        width = 400
      ))
    }
    else if (image_file == "www/milkcow_facts.png"){
      return(list(
        src = image_file,
        style="display: block; margin-left: auto; margin-right: auto;",
        height = 400,
        width = 650
      ))
    }
  }, deleteFile = FALSE)

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)