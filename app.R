# app.R

# Loading necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Loading five countries data
ireland_data <- read.csv("C:/Users/chels/Downloads/shiny_app/hdro_indicators_irl.csv")
india_data <- read.csv("C:/Users/chels/Downloads/shiny_app/hdro_indicators_ind.csv")
canada_data <- read.csv("C:/Users/chels/Downloads/shiny_app/hdro_indicators_can.csv")
australia_data <- read.csv("C:/Users/chels/Downloads/shiny_app/hdro_indicators_aus.csv")
singapore_data <- read.csv("C:/Users/chels/Downloads/shiny_app/hdro_indicators_sgp.csv")

# Combining entire data into a single data frame
all_data <- bind_rows(
  ireland_data %>% mutate(Country = "Ireland"),
  india_data %>% mutate(Country = "India"),
  canada_data %>% mutate(Country = "Canada"),
  australia_data %>% mutate(Country = "Australia"),
  singapore_data %>% mutate(Country = "Singapore")
)

# Defining UI
ui <- fluidPage(
  titlePanel(textOutput("Title")),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Choose Country", choices = unique(all_data$Country)),
      fileInput("file1", "Upload additional HDI data", accept = c(".csv")),
      uiOutput("column_selector"),
      numericInput("rows", "Displayed number of rows:", 5, min = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", dataTableOutput("table")),
        tabPanel("1st Plot", 
                 selectInput("xvar1", "X-axis variable:", choices = NULL),
                 selectInput("yvar1", "Y-axis variable:", choices = NULL),
                 selectInput("color1", "Color by:", choices = NULL),
                 plotOutput("plot1")),
        tabPanel("2nd Plot", 
                 selectInput("xvar2", "X-axis variable:", choices = NULL),
                 selectInput("yvar2", "Y-axis variable:", choices = NULL),
                 selectInput("color2", "Color by:", choices = NULL),
                 plotOutput("plot2"))
      )
    )
  )
)

# Defining server logic
server <- function(input, output, session) {
  
  # Reactive expression for managing the chosen data
  data_selected <- reactive({
    data <- all_data %>% filter(Country == input$country)
    req(data)
    data
  })
  
  # Reactive expression for managing the uploaded data
  data_uploaded <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data_updated <- read.csv(inFile$datapath)
    data_updated
  })
  
  # Combining the selected data and uploaded data
  data_combined <- reactive({
    data <- data_selected()
    data_updated <- data_uploaded()
    if (!is.null(data_updated)) {
      data <- bind_rows(data, data_updated)
    }
    data
  })
  
  # Updating column selectors based on the selected/combined data
  observe({
    data <- data_combined()
    updateSelectInput(session, "xvar1", choices = names(data))
    updateSelectInput(session, "yvar1", choices = names(data))
    updateSelectInput(session, "color1", choices = names(data))
    updateSelectInput(session, "xvar2", choices = names(data))
    updateSelectInput(session, "yvar2", choices = names(data))
    updateSelectInput(session, "color2", choices = names(data))
  })
  
  # Displaying the title
  output$Title <- renderText({
    paste("Human Development Indicators for", input$country)
  })
  
  # Display the selected data table
  output$table <- renderDataTable({
    data <- data_combined()
    req(input$rows)
    datatable(data[, input$columns], options = list(pageLength = input$rows))
  })
  
  # Displaying Plot 1
  output$plot1 <- renderPlot({
    data <- data_combined()
    req(input$xvar1, input$yvar1, input$color1)
    ggplot(data, aes_string(x = input$xvar1, y = input$yvar1, color = input$color1)) +
      geom_point() +
      theme_minimal()
  })
  
  # Displaying Plot 2
  output$plot2 <- renderPlot({
    data <- data_combined()
    req(input$xvar2, input$yvar2, input$color2)
    ggplot(data, aes_string(x = input$xvar2, y = input$yvar2, color = input$color2)) +
      geom_point() +
      theme_minimal()
  })
  
  # Dynamically generate column selector UI
  output$column_selector <- renderUI({
    data <- data_combined()
    req(data)
    checkboxGroupInput("columns", "Select columns to display", choices = names(data), selected = names(data))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
