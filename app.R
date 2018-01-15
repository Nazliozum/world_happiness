library(shiny)
library(ggplot2)
library(dplyr)

happiness2015 <- read.csv("results/happiness_2015_clean.csv", stringsAsFactors = TRUE)
happiness2016 <- read.csv("results/happiness_2016_clean.csv", stringsAsFactors = TRUE)
happiness2017 <- read.csv("results/happiness_2017_clean.csv", stringsAsFactors = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
   # Application title
   titlePanel("World Happiness"),
   
   sidebarLayout(
     
     # Sidebar with inputs  
     sidebarPanel(
        uiOutput("region"), 
        uiOutput("country"),
        uiOutput("variable_1"),
        uiOutput("variable_2")
     ),
        
      mainPanel(
        actionButton("button2015", "2015"),
        actionButton("button2016", "2016"),
        actionButton("button2017", "2017"),
        hr(),
        plotOutput("scatterplot"),
        br(), br(), br(),
        tableOutput("rank_table")
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  mydata <- reactiveValues(df_data = NULL)
  
  observeEvent(input$button2015, {
    mydata$df_data <- happiness2015
  })
  
  observeEvent(input$button2016, {
    mydata$df_data <- happiness2016
  })
  
  observeEvent(input$button2017, {
    mydata$df_data <- happiness2017
  })
  
  
  output$region <- renderUI({
    checkboxGroupInput("region", "Region:",
                       levels(mydata$df_data$Region)
    )
  })
  
  
  output$country <- renderUI ({
      if(is.null(input$region))
        return()
    
    df_small <- 
        mydata$df_data %>% 
        filter(Region %in% input$region) %>% 
        droplevels()
    
      countries <- c("Select All", levels(df_small$Country))
    
      selectInput("country", "Select countries:", multiple = TRUE,
                  choices = countries
      )
  })
  
  observe({
    if ("Select All" %in% input$country) {
      # choose all the choices _except_ "Select All"
      selected_choices <- setdiff(choices, "Select All")
      updateSelectInput(session, "country", selected = selected_choices)
    }
  })
  
  
  filtered_data <- reactive({
    mydata$df_data %>% 
      filter(
        Region %in% c(input$region),
        Country %in% c(input$country)
      )
  })
  
  
  output$variable_1 <- renderUI({
    radioButtons("variable_1", "First variable:", 
                 choices = c("Happiness.Rank", "Happiness.Score", "Economy", 
                             "Family", "Health", "Freedom", "Government.Corruption", 
                             "Generosity", "Dystopia.Residual"), selected = "Happiness.Score")
  })
  
  
  output$variable_2 <- renderUI({
    radioButtons("variable_2", "Second variable:", 
                 choices = c("Happiness.Rank", "Happiness.Score", "Economy", 
                             "Family", "Health", "Freedom", "Government.Corruption", 
                             "Generosity", "Dystopia.Residual"), selected = "Economy")
  })
  
  output$scatterplot <- renderPlot({
    if (is.null(mydata$df_data)) return()
    
    ggplot(filtered_data()) +
      geom_point(aes_string(x = input$variable_2, y = input$variable_1, colour = "Region")) +
      ggtitle(paste0(input$variable_1, " vs. ", input$variable_2)) +
      theme_bw() +
      theme(legend.position = "right")
  })
  
  output$rank_table <- renderTable({
    if (is.null(mydata$df_data)) return()
    mydata$df_data %>% 
      arrange(Happiness.Rank) %>% 
      select(Happiness.Rank, Country) %>% 
      top_n(10, desc(Happiness.Rank))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

