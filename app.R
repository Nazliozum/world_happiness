library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)

happiness2015 <- read.csv("results/happiness_2015_clean.csv", stringsAsFactors = TRUE)
happiness2016 <- read.csv("results/happiness_2016_clean.csv", stringsAsFactors = TRUE)
happiness2017 <- read.csv("results/happiness_2017_clean.csv", stringsAsFactors = TRUE)
countries_all <- read.csv("results/countries_aggregated.csv", stringsAsFactors = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("World Happiness"),
  fluidRow(
    column(2,
           br(),
           p("Make some explanation about the app here"),
           br(),
           uiOutput("region"), 
           uiOutput("country"),
           hr(), br(),
           uiOutput("variable_1"),
           uiOutput("variable_2"),
           hr(), br(),
           sliderInput("filter_Happiness.Score", "Happiness Score", 
                       min = 0, max = 10, value = c(0, 10), step = 0.1),
           sliderInput("filter_Economy", "Economy score", 
                       min = 0, max = 2, value = c(0, 2), step = 0.1),
           sliderInput("filter_Family", "Family score", 
                       min = 0, max = 2, value = c(0, 2), step = 0.1),
           sliderInput("filter_Health", "Health score", 
                       min = 0, max = 2, value = c(0, 2), step = 0.1),
           sliderInput("filter_Freedom", "Freedom score", 
                       min = 0, max = 2, value = c(0, 2), step = 0.1),
           sliderInput("filter_Government.Corruption", "Government corruption score", 
                       min = 0, max = 2, value = c(0, 2), step = 0.1),
           sliderInput("filter_Generosity", "Generosity Score", 
                       min = 0, max = 2, value = c(0, 2), step = 0.1)
           
    ),
    
    column(8,
           tabsetPanel(
             tabPanel("Plot",
                      br(),
                      actionButton("button2015", "2015"),
                      actionButton("button2016", "2016"),
                      actionButton("button2017", "2017"),
                      hr(),
                      plotlyOutput("scatterplot", 
                                 height = 800),
                      br(),
                      DT::dataTableOutput("results_table")
             ),
             
             tabPanel("Table", 
                      br(),
                      tableOutput("country_table"),
                      br(),
                      fluidRow(
                        column(6, 
                               plotOutput("time_series_1")),
                        column(6, 
                               plotOutput("time_series_2"))
                      )
                      )
           )
        ),
    
    column(2,
          wellPanel(
            h4("Top 10"), 
            DT::dataTableOutput("rank_table_top")),
          br(), br(), 
          wellPanel(
            h4("Bottom 10"),
            DT::dataTableOutput("rank_table_bottom"))
        )

)
)

server <- function(input, output, session) {
  
  countries <- reactive({ 
    df_small <- 
      countries_all %>% 
      filter(Region %in% input$region) %>%
      select(Country) %>% 
      droplevels()
    
    c("Select All", levels(df_small$Country))
  
      })
  
  data_year <- reactiveValues(data = NULL, year = NULL)
  
  observeEvent(input$button2015, {
    data_year$data <- happiness2015
    year <- "2015"
  })
  
  
  observeEvent(input$button2016, {
    data_year$data <- happiness2016
    data_year$year <- "2016"
  })
  
  observeEvent(input$button2017, {
    data_year$data <- happiness2017
    data_year$year <-  "2017"
  })
  
  output$year_name <- renderPrint({ data_year$year })
  
  output$region <- renderUI({
    selectInput("region", "Region:",
                choices = levels(countries_all$Region), multiple = TRUE,
                selected = "North America"
    )
  })
  
  
  
  output$country <- renderUI ({
    if(is.null(input$region))
      return()
    
    selectInput("country", "Select countries:", multiple = TRUE,
                choices = countries(),
                selected = c("Canada", "United States")
    )
  })
  
  observe({
    if ("Select All" %in% input$country) {
      # choose all the choices _except_ "Select All"
      selected_choices <- setdiff(countries(), "Select All")
      updateSelectInput(session, "country", selected = selected_choices)
    }
  })
  
  
  filtered_data <- reactive({
    data_year$data %>% 
      filter(
        Region %in% c(input$region),
        Country %in% c(input$country),
        
        Happiness.Score >= c(input$filter_Happiness.Score)[1],
        Happiness.Score <= c(input$filter_Happiness.Score)[2],
        
        Economy >= c(input$filter_Economy)[1],
        Economy <= c(input$filter_Economy)[2],
        
        Family >= c(input$filter_Family)[1],
        Family <= c(input$filter_Family)[2],
        
        Health >= c(input$filter_Health)[1],
        Health <= c(input$filter_Health)[2],
        
        Freedom >= c(input$filter_Freedom)[1],
        Freedom <= c(input$filter_Freedom)[2],
        
        Government.Corruption >= c(input$filter_Government.Corruption)[1],
        Government.Corruption <= c(input$filter_Government.Corruption)[2],
        
        Generosity >= c(input$filter_Generosity)[1],
        Generosity <= c(input$filter_Generosity)[2]
      ) %>% 
      arrange(Country)
  })
  
  
  output$variable_1 <- renderUI({
    selectInput("variable_1", "Y-variable:", 
                choices = c("Happiness.Rank", "Happiness.Score", "Economy", 
                            "Family", "Health", "Freedom", "Government.Corruption", 
                            "Generosity", "Dystopia.Residual"), selected = "Happiness.Score")
  })
  
  
  output$variable_2 <- renderUI({
    selectInput("variable_2", "X-variable:", 
                choices = c("Happiness.Rank", "Happiness.Score", "Economy", 
                            "Family", "Health", "Freedom", "Government.Corruption", 
                            "Generosity", "Dystopia.Residual"), selected = "Economy")
  })
  
  output$scatterplot <- renderPlotly({
    if (is.null(data_year$data)) return()
    
    # s <-  input$results_table_rows_selected
    
    p <- ggplot(filtered_data()) +
      geom_point(aes_string(x = input$variable_2, y = input$variable_1, colour = "Region", label = "Country"), size = 3) +
      ggtitle(paste0(input$variable_1, " vs. ", input$variable_2)) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("label", "x", "y")) 
  })
  
  
  output$rank_table_top <- DT::renderDataTable({
    if (is.null(data_year$data)) {
      return()
    } else {
      data_year$data %>% 
        arrange(Happiness.Rank) %>% 
        top_n(10, desc(Happiness.Rank)) %>% 
        select(Country)
        
    }
  },
  options = list(lengthChange = FALSE,
                 scrollX = "100%")
  )
  
  
  output$rank_table_bottom <- DT::renderDataTable({
    if (is.null(data_year$data)) {
      return()
  } else {
    data_year$data %>% 
      arrange(Happiness.Rank) %>% 
      top_n(10, Happiness.Rank) %>% 
      select(Country)
      
    }
  },
  options = list(lengthChange = FALSE, 
                 scrollX = "100%")
)
  
  
  output$results_table <- DT::renderDataTable({
    if (is.null(data_year$data)) {
      return()
    } else {
    filtered_data()
      }
    },
    options = list(lengthChange = FALSE, 
                   scrollCollapse = TRUE,
                   scrollX = "100%"),
    selection = "single"
  )
  
  

  output$time_series_1 <- renderPlot({

  })
  
  output$time_series_2 <- renderPlot({
    
  })
    
  output$country_table <- renderTable({
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)