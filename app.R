library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)

happiness2015 <- read.csv("results/happiness_2015_clean.csv", stringsAsFactors = TRUE)
happiness2016 <- read.csv("results/happiness_2016_clean.csv", stringsAsFactors = TRUE)
happiness2017 <- read.csv("results/happiness_2017_clean.csv", stringsAsFactors = TRUE)
countries_all <- read.csv("results/countries_aggregated.csv", stringsAsFactors = TRUE)



# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = shinytheme("journal"),
  title = "World Happiness",
  tabPanel("Plot",
    sidebarPanel(width = 3,
           helpText("Select year"),
           actionButton("button2015", "2015"),
           actionButton("button2016", "2016"),
           actionButton("button2017", "2017"),
           br(), br(), 
           helpText("Choose variables you want to see on the plot"),
           uiOutput("variable_1"),
           uiOutput("variable_2"),
           br(),
           helpText("Click while pressing `command` in order to choose multiple countries"),
           uiOutput("country"),
           br(), hr(),
           span("Data source:", 
                tags$a("Kaggle",
                       href = "https://www.kaggle.com/unsdsn/world-happiness")),
           br(), br(),
           em(
             span("Created by", a(href = "https://nazliozum.github.io/", "Nazlı Özüm Kafaee")),
             br(),
             span("Code", a(href = "https://github.com/Nazliozum/world_happiness", "on GitHub"))
           )
           
    ),
    
    mainPanel(
           tabsetPanel(
             tabPanel("Plot",
                      br(),
                      h2(textOutput("year_name_plot")),
                      plotlyOutput("scatterplot", 
                                 height = 800)
             ),
             
             tabPanel("Table", 
                      br(),
                      DT::dataTableOutput("results_table")
                      ),
             
             tabPanel(textOutput("tab_name_rankings"),
               h1(textOutput("year_name")),
               h2("Top 10"),
               wellPanel( 
                 dataTableOutput("rank_table_top")),
               br(), 
               h2("Bottom 10"),
               wellPanel(
                 dataTableOutput("rank_table_bottom"))
             )
           )
        )

      ),
      tabPanel("About",
               br(),
               span("This app will help you explore the data behind the ", 
                    tags$a("World Happiness Report.", href = "http://worldhappiness.report")),
               br(), hr(), br())
)

##############################################
### SERVER STARTS HERE #######################
##############################################

server <- function(input, output, session) {
  
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
    
                            
  data_year <- reactiveValues(data = happiness2015, year = "2015")
  
  
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
  
  
  filtered_data <- reactive({
    data_year$data %>% 
      filter(Country %in% c(input$country)) %>% 
      arrange(Country)
  })
  
  
  output$year_name_plot <- renderText({ 
    paste("Data for ", data_year$year) 
  })
  
  
  countries <- reactive({ 
    df_small <- 
      countries_all %>% 
      select(Country) %>% 
      droplevels()
    
    c("Select All", levels(df_small$Country))
    
  })
  
  
  output$country <- renderUI ({
  
    selectInput("country", "Countries:", 
                multiple = TRUE,
                choices = countries(), 
                selected = c("Canada", "United States", "Mexico"),
                selectize = FALSE,
                size = 10)
  })

  
  observe({
    if ("Select All" %in% input$country) {
      # choose all the choices _except_ "Select All"
      selected_choices <- setdiff(countries(), "Select All")
      updateSelectInput(session, "country", selected = selected_choices)
    }
  })

  
  output$scatterplot <- renderPlotly({
    if (is.null(data_year$data)) return()
    
    p <- ggplot(filtered_data()) +
      geom_point(aes_string(x = input$variable_2, y = input$variable_1, 
                            colour = "Region", label = "Country"), size = 3) +
      ggtitle(paste0(input$variable_1, " vs. ", input$variable_2)) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("label", "x", "y"), height = 500) 
  })
  
  
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
  
  
  output$tab_name_rankings <- renderText({ 
    paste("Happiness Ranking: ", data_year$year) 
  })
  
  
  output$year_name <- renderText({ 
    paste("Happiness Ranking in ", data_year$year) 
  })
  
  
  output$rank_table_top <- DT::renderDataTable({
    if (is.null(data_year$data)) {
      return()
    } else {
      data_year$data %>% 
        mutate(Rank = Happiness.Rank) %>% 
        arrange(Rank) %>% 
        top_n(10, desc(Rank))
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
      mutate(Rank = Happiness.Rank) %>% 
      arrange(Rank) %>% 
      top_n(10, Rank)
    }
  },
  
  options = list(lengthChange = FALSE, 
                 scrollX = "100%")
)
  
}

# Run the application 
shinyApp(ui = ui, server = server)