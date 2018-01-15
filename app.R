library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

happiness2015 <- read_csv("data/happiness_2015.csv")
names(happiness2015) <- c("Country", "Region", "Happiness.Rank", 
                          "Happiness.Score", "Standard.Error", "Economy", 
                          "Family", "Health", "Freedom", "Government.Corruption", 
                          "Generosity", "Dystopia.Residual")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("World Happiness"),
   
   sidebarLayout(
     
     # Sidebar with inputs  
     sidebarPanel(
        uiOutput("variable_1"),
        uiOutput("variable_2")
     ),
        
      mainPanel(
         plotOutput("scatterplot"),
         br(), br(), br(),
         tableOutput("rank_table")
      )
    )
  )

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  happiness_2015 <- reactive ({
    happiness2015 %>% select(-Standard.Error)
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
    ggplot(happiness_2015()) +
      geom_point(aes_string(x = input$variable_2, y = input$variable_1, colour = "Region")) +
      ggtitle(paste0(input$variable_1, " vs. ", input$variable_2)) +
      theme_bw() +
      theme(legend.position = "right")
  })
  
  output$rank_table <- renderTable({
    happiness_2015() %>% 
      arrange(Happiness.Rank) %>% 
      select(Happiness.Rank, Country) %>% 
      top_n(10, desc(Happiness.Rank))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

