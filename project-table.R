#Jeanie's branch
library(tidyverse)
library(tidytext)
library(textdata)
library(shiny)
library(shinyWidgets)
library(plotly)


project=projectcsv%>%
  select(X1, Title, Upvotes, Content)%>%
  unnest_tokens(word,Title)%>%
  anti_join(stop_words)%>% 
  left_join(get_sentiments("bing"))%>% 
  group_by(X1) %>% 
  summarize(
    positive = sum(sentiment == "positive", na.rm = TRUE), 
    negative = sum(sentiment == "negative", na.rm = TRUE), 
    netural = n() - positive - negative) %>%
  mutate(
    X1,
    sentiment = case_when(
      positive > negative ~ "positive",
      positive < negative ~ "negative",
      TRUE ~ "netural"
    ))%>% 
  left_join(select(projectcsv, X1, Title, Upvotes, Content)) %>% 
  mutate(title = str_trunc(Title, 80)) %>% 
  select(sentiment, Title, Upvotes, Content)


server <- function(input, output) {
    
  
    output$table <- DT::renderDataTable(DT::datatable({
      data <- project
      if (input$sentiment != "All") {
        data <- data[data$sentiment == input$sentiment,]
      }
      if (input$Title != "All") {
        data <- data[data$Title == input$Title,]
      }
      if (input$Content != "All") {
        data <- data[data$Content == input$Content,]
      }
      if (input$Upvotes != "All") {
        data <- data[data$Upvotes == input$Upvotes,]
      }
      data
    }))
    }
    
   
  

  ui <- fluidPage(
    setBackgroundColor(
      color = "gray",
      gradient = c("linear", "radial"),
      direction = c("bottom", "top", "right", "left"),
      shinydashboard = FALSE
    ),
    titlePanel("DataTable"),
    
    fluidRow(
      column(4,
             selectInput("sentiment",
                         "Title Sentiment:",
                         c("All",
                           unique(as.character(project$sentiment))))
      ),
      column(4,
             selectInput("Title",
                         "Title:",
                         c("All",
                           unique(as.character(project$Title))))
      ),
      column(4,
             selectInput("Content",
                         "Content:",
                         c("All",
                           unique(as.character(project$Content))))
      ),
      column(4,
             selectInput("Upvotes",
                         "Upvotes:",
                         c("All",
                           unique(as.character(project$Upvotes))))
      )
    ),
    
    DT::dataTableOutput("table"),
    )
  
shinyApp(ui, server)

