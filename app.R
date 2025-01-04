# Load necessary libraries
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(dplyr)
library(ggplot2)

# Sample data (you can replace this with actual product/movie/book data)
data <- data.frame(
  user_id = rep(1:5, each = 3),
  item_id = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
  rating = c(5, 4, 3, 4, 5, 2, 5, 4, 3, 2, 3, 5, 4, 4, 4)
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Recommendation System"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Recommend", tabName = "recommend", icon = icon("film")),
      menuItem("Ratings", tabName = "ratings", icon = icon("star"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "recommend",
              fluidRow(
                box(title = "Input User Preferences", width = 12,
                    sliderInput("rating", "Rate Item 1:", min = 1, max = 5, value = 3),
                    actionButton("get_recommendations", "Get Recommendations")
                )
              ),
              fluidRow(
                box(title = "Recommended Items", width = 12,
                    tableOutput("recommendations_table")
                )
              )
      ),
      tabItem(tabName = "ratings",
              fluidRow(
                box(title = "View Ratings", width = 12,
                    plotOutput("rating_plot")
                )
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Data Preprocessing: Create the rating matrix for collaborative filtering
  ratings_matrix <- as(data, "realRatingMatrix")
  
  # Recommendation model based on Collaborative Filtering
  recommender_model <- Recommender(ratings_matrix, method = "UBCF")
  
  # When the user clicks the "Get Recommendations" button
  observeEvent(input$get_recommendations, {
    
    # Use the rating input by the user (for example, rate item 1)
    new_user_ratings <- matrix(c(input$rating, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)
    colnames(new_user_ratings) <- colnames(ratings_matrix)
    
    # Make recommendation for the new user based on their input
    new_user_ratings <- as(new_user_ratings, "realRatingMatrix")
    recommendations <- predict(recommender_model, new_user_ratings, n = 3)
    
    # Get the recommended items
    recommended_items <- as(recommendations, "list")
    
    # Prepare the table output with recommendations
    recommended_items_table <- data.frame(
      Item = c("Item 1", "Item 2", "Item 3"),
      Rating = round(recommended_items[[1]], 2)
    )
    
    output$recommendations_table <- renderTable({
      recommended_items_table
    })
  })
  
  # Plot the ratings distribution
  output$rating_plot <- renderPlot({
    ggplot(data, aes(x = rating)) +
      geom_bar() +
      ggtitle("Rating Distribution") +
      xlab("Ratings") + ylab("Frequency")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
