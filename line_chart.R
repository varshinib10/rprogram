library(shiny)

ui <- fluidPage(
  titlePanel("Line Chart"),
  sidebarLayout(
    sidebarPanel(
      numericInput("points", "Number of Points:", value = 30, min = 10, max = 100)
    ),
    mainPanel(
      plotOutput("lineChart")
    )
  )
)

server <- function(input, output) {
  output$lineChart <- renderPlot({
    x <- 1:input$points
    y <- cumsum(rnorm(input$points))
    plot(x, y, type = "o", col = "darkgreen", main = "Line Chart", xlab = "Index", ylab = "Values")
  })
}

shinyApp(ui = ui, server = server)
