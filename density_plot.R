library(shiny)

ui <- fluidPage(
  titlePanel("Density Plot"),
  sidebarLayout(
    sidebarPanel(
      numericInput("dataCount", "Number of Data Points:", value = 500, min = 100, max = 1000)
    ),
    mainPanel(
      plotOutput("densityPlot")
    )
  )
)

server <- function(input, output) {
  output$densityPlot <- renderPlot({
    plot(density(rnorm(input$dataCount)), col = "red", lwd = 2, main = "Density Plot")
  })
}

shinyApp(ui = ui, server = server)
