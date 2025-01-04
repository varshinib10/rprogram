library(shiny)
library(forecast)

ui <- fluidPage(
  titlePanel("Real-Time ARIMA Forecasting"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload Time Series Data (CSV)", accept = ".csv"),
      numericInput("forecast_horizon", "Forecast Horizon:", value = 10, min = 1, max = 100)
    ),
    mainPanel(
      plotOutput("forecastPlot"),
      verbatimTextOutput("forecastOutput")
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression for reading the uploaded CSV file
  dataInput <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  output$forecastPlot <- renderPlot({
    req(dataInput())
    ts_data <- ts(dataInput()[, 1], frequency = 12)  # Assuming the time series is in the first column
    
    # Fit ARIMA model and make forecast
    fit <- auto.arima(ts_data)
    forecasted_values <- forecast(fit, h = input$forecast_horizon)
    
    # Plot the forecasted data
    plot(forecasted_values, main = "ARIMA Forecast", xlab = "Time", ylab = "Value")
  })
  
  output$forecastOutput <- renderPrint({
    req(dataInput())
    ts_data <- ts(dataInput()[, 1], frequency = 12)  # Assuming the time series is in the first column
    
    # Fit ARIMA model and show summary
    fit <- auto.arima(ts_data)
    summary(fit)
  })
}

shinyApp(ui = ui, server = server)

