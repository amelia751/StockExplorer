# Load necessary libraries
library(shiny)
library(plotly)
library(quantmod)
library(dplyr)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Stock Explorer"),

  sidebarLayout(
    sidebarPanel(
      selectInput("ticker", "Select Stocks", choices = c("AAPL", "GOOGL", "MSFT"), selected = "AAPL"),
      dateRangeInput("dates", "Select dates", start = Sys.Date() - 180, end = Sys.Date())
    ),

    mainPanel(
      fluidRow(
        column(4, uiOutput("price")),
        column(4, uiOutput("change")),
        column(4, uiOutput("change_percent"))
      ),
      fluidRow(
        column(9, plotlyOutput("price_history")),
        column(3, tableOutput("latest_data"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Reactive expression to get the stock data based on ticker and date range
  get_data <- reactive({
    data <- tryCatch({
      getSymbols(input$ticker, src = "yahoo", from = input$dates[1], to = input$dates[2], auto.assign = FALSE)
    }, error = function(e) {
      NULL  # Return NULL if there's an error fetching data
    })

    if (is.null(data)) {
      print("Error: No data retrieved for selected ticker or date range.")
    } else {
      print("Data retrieved successfully:")
      print(head(data))  # Print the first few rows for verification
      print(is.numeric(data))
    }

    data  # Return the data (NULL if error occurred)
  })

  # Reactive to calculate change
  get_change <- reactive({
    data <- get_data()
    if (is.null(data)) return(NA)  # Return NA if data is NULL

    close <- Cl(data)  # Get closing prices
    if (length(close) >= 2) {
      change <- close[[length(close)]] - close[[1]]  # Difference from start to end of date range
      print(paste("Calculated change from start to end of date range:", change))  # Debugging print
      change
    } else {
      print("Insufficient data to calculate change.")
      NA  # Not enough data to calculate change
    }
  })


  # Reactive to calculate percentage change
  get_change_percent <- reactive({
    data <- get_data()
    if (is.null(data)) return(NA)  # Return NA if data is NULL

    close <- Cl(data)  # Get closing prices
    if (length(close) >= 2) {
      change <- close[[length(close)]] - close[[1]]
      percent_change <- (change / close[[1]]) * 100
      print(paste("Calculated percent change:", percent_change))  # Debugging print
      percent_change
    } else {
      print("Insufficient data to calculate percent change.")
      NA  # Not enough data to calculate percent change
    }
  })

  # Render current price
  output$price <- renderText({
    close <- Cl(get_data())
    if (!is.null(close) && length(close) > 0) {
      paste0("Current Price: $", round(close[length(close)], 2))
    } else {
      "No data available"
    }
  })

  # Render change in price
  output$change <- renderText({
    change <- get_change()
    if (!is.na(change)) {
      paste0("Change: $", round(change, 2))
    } else {
      "Change: N/A"
    }
  })

  # Render change percent
  output$change_percent <- renderText({
    percent_change <- get_change_percent()
    if (!is.na(percent_change)) {
      paste0("Percent Change: ", round(percent_change, 2), "%")
    } else {
      "Percent Change: N/A"
    }
  })

  # Plotly price history plot
  output$price_history <- renderPlotly({
    data_xts <- get_data()
    if (is.null(data_xts)) return(NULL)

    data_df <- data.frame(Date = index(data_xts), coredata(data_xts))
    colnames(data_df) <- c("Date", "Open", "High", "Low", "Close", "Volume")

    p <- plot_ly(data = data_df, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines') %>%
      layout(title = paste("Price History for", input$ticker),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Closing Price"))
    p
  })

  # Latest data table
  output$latest_data <- renderTable({
    data <- tail(get_data(), 1)
    if (is.null(data)) return(data.frame(Category = "No data", Value = "N/A"))

    data.frame(Category = c("Open", "High", "Low", "Close", "Volume"),
               Value = round(c(Op(data), Hi(data), Lo(data), Cl(data), Vo(data)), 2))
  })
}


# Run the application
shinyApp(ui = ui, server = server)
