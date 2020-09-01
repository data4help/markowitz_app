library(readxl)
library(reticulate)
library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(shinycssloaders)
library(shinydashboard)
library(DT)

# Importing the python class ----
use_condaenv("markowitz", required=TRUE)
path = "/Users/paulmora/Documents/projects/markowitz_app/"
source_python(paste(path, "efficient_frontier_functions.py", sep=""))

# Importing ticker list ----
ticker = read_excel(paste(path, "ticker.xlsx", sep=""), skip=3)
clean_tickers = ticker %>% filter(!is.na(ticker$Name))
stock_names_ticker = setNames(clean_tickers$Ticker, clean_tickers$Name)

# Python to R function ---- 
transform_python_data_to_r = function (python_data) {
  data_list = py_to_r(python_data)  # get data out of dictionary
  data = lapply(data_list, py_to_r)  # get data into
  return = data
}

# User Interface ----
ui = dashboardPage(
  dashboardHeader(title="Portfolio Creator"),
  
  #* Sidebar----
  dashboardSidebar(
    #** Selecting Stocks ----
    selectizeInput(inputId = "stocks_chosen",
                   label = "Choose Stocks",
                   choices = NULL,
                   multiple = TRUE),
  
    br(),
    
    #** Date range ----
    dateRangeInput(inputId = "daterange",
                   label = "Selecting Date Range",
                   start = "2010-01-01",
                   end = as.character(Sys.Date())),
    
    br(),
    
    #** Risk-free rate inclusion ----
    checkboxInput(inputId = "risk_free_button",
                  label = "Including the risk-free rate?",
                  value = FALSE),
    
    br(),
    
    #** Submit button for plotting ----
    actionButton(inputId = "submit",
                 label = "Submit")
  ),
  
  #* Main body ----
  dashboardBody(
    
    #** specific returns and risk ----
    fluidRow(
      valueBoxOutput("tangency"),
      valueBoxOutput("mvp")
    ),
    
    #** stock rankings and markowitz ---- 
    fluidRow(
      box(title = "Stock Rankings",
          solidHeader = TRUE,
          width = 4,
          collapsible = TRUE,
          div(DT::DTOutput("stock_rankings"), style = "font-size: 110%;")),
      box(title = "Markowitz Diagram", solidHeader = TRUE,
          width = 8, collapsible = TRUE,
          plotlyOutput("markowitz") %>% withSpinner(color="#0dc5c1"))
    ),
    
    fluidRow(
      box(title = "Correlation Matrix",
          solidHeader = TRUE,
          width = 4,
          collapsible = TRUE,
          div(plotlyOutput("heatmap"))),
      box(title = "Time Series Performance", solidHeader = TRUE,
          width = 8, collapsible = TRUE,
          plotOutput("time_series"))
    ),
  )
)


# Server ---- 
server = function(input, output, session) {
  
  #* Updating the selectize input of stocks ----
  updateSelectizeInput(session, "stocks_chosen", 
                       choices = stock_names_ticker,
                       server = TRUE)
  
  #* Fetching returns for the chosen stocks ---- 
  data = reactive({
    port_calc = MarkowitzReturns(c(input$stocks_chosen),
                                 input$daterange[1],
                                 input$daterange[2])
    python_return_information = py_get_attr(port_calc, "results")
    return_data = transform_python_data_to_r(python_return_information)
    return(return_data)
  })
  
  observeEvent(event_data("plotly_click"), {
    point = event_data("plotly_click")
    returns = data()
    weights = returns$portfolio_weights %>% slice(point$pointNumber+1)
    stock_returns = returns$stock_time_series

    portfolio_time_series = as.matrix(stock_time_series) %*% t(weights)
    df_portfolio_time_series = data.frame(
      Return = portfolio_time_series,
      Date = as.Date(rownames(portfolio_time_series))
    ) 
    
    return_plot = ggplot(data=df_portfolio_time_series, 
                         aes(x=Date, y=Return, group=1)) + 
      geom_line()
    
    output$time_series = renderPlot({return_plot})
    
  })

  #* plotting the basic stocks, portfolios and efficient frontier ----
  main_plot = eventReactive(input$submit, {
    # get return information
    returns = data()
    
    # extract stock name from ticker
    stock_names = clean_tickers %>% 
      filter(Ticker %in% input$stocks_chosen) %>% 
      pull(Name)

    # filling stock individual table
    returns$stocks$name = stock_names 
    output$stock_rankings = DT::renderDataTable(data.frame(returns$stocks))
    
    # plotting correlation heatmap
    correlation_data = returns$stock_correlation
    output$heatmap = renderPlotly({
      plot_ly(x=stock_names,
              y=stock_names,
              z=as.matrix(correlation_data), 
              type="heatmap")
    })
    
    # plotting all kind of portfolios
    plot = ggplot()  +
      geom_point(data=returns$stocks, 
                 aes(risk, return)) +
      geom_point(data=returns$portfolio, 
                 aes(risk, return, color=sharpe_ratio)) +
      geom_point(data=returns$efficient_frontier, 
                 aes(risk, return))
    if (input$risk_free_button) {
      plot = plot + geom_line(data=returns$cml, aes(cml_xaxis, cml)) 
    }
    
    return(ggplotly(plot))
  })

  #* Sending the plot to UI ----
  output$markowitz = renderPlotly({main_plot()})
  
  #* filling boxes ----
  output$tangency <- renderValueBox({
    valueBox("Tangency", "Return: XX Risk: XX", 
             icon = icon("fire"), color = "yellow")
  })
  output$mvp <- renderValueBox({
    valueBox("as", "tangecy", 
             icon = icon("fire"), color = "yellow")
  })

}

shinyApp(ui = ui, server = server)
