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

# Color picking color
color_picker = function(type, number) {
  if (((number<= 100) & (type=="return")) | ((number>=100) & (type=="risk"))){
    color = "red"
  } else{
    color = "green"
  }
  return(color)
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
      valueBoxOutput("portfolio_return"),
      valueBoxOutput("portfolio_risk")
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

      box(title = "Portfolio weights", solidHeader = TRUE,
          width = 4, collapsible = TRUE,
          plotOutput("portfolio_weights")),
      
            
      box(title = "Time Series Performance", solidHeader = TRUE,
          width = 4, collapsible = TRUE,
          plotlyOutput("time_series"))
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
  

  # extracting return and risk for selected portfolio
  port_return_data = reactive({
    returns = data()
    point = pointer()
    portfolio_data = returns$portfolio
    port_return = portfolio_data %>% 
      select(return) %>% 
      slice(point$pointNumber+1)

    return(port_return)
  })

  port_risk_data = reactive({
    returns = data()
    point = pointer()
    portfolio_data = returns$portfolio
    port_risk = portfolio_data %>% 
      select(risk) %>% 
      slice(point$pointNumber+1)
    
    return(port_risk)
  })
  
  pointer = eventReactive(event_data("plotly_click", source="main_plot"), {
    point = event_data("plotly_click", source="main_plot")
    return(point)
  })
  
  #* reaction for selecting portfolio ----
  observeEvent(event_data("plotly_click", source="main_plot"), {
    
    # extract the weights and stock time series data
    point = pointer()
    returns = data()
    weights = returns$portfolio_weights %>% slice(point$pointNumber+1)
    stock_time_series = returns$stock_time_series
    
    # building dataframe for time series plotting
    portfolio_time_series = as.matrix(stock_time_series) %*% t(weights)
    df_portfolio_time_series = data.frame(
      Return = portfolio_time_series,
      Date = as.Date(rownames(portfolio_time_series))
    ) 
    
    # building dataframe for portfolio weights plotting
    df_weights = data.frame(
      stocks=input$stocks_chosen,
      weights=as.vector(t(weights))
    )
    
    # plotting weights
    weight_plot = ggplot(df_weights, aes(x="", y=weights, fill=stocks)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void()
    output$portfolio_weights = renderPlot({weight_plot})
    
    # plotting time series
    ts_plot = ggplot(data=df_portfolio_time_series, 
                         aes(x=Date, y=Return, group=1)) + 
      geom_line()
    output$time_series = renderPlotly({ts_plot})
    
  })
  
  # plotting top boxes
  observe({
    click = event_data("plotly_click", source="main_plot")
    if (!is.null(click)) {
      returns = data()
      port_return = port_return_data()
      port_risk = port_risk_data()

      pct_return = round(((port_return/returns$tangency$return)*100), 2)
      pct_risk = round(((port_risk/returns$tangency$risk)*100), 2)

      color_returns = color_picker("return", pct_return)
      color_risk = color_picker("risk", pct_risk)
      print(color_returns)
      return_box = valueBox(paste("Portfolio Return: ", round(port_return, 2)), 
                            paste(pct_return, "% of Tangency Portfolio Return"), 
                            icon = icon("fire"),
                            color = color_picker("return", pct_return))
      risk_box = valueBox(paste("Portfolio Risk: ", round(pct_risk, 2)), 
                          paste(pct_risk, "% of Tangency Portfolio Risk"), 
                          icon = icon("fire"),
                          color = color_picker("risk", pct_risk))
    } else {
      risk_box = valueBox("Please Select Portfolio", 
                          "Return: XX Risk: XX", 
                          icon = icon("fire"), color = "yellow")
      return_box = valueBox("Please Select Portfolio", 
                            "Return: XX Risk: XX", 
                            icon = icon("fire"), color = "yellow")
    }
    output$portfolio_risk = renderValueBox({risk_box})
    output$portfolio_return = renderValueBox({return_box})
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
    
    return(ggplotly(plot, source="main_plot"))
  })

  #* Sending the plot to UI ----
  output$markowitz = renderPlotly({main_plot()})

}

shinyApp(ui = ui, server = server)
