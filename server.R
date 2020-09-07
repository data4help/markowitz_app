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
source_python("efficient_frontier_functions.py")

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
  if (type=="more_is_good") {
    if (number<100) {
      color = "red"
    } else {
      color = "green"
    }
  } else if (type=="more_is_bad") {
    if (number<=100) {
      color = "green"
    } else {
      color = "red"
    }
  }
  return(color)
}

shinyServer(function(input, output, session) {
  
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
  
  
  # extracting return, risk and sharpe ratio for selected portfolio ----
  return_df = eventReactive(event_data("plotly_click", source="main_plot"), {
    returns = data()
    return_list = list("0"=returns$portfolio,
                       "1"=returns$stocks,
                       "2"=returns$efficient_frontier,
                       "3"=returns$cml)
    point = event_data("plotly_click", source="main_plot")
    return_number = point$curveNumber
    return(return_list[[toString(return_number)]])
  })  
  
  port_return_data = reactive({
    returns = return_df()
    point = pointer()
    port_return = returns %>% 
      select(return) %>% 
      slice(point$pointNumber+1)
    return(port_return)
  })
  
  port_risk_data = reactive({
    returns = return_df()
    point = pointer()
    port_risk = returns %>% 
      select(risk) %>% 
      slice(point$pointNumber+1)
    return(port_risk)
  })
  
  port_sr_data = reactive({
    returns = return_df()
    point = pointer()
    port_sharpe_ratio = returns %>% 
      select(sharpe_ratio) %>% 
      slice(point$pointNumber+1)
    return(port_sharpe_ratio)
  })
  
  # getting the right row to extract the weights of the portfolios
  pointer = eventReactive(event_data("plotly_click", source="main_plot"), {
    point = event_data("plotly_click", source="main_plot")
    return(point)
  })
  
  #* reaction for selecting portfolio ----
  observeEvent(event_data("plotly_click", source="main_plot"), {
    
    # extract the weights and stock time series data
    point = pointer()
    returns = data()
    weight_list = list("0"=returns$portfolio_weight,
                       "1"=returns$stock_weight,
                       "2"=returns$ef_weights)
    if (point$curveNumber %in% c("0", "1", "2")) {
      weights = weight_list[[toString(point$curveNumber)]] %>% 
        slice(point$pointNumber+1)
    } else {
      row_sharpe_ratio = which.max(returns$efficient_frontier$sharpe_ratio)
      weights = returns$ef_weights %>% slice(row_sharpe_ratio)
    }
    stock_time_series = returns$stock_time_series
    
    # building dataframe for time series plotting
    portfolio_time_series = as.matrix(stock_time_series) %*% t(weights)
    df_portfolio_time_series = data.frame(
      Return = portfolio_time_series,
      Date = as.Date(rownames(portfolio_time_series))
    ) 
    
    # building dataframe for portfolio weights plotting
    weight_decimal = round(as.vector(t(weights)), 2)
    adj_stock_names = paste(stock_names(), " (", weight_decimal, ")", sep="")
    df_weights = data.frame(
      stocks=as.vector(adj_stock_names),
      weights=as.vector(t(weights))
    )
    
    # plotting weights
    weight_plot = ggplot(df_weights, aes(x="", y=weights, fill=stocks)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void(base_size=15) +
      labs(fill="Stock Weights")
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
      return_type = click$curveNumber
      
      returns = data()
      port_sf = port_sr_data()
      port_return = port_return_data()
      port_risk = port_risk_data()
      
      pct_sr_data = round(((port_sf/returns$tangency$sharpe_ratio)*100), 2)
      pct_return = round(((port_return/returns$tangency$return)*100), 2)
      pct_risk = round(((port_risk/returns$tangency$risk)*100), 2)
      
      sr_box = valueBox(paste("Portfolio Sharpe Ratio: ", 
                              round(port_sf, 2)), 
                        paste(pct_sr_data,
                              "% of Tangency Portfolio Sharpe Ratio"), 
                        icon = icon("fire"),
                        color = color_picker("more_is_good", pct_sr_data))
      return_box = valueBox(paste("Portfolio Return: ", round(port_return, 2)), 
                            paste(pct_return, "% of Tangency Portfolio Return"), 
                            icon = icon("fire"),
                            color = color_picker("more_is_good", pct_return))
      risk_box = valueBox(paste("Portfolio Risk: ", round(port_risk, 2)), 
                          paste(pct_risk, "% of Tangency Portfolio Risk"), 
                          icon = icon("fire"),
                          color = color_picker("more_is_bad", pct_risk))
    } else {
      sr_box = valueBox("Please Select Portfolio", "",
                        icon = icon("fire"), color = "yellow")
      return_box = valueBox("Please Select Portfolio", "",
                            icon = icon("fire"), color = "yellow")
      risk_box = valueBox("Please Select Portfolio", "",
                          icon = icon("fire"), color = "yellow")
    }
    output$portfolio_sharpe_ratio = renderValueBox({sr_box})
    output$portfolio_return = renderValueBox({return_box})
    output$portfolio_risk = renderValueBox({risk_box})
  })
  
  # extracting the real names from the ticker description
  stock_names = reactive({
    names = clean_tickers %>% 
      filter(Ticker %in% input$stocks_chosen) %>% 
      pull(Name)
    return(names)
  })
  
  #* plotting the basic stocks, portfolios and efficient frontier ----
  main_plot = eventReactive(input$submit, {
    # get return information
    returns = data()
    
    # filling stock individual table
    returns$stocks$name = stock_names()
    output$stock_rankings = DT::renderDataTable(
      data.frame(
        "Stock" = returns$stocks$name,
        "Return" = round(returns$stocks$return, 2),
        "Risk" = round(returns$stocks$risk, 2)
      )
    )
    
    # plotting correlation heatmap
    correlation_data = returns$stock_correlation
    output$heatmap = renderPlotly({
      plot_ly(x=stock_names(),
              y=stock_names(),
              z=as.matrix(correlation_data), 
              type="heatmap",
              colors="Greys")
    })
    
    # plotting all kind of portfolios
    plot = ggplot()  +
      geom_point(data=returns$portfolio, 
                 aes(risk, return, color=sharpe_ratio, shape="Portfolio")) +
      geom_point(data=returns$stocks, 
                 aes(risk, return, shape="Stocks"), color="blue", size=7) +
      geom_point(data=returns$efficient_frontier, 
                 aes(risk, return, 
                     shape="Efficient Frontier"),
                 color="coral") +
      labs(y="Risk", x="Return", color="Sharpe Ratio", shape="") +
      scale_shape_manual(values=c("Portfolios"=1,
                                  "Stocks"=3,
                                  "Efficient Frontier"=3,
                                  "Capital Market Line" = 1)) +
      scale_colour_gradient(low="pink", high="darkorchid4") +
      theme(legend.margin = 2)
    
    if (input$risk_free_button) {
      plot = plot + geom_point(data=returns$cml, 
                               aes(risk, return, shape="Capital Market Line")) 
    }
    
    return(ggplotly(plot, source="main_plot"))
  })
  
  #* Sending the plot to UI ----
  output$markowitz = renderPlotly({main_plot()})
  
})
