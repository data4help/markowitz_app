library(readxl)
library(reticulate)
library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(shinycssloaders)

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
ui = fluidPage(
  
  #* Navigation Bar ----
  navbarPage("Portfolio Selection",
    
    # Portfolio plotting page ----         
    tabPanel("Main", 
        
      #* Layout style ----
      sidebarLayout(
        
        #** Left side - Input Data ---- 
        sidebarPanel(
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
          
          #** Submit button for plotting ----
          actionButton(inputId = "submit",
                       label = "Submit"),
        ),
        #* Right side - Displaying plots ----
        mainPanel(
          plotOutput("scatterplot") %>% withSpinner(color="#0dc5c1")
        )
      )
    ),
    tabPanel("Correlation")
  )
)
  
# Server ---- 
server = function(input, output, session) {
  
  #* Updating the selectize input of stocks ----
  updateSelectizeInput(session, 
                       "stocks_chosen", 
                       choices = stock_names_ticker,
                       server = TRUE)
  
  #* Fetching returns for the chosen stocks ---- 
  data = reactive({
    port_calc = MarkowitzReturns(c(input$stocks_chosen),
                                 input$daterange[1],
                                 input$daterange[2])
    python_return_information = py_get_attr(port_calc, "results")
    returns = transform_python_data_to_r(python_return_information)
  })
  
  #* plotting the basic stocks, portfolios and efficient frontier ----
  main_plot = eventReactive(input$submit, {
    returns = data()
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
    plot
  })
  
  #* Sending the plot to UI ----
  output$scatterplot = renderPlot({main_plot()})

}

shinyApp(ui = ui, server = server)
