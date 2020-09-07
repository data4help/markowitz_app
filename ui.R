library(readxl)
library(reticulate)
library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(shinycssloaders)
library(shinydashboard)
library(DT)

dashboardPage(
  dashboardHeader(title="Portfolio Creator"),
  
  #* Sidebar----
  dashboardSidebar(
    #** Selecting Stocks ----
    selectizeInput(inputId = "stocks_chosen",
                   label = "Choose Stocks",
                   choices = NULL,
                   multiple = TRUE,
                   options=list(placeholder="Type to search for stock")),
    
    br(),
    
    #** Date range ----
    dateRangeInput(inputId = "daterange",
                   label = "Selecting Date Range",
                   start = "2010-01-01",
                   end = as.character(Sys.Date())),
    
    br(),
    
    #** Risk-free rate inclusion ----
    checkboxInput(inputId = "risk_free_button",
                  label = "Showing the Capital Market Line (CML)",
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
      valueBoxOutput("portfolio_sharpe_ratio"),
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