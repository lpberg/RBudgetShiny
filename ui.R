library(plotly)
library(shiny)

ui <- fluidPage(
  titlePanel("budget"),
  sidebarLayout(
    sidebarPanel(width=4,
                 uiOutput("valRange"),
                 uiOutput("categories"),
                 uiOutput("dateRange")
    ),
    mainPanel(width = 8,
              tabsetPanel(type="tabs",
                          tabPanel("Scatter Plot",  
                                   plotlyOutput("plot1"),
                                   uiOutput("table")
                          ),
                          tabPanel("Monthly Summary",
                                   plotlyOutput("plot2"),
                                   uiOutput("table2")
                          )
              )
    )
  )
)