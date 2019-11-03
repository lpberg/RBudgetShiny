library(plotly)
library(shiny)
library(DT)

library(shinydashboard)

ui <- dashboardPage(title = "Budget Breakdown",
    dashboardHeader(),
    dashboardSidebar(uiOutput("accounts"),
                     uiOutput("valRange"),
                     uiOutput("categories"),
                     uiOutput("dateRange")),
    dashboardBody(
        
        tabsetPanel(
            type="tabs",
            tabPanel("Scatter Plot",  
                     plotlyOutput("plot1"),
                     DTOutput("transaction_list")
            ),
            tabPanel("Monthly Summary",
                     plotlyOutput("plot2"),
                     DTOutput("monthly_summary_list")
            )
        )
        
    )
)
# 
# 
# ui <- fluidPage(
#   titlePanel("budget"),
#   sidebarLayout(
#     sidebarPanel(width=4,
#                  
#     ),
#     mainPanel(width = 8,
#               tabsetPanel(
#                   type="tabs",
#                   tabPanel("Scatter Plot",  
#                          plotlyOutput("plot1"),
#                          DTOutput("transaction_list")
#                   ),
#                   tabPanel("Monthly Summary",
#                           plotlyOutput("plot2"),
#                           DTOutput("monthly_summary_list")
#                   )
#               )
#     )
#   )
# )