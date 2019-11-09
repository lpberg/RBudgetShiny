library(plotly)
library(shiny)
library(DT)
library(shinydashboard)

ui <- dashboardPage(
    title = "Budget Breakdown",
    dashboardHeader(title = "Budget Breakdown"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            box(width = 12,
                column(width = 4,
                    uiOutput("accounts"),
                    uiOutput("valRange")
                ),
                column(width = 4,
                    uiOutput("categories"),
                    uiOutput("dateRange")
                ),
                column(width = 4,
                    actionButton("groc_only", "Groceries"),
                    actionButton("util_only", "Utilities"),
                    actionButton("stream_only", "Streaming Services"),
                    
                )
            )
        ),
        
        tabsetPanel(
            type="tabs",
            tabPanel(
                "Debit: Monthly Summary",
                column(width=9,
                       plotlyOutput("monthly_summary")
                 ),
                column(width=3,
                       DTOutput("monthly_summary_list")
                )
            ),
            tabPanel(
                "Debit: Transactions",
                plotlyOutput("transaction_scatter"),
                DTOutput("transaction_list")
            ),
            tabPanel(
                "Credit: Transactions",
                plotlyOutput("credit_transaction_scatter"),
                DTOutput("credit_transaction_list")
            )
        )
    )
)