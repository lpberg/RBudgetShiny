library(plotly)
library(shiny)
library(DT)
library(shinydashboard)
library(timevis)

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
                    actionButton("amaz_only", "Amazon"),
                    actionButton("rest_only", "Restarunts"),
                    actionButton("cafe_only", "Coffee"),
                )
            )
        ),
        fluidRow(
            box(width = 12,
                tabsetPanel(
                    type="tabs",
                    tabPanel(
                        "Debit: Monthly Summary",
                        plotlyOutput("monthly_summary"),
                        DTOutput("monthly_summary_list")
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
                    ),
                    tabPanel(
                        "Timeline",
                        tags$h4("Annual Timeline"),
                        timevisOutput("timeline_year"),
                        tags$h4("Monthly Timeline"),
                        timevisOutput("timeline_month")
                    )
                )
                
            )
        )
    )
)