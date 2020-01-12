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
                    uiOutput("valRange"),
                    uiOutput("dateRange")
                ),
                column(width = 4,
                    uiOutput("category"),
                    actionButton("groc_only", "Groceries (top 3)"),
                    actionButton("util_only", "Utilities"),
                    actionButton("amaz_only", "Amazon")
                ),
                column(width = 4,
                    uiOutput("places")
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