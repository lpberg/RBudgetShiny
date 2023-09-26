library(plotly)
library(shiny)
library(DT)
library(shinydashboard)
# library(timevis)

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
                    actionButton("retail_only", "Retail"),
                    actionButton("restaurants_only","Restaurants"),
                    actionButton("streaming_only","Streaming Services"),
                    actionButton("clear_places","Clear")
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
                        "Monthly Summary",
                        plotlyOutput("monthly_summary"),
                        column(
                            width = 6,
                            tags$h4("Totals by Month"),
                            DTOutput("monthly_summary_by_month"),
                            tags$h4("Totals by Month, Place"),
                            DTOutput("monthly_summary_list")
                        ),
                        column(
                            tags$h4("Place Summary"),
                            width = 6,
                            DTOutput("monthly_summary_by_desc")
                        )
                    ),
                    tabPanel(
                        "Individual Transactions",
                        tags$h4("Transactions Over Time"),
                        plotlyOutput("transaction_scatter"),
                        column(
                            width = 4,
                            tags$h4("Individual Transactions"),
                            DTOutput("transaction_list")
                        )
                    ),
                    tabPanel(
                        "Credit Transactions",
                        plotlyOutput("credit_transaction_scatter"),
                        DTOutput("credit_transaction_list")
                    )
                    # tabPanel(
                    #     "Timeline",
                    #     tags$h4("Annual Timeline"),
                    #     timevisOutput("timeline_year"),
                    #     tags$h4("Monthly Timeline"),
                    #     timevisOutput("timeline_month")
                    # )
                )
                
            )
        )
    )
)