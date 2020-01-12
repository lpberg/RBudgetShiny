library(tidyverse)
library(dplyr)
library(readxl)
library(shiny)
library(DT)
library(plotly)
library(lubridate)
library(shinydashboard)
library(timevis)

source(file.path('helperFunctions.R', fsep = .Platform$file.sep))

#read in transactions from csv
all_transactions <- readInTransactions("transactions.csv")
timeline_items <- readxl::read_excel('timeline.xlsx',sheet = 'cal')

server <- function(session,input, output) {
  #------------------------Update Categories Buttons------------------------
  observeEvent(input$groc_only, {
    df <- allDebitTransactions() %>% filter(transaction_cat == "Groceries")
    updateSelectInput(session, "categories",selected = unique(df$description))
  })
  
  observeEvent(input$amaz_only, {
    updateSelectInput(session, "categories",selected = c('Amazon'))
  })
  
  observeEvent(input$stream_only, {
    updateSelectInput(session, "categories",selected = c('Netflix',"Hulu","AUTH : GOOGLE*GOOGLE MUSIC"))
  })
  
  observeEvent(input$util_only, {
    df <- allDebitTransactions() %>% filter(transaction_cat %in%  c("Utilities","Internet"))
    updateSelectInput(session, "categories",selected = unique(df$description))
  })
  
  observeEvent(input$rest_only, {
    df <- allDebitTransactions() %>% filter(transaction_cat == "Restaurants")
    updateSelectInput(session, "categories",selected = unique(df$description))
  })
  
  observeEvent(input$cafe_only, {
    df <- allDebitTransactions() %>% filter(transaction_cat == "Coffee Shops")
    updateSelectInput(session, "categories",selected = unique(df$description))
  })
  
  #------------------------Reactive Data Sources------------------------
  
  allDebitTransactions <- reactive({
    df = all_transactions %>% filter(transaction_type == 'debit')
  })
  
  allCreditTransactions <- reactive({
    df = all_transactions %>% filter(transaction_type == 'credit')
  })
  
  aggregateByMonth <- reactive({
    df = getFilteredData() %>% 
      group_by(
        label = `description`, 
        month = paste0(floor_date(`posting_date`,"month"))
      ) %>%
      summarize(amount = sum(`amount`)
      )
    return(df)
  })
  
  getFilteredData <- reactive({
    filtered_df = allDebitTransactions() %>%
      filter(`posting_date` >= input$dateRange[1]) %>% 
      filter(`posting_date` <= input$dateRange[2]) %>%
      filter(`account_name` %in%  input$accounts) %>%
      filter(`amount` >= input$valRange[1]) %>% 
      filter(`amount` <= input$valRange[2]) %>% 
      filter(`description` %in% input$categories)
    return(filtered_df)
  })
    
  #------------------------Sidebar Filters------------------------
  
  output$accounts = renderUI({
    selectInput(
      "accounts", 
      "Accounts:", 
      unique(allDebitTransactions()$account_name),
      multiple = FALSE, 
      selected = c("Fidelity Rewards Visa Signature")
    )
  })
  
  output$valRange = renderUI({
    sliderInput(
      "valRange", 
      "Amount Range", 
      min = 0, 
      max = 5000, 
      value=c(1,5000), 
      step = NULL, 
      round = TRUE
    )
  })
  
  output$categories = renderUI({
    selectInput(
      "categories", 
      "Categories:", 
      unique(c(allDebitTransactions()$description)),
      multiple = TRUE, 
      selected = c('Hy-Vee')
    )
  })
  
  output$dateRange = renderUI({
    dateRangeInput('dateRange',
      label = 'Date range', 
      format = 'MM-yy',startview = "month",
      start = min(allDebitTransactions()$posting_date), 
      end = max(allDebitTransactions()$posting_date)
    )
  })
  
  #------------------------Transaction tab content------------------------
  
  output$transaction_scatter <- renderPlotly({
    p <- plot_ly(
      getFilteredData(), 
       type = 'bar', 
       x = ~`posting_date`, 
       y = ~`amount`, 
       color = ~`description`
    ) %>% 
      layout(yaxis = list(title = 'Amount ($)'), 
             xaxis = list(title = 'Date')
       )
    return(p)
  })
  
  output$transaction_list <- DT::renderDataTable({
    DT::datatable(
      getFilteredData() %>% select("Date" = posting_date,
               "Label" = description,
               "Amount ($)" = amount),
      rownames = FALSE,
      options = list(pageLength = 50)
    )
  })
   

   
  #------------------------Monthly Summary tab content------------------------
  
   output$monthly_summary <- renderPlotly({
        p <- plot_ly(
          aggregateByMonth(), 
          x = ~month, 
          y = ~amount, 
          type = 'bar', 
          text = ~amount,
          textfont = list(color = '#000000', size = 16),
          color = ~label, 
          textposition = 'auto') %>%
          layout(yaxis = list(title = 'Amount ($)'), 
                 xaxis = list(title = 'Month',
                                type = 'date',
                                tickformat = "%b %y"
                              ),
                 barmode = 'stack')
        return(p)
   })
  
  output$monthly_summary_list <- DT::renderDataTable({
    DT::datatable(
      aggregateByMonth() %>% select('Label' = label,"Month" = month, "Amount ($)" = amount),
      rownames = FALSE,
      options = list(pageLength = 50)
    )   
  })
  #------------------------Credit tab content------------------------
  
  output$credit_transaction_scatter <- renderPlotly({
    p <- plot_ly(
      allCreditTransactions(), 
      type = 'bar', 
      x = ~`posting_date`, 
      y = ~`amount`, 
      color = ~`transaction_cat`
    )
    return(p)
  })
  
  output$credit_transaction_list <- DT::renderDataTable({
    df <- allCreditTransactions()
    DT::datatable(
      df %>% select("Date" = posting_date,
                    "Label" = description,
                    "Amount ($)" = amount),
      rownames = FALSE,
      options = list(pageLength = 50)
    )
  })
  
  #------------------------Timeline tab content------------------------
  retList <- function(id,group,content,start){
    return(list(id = id, group = group,content = content, start = start))
  }
  
  output$timeline_year <- renderTimevis({
    
    timeline_annual_items <- timeline_items %>% filter(period == "year")
    
    tv <- timevis()
    
    for(i in 1:nrow(timeline_annual_items)){
      item <- timeline_annual_items[i,]
      tv <- tv %>% addItem(retList(item$id,item$group,paste0(item$content," (",item$amount,")"),item$date))
    }
    tv <- tv %>% setWindow(start = "2020-01-01", end="2020-12-30")
    tv
  })
  
  output$timeline_month <- renderTimevis({
    
    timeline_monthly_items <- timeline_items %>% filter(period == "month")
    
    tv <- timevis(groups = data.frame(id = c("bill","inv"),content = c("Bills","Investment"),style = c("color:red","color:green")))
    
    for(i in 1:nrow(timeline_monthly_items)){
      item <- timeline_monthly_items[i,]
      tv <- tv %>% addItem(retList(item$id,item$group,paste0(item$content," (",item$amount,")"),item$date))
    }
    tv <- tv %>% setWindow(start = "2020-01-01", end="2020-01-31")
    tv
  })

}