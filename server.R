library(tidyverse)
library(dplyr)
library(shiny)
library(DT)
library(lubridate)
library(shinydashboard)
library(shinyWidgets)
library(RColorBrewer)
library(plotly)

source(file.path('helperFunctions.R', fsep = .Platform$file.sep))

#read in transactions from csv
all_transactions <- readInTransactions("transactions.csv")
print(str(all_transactions))

server <- function(session,input, output) {
  
  
  #------------------------Reactive Data Sources------------------------
  
  allTransactions <- reactive({
    all_transactions %>% filter(`account_name` %in%  input$accounts)
  })
  
  getFilteredData <- reactive({
    filtered_df = allTransactions() %>%
      filter(month_year %in% input$dateRange) %>% 
      filter(`amount` >= input$valRange[1]) %>% 
      filter(`amount` <= input$valRange[2]) %>% 
      filter(`description` %in% input$places)
    filtered_df <- filtered_df[!duplicated(filtered_df), ]
    return(filtered_df)
  })
  
  # allDebitTransactions <- reactive({
  #   getFilteredData() %>% filter(transaction_type == 'debit')
  # })
  # 
  allCreditTransactions <- reactive({
    getFilteredData() #%>% filter(transaction_type == 'credit')
  })
  
  aggregateDebitByMonth <- reactive({
    df = getFilteredData() %>%
      group_by(
        desc = `description`,
        month = paste0(floor_date(`posting_date`,"month"))
      ) %>%
      summarize(amount = sum(`amount`),n=n())
    return(df)
  })
    
  #------------------------Update places Buttons------------------------
  grocceries <- c("Walmart","Costco","Target")
  observeEvent(input$groc_only, {
    # updateSelectInput(session, "category",selected = c('Groceries'))
    updateSelectInput(session, "places",choices = grocceries,selected = grocceries)
  })
  # 
  retail <- c('Amazon','Target')
  observeEvent(input$retail_only, {
    updateSelectInput(session, "category",selected = c('Retail'))
    updateSelectInput(session, "places",selected = retail,choices = retail)
  })
  # 
  utilities <- c('Tennis Sanitation','City of Woodbury',"Comcast")
  observeEvent(input$util_only, {
    updateSelectInput(session, "places",selected = utilities,choices = utilities)
  })
  # 
  streaming_services <- c('Youtube Tv','Youtube Premium',"Netflix",'Hulu','Disney Plus','Apple')
  observeEvent(input$streaming_only, {
    updateSelectInput(session, "places",selected = streaming_services,choices = streaming_services)
  })
  #
  observeEvent(input$restaurants_only, {
    updateSelectInput(session, "category",selected = "")
    updateSelectInput(session, "category",selected = c('Restaurants'))
  })
  #
  observeEvent(input$clear_places, {
    updateSelectInput(session, "places",selected = "")
    #updateSelectInput(session, "category",selected = c('Restaurants'))
  })
  
  #------------------------Filters------------------------
  
  output$accounts = renderUI({
    pickerInput(
      "accounts", 
      "Accounts:", 
      unique(all_transactions$account_name),
      multiple = TRUE, 
      selected = unique(all_transactions$account_name),
      options = list(`actions-box` = TRUE)
    )
  })
  
  output$category = renderUI({
    pickerInput(
      "category", 
      "Category:", 
      choices = unique(allTransactions()$transaction_cat),
      multiple = TRUE, 
      options = list(`actions-box` = TRUE),
      # selected = c("Groceries"),
    )
  })
  
  output$places = renderUI({
    df <- allTransactions() %>% filter(transaction_cat %in% input$category)
    selectInput(
      "places", 
      "Places:", 
      choices = unique(df$description),
      selected = unique(df$description),
      multiple = TRUE
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
  
  output$dateRange = renderUI({
    pickerInput(
      "dateRange", 
      "Dates",
      choices = unique(all_transactions$month_year),
      selected = unique(all_transactions$month_year),
      multiple = TRUE, 
      options = list(`actions-box` = TRUE)
    )
  })
   
  #------------------------Monthly Summary tab content------------------------
  
   output$monthly_summary <- renderPlotly({
     shiny::validate(need(nrow(aggregateDebitByMonth()) != 0," "))
     df = aggregateDebitByMonth()
     p <- plot_ly() %>%
     add_trace(
       x = df$month,
       y = df$amount,
       type = 'bar',
       color = df$desc,
       text = ~df$amount,
       textfont = list(color = '#000000', size = 16),
       textposition = 'auto',
       colors = brewer.pal(length(names(table(df$desc))),"Paired"),
       hovertemplate = paste(df$desc,'($%{y})<br>%{x}')
       ) %>%
       layout(yaxis = list(title = 'Amount ($)'),
              xaxis = list(title = 'Month',
                           type = 'date',
                           tickformat = "%b %y"
              ),
              barmode = 'stack')
     return(p)
   })

   output$monthly_summary_by_month <- DT::renderDataTable({
     shiny::validate(need(!is.na(aggregateDebitByMonth())," "))
     DT::datatable(
       aggregateDebitByMonth() %>%
         group_by(month) %>% summarise(ta = sum(amount)) %>%
         mutate(month_label = paste0(month(month,abbr = T,label = T)," ",year(month))) %>%
         select("Month" = month_label,
                "Hidden Date" = month,
                "Total ($)" = ta),
       rownames = FALSE,
       options = list(pageLength = 50,
                      dom = 't',
                      order = list(list(1, 'desc')),
                      columnDefs = list(list(visible=FALSE, targets=c(1)))
       )
     )
   })

   output$monthly_summary_list <- DT::renderDataTable({
     shiny::validate(need(!is.na(aggregateDebitByMonth())," "))
     DT::datatable(
       aggregateDebitByMonth() %>%
         mutate(month_label = paste0(month(month,abbr = T,label = T)," ",year(month))) %>%
         select("Month" = month_label,
                "Hidden Date" = month,
                'Place' = desc,
                "# of Transactions" = n,
                "Total ($)" = amount),
       rownames = FALSE,
       options = list(pageLength = 50,
                      dom = 't',
                      order = list(list(1, 'desc')),
                      columnDefs = list(list(visible=FALSE, targets=c(1)))
                  )
     )
   })

   output$monthly_summary_by_desc <- DT::renderDataTable({
     shiny::validate(need(!is.na(aggregateDebitByMonth())," "))
     DT::datatable(
       aggregateDebitByMonth() %>%
         group_by(desc) %>% summarize(total = sum(amount),n=n(),ave = round(sum(amount)/n()),digits=2) %>%
         select('Place' = desc,
                "# of Transactions" = n,
                "Ave. Transaction" = ave,
                "Total ($)" = total),
       rownames = FALSE,
       options = list(pageLength = 50,dom = 't')
     )
   })
  
  #------------------------Transaction tab content------------------------
  
   output$transaction_scatter <- renderPlotly({
     df <- getFilteredData()
     p <- plot_ly(
       df,
        # type = 'bar',
        size = 15,
        x = ~`posting_date`,
        y = ~`amount`,
        color = ~`description`,
       textposition = "auto",
        hovertemplate = paste(df$description,'($%{y})<br>%{x}'),
        colors = brewer.pal(length(names(table(df$description))),"Paired")
     ) %>%
       layout(yaxis = list(title = 'Amount ($)'),
              xaxis = list(title = 'Date')
        )
     return(p)
   })

   output$transaction_list <- DT::renderDataTable({
     DT::datatable(
       getFilteredData() %>%
         mutate(date_label = paste(
                               month(posting_date,label = TRUE),
                               wday(posting_date),
                               year(posting_date),
                               "(",wday(posting_date,label = TRUE),")"
                               )) %>%
         select("Date" = date_label,
                "Real Date" = posting_date,
                "Label" = description,
                "Amount ($)" = amount),
       rownames = FALSE,
       options = list(pageLength = 50,
                      dom = 'ft',
                      order = list(list(1, 'desc')),
                      columnDefs = list(list(visible=FALSE, targets=c(1))))
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
  # retList <- function(id,group,content,start){
  #   return(list(id = id, group = group,content = content, start = start))
  # }
  # 
  # output$timeline_year <- renderTimevis({
  #   
  #   timeline_annual_items <- timeline_items %>% filter(period == "year")
  #   
  #   tv <- timevis()
  #   
  #   for(i in 1:nrow(timeline_annual_items)){
  #     item <- timeline_annual_items[i,]
  #     tv <- tv %>% addItem(retList(item$id,item$group,paste0(item$content," (",item$amount,")"),item$date))
  #   }
  #   tv <- tv %>% setWindow(start = "2020-01-01", end="2020-12-30")
  #   tv
  # })
  
  # output$timeline_month <- renderTimevis({
  #   
  #   timeline_monthly_items <- timeline_items %>% filter(period == "month")
  #   
  #   tv <- timevis(groups = data.frame(id = c("bill","inv"),content = c("Bills","Investment"),style = c("color:red","color:green")))
  #   
  #   for(i in 1:nrow(timeline_monthly_items)){
  #     item <- timeline_monthly_items[i,]
  #     tv <- tv %>% addItem(retList(item$id,item$group,paste0(item$content," (",item$amount,")"),item$date))
  #   }
  #   tv <- tv %>% setWindow(start = "2020-01-01", end="2020-01-31")
  #   tv
  # })

}