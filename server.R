library(tidyverse)
library(dplyr)
library(readxl)
library(shiny)
library(DT)
library(plotly)
library(lubridate)
source(file.path('helperFunctions.R', fsep = .Platform$file.sep))
#read in transactions from excel
# all_transactions<- read_csv("transactions.csv")
# all_transactions$Date <- mdy(all_transactions$Date)
# all_transactions$`Account Name` <- as.character(all_transactions$`Account Name`)
# all_transactions <- rename(all_transactions, 
#                                 `posting_date` = `Date`,
#                                 `description` = `Description`,
#                                 `orig_description` = `Original Description`,
#                                 `amount` = `Amount`,
#                                 `transaction_type` = `Transaction Type`,
#                                 `acount_name` = `Account Name`,
#                                 `transaction_cat` = `Category`)
all_transactions <- readInTransactions("transactions.csv")
#set amounts to be inverse (not negative)
# all_transactions$amount <- all_transactions$amount / -1

summary(all_transactions)

server <- function(input, output) {
  #UI element for selecting categories
  output$accounts = renderUI({
    selectInput("accounts", "Accounts:", 
                unique(all_transactions$account_name),
                multiple = TRUE, selected = c('S/D - Money Manager'))
  })
  #UI element for setting the amount range to display
  output$valRange = renderUI({
    sliderInput("valRange", "Amount Range", 
                min = 0, 
                max = 2000, 
                value=c(1,2000), 
                step = NULL, 
                round = TRUE)
  })
  #UI element for selecting categories
  output$categories = renderUI({
    selectInput("categories", "Categories:", 
                unique(c(c(all_transactions$description),c(all_transactions$transaction_cat))),
                multiple = TRUE, selected = c('Target'))
  })
  #UI element for setting date range of transactions
  output$dateRange = renderUI({
    dateRangeInput('dateRange',
         label = 'Date range', 
         format = 'MM-yy',startview = "week",
         start = min(all_transactions$posting_date), 
         end = max(all_transactions$posting_date))
  })
  aggregateByMonth <- reactive({
    df = getFilteredData() %>% group_by(lab1 = `description`, 
        month = paste(
          months(floor_date(`posting_date`,"month")),
           year(floor_date(`posting_date`,"month")))) %>%
      summarize(amount = sum(`amount`))
    return(df)
  })
  getFilteredData <- reactive({
    filtered_df = all_transactions %>%
       select(c(`amount`,`description`,`transaction_cat`,`posting_date`,`account_name`)) %>%
      filter(`posting_date` >= input$dateRange[1] & `posting_date` <= input$dateRange[2]) %>%
      filter(`account_name` %in%  input$accounts) %>%
      filter(`amount` >= input$valRange[1] & `amount` <= input$valRange[2])
    #only show labels from input$labels (show all is default) 
    if (length(input$categories) != 0){
      filtered_df = filtered_df %>%
        filter(`description` %in% input$categories | `transaction_cat` %in% input$categories)
    }
    return(filtered_df)
  })
  
  output$plot1 <- renderPlotly({
    df = getFilteredData()
    p <- plot_ly(data = df, x = ~`posting_date`, y = ~`amount`, color = ~`transaction_cat`)
    return(p)
  })
  
  output$plot2 <- renderPlotly({
    # OLD WAY
    # df = aggregateByMonth()
    # p <- plot_ly(df, x = ~month, y = ~amount, type = 'bar', color = ~lab1) %>%
    #   layout(yaxis = list(title = 'Count'), barmode = 'group')
    
    # NEW WAY   
    df = getTransactionsByMonthByDescription(all_df = all_transactions,
                                             minDate = input$dateRange[1],
                                             maxDate = input$dateRange[2],
                                             descriptions = input$categories,
                                             minAmount = input$valRange[1],
                                             maxAmount = input$valRange[2])
    p <- plotMonthlyTransactionSummaryByDescriptions(df)
    return(p)
  })
  
   output$table <- renderUI({
     output$tt <- DT::renderDataTable(DT::datatable(getFilteredData(), rownames = FALSE, options = list( #disabling rowname prevents the index from being displayed
       pageLength = 50
     )))
     dataTableOutput('tt')
   })
   
   output$table2 <- renderUI({
     output$tt2 <- DT::renderDataTable(DT::datatable(aggregateByMonth(), rownames = FALSE, options = list( #disabling rowname prevents the index from being displayed
       pageLength = 50
     )))
     dataTableOutput('tt2')
   })
   
}

# Run the application 
#shinyApp(ui = ui, server = server)

