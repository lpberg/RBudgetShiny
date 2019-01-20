library(tidyverse)
library(dplyr)
library(readxl)
library(shiny)
library(DT)
library(plotly)
library(lubridate)

#read in transactions from excel
all_transactions<- read_csv("transactions.csv")

# all_transactions$Description = str_replace_all(all_transactions$Description, "\\s+", " ")

#read in dictionary from excel
# dict <- read_excel("dictionary.xls")
# dict$Description = str_replace_all(dict$Description, "\\s+", " ")
#combine transaction table with dictionary table to apply labels
# all_transactions = left_join(all_transactions, dict, by="Description")
# 
# blanks <- all_transactions[is.na(all_transactions$specific_catagory),]
# if(nrows(blanks) > 0) {
#   write_excel_csv(blanks,path = "blanks.csv")
# }
summary(all_transactions)
#reformat posting date column
all_transactions$Date <- mdy(all_transactions$Date)
all_transactions$`Account Name` <- as.character(all_transactions$`Account Name`)
all_transactions <- rename(all_transactions, 
                                `posting_date` = `Date`,
                                `description` = `Description`,
                                `orig_description` = `Original Description`,
                                `amount` = `Amount`,
                                `transaction_type` = `Transaction Type`,
                                `acount_name` = `Account Name`,
                                `transaction_cat` = `Category`)

#set amounts to be inverse (not negative)
# all_transactions$amount <- all_transactions$amount / -1

summary(all_transactions)

server <- function(input, output) {
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
    selectInput("categories", "Select Categories:", 
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
       select(c(`amount`,`description`,`transaction_cat`,`posting_date`)) %>%
      filter(`posting_date` >= input$dateRange[1] & `posting_date` <= input$dateRange[2]) %>%
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
    df = aggregateByMonth()
    p <- plot_ly(df, x = ~month, y = ~amount, type = 'bar', color = ~lab1) %>%
      layout(yaxis = list(title = 'Count'), barmode = 'stack')
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

