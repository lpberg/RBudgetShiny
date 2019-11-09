readInTransactions <- function(fileName){
    all_transactions <- read_csv(fileName)
    all_transactions$Date <- mdy(all_transactions$Date)
    all_transactions$`Account Name` <- as.character(all_transactions$`Account Name`)
    all_transactions <- rename(all_transactions, 
      `posting_date` = `Date`,
      `description` = `Description`,
      `orig_description` = `Original Description`,
      `amount` = `Amount`,
      `transaction_type` = `Transaction Type`,
      `account_name` = `Account Name`,
      `transaction_cat` = `Category`)
    return(all_transactions)
}

sankeyByDesc <- function(all_trans,cats){
  all_trans <- all_trans %>% filter(transaction_type == "debit") %>% filter_("transaction_cat!=description")
  all_trans <- all_trans %>% filter(transaction_cat %in% cats)
  all_trans_g <- all_trans %>% group_by(transaction_cat,description) %>% tally(as.integer(amount))
  nodes = unique(append(all_trans_g$transaction_cat,all_trans_g$description))
  all_trans_g$transaction_cat_i = match(all_trans_g$transaction_cat, nodes) - 1
  all_trans_g$description_i = match(all_trans_g$description, nodes) - 1
  
  p <- plot_ly(
    type = "sankey",
    node = list(
      label = unique(append(all_trans_g$transaction_cat,all_trans_g$description)),
      pad = 15,
      thickness = 20,
      line = list(
        color = "black",
        width = 0.5
      )
    ),
    link = list(
      source = all_trans_g$transaction_cat_i,
      target = all_trans_g$description_i,
      value =  all_trans_g$n,
      label =  all_trans_g$n
    )
  ) 
  return(p)
}

