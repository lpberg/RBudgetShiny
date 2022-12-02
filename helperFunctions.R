readInTransactions <- function(fileName){
    all_transactions <- read_csv(fileName)
    # all_transactions <- dplyr::rename(all_transactions,posting_date = 1)
    all_transactions$posting_date <- lubridate::mdy(all_transactions$posting_date)
    all_transactions$Account <- as.character(all_transactions$Account)
    all_transactions$Amount <- abs(all_transactions$Amount)
    print(str(all_transactions))
    all_transactions <- dplyr::rename(all_transactions, 
      # posting_date = Date,
      `description` = `Description`,
      # `orig_description` = `Description`,
      `amount` = `Amount`,
      # `transaction_type` = `charge`,
      `account_name` = `Account`,
      `transaction_cat` = `Category`)
    all_transactions <- all_transactions %>% 
      filter(account_name %in% c("Visa Signature Rewards - Ending in 2163"))
    
    all_transactions$month_year <- paste0(month(all_transactions$posting_date,abbr = T,label = T), " ",
                                          year(all_transactions$posting_date)
                                    )
    print(nrow(all_transactions))
    all_transactions <- unique(all_transactions)
    print(nrow(all_transactions))
    return(all_transactions)
}
# Date	Account	Description	Category	Tags	Amount

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

