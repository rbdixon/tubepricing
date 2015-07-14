# KISS
submission_kiss = function(TRAIN, TEST) {
  PRICES = TRAIN %>% 
    mutate(
      price = cost / quantity
    ) %>% 
    summarize(
      price = mean(price)
    )
  
  SUBMISSION = TEST %>% 
    mutate(
      cost = quantity * PRICES[1,"price"]
    ) %>% 
    select( id, cost )
  
  return(SUBMISSION)
}

write.table( submission_kiss(train.set, test.set), "reports/submission_kiss.csv", row.names = FALSE, sep=",", quote = FALSE)