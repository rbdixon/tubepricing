# KISS - average of quotes
submission_kiss = function(TRAIN, TEST) {
  SUBMISSION = TEST %>% 
    mutate(
      cost = mean(TRAIN$cost)
    ) %>% 
    select( id, cost )
  
  return(SUBMISSION)
}

write.table( submission_kiss(train.set, test.set), "reports/submission_kiss.csv", row.names = FALSE, sep=",", quote = FALSE)