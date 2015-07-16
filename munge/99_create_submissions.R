MODELS %>% 
  rowwise %>% 
  do({
    fn=sprintf("reports/submission_%s_%s.csv", .$method, .$parameter)
    
    data.frame(
      id = 1:length(.$predict),
      cost = .$predict
    ) %>% 
      mutate( cost=replace(cost, cost<=0, min(max(0, cost))) ) %>% 
      write.table( fn, row.names = FALSE, sep=",", quote = FALSE)
    data.frame()
  })