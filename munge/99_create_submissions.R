MODELS %>% 
  rowwise %>% 
  do({
    fn=sprintf("reports/submission_%s_%s%s%s_%s.csv", .$method,
               year(now()), month(now()), day(now()),
               system("git describe --tags --dirty", intern=TRUE)
    )
    
    data.frame(
      id = 1:length(.$predict),
      cost = .$predict
    ) %>% 
      mutate( cost=replace(cost, cost<=0, min(max(0, cost))) ) %>% 
      write.table( fn, row.names = FALSE, sep=",", quote = FALSE)
    data.frame()
  })