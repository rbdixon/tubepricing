# Setup
set.seed(20150716)
registerDoMC(cores=4)

# Prepare train and test data
prep_data = function(D) {
  D %>% 
    tbl_df %>% 
    mutate_each( funs(factor), tube_assembly_id, supplier ) %>% 
    #     mutate(
    #       # Extract date
    #       quote_date = ymd(quote_date),
    #       quote_year = year(quote_date),
    #       quote_quarter = quarter(quote_date)
    #     ) %>% 
    select(
      # Can't predict on ID alone since they are not duplicated in TEST
      -tube_assembly_id,
      -supplier,
      -annual_usage,
      -min_order_quantity,
      -bracket_pricing,
      -quote_date
      #       -quote_year,
      #       -quote_quarter
    )
}

# Prep data
TRAIN = prep_data( train.set[1:10, ])
TEST =  prep_data(test.set)

# Evaluation function
caret_rmlse = function(data, lev, model) {
  c(RMSLE=rmsle(data$obs, data$pred))
}

# Set up training structure
tc = trainControl(
  method = "repeatedcv",
  repeats = 10,
  savePredictions = TRUE,
  verboseIter = FALSE,
  summaryFunction = caret_rmlse
)

# Training formula
train_formula = cost ~ .

# Uncomment to retrain
rm(MODELS)

if (!exists("MODELS")) {
  # Specify the types of models to train
  MODELS = data.frame(
    method = c("lm", "rf")
  )
  
  MODELS %<>%
    group_by(method) %>% 
    group_by(method) %>%
    # Train models
    do(
      model = train( train_formula ,
                     data = TRAIN,
                     method = .$method,
                     trControl = tc,
                     metric = "RMSLE"
      )
    ) %>% 
    rowwise %>% 
    # Extract performance measures
    do({
      predictions = predict(.$model, TEST)
      data.frame(
        method = .$method, 
        model = I(list(.$model)), 
        # parameter = .$model$results[,1],
        RMSLE = .$model$results[,"RMSLE"],
        RMSLESD = .$model$results[,"RMSLESD"],
        predict = I(list(predictions)),
        LTEZ_cost = length(which(predictions <= 0)),
        NA_cost = length(which(is.na(predictions)))
      )
    }) %>% 
    arrange(desc(RMSLE)) %>% 
    identity
}
print(MODELS)

# Save model performance log
MODELS %>% 
  mutate(
    date = now(),
    commit = system("git describe --tags --dirty", intern=TRUE)
  ) %>% 
  select( date, commit, method, RMSLE, RMSLESD, contains("cost")) %>% 
  write.table( "reports/training.log", append=TRUE, quote=FALSE, sep=",", col.names=FALSE, row.names=FALSE)

cache("MODELS")