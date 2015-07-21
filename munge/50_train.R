# Setup
set.seed(20150716)
registerDoMC(cores=4)

# Prepare train and test data
prep_data = function(TRAIN, TEST, PARTS) {
  R = bind_rows(
    mutate(train.set, dataset = "TRAIN", id = 1:n()),
    mutate(test.set,  dataset = "TEST")
  ) %>% 
    tbl_df %>% 
    mutate_each( funs(factor), tube_assembly_id, supplier, dataset ) %>% 
    mutate(
      # Random number for partitioning
      randpart = runif(n()),
      
      # Extract date
      quote_date = ymd(quote_date),
      quote_year = as.integer(year(quote_date)) - min(as.integer(year(quote_date))),
      # quote_quarter = factor(quarter(quote_date))
      
      # Transform cost
      cost = log(cost+1)
    ) %>% 
    
    # Add PARTS details
    left_join(PARTS, by="tube_assembly_id") %>%
    # Replace NA values with zeros where required
    mutate_each( funs( replace(., is.na(.), 0) ), num_parts) %>%
    # Factorize where needed
    mutate_each( funs( factor ), num_parts ) %>% 
    
    # Add TUBE detauls
    left_join(TUBE, by="tube_assembly_id") %>% 
    mutate(
      bend_radius = replace( bend_radius, bend_radius==9999, 0 ),
      length = pmin( length, mean(length)+2*sd(length) )
    ) %>% 
    mutate_each( funs(factor(replace(as.character(.), is.na(.), "SP-NONE"))), starts_with("spec"), material_id ) %>% 
    mutate_each( funs(factor(replace(as.character(.), is.na(.), "No"))), starts_with("forming_") ) %>% 
    
    # Merge levels
    mutate(
      num_parts = revalue( num_parts, c("7"="7+", "8"="7+", "9"="7+", "10"="7+", "11"="7+", "12"="7+", "13"="7+"))
    ) %>% 
    mutate_each( funs(factor), num_bends, num_boss, num_bracket, other ) %>% 
    
    # Remove columns not used for modeling
    select(
      # Can't predict on ID alone since they are not duplicated in TEST
      -tube_assembly_id,
      # Not all suppliers in TEST are present in TRAIN
      -supplier,
      # Use the year and quarter
      -quote_date
    )
  
  # Arrange columns
  first_cols = c("id")
  last_cols = c("cost", "randpart")
  middle_cols = setdiff( colnames(R), c(first_cols, last_cols))
  select_(R, .dots=c(first_cols, middle_cols, last_cols))
}

# Prep data
DATA = prep_data( train.set, test.set, PARTS )

TRAIN = DATA %>% 
  filter( dataset == "TRAIN" ) %>% 
  # Subset to speed development
  # filter( randpart <= 0.15 ) %>% 
  select( -randpart, -dataset )
  
TEST = DATA %>% 
  filter( dataset == "TEST" ) %>% 
  select( -dataset, -randpart, -cost )

# Export TRAIN and TEST
export_csv( TRAIN, "train" )
export_csv( TEST, "test" )

# Evaluation function
# function(obs, pred) sqrt( 1/length(obs) * sum((log(pred+1) - log(obs+1))^2))
caret_rmsle = function(data, lev, model) {
  # Calculate RMSLE in the non-log transformed space so it compares to the leaderboard
  c(RMSLE=rmsle(
    exp(data$obs)-1, 
    exp(data$pred)-1
  ))
}

# Set up training structure
tc = trainControl(
  method = "repeatedcv",
  number = 5,
  # repeats = 5,
  savePredictions = TRUE,
  summaryFunction = caret_rmsle,
  # summaryFunction = defaultSummary,
  verboseIter = TRUE
)

# Training formula, exclude id and randpart
train_formula = cost ~ . -id

# Uncomment to retrain
#rm(MODELS)

if (!exists("MODELS")) {
  # Specify the types of models to train
  MODELS = data.frame(
    method = c("rf")
  )
  
  MODELS %<>%
    group_by(method) %>% 
    group_by(method) %>%
    # Train models
    do(
      model = train( train_formula ,
                     # preProcess = "",
                     data = TRAIN,
                     method = .$method,
                     trControl = tc,
                     metric = "RMSLE",
                     maximize = FALSE
      )
    ) %>% 
    rowwise %>% 
    # Extract performance measures
    do({
      # Reverse the log transformation!
      predictions = exp(predict(.$model, TEST))-1
      data.frame(
        method = c(as.character(.$method)), 
        model = I(list(.$model)),
        # parameter = .$model$results[,1],
        RMSLE = min(.$model$results[,"RMSLE"]),
        RMSLESD = min(.$model$results[,"RMSLESD"]),
        predict = I(list(predictions)),
        LTEZ_cost = length(which(predictions <= 0)),
        NA_cost = length(which(is.na(predictions))),
        train_p = nrow(TRAIN) / nrow(train.set),
        resample_perf_NaN = length(which(is.nan(.$model$resample$RMSLE)))
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