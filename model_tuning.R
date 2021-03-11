


## Default parameters

#nrounds: 1000 
#max_depth: 6 
#eta: 0.3     
#gamma: 0
#colsample_bytree: 1 
#min_child_weight:1 
#subsample 1: 
  
  
  

grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

ctrl <- trainControl(
  method = "none",
  verboseIter = FALSE,
  allowParallel = TRUE
  
)

xgb_base <- train(
  x = as.matrix(train_x),
  y = train_y,
  trControl = ctrl,
  tuneGrid = grid_default,
  method = "xgbTree",
  verbose = TRUE
  
)

defaultSummary(data.frame(obs = test_y, 
                          pred = predict(xgb_base, as.matrix(test_x))))




## STEP 1: Iteration numbers and Learning Rate 

nrounds <- 1000

tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50), 
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

ctrl <- trainControl(
  method = "cv", 
  number = 10, 
  verboseIter = FALSE, 
  allowParallel = TRUE  
)


xgb_tune <- train(
  x = as.matrix(train_x),
  y = train_y,
  trControl = ctrl,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)


defaultSummary(data.frame(obs = test_y, 
                          pred = predict(xgb_tune, as.matrix(test_x))))


tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}


tuneplot(xgb_tune)

xgb_tune$bestTune











## STEP 2: max_depth and  min_child_weight


tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = 4,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)


xgb_tune2 <- train(
  x = as.matrix(train_x),
  y = train_y,
  trControl = ctrl,
  tuneGrid = tune_grid2,
  method = "xgbTree",
  verbose = TRUE
)


tuneplot(xgb_tune2)

xgb_tune2$bestTune

defaultSummary(data.frame(obs = test_y, 
                          pred = predict(xgb_tune2, as.matrix(test_x))))



##Step 3: colsample and  subsample 

tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = 4, 
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0), 
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0) 
)

xgb_tune3 <- train(
  x = as.matrix(train_x),
  y = train_y,
  trControl = ctrl,
  tuneGrid = tune_grid3,
  method = "xgbTree",
  verbose = TRUE
)


tuneplot(xgb_tune3)


xgb_tune3$bestTune
defaultSummary(data.frame(obs = test_y, 
                          pred = predict(xgb_tune3, as.matrix(test_x))))




## Step 4: Gamma
tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = 5,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune4 <- train(
  x = as.matrix(train_x),
  y = train_y,
  trControl = ctrl,
  tuneGrid = tune_grid4,
  method = "xgbTree",
  verbose = TRUE
)

tuneplot(xgb_tune4)
xgb_tune4$bestTune

defaultSummary(data.frame(obs = test_y, 
                          pred = predict(xgb_tune4, as.matrix(test_x))))





##Step 5: Learning Rate


tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 1000, by = 50),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = xgb_tune4$bestTune$gamma,
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune5 <- train(
  x = as.matrix(train_x),
  y = train_y,
  trControl = ctrl,
  tuneGrid = tune_grid5,
  method = "xgbTree",
  verbose = TRUE
)

tuneplot(xgb_tune5)

xgb_tune5$bestTune
defaultSummary(data.frame(obs = test_y, 
                          pred = predict(xgb_tune5, test_x)))



## Step 6: Final Model

final_grid <- expand.grid(
  nrounds = xgb_tune5$bestTune$nrounds,
  eta = xgb_tune5$bestTune$eta,
  max_depth = xgb_tune5$bestTune$max_depth,
  gamma = xgb_tune5$bestTune$gamma,
  colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
  min_child_weight = xgb_tune5$bestTune$min_child_weight,
  subsample = xgb_tune5$bestTune$subsample
)


xgb_final_model <- train(
  x = as.matrix(train_x),
  y = train_y,
  trControl = ctrl,
  tuneGrid = final_grid,
  method = "xgbTree",
  verbose = TRUE
)


#test hatasi
defaultSummary(data.frame(obs = test_y, 
                          pred = predict(xgb_final_model, as.matrix(test_x))))




## Step 7: Savingm Final Model




save(xgb_final_model, file = "xgb_final_model.rda")
rm(xgb_final_model)
load("xgb_final_model.rda")

predict(xgb_final_model, test_x)








