library(xgboost)
##
##  
##
##  RandomForest tune
target  <- "Survived";
##
##  Split data
Data.train   <- Data[!is.na(Data[,target]),]
Data.test    <- Data[ is.na(Data[,target]),]
indexes <- 
  createDataPartition(Data.train[,target], times = 1, p = 0.6, list = F)
Data.train.train <- Data.train[ indexes,]
Data.train.test  <- Data.train[-indexes,]
##
##  Train control
control <- 
  trainControl(method="repeatedcv", 
               number=10, 
               repeats=3, 
               search="grid")
grid   <- expand.grid(
  eta = c(0.1,0.3,0.5),
  nrounds = c(75,100,150),
  max_depth = 6:8,
  min_child_weight = c(1.0,2.0,2.5),
  colsample_bytree = c(0.6,0.8),
  gamma = c(0:5),
  subsample = c(0.6)
)
xgb.fit <- train(as.formula(paste0(target, "~.")),  ## target~.
                data=Data.train.train, 
                method="xgbTree", 
                tuneGrid = grid,
                trControl= control)
plot(xgb.fit)
preds <- predict(xgb.fit, Data.train.test)
confusionMatrix(preds, Data.train.test[,target])
##
##  Fit real test
if( sum(ls()=="submission")==0 ) submission <- list();
submission$xgb <- predict(xgb.fit, Data.test)
rm(list = ls()[!ls() %in% c("submission","Data","targat")])

