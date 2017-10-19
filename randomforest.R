require(randomForest)
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
grid   <- expand.grid(mtry = seq(ncol(Data)-1))
rf.fit <- train(as.formula(paste0(target, "~.")),  ## target~.
                data=Data.train.train, 
                method="rf", 
                tuneGrid = grid,
                trControl= control)
plot(rf.fit)
plot(varImp(rf.fit))
preds <- predict(rf.fit, Data.train.test)
confusionMatrix(preds, Data.train.test[,target])

##
##  Fit real test
if( sum(ls()=="submission")==0 ) submission <- list();
submission$rf <- predict(rf.fit, Data.test)
rm(list = ls()[!ls() %in% c("submission","Data","targat")])

