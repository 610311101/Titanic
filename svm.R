require(kernlab)
##
##  
##
##  SVM
target  <- "Survived";
targt.levels <- levels(Data[,target])
levels(Data[,target]) <- paste0("Y",levels(Data[,target]))
feature.names <- colnames(Data)
colnames(Data) <- gsub('[.]',"",colnames(Data))
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
               search="grid",
               classProbs=TRUE)
grid <- 
  expand.grid(
  sigma = c(.01, .015, 0.2),
  C     = c(0.75, 0.9, 1, 1.1, 1.25)
  )

svm.fit <- train(as.formula(paste0(target, "~.")), 
                 Data.train.train,
                 method="svmRadial", 
                 tuneGrid = grid,
                 trControl= control,
                 metric="Accuracy")
plot(svm.fit)
plot(varImp(svm.fit))
preds <- predict(svm.fit, Data.train.test)
confusionMatrix(preds, Data.train.test[,target])


##
##  Fit real test
if( sum(ls()=="submission")==0 ) submission <- list();
preds <- predict(svm.fit, Data.test);  levels(preds) <- c("0","1")
submission$svm <- preds;
rm(list = ls()[!ls() %in% c("submission","Data","targat")])