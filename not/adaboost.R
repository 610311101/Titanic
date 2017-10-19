



confusionMatrix(test$Direction, predict(modelFit, test))



##
##  
##
##  LDA
target  <- "Survived";
dummy <- dummyVars(~.,Data[,!colnames(Data)%in%target])
Data.dummy <- predict(dummy,Data[,!colnames(Data)%in%target])
Data.X <- data.frame(Data.dummy) 
Data <- cbind(select(Data,target),Data.X)
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
lda.fit <- train(as.formula(paste0(target, "~.")),
                 method='qda',
                 #preProcess=c('scale', 'center'),
                 data=Data.train.train)

control <- 
  trainControl(method="repeatedcv", 
               number=10, 
               repeats=3)
knn.fit <- train(as.formula(paste0(target, "~.")), 
                 data = Data.train.train,
                 method = "knn",
                 trControl=control,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
plot(knn.fit)
preds <- predict(knn.fit, Data.train.test)
confusionMatrix(preds, Data.train.test[,target])
##
##  Fit real test
if( sum(ls()=="submission")==0 ) submission <- list();
preds <- predict(knn.fit, Data.test);
submission$knn <- preds;
rm(list = ls()[!ls() %in% c("submission","Data","targat")])