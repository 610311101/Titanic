## ----
##  Combine train and test to one data.
train <- read.csv("train.csv");
test  <- read.csv("test.csv");
test$Survived <- NA;
train$train <- T;
test$train <- F;
Data <- rbind(train,test);
Data$index <- 1:nrow(Data);
Data$index
saveRDS(Data,"Data.RDS");
