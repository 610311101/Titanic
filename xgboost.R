library(xgboost);
library(Matrix);
library("dplyr");
rm(list =ls())
splitData <- readRDS("splitData.RDS");
## ----
##  Balance train.
train_1 <-  splitData$train %>% filter( Survived==1 );
sample_train_1 <- train_1[ sample(1:nrow(train_1), 300, replace = T), ];
train <- rbind(
  splitData$train,
  sample_train_1
);
train.label <- train$Survived;
train.train <- train %>% select(Pclass, Age, SibSp, Parch, Fare, female, male, C, Q, S);
check.label <- splitData$check$Survived;
check.train <- splitData$check %>% select(Pclass, Age, SibSp, Parch, Fare, female, male, C, Q, S);
## ----
##  xgboost
fit <- xgboost(data = as.matrix(train.train), label = train.label, max_depth = 10,
               eta = 0.1, nthread = 2, nrounds = 20,objective = "binary:logistic");
pred <- predict(fit, as.matrix(check.train));
check.Survived <- 1*(pred > 0.5);
table(check.Survived,check.label);
## ----
##  variable importance
importance <- xgb.importance(feature_names = colnames(train.train), model = fit)
importance
