## ----
library("randomForest")
## ----
##  fit
rf.fit <- 
  randomForest(
    Survived~.,
    train.train$train.train %>% select(-PassengerId),
    ntree = 1200,
    classwt = c(0.5,0.5),
    maxnodes = 2
  )
rf.fit
## ----
##  valid.1
v1 <- predict(
  rf.fit,
  train.train$train.valid.1 %>% select(-PassengerId,-Survived)
)
v1.predict.table   <- table(predict = v1, real = train.train$train.valid.1$Survived)
v1.predict.table
v1.predict.auccury <- sum(diag(v1.predict.table))/sum(v1.predict.table)
v1.predict.auccury
## ----
##  valid.2
v2 <- predict(
  rf.fit,
  train.train$train.valid.2 %>% select(-PassengerId,-Survived)
)
v2.predict.table   <- table(predict = v2, real = train.train$train.valid.2$Survived)
v2.predict.table
v2.predict.auccury <- sum(diag(v2.predict.table))/sum(v2.predict.table)
v2.predict.auccury
## ----
##  valid.3
v3 <- predict(
  rf.fit,
  train.train$train.valid.3 %>% select(-PassengerId,-Survived)
)
v3.predict.table   <- table(predict = v3, real = train.train$train.valid.3$Survived)
v3.predict.table
v3.predict.auccury <- sum(diag(v3.predict.table))/sum(v3.predict.table)
v3.predict.auccury
## ----
##  變數影響力
##  rf.fit$importance 


























