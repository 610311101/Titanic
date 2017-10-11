## ----
##  package
library('dplyr')
library('data.table')
library('stringr')
library("knitr")
library('tibble')
library("ggplot2")
library("tm")
library("slam")
source("function.R")
## ----
Fulltrain <- D %>% filter(!is.na(Survived))  ##  全部訓練資料 => 549個0以及342個1
test      <- D %>% filter(is.na(Survived))
## ----
##  建立Survived對稱資料
sample.size.each  <- min(table(Fulltrain$Survived))
train <- c()
for( i in 1:length(levels(Fulltrain[,"Survived"])) ){
  i.train <- Fulltrain %>% 
    filter(
      Survived == levels(Fulltrain[,"Survived"])[i]  ##  Survived是i的資料
    ) %>% 
    sample_n(size = sample.size.each) ##  隨機產生排列
  train <- rbind(
    train,
    i.train
  )
}
rm( list = setdiff(ls(),c("train","test")) )
## ----
library("randomForest")
## ----
##  fit
rf.fit <- 
  randomForest(
    Survived~.,
    train %>% select(-PassengerId),
    maxnodes = 8,
    ntree = 1200,
    classwt = c(0.7,0.3)
  )
## ----
##  test
test.predict <- predict(
  rf.fit,
  test %>% select(-PassengerId)
)
submission <- 
  cbind(
    test %>% select(PassengerId),
    Survived = test.predict
)
write.csv(submission,"mysubmission.csv",row.names = F)
## ----
##  做多次取平均
Fulltrain <- D %>% filter(!is.na(Survived))  ##  全部訓練資料 => 549個0以及342個1
test      <- D %>% filter(is.na(Survived))
submission <- list()
test.predict.matrix <- c()
K <- 100  ##  100次
for( k in seq(K) ){
  sample.size.each  <- min(table(Fulltrain$Survived))
  train <- c()
  for( i in 1:length(levels(Fulltrain[,"Survived"])) ){
    i.train <- Fulltrain %>% 
      filter(
        Survived == levels(Fulltrain[,"Survived"])[i]  ##  Survived是i的資料
      ) %>% 
      sample_n(size = sample.size.each) ##  隨機產生排列
    train <- rbind(
      train,
      i.train
    )
  }
  rf.fit <- 
    randomForest(
      Survived~.,
      train %>% select(-PassengerId),
      ntree = 1200,
      maxnodes = 8,
      classwt = c(0.7,0.3)
    )
  test.predict <- predict(
    rf.fit,
    test %>% select(-PassengerId)
  )
  test.predict <- as.numeric(test.predict) - 1
  test.predict.matrix <- cbind(test.predict.matrix,test.predict)

}
submission <-
  cbind(
    test %>% select(PassengerId),
    Survived = (rowMeans(test.predict.matrix)>0.99)*1
  )
table(submission$Survived)
write.csv(submission,"mysubmission.csv",row.names = F)
## ----
library("e1071")
Fulltrain <- D %>% filter(!is.na(Survived))  ##  全部訓練資料 => 549個0以及342個1
test      <- D %>% filter(is.na(Survived))
## ----
##  建立Survived對稱資料
sample.size.each  <- min(table(Fulltrain$Survived))
train <- c()
for( i in 1:length(levels(Fulltrain[,"Survived"])) ){
  i.train <- Fulltrain %>% 
    filter(
      Survived == levels(Fulltrain[,"Survived"])[i]  ##  Survived是i的資料
    ) %>% 
    sample_n(size = sample.size.each) ##  隨機產生排列
  train <- rbind(
    train,
    i.train
  )
}
rm( list = setdiff(ls(),c("train","test")) )
## ----
##  fit
svm.fit <- 
  svm(
    Survived~.,
    train %>% select(-PassengerId),
    kernel ="radial"
  )
## ----
##  test
test.predict <- predict(
  svm.fit,
  test %>% select(-PassengerId,-Survived)
)
submission <- 
  cbind(
    test %>% select(PassengerId),
    Survived = test.predict
  )
table(submission$Survived) 
write.csv(submission,"mysubmission.csv",row.names = F)





















