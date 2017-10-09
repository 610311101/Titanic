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
    maxnodes = 10,
    ntree = 800,
    classwt = c(0.45,0.55)
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






































