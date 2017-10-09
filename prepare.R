## ----
train         <- D %>% filter(!is.na(Survived))  ##  全部訓練資料 => 549個0以及342個1
train.train   <- 
  list(
    train.train   = c(),
    train.valid.1 = c(),
    train.valid.2 = c(),
    train.valid.3 = c()
)
## ----
##  建立Survived對稱資料
##  k個交叉驗證資料
k <- 3
sample.size.total <- floor(min(table(train$Survived))/(k+1))*(k+1)
sample.size.each  <- sample.size.total/(k+1)
for( i in 1:length(levels(train[,"Survived"])) ){
  i.train <- train %>% 
    filter(
      Survived == levels(train[,"Survived"])[i]  ##  Survived是i的資料
    ) %>% 
    sample_n(size = sample.size.total)  ##  隨機產生排列
  for( j in 1:(k+1) ){
    train.train[[j]] <- rbind(
      i.train[1:sample.size.each,],
      train.train[[j]]
    )
    i.train <- i.train[-c(1:sample.size.each),] 
  }
}
rm( list = ls()[ls()!="train.train"] )





























