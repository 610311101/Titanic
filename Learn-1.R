## ----
##  Age遺失值過多
##  忽略Age變數
D <- D %>% select(-Age) %>% mutate(Survived = as.factor(Survived))
train         <- D %>% filter(!is.na(Survived))
train.train   <- 
train.valid.1 <- 
train.valid.2
train.valid.3
test  <- D %>% filter(!is.na(Survived))





