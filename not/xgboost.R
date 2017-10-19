train.label <- Data %>% filter(!is.na(Survived)) %>% select(Survived)  
train.label <- unlist(train.label,use.names = F) %>% as.numeric() - 1
train.data  <- Data %>% filter(!is.na(Survived)) %>% select(-Survived) 
dummy       <- dummyVars(~.,train.data)
train.data <- predict(dummy,train.data)
train.data <- Matrix(as.matrix(train.data),sparse = T)

test.data  <- Data %>% filter(is.na(Survived)) %>% select(-Survived) 
dummy      <- dummyVars(~.,test.data)
test.data <- predict(dummy,test.data)
test.data <- Matrix(as.matrix(test.data),sparse = T)

dtrain <- xgb.DMatrix(train.data, label = train.label)
dtest  <- xgb.DMatrix(test.data)

# xgb_normalizedgini <- function(preds, dtrain){
#   actual <- getinfo(dtrain, "label")
#   score <- NormalizedGini(preds,actual)
#   return(list(metric = "NormalizedGini", value = score))
# }

## ----
##  gbtree 的參數設定
param <- list(
  booster = "gbtree",
  eta = 0.05,
  objective = "binary:logistic",
  gamma = 2,
  max_depth = 6,
  min_child_weight = 2.5,
  subsample = 0.8,
  colsample_bytree = 0.6
)
## ----
xgb_model <- xgb.train(data = dtrain,
                       params = param,
                       nrounds = 800,
                       verbose = 1,
                       watchlist = list(train=dtrain),
                       print_every_n = 25
)
## ----
## 分類 test
DesicionLine <- 0.6
preds <- 1*(predict(xgb_model,dtest)>DesicionLine) 
result <- fread("Titanic/data/submission.csv")
result$Survived <- preds
write.csv(result,"mysubmission1.csv", row.names = F)


## ----
##  變數重要性
importance_matrix <- xgb.importance(
  dimnames(dtrain)[[2]], 
  model=xgb_model
)
xgb.plot.importance(importance_matrix)
