library(xgboost);
library(Matrix);
library("dplyr");
selectData <- readRDS("selectData.RDS");
## ----
##  Set train and check
index_0 <- selectData %>% select(index, train, Survived) %>% 
  filter(train==T, Survived==0) %>% select(index) %>% unlist(use.names = F);
index_1 <- selectData %>% select(index, train, Survived) %>% 
  filter(train==T, Survived==1) %>% select(index) %>% unlist(use.names = F);
check_index <- c(
  sample(index_0, 50),
  sample(index_1, 50)
)
check <- selectData[ check_index,];
train <- selectData[-check_index,] %>% filter(train==T);
test  <- selectData %>% filter(train==F);
splitData <- list("train" = train, "check" = check, "test" = test)
saveRDS(splitData, "splitData.RDS")



