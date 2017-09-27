##### Random Forest
library("randomForest");
library("ggplot2");
source("Reformate.R");
##### ----
model.train <- train %>% select(-PassengerId,-Name,-Cabin);
model.fit1  <- randomForest(Survived~., model.train)
varImpPlot(model.fit1, main="Important variables for train");
model.fit2 <- randomForest(Survived~mr+Fare+Sex+Age, model.train)
varImpPlot(model.fit2, main="Important variables for train");

