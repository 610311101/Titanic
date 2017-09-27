##### Random forest
library("slam");
library("tm");
library("dplyr");
##### --------------
rm(list = ls());
# train
train <- fread("train.csv");
train <-
  train %>%
  # Remove missing
  na.omit %>%
  mutate(
    Survived = as.factor(Survived),
    Pclass = as.factor(Pclass),
    Sex = as.factor(Sex),
    Embarked = as.factor(Embarked),
    Age = as.numeric(Age),
    SibSp = as.numeric(SibSp),
    Parch = as.numeric(Parch),
    Fare = as.numeric(Fare),
    Ticket = substr(Ticket, 1, 1) %>% as.factor()
  ) %>%
  as.data.table;
# Reformate Name
reformateName <- train %>% 
  select(Name) %>%
  DataframeSource %>%
  Corpus %>%
  tm_map(content_transformer(removeNumbers)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(removePunctuation)) %>%
  TermDocumentMatrix(control=list(wordLengths = c(1, Inf)))
# High words freq term
row_sums(reformateName) %>% sort %>% tail(20);
# Finally
train <- cbind(
  train,
  reformateName[
    c("mr","miss","mrs","william","master","john","henry"),
    ] %>% as.matrix() %>% t %>% as.data.table
);
# Clear other
rm(reformateName)

# test
test <- fread("test.csv");
test <-
  test %>%
  # Remove missing
  na.omit %>%
  mutate(
    #Survived = as.factor(Survived),
    Pclass = as.factor(Pclass),
    Sex = as.factor(Sex),
    Embarked = as.factor(Embarked),
    Age = as.numeric(Age),
    SibSp = as.numeric(SibSp),
    Parch = as.numeric(Parch),
    Fare = as.numeric(Fare),
    Ticket = substr(Ticket, 1, 1) %>% as.factor()
  ) %>%
  as.data.table;
# Reformate Name
reformateName <- test %>% 
  select(Name) %>%
  DataframeSource %>%
  Corpus %>%
  tm_map(content_transformer(removeNumbers)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(removePunctuation)) %>%
  TermDocumentMatrix(control=list(wordLengths = c(1, Inf)))
# High words freq term
row_sums(reformateName) %>% sort %>% tail(20);
# Finally
test <- cbind(
  test,
  reformateName[
    c("mr","miss","mrs","william","master","john","henry"),
    ] %>% as.matrix() %>% t %>% as.data.table
);
# Clear other
rm(reformateName)
##### --------------
# Model
model.train <- train %>% select(PassengerId, Survived, mr, Fare, Sex, Age, william, SibSp, mr, miss, mrs);
model.fit <- randomForest(Survived~mr+Fare+Sex+Age, model.train, 
                          ntree = 800, maxnodes = 150);
model.fit
model.test <- test %>% select(PassengerId, mr, Fare, Sex, Age, william, SibSp, mr, miss, mrs);
predict   <- predict(model.fit, model.test[,-1]);
predict.data.frame <- as.data.frame(predict); 
predict.data.frame$PassengerId <- model.test$PassengerId;
##### --------------
# Make submission
submission <- read.csv("submission.csv");
submission$Survived <- 0;
submission <- merge(submission, predict.data.frame, all = T)
submission[is.na(submission)] <- 0;
mysubmission <- submission %>% select(PassengerId, Survived);
write.csv(mysubmission, "mysubmission.csv", row.names = F);



