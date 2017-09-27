##### Reformate data
library("slam");
library("tm");
library("dplyr");
##### --------------
rm(list = ls());
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


