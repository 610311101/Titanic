setwd("C:/Users/Hou/Desktop")
rm(list = ls())
library(data.table)
library(dplyr)
library(caret)
library(xgboost)
library(doSNOW)
library(Matrix)
library(MLmetrics)
## ----
##  Load train + test
train <- fread("Titanic/data/train.csv",stringsAsFactors = F);
test <- fread("Titanic/data/test.csv",stringsAsFactors = F);
Data <- merge(train, test, all=T)
rm(list = c("train","test"))
View(Data)

## ----
##  Clean
Data <- 
  mutate(
    Data,
    Embarked = if_else(Embarked!="",Embarked,"S"),
    MissingAge = if_else(is.na(Age),"Y","N"),
    FamilySize = SibSp + Parch + 1,
    Name = tolower(Name),
    Miss = if_else(grepl("miss",Name),1,0),
    Mr   = if_else(grepl("mr",Name),1,0),
    Mrs  = if_else(grepl("mrs",Name),1,0),
    NoTitle = if_else(Miss+Mr+Mrs==0,1,0),
    MissingAge = as.factor(MissingAge),
    MissingCabin = if_else(Cabin=="","Y","N"),
    Ticket      = substr(Ticket,1,1),
    Ticket = gsub("[^123]","4",Ticket)
    CabinA = if_else(grepl("A",Cabin),1,0),
    CabinB = if_else(grepl("B",Cabin),1,0),
    CabinC = if_else(grepl("C",Cabin),1,0),
    CabinD = if_else(grepl("D",Cabin),1,0),
    CabinE = if_else(grepl("E",Cabin),1,0),
    CabinF = if_else(grepl("F",Cabin),1,0),
    CabinG = if_else(grepl("G",Cabin),1,0),
    CabinT = if_else(grepl("T",Cabin),1,0),
    CabinCounts = nchar(gsub("[0-9 ]","",Cabin))
  )

Data$Ticket
grepl("/",Data$Ticket) %>% table  # 144 
grepl("PC",Data$Ticket) %>% table  # 92 
grepl("SOTON",Data$Ticket) %>% table  # 92 
grepl("STON",Data$Ticket) %>% table  # 92


grepl("34",Data$Ticket) %>% table  # 92

gsub("[^A-Z]","",Data$Ticket) %>% table


Data$Ticket <- gsub("[.]",'',Data$Ticket)

Pclass = as.factor(Pclass),
Sex = as.factor(Sex),
Embarked = as.factor(Embarked),
Survived = as.factor(Survived),
Data$Miss <- as.factor(Data$Miss)
Data$Mr   <- as.factor(Data$Mr)
Data$Mr   <- as.factor(Data$Mr)
Data$Mrs   <- as.factor(Data$Mrs)
Data$NoTitle   <- as.factor(Data$NoTitle)
Data$MissingCabin   <- as.factor(Data$MissingCabin)

## ----
##  Feature select
feature <- c("Survived","Pclass","Sex","Age","Fare","Embarked","MissingAge","FamilySize","Miss","Mr","Mrs","NoTitle","MissingCabin")
Data    <- Data[, feature]

## ----
##  Dummy factor variable without target
dummy      <- dummyVars(~., data = Data[,-1])
dummy.Data <- predict(dummy,Data[,-1])

## ----
##  Handle missing of age
preprocess      <- preProcess(dummy.Data, method = "bagImpute")
bagImpute.Data  <- predict(preprocess, dummy.Data)
Data$Age  <- bagImpute.Data[,6]
Data$Fare <- bagImpute.Data[,7]
## ----
##  Split data
train <- filter(Data,!is.na(Survived))
test  <- filter(Data, is.na(Survived))
indexes <- createDataPartition(train$Survived,
                               times = 1,
                               p = 0.5,
                               list = F)
train.train <- train[indexes,]
train.test <- train[-indexes,]

## ----
##  Train control

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              search = "grid")
tune.gird <- expand.grid(
  eta = c(0.05),
  nrounds = c(100),
  max_depth = 6,
  min_child_weight = c(2.5),
  colsample_bytree = c(0.6),
  gamma = c(2),
  subsample = 1)
## ----
##  Train
# cl <- makeCluster(3,type = "SOCK")
# registerDoSNOW(cl)
caret.cv <- caret::train(Survived~.,data = train.train, method = "xgbTree", tuneGrid = tune.gird, trControl = train.control)
# stopCluster(cl)
preds <- predict(caret.cv
                 ,train.test)
confusionMatrix(preds, train.test$Survived)

## ----
##  variable important
gbmImp <- varImp(caret.cv, scale = FALSE)
plot(gbmImp, top = 10)

## ----
##  Make submission
result <- fread("Titanic/data/submission.csv")
test.preds <- predict(caret.cv,test)
result$Survived <- test.preds
write.csv(result,"mysubmission.csv", row.names = F)



View(tune.gird)
prop.table(table(train$Survived))
prop.table(table(train.train$Survived))
prop.table(table(train.test$Survived))

View(Data)
View(bagImpute.Data)
View(dummy.Data)
colnames(Data)

dummy.Data[,Age]




