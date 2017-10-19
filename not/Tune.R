setwd("C:/Users/Hou/Desktop")
rm(list = ls())
library(data.table)
library(tm)
library(dplyr)
library(caret)
library(xgboost)
library(doSNOW)
library(Matrix)
library(MLmetrics)
library(slam)
## ----
##  Load train + test
train <- fread("Titanic/data/train.csv",stringsAsFactors = F);
test <- fread("Titanic/data/test.csv",stringsAsFactors = F);
Data <- merge(train, test, all=T)
rm(list = c("train","test"))

## ----
##  Clean
Data <- 
  mutate(
    Data,
    Embarked   = if_else(Embarked!="",Embarked,"S"),
    MissingAge = if_else(is.na(Age),"Y","N"),
    FamilySize = SibSp + Parch + 1,
    MissingCabin = if_else(Cabin=="","Y","N")
  )

## ----
##  Name
Data <- 
  mutate(
    Data,
    Name    = tolower(Name),
    Miss    = if_else(grepl("miss",Name),1,0),
    Mr      = if_else(grepl("mr",Name),1,0),
    Mrs     = if_else(grepl("mrs",Name),1,0),
    NoTitle = if_else(Miss+Mr+Mrs==0,1,0)
  )

## ----
##  Ticket
Ticket <- gsub("[[:punct:] ]","",Data$Ticket)
TicketType    <- gsub("[0-9]","",Ticket)
indexes       <- names(table(TicketType[TicketType!=""]))
Type <- lapply(indexes, function(x) 1*(TicketType %in% x))
Type <- data.frame(do.call("cbind",Type));  colnames(Type) <- indexes;
TicketNumber  <- substr(gsub("[A-Za-z]","",Ticket),1,2)
indexes       <- names(table(TicketNumber[TicketNumber!=""]))
Number <- lapply(indexes, function(x) 1*(TicketNumber %in% x))
Number <- data.frame(do.call("cbind",Number));  colnames(Number) <- indexes;
Ticket <- cbind(Type,Number)
PCA <- prcomp(Ticket)
#InformationLine <- 0.81
#Ticket <- PCA$x[,cumsum(PCA$sdev)/sum(PCA$sdev)< InformationLine]
Ticket <- PCA$x[,c(1,2)]
colnames(Ticket) <- paste0("Ticket",colnames(Ticket))
rm(list = ls()[!ls()%in%c("Ticket","Data")])

## ----
##  Cabin
Cabin <- gsub("[[:punct:] ]","",Data$Cabin)
CabinType    <- gsub("[0-9]","",Cabin)
indexes <- c("A","B","C","D","E","F","G","T")
Type <- lapply(indexes, function(x) 1*(CabinType %in% x))
Type <- data.frame(do.call("cbind",Type));  colnames(Type) <- indexes;
CabinCount <- nchar(CabinType)
Cabin <- data.frame(Type,"CabinCount" = CabinCount) 
PCA <- prcomp(Cabin)
# InformationLine <- 0.81
# Cabin <- PCA$x[,cumsum(PCA$sdev)/sum(PCA$sdev)< InformationLine]
Cabin <- PCA$x[,c(1:2)]
colnames(Cabin) <- paste0("Cabin",colnames(Cabin))

Data <- cbind(Data,Ticket,Cabin)
rm(list = ls()[!ls()%in%c("Data")])

## ----
##  Feature
Data <- 
  mutate(
    Data,
    Pclass = as.factor(Pclass),
    Sex    = as.factor(Sex),
    Embarked = as.factor(Embarked),
    Survived = as.factor(Survived),
    MissingAge = as.factor(MissingAge),
    MissingCabin = as.factor(MissingCabin),
    Miss = as.factor(Miss),
    Mr = as.factor(Mr),
    Mrs = as.factor(Mrs),
    NoTitle = as.factor(NoTitle)
  )
feature <- colnames(Data)[!colnames(Data) %in% c("PassengerId","Name","Ticket","Cabin")]
Data <- Data[,feature]
str(Data)

## ----
##  Dummy factor variable without target
dummy      <- dummyVars(~., data = Data[,-8])
dummy.Data <- predict(dummy,Data[,-8])

##  Handle missing of age
preprocess      <- preProcess(dummy.Data, method = "bagImpute")
bagImpute.Data  <- predict(preprocess, dummy.Data)
Data$Age  <- bagImpute.Data[,3]
Data$Fare <- bagImpute.Data[,6]
rm(list = ls()[ls()!="Data"])

##  Split data
train <- filter(Data,!is.na(Survived))
test  <- filter(Data, is.na(Survived))
indexes <- createDataPartition(train$Survived,
                               times = 1,
                               p = 0.5,
                               list = F)
train.train <- train[indexes,]
train.test <- train[-indexes,]

##  Train control
train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              search = "grid")
tune.gird <- expand.grid(
  eta = c(0.1,0.3,0.5),
  nrounds = c(75,100,150),
  max_depth = 6:8,
  min_child_weight = c(1.0,2.0,2.5),
  colsample_bytree = c(0.6,0.8),
  gamma = c(0:5),
  subsample = c(0.6))


##  Train
cl <- makeCluster(3,type = "SOCK")
registerDoSNOW(cl)
caret.cv <- caret::train(Survived~.,data = train.train, method = "xgbTree", tuneGrid = tune.gird, trControl = train.control)
caret.cv$bestTune
stopCluster(cl)
preds <- predict(caret.cv, train.test)
confusionMatrix(preds, train.test$Survived)

## ----
##  variable important
gbmImp <- varImp(caret.cv, scale = FALSE)
plot(gbmImp)













## ----
##  Make submission
result <- fread("Titanic/data/submission.csv")
test.preds <- predict(caret.cv,test)
result$Survived <- test.preds
write.csv(result,"mysubmission.csv", row.names = F)




