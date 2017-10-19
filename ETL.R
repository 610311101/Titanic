setwd("C:/Users/Hou/Desktop")
rm(list = ls())
library(data.table)
library(tm)
library(dplyr)
library(caret)
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
Data$Embarked[Data$Embarked==""] <- "S";
Data$MissingAge <- ifelse(is.na(Data$Age),"Y","N");
Data$FamilySize <- Data$SibSp + Data$Parch + 1;
Data$EmptyCabin <- ifelse(Data$Cabin=="","Y","N");

## ----
##  Title
Name       <- strsplit(Data$Name,"[,.]")
Title      <- sapply(Name, function(x) gsub(" ","",x[2])) 
TitleType  <- names(table(Title))
Data$Title <- Title;

## ----
##  Ticket
Ticket <- gsub("[[:punct:] ]","",Data$Ticket)
TicketType    <- gsub("[0-9]","",Ticket)
Data$TicketType <- 
  ifelse(
    TicketType%in%c("FC","PC","WEP"),
    "1",
    ifelse(
      TicketType%in%c("CASOTON","FCC","PPP","SC","SCAH","SCAHBasle","SCOW","SCParis","SOC","SOP","SWPP"),
      "2",
      ifelse(
        TicketType%in%c("Fa","LINE","LP","PP","SOTONO","SOTONOQ","SP","STONO","STONOQ"),
        "3",
        "unknown"
      )
    )
  )

## ----
##  Cabin
Cabin <- gsub("[[:punct:] ]","",Data$Cabin)
CabinType    <- gsub("[0-9]","",Cabin)
Data$CabinType <- 
  ifelse(
    CabinType %in% c("A","B","BB","BBB","BBBB","C","CC","CCC","DD","E","T"),
    "1",
    "0"
  )

## ----
##  All in Data

## ----
##  Feature and select
feature <- c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","MissingAge","FamilySize","EmptyCabin","Title","TicketType","CabinType")
Data <- as.data.frame(Data)[,feature]
Data <- mutate(
  Data,
  Pclass = as.factor(Pclass),
  Sex    = as.factor(Sex),
  Embarked = as.factor(Embarked),
  Survived = as.factor(Survived),
  MissingAge = as.factor(MissingAge),
  EmptyCabin = as.factor(EmptyCabin),
  Title = as.factor(Title),
  TicketType = as.factor(TicketType),
  CabinType = as.factor(CabinType)
)
rm(list = ls()[!ls()%in%c("Data")])
str(Data, strict.width = "cut")
colnames(Data)

## ----
##  Handle missing
##  Dummy factor variable without target
dummy      <- dummyVars(~.,  data = select(Data,-Survived))
dummy.Data <- predict(dummy, select(Data,-Survived))
preprocess      <- preProcess(dummy.Data, method = "bagImpute")
bagImpute.Data  <- predict(preprocess, dummy.Data)
bagImpute.Data  <- as.data.frame(bagImpute.Data)
Data <- cbind("Survived" = Data$Survived,bagImpute.Data) 

str(Data, strict.width = "cut")
rm(list = ls()[ls()!="Data"])

