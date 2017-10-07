## ----
##  package
library('dplyr')
library('data.table')
library('stringr')
library("knitr")
library('tibble')
library("ggplot2")
library("tm")
library("slam")
## ----
##  資料概況
setwd("C:/Users/Hou/Desktop/Titanic/data");
rm(list = ls())
D <- merge(
  read.csv("train.csv"), ##  train data
  read.csv("test.csv"), ##  test data
  all = T
)
##  12個變數：
##  "PassengerId" "Pclass" "Name" "Sex" "Age" "SibSp"
##  "Parch" "Ticket" "Fare" "Cabin" "Embarked" "Survived"
sum(is.na(D[,"Age"]))  ##  Age有263個missing
filter(D, !is.na(Survived), is.na(Age)) %>% nrow  ##  train有177個
filter(D,  is.na(Survived), is.na(Age)) %>% nrow  ##  test有86個
sum(is.na(D[,"Fare"]))  ##  Fare有1個missing
filter(D, is.na(Survived), is.na(Fare)) %>% nrow  ##  test有1個
table(D[,"Embarked"])  ##  Embarked有2個""
D[D[,"Embarked"]=="",][,"Survived"]  ##  其中train有2個""
D[D[,"Cabin"]=="",] %>% nrow  ##  Cabin有1014個""
table(D[,"Survived"])  ##  0:549 1:342
table(D[,"Survived"])[1]/sum(table(D[,"Survived"]))  ##  全猜0的正確率是0.61
## ----
##  清洗變數
D[,"Embarked"] <- as.character(D[,"Embarked"])  ##  Embarked轉換成字串
D[D[,"Embarked"] == "","Embarked"] <- NA  ##  Embarked有2個"" => 補NA 
D <- mutate(D,Embarked = as.factor(Embarked))  ##  重新定義factor
D[,"Ticket"]  <- as.character(D[,"Ticket"])  ##  Ticket轉換成字串
D[,"Ticket"]  <- gsub("[[:punct:] ]","",D[,"Ticket"])  ##  除去多餘的符號
D[,"Name"] <- as.character(D[,"Name"])  ##  Name轉換成字串
D[,"Name"]  <- gsub("[[:punct:]]","",D[,"Name"])  ##  除去多餘的符號
D[,"Name"]  <- gsub("(\\w)","\\L\\1",D[,"Name"],perl=TRUE)  ##  改成小寫英文
D[,"Cabin"]  <- as.character(D[,"Cabin"])  ##  Cabin轉換成字串
D[,"Cabin"]  <- gsub(" ","", D[,"Cabin"])  ##  除去空白
D[D[,"Cabin"]=="","Cabin"] <- "Unknown"  ##  ""補上Unknown
glimpse(D)

































