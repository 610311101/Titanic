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
D[D[,"Cabin"]=="","Cabin"] <- "unknown"  ##  ""補上unknown
d <- na.omit(D)  ##  除去D中NA
glimpse(d)
## ----
Name.tdm <- ##  Name文本矩陣
  Corpus(VectorSource(d[,"Name"])) %>% ##  Name建立書冊
  TermDocumentMatrix(
    control = list(
      wordLengths = c(1,Inf)  ##  單字長度範圍
    )
  )
Name <- as.data.frame(as.matrix(t(Name.tdm)))  ##  定義成資料文本矩陣
Ticket.tdm <- ##  Ticket文本矩陣
  Corpus(VectorSource(d[,"Ticket"])) %>% ##  Ticket建立書冊
  TermDocumentMatrix(
    control = list(
      wordLengths = c(1,Inf)  ##  單字長度範圍
    )
  )
Ticket <- as.data.frame(as.matrix(t(Ticket.tdm)))  ##  定義成資料文本矩陣
Cabin.tdm <- ##  Cabin文本矩陣
  Corpus(VectorSource(d[,"Cabin"])) %>% ##  Cabin建立書冊
  TermDocumentMatrix(
    control = list(
      wordLengths = c(1,Inf)  ##  單字長度範圍
    )
  )
Cabin <- as.data.frame(as.matrix(t(Cabin.tdm)))  ##  定義成資料文本矩陣
## ----
##  類別變數編碼：
Pclass <- as.data.frame(model.matrix(~as.factor(d[,"Pclass"])-1))
colnames(Pclass) <- c("P1","P2","p3")
Sex <- data.frame("Sex" = as.numeric(d[,"Sex"])-1)  ##  1:male
Embarked <- as.data.frame(model.matrix(~as.factor(d[,"Embarked"])-1))
colnames(Embarked) <- c("C","Q","S")
## ----
##  合併
d.dummy <- cbind(
  d %>% select(Survived,Age,SibSp,Parch,Fare),
  Pclass,Sex,Name,Embarked,Ticket,Cabin
)
## ----
##  相關係數
d.dummy.cor <- cor(d.dummy)
tail(sort(abs(d.dummy.cor["Survived",])),20)  ##  與Survived高相關的
tail(sort(abs(d.dummy.cor["Age",])),20)  ##  與Age高相關的
tail(sort(abs(d.dummy.cor["unknown",])),20)  ##  與unknown高相關的









