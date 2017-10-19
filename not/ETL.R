setwd("C:/Users/Hou/Desktop/Titanic")
rm(list = ls())
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
##  載入資料
setwd("C:/Users/Hou/Desktop/Titanic/data");
source("function.R")
D <- merge(
  read.csv("train.csv"), ##  train data
  read.csv("test.csv"), ##  test data
  all = T
)
##  12個變數：
##  "PassengerId" "Pclass" "Name" "Sex" "Age" "SibSp"
##  "Parch" "Ticket" "Fare" "Cabin" "Embarked" "Survived"
colSums(is.na(D))  ##  每個變數遺失值的數量
colSums(D == "")   ##  變數是空的數量
filter(D, is.na(Age)) %>% nrow                    ##  Age有263個missing => 數量多不可亂補
filter(D, !is.na(Survived), is.na(Age)) %>% nrow  ##  train有177個
filter(D,  is.na(Survived), is.na(Age)) %>% nrow  ##  test有86個
sum(is.na(D[,"Fare"]))                            ##  Fare只有1個missing
filter(D, is.na(Survived), is.na(Fare)) %>% nrow  ##  在test有1個
table(D[,"Embarked"])                ##  Embarked有2個""
D[D[,"Embarked"]=="",][,"Survived"]  ##  其中train有2個""
D[D[,"Cabin"]=="",] %>% nrow  ##  Cabin有1014個""
table(D[,"Survived"])                                ##  0:549 1:342
table(D[,"Survived"])[1]/sum(table(D[,"Survived"]))  ##  0的比例0.61
## ----
##  補少量的空值和遺失值
filter(D, is.na(Fare))                                            ##  Fare遺失值的資料
similar <- filter(D, Age<63&Age>58, Pclass == 3)                  ##  挑年紀差不多而且階級一樣的人
D[,"Fare"][is.na(D[,"Fare"])] <- mean(na.omit(similar[,"Fare"]))  ##  補mean
D[,"Embarked"] <- as.character(D[,"Embarked"])  ##  Embarked轉換成字串
filter(D, Embarked=="")  ##  Embarked是空值

similar <- filter(D, Fare<81&Fare>79, Sex == "female")  ##  挑Fare差不多而且都是女性
similar <- similar %>% filter(Survived==1)              ##  挑選都是活著
D[D[,"Embarked"] == "","Embarked"] <- "S"               ##  補多數的S
D <- mutate(D,Embarked = as.factor(Embarked))           ##  重新定義factor
## ----
##  Name
## -------暫停
## -------以下保留

D[,"Ticket"]  <- as.character(D[,"Ticket"])  ##  Ticket轉換成字串
D[,"Ticket"]  <- gsub("[[:punct:] ]","",D[,"Ticket"])  ##  除去多餘的符號
D[,"Name"] <- as.character(D[,"Name"])  ##  Name轉換成字串
D[,"Name"]  <- gsub("[[:punct:]]","",D[,"Name"])  ##  除去多餘的符號
D[,"Name"]  <- gsub("(\\w)","\\L\\1",D[,"Name"],perl=TRUE)  ##  改成小寫英文
D[,"Cabin"]  <- as.character(D[,"Cabin"])  ##  Cabin轉換成字串
D[,"Cabin"]  <- gsub(" ","", D[,"Cabin"])  ##  除去空白
D[D[,"Cabin"]=="","Cabin"] <- NA  ##  ""補上NA
## ----
##  增加字串變數
Name   <- select_Name(data = D, frequency = 40)
Ticket           <- select_Ticket(data = D, stop = 1, frequency = 40)
colnames(Ticket) <- paste0("Ticket.",colnames(Ticket))
Cabin           <- select_Cabin(data = D, stop = 1, frequency = 40)
colnames(Cabin) <- paste0("Cabin.",colnames(Cabin))
D <- cbind(
  D,
  Name,
  Ticket,
  Cabin
)
## ----
##  類別變數編碼
Pclass           <- as.data.frame(model.matrix(~as.factor(D[,"Pclass"])-1))
colnames(Pclass) <- paste0("Pclass.",c("1","2","3"))
Embarked           <- as.data.frame(model.matrix(~D[,"Embarked"]-1))
colnames(Embarked) <- paste0("Embarked.",c("C","Q","S"))
D <- mutate(D,Sex = as.numeric(D[,"Sex"])-1)  ##  male代表1
## ----
##  增加類別編碼變數
D <- cbind(
  D,
  Pclass,
  Embarked
)
## ----
##  連續變數補遺失值
D[,"Age"][is.na(D[,"Age"])] <- median(na.omit(D[,"Age"]))  ##  Age補中位數
## ----
##  除去舊的變數
D <- D %>% select(-Name,-Ticket,-Cabin,-Pclass,-Embarked)
## ----
D <- mutate(D, Survived = as.factor(Survived))  ##  Survived宣告類別變數
D <- 
  cbind(
    D %>% select(Survived,PassengerId),
    D %>% select(-Survived,-PassengerId) %>% scale()
  )
rm(list = ls()[ls()!="D"])






















