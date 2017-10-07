## ----
library(ggfortify);
## ----
d <- na.omit(D)  ##  除去NA
## ----
##  字串變數處理 => 文本矩陣
Name.tdm <- Corpus(VectorSource(d[,"Name"])) %>% ##  Name建立書冊
  TermDocumentMatrix(
    control = list(
      wordLengths = c(1,Inf)  ##  單字長度範圍
    )
  )
Name     <- as.data.frame(as.matrix(t(Name.tdm)))  ##  定義成資料文本矩陣
Ticket.tdm <- Corpus(VectorSource(d[,"Ticket"])) %>% ##  Ticket建立書冊
  TermDocumentMatrix(
    control = list(
      wordLengths = c(1,Inf)  ##  單字長度範圍
    )
  )
Ticket <- as.data.frame(as.matrix(t(Ticket.tdm)))  ##  定義成資料文本矩陣
Cabin.tdm <- Corpus(VectorSource(d[,"Cabin"])) %>% ##  Cabin建立書冊
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
Survived <- data.frame(Survived = d[,"Survived"])
## ----
##  合併
D.dummy <- cbind(
  d %>% select(Age,SibSp,Parch,Fare),
  Pclass,Name,Sex,Ticket,Cabin,Embarked,Survived
)
















