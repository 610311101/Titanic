library("randomForest")
source("function.R")
## ----
##  用randomForest評估整體train
rf.fit <- 
  randomForest(
    Survived~.,
    D %>% select(-PassengerId) %>% filter(!is.na(Survived)),
    ntree = 800
  )
rf.fit
rf.fit$importance
importance <- c(rf.fit$importance)
names(importance) <- rownames(rf.fit$importance)
importance <- sort(importance,decreasing = T)
variable.important <- names(head(importance,30))  ##  挑選影響最多的30個變數
D <- D %>% select(
  PassengerId,
  Survived,
  variable.important
)
## ----
D <- 
  D %>% 
  mutate(
    Ticket.a = Sex*Ticket.a,
    Ticket.p = Sex*Ticket.p,
    Ticket.s = Sex*Ticket.s,
    Ticket.1 = Sex*Ticket.1,
    Ticket.2 = Sex*Ticket.2,
    Ticket.3 = Sex*Ticket.3,
    Ticket.c = Sex*Ticket.c
  )

## ----
##  最後整體總評估一次
rf.fit <-  ##  整體train評估
  randomForest(
    Survived~.,
    D %>% select(-PassengerId) %>% filter(!is.na(Survived)),
    ntree = 800
  )
rf.fit
rf.fit$importance
importance <- c(rf.fit$importance)
names(importance) <- rownames(rf.fit$importance)
importance <- sort(importance,decreasing = T)
variable.important <- names(importance[importance>3])  ##  挑選影響大的變數
## ----
D <- D %>% select(
  PassengerId,
  Survived,
  variable.important
)
colnames(D)














## ----
##  Ticket.a 變數交配
D.Ticket.a <- D
d <- variable.dot(
  data = D.Ticket.a,
  main = "Ticket.a",
  ignore = c("PassengerId","Survived") 
)
rf.fit <-  ##  整體train評估
  randomForest(
    Survived~.,
    d %>% select(-PassengerId) %>% filter(!is.na(Survived)),
    ntree = 800
  )
rf.fit
rf.fit$importance
importance <- c(rf.fit$importance)
names(importance) <- rownames(rf.fit$importance)
importance <- sort(importance,decreasing = T)
variable.important <- names(importance[importance>10])  ##  挑選影響大的變數
D.Ticket.a <- d %>% select(
  Ticket.a.V3,
  Ticket.a.V1,
  Ticket.a.V2,
  Ticket.a.V4
)
##  合併
D <- cbind(
  D %>% select(-Ticket.a),
  D.Ticket.a
) 