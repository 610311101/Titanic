## ----
##  Age跟甚麼有關？
ggplot(
  D %>% na.omit,
  aes(
    x = Age,
    color = as.factor(Survived)
  )
) + geom_density()




























d <- na.omit(D)
ggplot(
  d,
  aes(
    x = Age, 
    color = as.factor(Survived)
  )
) + geom_density()  ##  小孩存活率高
## ----
##  小孩有哪些特徵？
k <- 16
d.Agelowk <- d %>% filter(Age<k) 
table(
  Survived = d.Agelowk$Survived,
  Pclass = d.Agelowk$Pclass
)  ##  Age<16對於Pclass是3的影響不大
D %>% filter(Age<k, Pclass!="3", is.na(Survived))








D.AgeisNA <- filter(D,is.na(Age))  ##  挑出Age是NA的263個資料
table(Pclass = D.AgeisNA$Pclass)  ##  208個Pclass是3
D.AgeisNA.Pclassnot3 <- filter(D.AgeisNA,Pclass != 3)  ##  剩下55個資料
table(Embarked = D.AgeisNA.Pclassnot3$Embarked)  ##  17:2:36來自C:Q:S
## ----
d <- na.omit(D)  ##  除去NA
ggplot(
  d,
  aes(
    x = Age, 
    color = as.factor(Pclass)
  )
) + geom_density()  ##  Pclass是1的Age補mean

ggplot(
  d %>% filter(Pclass == 3, Sex == "female"),
  aes(
    x = Age, 
    color = as.factor(Survived)
  )
) + geom_density()

ggplot(
  d %>% filter(Pclass == 3, Sex == "female"),
  aes(
    x = Age, 
    color = as.factor(Survived)
  )
) + geom_density()


















