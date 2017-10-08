## ----
##  Age
## ----
D[,"Age"][is.na(D[,"Age"])] <- median(na.omit(D[,"Age"]))  ##  補中位數





































D <- D %>%  
  mutate(
    Age = 
      if_else(
        Age<17,"young",if_else(
          Age>=17&&Age<40,"strong","old"
        )
      )
  )


D$Age %>% summary



ggplot(
  D %>% na.omit,
  aes(
    x = Age, color = as.factor(Pclass)
  )
) + geom_density()






D %>% 
  filter(is.na(Age),!is.na(Survived)) %>%  ##  遺失Age的train
  select(Age,Survived,Sex,Fare,Pclass,Embarked,mr,mrs,miss) %>%  ##  可能相關變數
  arrange(Sex,Pclass)


D.female <- 
  D %>% 
  filter(is.na(Age),!is.na(Survived),Sex=="female",Pclass=="3") %>%
  select(Survived,Embarked,Pclass,Fare,Parch,Cabin) 
  
D.female$Survived %>% table




D %>% 
  filter(is.na(Age),!is.na(Survived),Sex=="female") %>%  ##  遺失Age的train
  ggplot(
    aes(x = as.factor(Survived), fill = Sex)
  ) + geom_bar()




D  %>% filter(Embarked=="Q") %>% select(Age)



ggplot(
  D  ,
  aes(
    x = Age,
    color = as.factor(Embarked)
  )
) + geom_density()



d <- na.omit(D)
ggplot(
  d,
  aes(
    x = Age
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


















