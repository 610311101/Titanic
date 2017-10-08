## ----
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
k <- 19
d.Agelowk <- d %>% filter(Age<k)
table(Survived = d.Agelowk$Survived)
table(d.Agelowk$Survived,d.Agelowk$Embarked)

