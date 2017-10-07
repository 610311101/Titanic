## ----
##  Age跟甚麼有關？
d <- na.omit(D);
ggplot(
  d,
  aes(
    x = Age, 
    color = as.factor(Survived)
  )
) + geom_density()  ##  年齡<10存活機會增加
ggplot(
  d %>% filter(Pclass == 3, Sex == "female"),
  aes(
    x = Age, 
    color = as.factor(Survived)
  )
) + geom_density()




















