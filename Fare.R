## ----
##  Fare有一個遺失值
D.Fare.subset <- 
  D %>%
  filter(
    Pclass=="3",
    Sex=="male",
    Embarked=="S",
    Age < 70 & Age > 50 
  ) %>%
  select(
    Fare,
    Pclass,
    Sex,
    Embarked,
    Age
  )
#     Fare Pclass  Sex Embarked  Age
# 1 7.2500      3 male        S 59.0
# 2 8.0500      3 male        S 55.5
# 3 8.0500      3 male        S 51.0
# 4 6.2375      3 male        S 61.0
# 5 7.7500      3 male        S 51.0
# 6 7.0542      3 male        S 51.0
# 7     NA      3 male        S 60.5
mean(na.omit(D.Fare.subset[,"Fare"]))
# => 猜平均






























