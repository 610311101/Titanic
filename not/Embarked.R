## ----
##  Embarke有兩個NA
D %>% 
  filter(
    Pclass == 1,
    Sex == "female", 
    Survived == 1,
    Ticket.1 == 1, 
    Cabin.b == 1,
    Fare < 90 & Fare > 70
  ) %>%
  select(
    Embarked,
    Pclass,
    Sex,
    SibSp,
    Parch,
    Fare,
    Survived,
    Ticket.1,
    Cabin.b
  )
#   Embarked Pclass    Sex SibSp Parch Fare Survived Ticket.1 Cabin.b
# 1     <NA>      1 female     0     0 80.0        1        1       1
# 2        S      1 female     0     0 86.5        1        1       1
# 3        S      1 female     0     0 86.5        1        1       1
# 4        S      1 female     0     0 86.5        1        1       1
# 5     <NA>      1 female     0     0 80.0        1        1       1
# => 猜S

