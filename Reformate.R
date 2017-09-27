##### Reformate data
library("slam");
library("tm");
library("dplyr");
library("data.table");
##### --------------
rm(list = ls());
Full <- merge(read.csv("train.csv"),read.csv("test.csv"), all = T); data.table(Full);
# PassengerId V
# Pclass V
Full <- mutate(Full, Pclass = as.factor(Pclass));
# Name V
Full <- Full %>% mutate(
  Name = if_else(grepl("Mr", Name), "Mr",
                 if_else(grepl("Mrs", Name), "Mrs",
                         if_else(grepl("Miss", Name), "Miss","other")))
);
Full <- mutate(Full,Name = as.factor(Name))
# Sex V
# Family V
Full <- Full %>% mutate(family = SibSp + Parch);
# SibSp V
# Parch V
# Ticket V
Full <- Full %>% mutate(Ticket = as.factor(substr(Full$Ticket,1,1)));
# Fare V
# Cabin V
Full <- Full %>% mutate(Cabin = gsub("[0-9 ]","",Cabin));
Full <- Full %>% mutate(Cabin = if_else(nchar(Cabin)==1,Cabin,
                                        if_else(nchar(Cabin)>1,"more","unknown")));
Full <- Full %>% mutate(Cabin = as.factor(Cabin));
# Embarked V
Full$Embarked <- as.character(Full$Embarked);
Full$Embarked[Full$Embarked==""] <- NA;
Full$Embarked <- as.factor(Full$Embarked);
# Survived V
Full <- Full %>% mutate(Survived = as.factor(Survived))
data.table(Full)



