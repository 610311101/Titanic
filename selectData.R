library("dplyr");
rm(list = ls());
##  Data: train + test.
Data <- readRDS("Data.RDS");
## ----
##  1. Missing in each variable
Data %>% select(-Survived) %>% is.na() %>% colSums() %>% barplot();
## ----
par(mfrow = c(1,2))
##  2. Handle Age missing.
Age.mean <- Data$Age[!is.na(Data$Age)] %>% mean;
Data$Age[is.na(Data$Age)] <- Age.mean;
##  3. Handle Fare missing.
Fare.median <- Data$Fare[!is.na(Data$Fare)] %>% median;
Data$Fare[is.na(Data$Fare)] <- Fare.median;
##  Check no missing.
is.na(Data$Age) %>% sum;
is.na(Data$Fare) %>% sum;
## ----
##  4. Not use Name.
##  5. Not use Ticket.
##  6. Not use Cabin.
##  7. Embarked "S" replace nothing.
Embarked <- Data$Embarked
Embarked[ Embarked == "" ] <- "S"
Embarked <- Embarked %>% as.character() %>% as.factor()
Data$Embarked <- Embarked;
## ----
##  8. Select variable
selectData <- Data %>% select(index, train, Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked)
## ----
##  9. Dummy Sex.
Sex <- selectData$Sex;
Sex <- model.matrix(~Sex-1) %>% as.data.frame();
colnames(Sex) <- c("female","male");
selectData <- selectData %>% select(-Sex);
selectData <- data.frame(selectData,Sex);
## 10. Dummy Embarked.
Embarked <- selectData$Embarked;
Embarked <- model.matrix(~Embarked-1) %>% as.data.frame();
colnames(Embarked) <- c("C","Q","S");
selectData <- selectData %>% select(-Embarked);
selectData <- data.frame(selectData,Embarked);
saveRDS(selectData,"selectData.RDS");

