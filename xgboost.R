library(xgboost);
library(Matrix);
library("dplyr");
Titanic <- readRDS("Titanic.RDS");
## ----
##  Dummy variable: Sex and Embarked.
Sex      <- model.matrix(~Titanic$Data$Sex-1) %>% data.frame();
colnames(Sex) <- c("female","male");
Embarked <- model.matrix(~Titanic$Data$Embarked-1) %>% data.frame()
colnames(Embarked) <- c("C","Q","S");
feature <- data.frame(Sex,Embarked);
Titanic$Data <- cbind(
  Titanic$Data %>% select(PassengerId, Pclass,Age,SibSp, Parch, Fare , train),
  feature
);
## ----
##  Set train, label and test.
train <- Titanic$Data %>% filter(train==T) %>%
  select(-c(train,PassengerId)) %>% as.matrix;
test  <- Titanic$Data %>% filter(train==F) %>%
  select(-c(train,PassengerId)) %>% as.matrix();
label <- Titanic$Label$Survived;
## ----
##  Balance
names(label) <- 1:length(label);
index_1 <- names(label[label==1]) %>% as.numeric();
rand_1  <- sample( index_1,sum(label==0) - length(index_1) )
train <- rbind(
  train[rand_1, ],
  train
)
label <- c( label,label[rand_1] )

## ----
##  xgboost learning
fit <- xgboost(data = train, label = label, max_depth = 5,
                 eta = 0.3, nthread = 2, nrounds = 200,objective = "binary:logistic");
pred <- predict(fit, test);
Survived <- 1*(pred > 0.5);
submission <- read.csv("gender_submission.csv")
submission$Survived <- Survived;
write.csv(submission,"Mysubmission.csv",row.names = F)
## ----
##  variable importance
importance <- xgb.importance(feature_names = colnames(train), model = fit)
