setwd("C:/Users/Hou/Desktop");
library("caret");
rm(list = ls());
##
##  This code we will remove missing data, and show how to fitting
##  from xbgboost package.
##
##  Use Titanic demo code.
full_train       <- read.csv("Titanic/train.csv");
no_missing_train <- na.omit(full_train);
##
##  Sample 75% index from no_missing_train.
no_missing_train.index <- createDataPartition(no_missing_train$Survived, p = .75, list = FALSE)
##
##  train.train and train.test for no missing.
no_missing_train.train <- no_missing_train[ no_missing_train.index,];
no_missing_train.test  <- no_missing_train[-no_missing_train.index,];
##
##  
summary(no_missing_train.train)









