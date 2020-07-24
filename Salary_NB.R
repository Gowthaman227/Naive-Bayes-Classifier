## Loading Training and Testing dataset
Sal_train <- read.csv(file.choose())
View(Sal_train)
Sal_train <- Sal_train[,c(ncol(Sal_train),1:(ncol(Sal_train)-1))]
View(Sal_train)
Sal_test <- read.csv(file.choose())
View(Sal_test)
Sal_test <- Sal_test[,c(ncol(Sal_test),1:(ncol(Sal_test)-1))]
View(Sal_test)
table(Sal_train$Salary)
table(Sal_test$Salary)
## Building a NaiveBayes Model
library(e1071)
Sal_Model <- naiveBayes(Sal_train$Salary~.,data=Sal_train)
Sal_Model
## Prediction for train dataset
Model_train_pred <- predict(Sal_Model,Sal_train)
table(Model_train_pred)
mean(Model_train_pred==Sal_train$Salary)
## Cross Tabluation ##
library(gmodels)
CrossTable(Model_train_pred,Sal_train$Salary, 
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
## Prediction for test dataset
Model_test_pred <- predict(Sal_Model,Sal_test)
table(Model_test_pred)
mean(Model_test_pred==Sal_test$Salary)
## Cross Tabluation ##
CrossTable(Model_test_pred,Sal_test$Salary, 
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
