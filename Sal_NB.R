library(readr)
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
## Bulid a corpus Funtion
library(tm)
## Considered
Corpus_Sal_train <- VCorpus(VectorSource(Sal_train$occupation))
View(Corpus_Sal_train)
Corpus_Sal_test <- VCorpus(VectorSource(Sal_test$occupation))
# Clean up the corpus using tm_map()
CC_train <- tm_map(Corpus_Sal_train,content_transformer(tolower))
CC_test <- tm_map(Corpus_Sal_test,content_transformer(tolower))
CC_train <- tm_map(CC_train,removeNumbers)
CC_test <- tm_map(CC_test,removeNumbers)
CC_train <- tm_map(CC_train,removeWords,stopwords())
CC_test <- tm_map(CC_test,removeWords,stopwords())
CC_train <- tm_map(CC_train,removePunctuation)
CC_test <- tm_map(CC_test,removePunctuation)
CC_train <- tm_map(CC_train,stripWhitespace)
CC_test <- tm_map(CC_test,stripWhitespace)
## Creating DocumentTerm Matrix
Sal_train_dtm <- DocumentTermMatrix(CC_train)
dim(Sal_train_dtm)
Sal_train_dtm
Sal_test_dtm <- DocumentTermMatrix(CC_test)
Sal_test_dtm
# Indicator features for frequent words
# Dictionary of words which are used more than 5 times
Sal_train_dic <- findFreqTerms(Sal_train_dtm,5)
Sal_train_dic
Sal_test_dic <- findFreqTerms(Sal_test_dtm,5)
Sal_test_dic

Salary_train <- DocumentTermMatrix(CC_train,list(dictionary=Sal_train_dic))
Salary_train
Salary_test <- DocumentTermMatrix(CC_test,list(dictionary=Sal_test_dic))
Salary_test

## Convert counts to factors
convert_counts <- function(x){
  x <- ifelse(x>0,1,0)
  x <- factor(x,levels =c(0,1),labels="No","Yes")
   
}
convert_counts


Salary_train <- apply(Salary_train,MARGIN = 2,convert_counts)
Salary_test <- apply(Salary_test,MARGIN=2,convert_counts)
dim(Salary_train)
dim(Sal_train)
##  Training a model on the data ----
library(e1071)
Salary_Classifier <- naiveBayes(Salary_train,Sal_train$Salary)
Salary_Classifier <- naiveBayes(Salary_test,Sal_test$Salary)

Sal_pred <- predict(Salary_Classifier,Sal_train$Salary)
Sal_pred
table(Sal_pred)
prop.table(table(Sal_pred))

## Cross Tabluation ##
library(gmodels)
CrossTable(Sal_pred, Sal_train$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
