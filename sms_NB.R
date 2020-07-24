library(readr)
sms <- read.csv(file.choose())
View(sms)
str(sms)
table(sms$type)
# build a corpus using the text mining (tm) package
library(tm)
sms_Corpus <- VCorpus(VectorSource(sms$text))

# clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_Corpus, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_dtm

# creating training and test datasets
sms_raw_train <- sms[1:4169,]
sms_raw_test <- sms[4170:5559,]

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]

sms_Corpus_train <- corpus_clean[1:4169]
sms_Corpus_test  <- corpus_clean[4170:5559]

# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# Indicator features for frequent words
# Dictionary of words which are used more than 5 times
sms_dict <- findFreqTerms(sms_dtm_train, 5)
sms_dict

sms_train <- DocumentTermMatrix(sms_Corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_Corpus_test, list(dictionary = sms_dict))
sms_test

# convert counts to a factor
# custom function: if a word is used more than 0 times then mention 1 else mention 0
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}
convert_counts
# apply() convert_counts() to columns of train/test data
# Margin = 2 is for columns
# Margin = 1 is for rows
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)
sms_test
View(sms_test)
str(sms_test)
View(sms_train)

##  Training a model on the data ----
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

##  Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)
sms_test_pred
table(sms_test_pred)
prop.table(table(sms_test_pred))


library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
