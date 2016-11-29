#sets R's working directory to near where my files are
rm(list=ls(all=TRUE)) 
setwd('F:/AA_Clases/Textmining_Cognizant/20160813_CSE9098_Cognizant_Batch07_Text_Mining_Lab2')


# Filtering spam SMS using Naive Bayes

# read the sms data into the sms data frame
sms_raw <- read.csv("sms_spam.csv", 
                    stringsAsFactors = FALSE)

# examine the structure of the sms data
str(sms_raw)

# convert spam/ham to factor.
sms_raw$type <- factor(sms_raw$type)

# examine the type variable more carefully
str(sms_raw$type)

table(sms_raw$type)

# build a corpus using the text mining (tm) package
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))

# examine the sms corpus
print(sms_corpus)

inspect(sms_corpus[1:3])

# clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
inspect(corpus_clean[1:3])

corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
inspect(corpus_clean[1:3])

corpus_clean <- tm_map(corpus_clean, removePunctuation)
inspect(corpus_clean[1:3])


corpus_clean <- tm_map(corpus_clean, stripWhitespace)
inspect(corpus_clean[1:3])


# examine the clean corpus
inspect(sms_corpus[1:3])
inspect(corpus_clean[1:3])

# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)
# sms_dtm

# creating training and test datasets
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test <- sms_raw[4170:5559, ]
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]
sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5559]

# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))


# word cloud visualization
library(wordcloud)


wordcloud(sms_corpus_train, min.freq = 30)
wordcloud(sms_corpus_train, min.freq = 30, random.order = FALSE)
wordcloud(sms_corpus_train, min.freq = 50, random.order = FALSE)

# subset the training data into spam and ham groups
spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")
library("wordcloud")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5), 
          colors = brewer.pal(9, "Blues"))
	
# indicator features for frequent words
findFreqTerms(sms_dtm_train, 5)

sms_dict <- c(findFreqTerms(sms_dtm_train, 5))
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

# convert counts to a factor
convert_counts <- function(x) {
    x <- ifelse(x > 0, 1, 0)
    x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_test, MARGIN = 2, convert_counts)

summary(sms_train[, 1:5])

## Step 3: Training a model on the data ----
library(e1071)
library(C50)
smsClassifierC50 <- C5.0(sms_train, sms_raw_train$type)
library(randomForest)
sms_train <- as.data.frame(sms_train)
sms_train1 <- data.frame(apply(sms_train, 2, as.factor))
RfModel <- randomForest(sms_train1,sms_raw_train$type,ntree = 30)
RfModel$confusion
ImpAttr <- data.frame(RfModel$importance)
ImpRFAttr <- data.frame(terms = rownames(ImpAttr)
                        , MeanGini = ImpAttr$MeanDecreaseGini)
ImpRFAttr <- ImpRFAttr[order(ImpRFAttr$MeanGini,decreasing = TRUE),]
nrow(ImpRFAttr)
# Take first 500 important attributes and subset those in TDM
# Use DT, Logistic Regression or any classifier
# classify the data
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
names(sms_classifier)
summary(smsClassifierC50)
sms_classifier$tables[1:2]


## Step 4: Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)
sms_test_predC50 <- predict(smsClassifierC50, sms_test)
str(sms_test_pred)
conf.nb<-table(sms_raw_test$type, sms_test_pred)
conf.nbC50<-table(sms_raw_test$type, sms_test_predC50)


accuracy.nb <- sum(diag(conf.nb))/nrow(sms_test) * 100
sprintf("The accuracy of the model using naive bayes is %0.2f", accuracy.nb)
