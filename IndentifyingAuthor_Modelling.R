
rm(list=ls(all=TRUE)) 

# setting the working directory
setwd("C:/Users/Admin/Desktop/20160910_Batch17_CSE7306c_Lab06_FullDay/Piazza")

# read the author data into the author data frame
authorData <- read.csv("AuthorsDataset.csv", stringsAsFactors = FALSE)

# examine the structure of the author data
str(authorData)

# convert author to factor.
authorData$Author <- factor(authorData$Author)

# examine the Author variable more carefully
str(authorData$Author)

table(authorData$Author)

# build a corpus using the text mining (tm) package
library(tm)

# clean up the corpus using tm_map()
author_corpus <- Corpus(DataframeSource(data.frame(authorData$Text)))
# examine the author corpus
print(author_corpus)




author_corpus <- tm_map(author_corpus, removeNumbers)
author_corpus <- tm_map(author_corpus, removePunctuation)
author_corpus <- tm_map(author_corpus, stripWhitespace)
author_corpus <- tm_map(author_corpus, content_transformer(tolower))
author_corpus <- tm_map(author_corpus, removeWords, stopwords("english")) 
author_corpus <- tm_map(author_corpus,stemDocument, language ="english")
author_corpus <- tm_map(author_corpus, PlainTextDocument) 
##########################################################################
# this is the change in latest tm package release
# if you have used just tolower instead content_transformer(tolower) then again
# need to use PlainTextDocument also.
# author_corpus <- tm_map(author_corpus, tolower)  
# author_corpus = tm_map(author_corpus, PlainTextDocument)
############################################################################
# arriving at the training rows 
set.seed(1234)
trainrowsC = sample(1:250,200,replace=F)
trainrowsH = sample(251:500,200,replace=F)
trainrowsS = sample(501:750,200,replace=F)
Alltrainrows = c(trainrowsC,trainrowsH,trainrowsS)

# creating training and test datasets
author_raw_train <- authorData[Alltrainrows, ]
author_raw_test <- authorData[-Alltrainrows, ]
author_corpus_train <- author_corpus[Alltrainrows]
author_corpus_test <- author_corpus[-Alltrainrows]

# check that the proportion of spam is similar
prop.table(table(author_raw_train$Author))
prop.table(table(author_raw_test$Author))


##########################################################################
# term frequency
author_dtm <-DocumentTermMatrix(author_corpus,control=list(wordLengths=c(1,Inf), minDocFreq=1))
author_dtm1=author_dtm
author_dtm <-data.frame(inspect(author_dtm))

author_dtm1_tr = author_dtm1[Alltrainrows,] #dtm 
author_dtm1_te = author_dtm1[-Alltrainrows,] #dtm 


# Visualization process


library(tm)
library(wordcloud)
library(RColorBrewer)


#Visualization - creating the matrix with the column sums 
author_dtm_beforesparse = data.frame(inspect(author_dtm1))
freq <- colSums(as.matrix(author_dtm_beforesparse))   
length(freq)  

author_dtm_beforesparse_sum_df <- data.frame(word = names(author_dtm_beforesparse),freq=freq)
# author_dtm_beforesparse_sum_df      #8817 words with total freq 62710 and 
# #almost upto 2865 words are with the frequency more than 3
length(which(author_dtm_beforesparse_sum_df$freq>3) ==TRUE) #2865

# plotting using wordcloud  with min frequency 30 

set.seed(6)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(author_dtm_beforesparse_sum_df$word,author_dtm_beforesparse_sum_df$freq, scale=c(5,.1),min.freq=30,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)



################################## Authorwise visualization ######################
# subset the training data into spam and ham groups
Charles <- subset(author_raw_train, Author == "Charles")
HGWells <- subset(author_raw_train, Author == "HGWells")
Stevenson <- subset(author_raw_train, Author == "Stevenson")

library("wordcloud")
wordcloud(Charles$Text, max.words = 70, scale = c(3, 0.5),random.order = FALSE,
          colors = brewer.pal(9, "Reds"))
wordcloud(HGWells$Text, max.words = 70, scale = c(3, 0.5), random.order = FALSE,
          colors = brewer.pal(9, "Greens"))
wordcloud(Stevenson$Text, max.words = 70, scale = c(3, 0.5), random.order = FALSE,
          colors = brewer.pal(9, "Oranges"))
#################################Visualization ####################
# word cloud visualization
library(wordcloud)

wordcloud(author_corpus_train, min.freq = 30)
wordcloud(author_corpus_train, min.freq = 30, random.order = FALSE)  #decreasing frequency
wordcloud(author_corpus_train, min.freq = 50, random.order = FALSE) #decreasing frequency
###############################################################################

#remove sparse terms from a document-term matrix 
#Checking different levels of sparseremoval
#  Start by removing sparse terms:   Example
# removeSparseTerms(dtm, sparse = 0.1) # This makes a matrix that is 10% empty space, maximum.   
# inspect(dtms)

author_dtm1_75 <- removeSparseTerms(author_dtm1, 0.75)
author_dtm1_75_df <- data.frame(inspect(author_dtm1_75))  # 8 words (hand joe littl look time)
author_dtm1_85 <- removeSparseTerms(author_dtm1, 0.85) 
author_dtm1_85_df <- data.frame(inspect(author_dtm1_85))  # 42 words (day eye hand head joe littl look miss round time)
author_dtm1_90 <- removeSparseTerms(author_dtm1, 0.90)  
author_dtm1_90_df <- data.frame(inspect(author_dtm1_90))  # 88 words (appear black boy cours day estella even eye fire found hair hand havisham head home)
author_dtm1_95 <- removeSparseTerms(author_dtm1, 0.95)   
author_dtm1_95_df <- data.frame(inspect(author_dtm1_95))   # 266 words
author_dtm1_1 <- removeSparseTerms(author_dtm1, 0.5)   
author_dtm1_1_df <- data.frame(inspect(author_dtm1_1)) # zero words
# Implementing the removal of 90% sparseterms (90% sparsity is allowed >90 are removed)

author_dtm1_90<- removeSparseTerms(author_dtm1, 0.90) 

# Converting the documenttermmatrixof "author_dtm1_90" into data frame

author_dtm1_90_df <- data.frame(inspect(author_dtm1_90))

# Adding the class label column 
Author = authorData$Author
author_dtm1_90_df<-cbind(author_dtm1_90_df,Author)



############################### Data Dictionary ##############################
# indicator features for frequent words
findFreqTerms(author_dtm1_90, 5)
# Dictionary creation
author_dict <- c(findFreqTerms(author_dtm1_tr, 5))


################################## Building Model ####################

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}




author_dtm1_90_all = apply(author_dtm1_90, MARGIN = 2, convert_counts)

author_train = author_dtm1_90_all[Alltrainrows,]
author_test = author_dtm1_90_all[-Alltrainrows,]
author_train = data.frame(author_train) 
author_test = data.frame(author_test)

###################################################
require(C50)
## check for unilevel attrs in trainDataset
xx=data.frame(apply(author_train,2,function(x){length(unique(x))}))
unilevelattrs = which(xx[,1]==1)



dtc50=C5.0(author_train, author_raw_train$Author)
## Step 4: Evaluating model performance ----
author_test_pred <- predict(dtc50, author_test)
str(author_test_pred)
conf.nb<-table(author_raw_test$Author, author_test_pred)
conf.nb
accuracy.nb <- sum(diag(conf.nb))/nrow(author_test) * 100
sprintf("The accuracy of the model using C50 is %0.2f", accuracy.nb)


## Step 3: Training a model on the data ----  NaiveBayes model
library(e1071)
author_classifier <- naiveBayes(author_train, author_raw_train$Author)
names(author_classifier)


## Step 4: Evaluating model performance ----
author_test_pred <- predict(author_classifier, author_test)
str(author_test_pred)
conf.nb<-table(author_raw_test$Author, author_test_pred)
conf.nb

accuracy.nb <- sum(diag(conf.nb))/nrow(author_test) * 100
sprintf("The accuracy of the model using naive bayes is %0.2f", accuracy.nb)


#######################################   TFIDF  ############################
# indicator features for frequent words
findFreqTerms(author_dtm1, 30)
# Dictionary creation
author_dict1 <- c(findFreqTerms(author_dtm1, 30))

# Min frequency 30 words are chosen and TFIDF is considered
# # TFIDF
author_tfidf <-DocumentTermMatrix(author_corpus,control=list(weighting=weightTfIdf, dictionary = author_dict1))
author_tfidf <-data.frame(inspect(author_tfidf))

author_tfidf_tr = author_tfidf[Alltrainrows,]
author_tfidf_te = author_tfidf[-Alltrainrows,]
## Step 3: Training a model on the data ----
library(e1071)
author_classifier1 <- naiveBayes(author_tfidf_tr, author_raw_train$Author)
names(author_classifier1)


## Step 4: Evaluating model performance ----
author_test_pred1 <- predict(author_classifier1, author_tfidf_te)
str(author_test_pred1)
conf.nb1<-table(author_raw_test$Author, author_test_pred1)
conf.nb1

accuracy.nb1 <- sum(diag(conf.nb1))/nrow(author_tfidf_te) * 100
sprintf("The accuracy of the model using naive bayes is %0.2f", accuracy.nb1)

###################################################################
#######################################   TFIDF  and then binary ############################
# indicator features for frequent words
findFreqTerms(author_dtm1, 5)
# Dictionary creation
author_dict1 <- c(findFreqTerms(author_dtm1, 5))

# Min frequency 5 words are chosen and TFIDF is considered
# # TFIDF
author_tfidf <-DocumentTermMatrix(author_corpus,control=list(weighting=weightTfIdf, dictionary = author_dict1))
author_tfidf <-data.frame(inspect(author_tfidf))


author_tfidf_tr = author_tfidf[Alltrainrows,]
author_tfidf_te = author_tfidf[-Alltrainrows,]

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
author_tfidf_tr <- apply(author_tfidf_tr, MARGIN = 2, convert_counts)
author_tfidf_te <- apply(author_tfidf_te, MARGIN = 2, convert_counts)

## Step 3: Training a model on the data ----
library(e1071)
author_classifier1 <- naiveBayes(author_tfidf_tr, author_raw_train$Author)
names(author_classifier1)

## Step 4: Evaluating model performance ----
author_test_pred1 <- predict(author_classifier1, author_tfidf_te)
str(author_test_pred1)
conf.nb1<-table(author_raw_test$Author, author_test_pred1)
conf.nb1

accuracy.nb1 <- sum(diag(conf.nb1))/nrow(author_tfidf_te) * 100
sprintf("The accuracy of the model using naive bayes is %0.2f", accuracy.nb1)

