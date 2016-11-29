
rm(list=(ls(all=TRUE)))
setwd("H:/Case_Study")

# Install the below packages
# install.packages("base64enc")
# install.packages("openNLPmodels.en",
#                  repos = "http://datacube.wu.ac.at/",
#                  type = "source")
# 

# install.packages("openNLPmodels.en")

library(tm)
library(rJava)
library(NLP)
library(openNLP)
library(openNLPmodels.en)
library(qdap)

SentimentData <- read.csv("Sentiment.csv",header = T)
SentimentData <- SentimentData[which(SentimentData$Sentiment=="P" | SentimentData$Sentiment== "N"),]
Labels <- SentimentData$Sentiment
Text <- SentimentData$Katakana.text.Translated
SentimentData$Katakana.text.Translated <- gsub('[[:punct:]]', " ", SentimentData$Katakana.text.Translated)

# converting into corpus
docs <- Corpus(DataframeSource(data.frame(SentimentData$Katakana.text.Translated)))

# Spliting the text based on words
strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))

# Pre processing the text and creating the document term matrix
dt_matrix <- DocumentTermMatrix(docs,
                                control = list(tokenize = strsplit_space_tokenizer,
                                               wordLengths = c(6, Inf),
                                               removeNumbers = TRUE,
                                               removePunctuation = TRUE,
                                               stopwords = TRUE, 
                                               stemming = TRUE,
                                               tolower = TRUE ))
# Checking the document term matrix
inspect(dt_matrix[1:6,])

library(SnowballC)
library(tm)
library(RWeka)
library(RWekajars)
library(wordcloud)
library(rJava)

dt_matrix <- as.matrix(dt_matrix)
v <- sort(colSums(dt_matrix), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v) 
wordcloud(d$word, d$freq, min.freq=3)         

# creating the corpus
data_for_scoring <- data.frame(SentimentData$Katakana.text.Translated)
colnames(data_for_scoring)="text"

library(plyr)
library(stringr)
# Reading the dictionary of postive and Negative words

pos.words=scan('H:\\Case_Study\\words-dictionary\\positive-words.txt',what='character',comment.char=';')
neg.words=scan('H:\\Case_Study\\words-dictionary\\negative-words.txt',what='character',comment.char=';')

# function to calculate the sentiment score
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    # ignoring the puctuations
    sentence = gsub('[[:punct:]]', '', sentence)
    # In ASCII, these characters having octal codes 000 through 037, and 177 (DEL) will be ignored.
    sentence = gsub('[[:cntrl:]]', '', sentence)
    # ignoring the digits
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores)
  return(scores.df)
}

SentimentVector <- sapply(1:nrow(data_for_scoring), function(x) score.sentiment(data_for_scoring[x,],pos.words,neg.words, .progress='text'))
SentimentVector <- as.matrix(SentimentVector)
SentimentVector <- as.data.frame(SentimentVector)

data_for_scoring <- data.frame(data_for_scoring,SentimentVector)
colnames(data_for_scoring) <- c("tweets","Score")

# Segregating Positive negative and neutral sentences
PositiveSentimentDtFrame <- data_for_scoring[which(data_for_scoring$Score>0),]
NegativeSentimentDtFrame <- data_for_scoring[which(data_for_scoring$Score<0),]
NeutralSentimentDtFrame <- data_for_scoring[which(data_for_scoring$Score==0),]


PositiveSentCorp <- Corpus(DataframeSource(data.frame(PositiveSentimentDtFrame$tweets)))

# Spliting the text based on words
strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))

# Pre processing the text and creating the document term matrix
dt_matrixPositive <- DocumentTermMatrix(PositiveSentCorp,
                                control = list(tokenize = strsplit_space_tokenizer,
                                               wordLengths = c(6, Inf),
                                               removeNumbers = TRUE,
                                               removePunctuation = TRUE,
                                               stopwords = TRUE, 
                                               tolower = TRUE ))

# Checking the document term matrix
inspect(dt_matrixPositive[1:6,])

dt_matrixPositive <- as.matrix(dt_matrixPositive)
# write.csv(dt_matrixPositive,"myMat3.csv")
v <- sort(colSums(dt_matrixPositive), decreasing=TRUE)
#v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
dPositive <- data.frame(word=myNames, freq=v) 
wordcloud(dPositive$word, dPositive$freq, min.freq=1)


##################################Negative
NegativeSentCorp <- Corpus(DataframeSource(data.frame(NegativeSentimentDtFrame$tweets)))

# Spliting the text based on words
strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))

# Pre processing the text and creating the document term matrix
dt_matrixNegative <- DocumentTermMatrix(NegativeSentCorp,
                                        control = list(tokenize = strsplit_space_tokenizer,
                                                       wordLengths = c(6, Inf),
                                                       removeNumbers = TRUE,
                                                       removePunctuation = TRUE,
                                                       stopwords = TRUE, 
                                                       tolower = TRUE ))

# Checking the document term matrix
inspect(dt_matrixNegative[1:6,])

dt_matrixNegative <- as.matrix(dt_matrixNegative)
#write.csv(dt_matrixNegative,"myMat3.csv")
v <- sort(colSums(dt_matrixNegative), decreasing=TRUE)
#v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
dNegative <- data.frame(word=myNames, freq=v) 
wordcloud(dNegative$word, dNegative$freq, min.freq=2)

# Calculating the sentiment scores
scores = score.sentiment(docs, pos.words,neg.words, .progress='text')
table(scores$score)
SentimentLabelOutcome <- ifelse(scores>0,"P","N")
Labels <- as.character(Labels)
length(Labels)
ConfMatDictionaryApproach <- table(Labels,SentimentLabelOutcome)

library(caret)
confusionMatrix(ConfMatDictionaryApproach)
# Accuracy : 0.7388          
# Sensitivity : 0.7456          
# Specificity : 0.7000          
# Pos Pred Value : 0.9341          
# Neg Pred Value : 0.3256          
