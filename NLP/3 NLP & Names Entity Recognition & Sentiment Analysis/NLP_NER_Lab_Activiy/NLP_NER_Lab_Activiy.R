# Install the below packages
# install.packages("twitteR")
# install.packages("base64enc")
# install.packages("openNLPmodels.en",
#                  repos = "http://datacube.wu.ac.at/",
#                  type = "source")

# Steps
# 1. Load the required packages.
# 2. Complete the twitteR handshake
# 3. Once connected, search for tweets using a keyword, also provide the count of tweets
# 4. The tweets are collected as a list of documents.
# 5. Read the files of positive and negative words in R
# 6. Twitter data has many other attributes like date, user id, retweet,etc. To know more
# convert the tweets to a dataframe. You can now see the data populated in about 16 cols.
# 7. For analysis, we will only consider text col and retrieve it separately and ignore any duplicate tweets.
# 8. Now, we write a function to clean the data. like remove numbers, punctuations, stop-words etc.
# 9. Iteratively, read records (text) and using the string operations, match the words in the text
# with those of positive and negative words list
# 10. Suppose 10 positive words match in the text and 6 negative words match then 
# the score for that record = 10-6 

rm(list=(ls(all=TRUE)))
setwd("F:\\TA\\Labs\\NLP-Day_4\\28022016_CSE7306c_Lab04")

library(twitteR)
library(tm)
library(rJava)
.jinit(parameters = "-Xmx8g")
library(NLP)
library(openNLP)
library(openNLPmodels.en)
library(qdap) # provides parsing tools that enable analysis and visualization of text

# This function wraps the OAuth authentication handshake to twitteR session 
setup_twitter_oauth(consumer_key = "oMztDDL8ZiWjbaTEKs1R0PAMv",
                    consumer_secret = "Uu2jyGHPBm1qrdd0JM0Rj7SNApg7uL9zGsyIdIMYfawUS9utYF", 
                    access_token = "1340075053-MfECVSoYbS1KlzNIMGjHe8uSfKecjyRMY4mIDYT",
                    access_secret = "BcZwZpVaggE0P2AkA6Mqe3GOP8uZs5Qp5fV49CeTQdznN")

# Search and retrieve tweets from multiple twitter pages for keyword = Modi
sentiment_data <- searchTwitter("#modi", lang="en",n=100)
# Combining into a single data frame (list to data frame)
tweets.df <- do.call("rbind", lapply(sentiment_data, as.data.frame))
# Getting the unique tweets
tweets <- tweets.df[duplicated(tweets.df) == FALSE,]
# For text processing, create the corpus of tweets
data_for_scoring <- data.frame(tweets$text)
colnames(data_for_scoring)="text"
docs1 <- Corpus(DataframeSource(data.frame(data_for_scoring)))

library(plyr)
library(stringr)
library(stringi)
# Reading the dictionary of postive and Negative words
pos.words=scan('F:\\TA\\Labs\\NLP-Day_4\\28022016_CSE7306c_Lab04\\positive-words.txt',what='character',comment.char=';')
neg.words=scan('F:\\TA\\Labs\\NLP-Day_4\\28022016_CSE7306c_Lab04\\negative-words.txt',what='character',comment.char=';')

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

# Calculating the sentiment scores
scores = score.sentiment(docs1, pos.words,neg.words, .progress='text')
table(scores$score)