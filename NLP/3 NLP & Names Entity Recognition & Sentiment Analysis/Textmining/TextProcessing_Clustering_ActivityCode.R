rm(list=ls(all=T))
##20160821- TM-Clustering activity and using cosine dissimilarity
# setwd("F:/AA_Clases/Textmining_Cognizant/Day2")
setwd("")
## Loading the required libraries
library(tm)
library(wordcloud)
library(RWeka)
library(RWekajars)
library(rJava)
library(RColorBrewer)
library(SnowballC)
library(twitteR)
library(proxy)

###Reading the docs
docs<-Corpus(DirSource("Text Docs_Clustering"), readerControl = list(reader=readPlain))

##Performing text preprocessing steps
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs , stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stemDocument, language = "english")

##Constructing Document-Term matrix
dt_matrix <-DocumentTermMatrix(docs) 

##Removing sparse terms
dt_matrix <- removeSparseTerms(dt_matrix, 0.75)

##Computing similarity
A<-dist(as.matrix(dt_matrix), method = "cosine")

### Clustering the documents using k-means 
unigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
dtm1 <- DocumentTermMatrix(docs, control = list(tokenize = unigramTokenizer))
dtm1 <- removeSparseTerms(dtm1, 0.75)
m_bigram <- as.data.frame(as.matrix(dtm1))
m <- as.matrix(dtm1)
rownames(m)<-1:nrow(m)
norm_eucl <- function(m) {
  t(apply(m, 1, function(x){x/sqrt(sum(x**2))}))
}
m_norm1 <- norm_eucl(m)
m_norm <- norm_eucl(m)
set.seed(20160507)
cl <- kmeans(m_norm,3)
table(cl$cluster)
