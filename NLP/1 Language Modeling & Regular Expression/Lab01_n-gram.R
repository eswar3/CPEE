rm(list=ls(all=TRUE))
setwd("C:/Users/Mahesh/Desktop/20160731_Batch17_CSE 7306c_TMIntro_Day01")

library(stringr) #for string manipulations
library(openNLP) #for natural language processing
library(tm) #textmining package
library(qdap) # quantitative analysis of text data
library(RWeka) #enable ngrams
library(ggplot2) #for plots

## ----location_of_txt_docs------------------------------------------------

cname <- file.path(getwd(), "txt")

## ----load_corpus---------------------------------------------------------
corpus <- Corpus(DirSource(cname))
rm(cname)

corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus, stripWhitespace) 
corpus <- tm_map(corpus, tolower) 
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Using the RWeka package for the 1-gram(single word) tokenization, 2-grams sets 
# and 3-grams sets for further exploratory analysis.
Onegram <- NGramTokenizer(corpus, Weka_control(min = 1, max = 1,delimiters = " \\r\\n\\t.,;:\"()?!"))
Bigram <- NGramTokenizer(corpus, Weka_control(min = 2, max = 2,delimiters = " \\r\\n\\t.,;:\"()?!"))
Trigram <- NGramTokenizer(corpus, Weka_control(min = 3, max = 3,delimiters = " \\r\\n\\t.,;:\"()?!"))

# Exploratory Analysis Results
# converting tokens of n-grams into tables

Tab_onegram <- data.frame(table(Onegram))
Tab_bigram <- data.frame(table(Bigram))
Tab_trigram <- data.frame(table(Trigram))

tail(Tab_bigram, n=6)

# sorting the word distribution frequency  
OnegramGrp <- Tab_onegram[order(Tab_onegram$Freq,decreasing = TRUE),]
BigramGrp <- Tab_bigram[order(Tab_bigram$Freq,decreasing = TRUE),]
TrigramGrp <- Tab_trigram[order(Tab_trigram$Freq,decreasing = TRUE),]

# Three individual samples. Top 35 words are selected.
OneSamp <- OnegramGrp[1:35,]
colnames(OneSamp) <- c("Word","Frequency")
BiSamp <- BigramGrp[1:35,]
colnames(BiSamp) <- c("Word","Frequency")
TriSamp <- TrigramGrp[1:35,]
colnames(TriSamp) <- c("Word","Frequency")

# Plot Example - Most Frequently 1-gram
ggplot(OneSamp, aes(x=Word,y=Frequency)) + geom_bar(stat="Identity", fill="Blue") +geom_text(aes(label=Frequency), vjust=-0.20) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Example - Most Frequently 2-grams
ggplot(BiSamp, aes(x=Word,y=Frequency)) + geom_bar(stat="Identity", fill="Blue") +geom_text(aes(label=Frequency), vjust=-0.20) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Example - Most Frequently 3-grams
ggplot(TriSamp, aes(x=Word,y=Frequency)) + geom_bar(stat="Identity", fill="Blue") +geom_text(aes(label=Frequency), vjust=-0.20) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ngram modeler
# https://books.google.com/ngrams


