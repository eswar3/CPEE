rm(list=ls(all=T))
## ----load_packages-------------------------------------------------------
library(tm)              # Framework for text mining.
library(dplyr)           # Data wrangling, pipe operator %>%().
library(magrittr)       # provides forward piping of commands         
library(ggplot2)
library(RColorBrewer)    # Generate palette of colours for plots.
library(wordcloud)


## location of text documents
cname <- file.path("C:/Users/Mahesh/Desktop/Activity20160731/20160731_Batch17_CSE 7306c_TMIntro_Day01", "txt")
cname
dir("C:/Users/Mahesh/Desktop/Activity20160731/20160731_Batch17_CSE 7306c_TMIntro_Day01")

#or

setwd("C:/Users/Mahesh/Desktop/Activity20160731/20160731_Batch17_CSE 7306c_TMIntro_Day01")
dir()

##loading the corpus
# Corpus, representing a collection of text documents. 
#A corpus is an abstract concept, and there can exist several implementations in parallel. 
#The default implementation is the so-called VCorpus (short for Volatile Corpus). corpora are R objects held fully in memory. We denote this as volatile since once the R object is destroyed, the whole corpus is gone. Such a volatile corpus can be created via the constructor VCorpus(x, readerControl).

docs <- Corpus(DirSource(cname))   
class(docs)
class(docs[[1]])
summary(docs)
inspect(docs)
docs[[1]]$content


# A character representation of a document is available via as.character()
writeLines(as.character(docs[[1]]))


## ------------------------------------------------------------------------
viewDocs <- function(d, n) {d %>% extract2(n) %>% as.character() %>% writeLines()}
viewDocs(docs, 16)

## ------------------------------------------------------------------------
getTransformations()

#[1] "removeNumbers"     "removePunctuation" "removeWords"      
#[4] "stemDocument"      "stripWhitespace" 

## ----transform_slash-----------------------------------------------------
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

## ------------------------------------------------------------------------
## docs <- tm_map(docs, toSpace, "/|@|\\|")
rm(toSpace)
viewDocs(docs, 16)

## ------------------------------------------------------------------------
#tolower(c("I LIKE INSOFE"))
#tolower(docs[[1]]$content)

docs <- tm_map(docs, content_transformer(tolower))
viewDocs(docs, 16)

## ------------------------------------------------------------------------
docs <- tm_map(docs, removeNumbers)

viewDocs(docs, 16)

## ------------------------------------------------------------------------
docs <- tm_map(docs, removePunctuation)

viewDocs(docs, 16)

## ------------------------------------------------------------------------
docs <- tm_map(docs, removeWords, stopwords("english"))

viewDocs(docs, 16)

## ----list_stopwords------------------------------------------------------
length(stopwords("english"))
stopwords("english")

## ----remove_own_stopwords------------------------------------------------
# docs <- tm_map(docs, removeWords, c("department", "email"))
# viewDocs(docs, 16)

## ------------------------------------------------------------------------
docs <- tm_map(docs, stripWhitespace)

viewDocs(docs, 16)

## ----specific_transforms-------------------------------------------------
toString <- content_transformer(function(x, from, to) gsub(from, to, x))
docs <- tm_map(docs, toString, "america", "USA")
rm(toString)
viewDocs(docs, 16)

## ------------------------------------------------------------------------
docs <- tm_map(docs, stemDocument)

viewDocs(docs, 16)


## ----create_document_term_matrix, out.lines=20---------------------------
dtm <- DocumentTermMatrix(docs)

dtm
dim(dtm)
dtm <- as.matrix(dtm)
dtmDist <- dist(dtm)
## or cosine(t(dtm))

# ## get the cell value of max distance 
# #which(dtmDist==max(dtmDist), arr.ind = T)
# 
# #b <- order(dtmDist, na.last=TRUE, decreasing=TRUE, index.return=T)[1:50] 
# 
# dtmDist[208]
# idx <- which(matrix(dtmDist %in% head(sort(dtmDist, TRUE), 50), nr = nrow(dtmDist)), arr.ind = TRUE)
# idx

head(sort(dtmDist, TRUE), 50)


## ----inspect_dtm---------------------------------------------------------
inspect(dtm[1:5, 1000:1005])

## ----dtm_matrix----------------------------------------------------------
class(dtm)
dim(dtm)

## ----create_term_document_matrix, out.lines=20---------------------------
tdm <- TermDocumentMatrix(docs)
tdm

rm(tdm)

## ------------------------------------------------------------------------
freq <- colSums(as.matrix(dtm))
length(freq)

## ----out.lines=10--------------------------------------------------------
ord <- order(-freq)

# Least frequent terms.
freq[head(ord)]

## ------------------------------------------------------------------------
# Most frequent terms.
freq[tail(ord)]

# Frequency of frequencies.
head(table(freq), 15)
tail(table(freq), 15)

## ----dtm_to_m------------------------------------------------------------
m <- as.matrix(dtm)
dim(m)

## ----remove_sparse_terms-------------------------------------------------
dim(dtm)
dtms <- removeSparseTerms(dtm, 0.1)
#(1-0.1)*24 = 21. Retains words with frequency > 21
# as.matrix(removeSparseTerms(myTdm, .01))
# as.matrix(removeSparseTerms(myTdm, .99))
# as.matrix(removeSparseTerms(myTdm, .5))


dim(dtms)

## ------------------------------------------------------------------------
inspect(dtms)

## ------------------------------------------------------------------------
freq <- colSums(as.matrix(dtms))
freq
table(freq)

## ----freq_terms_1000-----------------------------------------------------
findFreqTerms(dtm, lowfreq=100)

## ----assoc---------------------------------------------------------------
findAssocs(dtm, "nation", corlimit=0.2)

## ----plot_correlations---------------------------------------------------
wf <- data.frame(word=names(freq), freq=freq)

library(ggplot2)   
p <- ggplot(subset(wf, freq>50), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p  


## ----word_count----------------------------------------------------------
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq, 14)
wf   <- data.frame(word=names(freq), freq=freq)
head(wf)

## ----plot_freq, fig.width=12---------------------------------------------

subset(wf, freq>100)                                                  %>%
  ggplot(aes(word, freq))                                              +
  geom_bar(stat="identity")                                            +
  theme(axis.text.x=element_text(angle=45, hjust=1))

## ----wordcloud, echo=FALSE-----------------------------------------------

set.seed(123)
wordcloud(names(freq), freq, min.freq=100)

## ----wordcloud_max_words-------------------------------------------------
set.seed(142)
wordcloud(names(freq), freq, max.words=100)

## ----wordcloud_colour----------------------------------------------------    
set.seed(142)
wordcloud(names(freq), freq, min.freq=100, colors=brewer.pal(6, "Dark2"))

## ----wordcloud_scale-----------------------------------------------------
set.seed(142)
wordcloud(names(freq), freq, min.freq=100, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

## ----wordcloud_rotate----------------------------------------------------
set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, min.freq=100, rot.per=0.2, colors=dark2)

#source types
getSources()

#[1] "DataframeSource" "DirSource"       "URISource"       "VectorSource"   
#[5] "XMLSource"       "ZipSource"
#DataframeSource:  data frame source interprets each row of the data frame x as a document. Values
#DirSource:give the path of directory to read files
#VectorSource: A vector source interprets each element of the vector x as a document
#XMLSource: read the XML files parse to get the xml elements
#URISource: Uniform Resource Identifier. Treats each URI as a document.

#readers for various file types
getReaders()
# [1] "readDOC"                 "readPDF"                
# [3] "readPlain"               "readRCV1"               
# [5] "readRCV1asPlain"         "readReut21578XML"       
# [7] "readReut21578XMLasPlain" "readTabular"            
# [9] "readTagged"              "readXML"

#readDoc: read MSWord document types
#readPDF: read pdf document types
#readPlain: read txt document tyeps
#readRCV1: inbuilt data of Reuters Corpus
#readTabular: reads in a text document from a tabular data structure (like a data frame or a list matrix)
#readTagged: text documents containing POS tags
#readXML: reading xml documents



