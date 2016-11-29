## Lab activity: to analyse relationship between two topics on twitter#
## download tweets
## find words that are frequently used among the documents 
## find possible relationship between the words using clusters and graphs
#install.packages("twitteR")
#install.packages("base64enc")
install.packages("openNLPmodels.en",
                 repos = "http://datacube.wu.ac.at/",
                 type = "source")

install.packages("bit")
install.packages("qdap")
rm(list=(ls(all=TRUE)))
#setwd("")

# load packages
library(tm) # text mining
library(igraph) #r simple graphs and network analysis
library(RColorBrewer)
library(twitteR) #use various functions to analyse twitter data in R 
library(SnowballC)

# This function wraps the OAuth authentication handshake to twitteR session 
setup_twitter_oauth(consumer_key = "pIzAzc3QpDFcR11ZxVyg",
                    consumer_secret = "JCdgvkAQi0M6DEhurNlurbi3PKMKo6ADNI5ainiY", 
                    access_token = "1462029176-LAGO6pq4k0dGnWjde83uEHk19dicibGOGntq1kI",
                    access_secret = "ummmdoYoBXylc8poUjEek0fa69ZxBRE8jmIoNL24")


#data <- searchTwitter("iphone OR samsung", n=1500)
iphone = searchTwitter("#iphone 6s", n=50)

samsung = searchTwitter("#samsung galaxy s6", n=50)

#----the data is read. let us convert it into a dataframe for data manipulations ----

iphone.df <- do.call("rbind", lapply(iphone, as.data.frame))
samsung.df <- do.call("rbind", lapply(samsung, as.data.frame))

#----you should note that there are 16 attributes for the data frames created -------
#---- we are interested in the text part alone for this analysis --------

# Clean text to remove odd characters
iphone.df$text <- sapply(iphone.df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
samsung.df$text <-sapply(samsung.df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

data1 <- data.frame("text" = iphone.df$text)
data2 <- data.frame("text" = samsung.df$text)
tweets <- rbind(data1,data2)
rm(data1,data2)

#---- extracting the text parts alone and combining the tweets into one single file -----
results = tweets
results = tweets$text

# remove retweet entities 
results = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", results)
# in RT type tweets, remove ?, : before or after a word, any email addresses
# remove at people
results = gsub("@\\w+", "", results)
# remove punctuation
results = gsub("[[:punct:]]", "", results)
# remove numbers
results = gsub("[[:digit:]]", "", results)
# remove html links
results = gsub("http\\w+", "", results)
# remove unnecessary spaces
results = gsub("[ \t]{2,}", "", results)
results = gsub("^\\s+|\\s+$", "", results)

# define "tolower error handling" function 
tryTolower = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

# lower case using tryTolower with sapply 
results = sapply(results, tryTolower)
names(results) = NULL

# remove empty results (if any)
results = results[results != ""]

# create corpus
corpus = Corpus(VectorSource(results))
corpus[[10]]$content

# remove stopwords
skipwords = c(stopwords("english"), 
              "iphone", "samsung")
corpus = tm_map(corpus, removeWords, skipwords)
corpus <- tm_map(corpus, removePunctuation)

# term-document matrix
# --note the control list which can restrict the length of words, apply stemming, compute TFIDF, stopwords, punctuations in a single step-----
tdm = TermDocumentMatrix(corpus,control = list(wordLengths = c(6, Inf),stemming = TRUE))
class(tdm)
# convert tdm to matrix
m = as.matrix(tdm)
# word counts
wc = rowSums(m)

# get those words above 50%
lim = quantile(wc, probs=0.5)
good = m[wc > lim,]

# remove columns (docs) with zeroes
good = good[,colSums(good)!=0]

# adjacency matrix: is a square matrix used to represent a finite graph
# The elements of the matrix indicate whether pairs of vertices are adjacent or not in the graph.

M = good %*% t(good)

# set zeroes in diagonal
diag(M) = 0

# graph, the weights are assigned based on the no of edges between two vertices. Check Help
# adding the words on the vertex as row names.
g = graph.adjacency(M, weighted=T, mode="undirected",
                    add.rownames=TRUE)

# let's superimpose a cluster structure with k-means clustering
kmg = kmeans(M, centers=8)
gk = kmg$cluster

# create nice colors for each cluster
gbrew = c("red", brewer.pal(8, "Dark2"))
gpal = rgb2hsv(col2rgb(gbrew))
gcols = rep("", length(gk))
for (k in 1:8) {
  gcols[gk == k] = hsv(gpal[1,k], gpal[2,k], gpal[3,k], alpha=0.5)
}

# prepare ingredients for plot
V(g)$size = 10
V(g)$label = V(g)$name
V(g)$degree = degree(g)

V(g)$label.color = hsv(0, 0, 0.2, 0.55)
V(g)$frame.color = NA
V(g)$color = gcols
E(g)$color = hsv(0, 0, 0.7, 0.3)
E(g)$weight


# layout to ensure non-overlapping vertices and edges
glay = layout.fruchterman.reingold(g)

# plot
plot(g, layout=glay)
title("\nGraph1 of tweets about iphone and samsung",
      col.main="gray40", cex.main=1.5, family="serif")

