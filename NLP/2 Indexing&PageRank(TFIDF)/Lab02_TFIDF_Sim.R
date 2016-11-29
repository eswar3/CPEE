rm(list=ls(all=T))
D1<- c("I learnt to compute inverse of a matrix")
D2<- c("Matrix inverse, is easy to compute")
D3<- c("term frequency - inverse document frequency matrix")
library(tm)
library(lsa)
docs <- VectorSource(c(D1,D2, D3))
inspect(VCorpus(docs))
docs<- tm_map(Corpus(docs), content_transformer(tolower))

strsplit_space_tokenizer <- function(x)
unlist(strsplit(as.character(x), "[[:space:]]+"))
              
dtm<- DocumentTermMatrix(docs, control = list(tokenize = strsplit_space_tokenizer,
                                              wordLengths = c(4, Inf),
                                              removeNumbers = TRUE,
                                              removePunctuation = TRUE,
                                              stopwords = TRUE, 
                                              stemming = TRUE,
                                              weighting = weightTfIdf))

# other weighting options
#---- weightTf, weightTfIdf, weightBin, weightSMART-----------
termFreq(docs[[3]])
inspect(dtm)


dtm$i #term wise document id
dtm$j #term wise position in Doc-term matrix.
dtm$v #termfrequency

dim(dtm)

### cosine similarity

# Convert to matrix
dt_matrix <- as.matrix(dtm)

#Compute the similarity between documents in a document-term matrix
dtmDist <- dist(dt_matrix[1:3,], method = "cosine")
dtmDist

## get the cell value of max distance 
#which(dtmDist==max(dtmDist), arr.ind = T)

#b <- order(dtmDist, na.last=TRUE, decreasing=TRUE, index.return=T)[1:50] 

dtmDist[208]
idx <- which(matrix(dtmDist %in% head(sort(dtmDist, TRUE), 50), nr = nrow(dtmDist)), arr.ind = TRUE)
idx



### PageRank
library(miniCRAN)
library(igraph)
library(magrittr)
g <- graph(c(
  1, 2, 1, 3, 
  2, 3, 3, 1, 
  4, 3), 
  directed=TRUE)
plot(g)
m <-page_rank(g)
m
#m <-page_rank_old(g, niter=10)
m

## manual computation
M = get.adjacency(g, sparse = FALSE)
M = t(M / rowSums(M))
n = nrow(M)
U = matrix(data=rep(1/n, n^2), nrow=n, ncol=n)
beta=0.85
A = beta*M+(1-beta)*U
e = eigen(A)
v <- e$vec[,1]
v <- as.numeric(v) / sum(as.numeric(v))
v

# Exercise
g2 <- graph(c( 1, 2, 1, 3, 1, 4, 2, 1, 3, 1, 4, 1, 4, 5, 4, 6, 4, 7),directed=TRUE)
plot(g2)
m <-page_rank(g2)
m
