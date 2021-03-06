# Clean workspace
rm(list=ls())
setwd("F:\\TA\\Labs\\AML\\Batch17\\CNN_RNN\\RNN")


## First of all, load in the data and preprocess it.
require(mxnet)

## Set basic network parameters.
batch.size = 32
seq.len = 32
num.hidden = 16
num.embed = 16
num.rnn.layer = 1
num.round = 5
learning.rate= 0.1
wd=0.00001
clip_gradient=1
update.period = 1

## download the data.
# download.data <- function(data_dir) {
#   dir.create(data_dir, showWarnings = FALSE)
#   if (!file.exists(paste0(data_dir,'input.txt'))) {
#     download.file(url='https://raw.githubusercontent.com/dmlc/web-data/master/mxnet/tinyshakespeare/input.txt',
#                   destfile=paste0(data_dir,'input.txt'), method='wget')
#   }
# }

## Make dictionary from text.
make.dict <- function(text, max.vocab=10000) {
  text <- strsplit(text, '')
  dic <- list()
  idx <- 1
  for (c in text[[1]]) {
    if (!(c %in% names(dic))) {
      dic[[c]] <- idx
      idx <- idx + 1
    }
  }
  if (length(dic) == max.vocab - 1)
    dic[["UNKNOWN"]] <- idx
  cat(paste0("Total unique char: ", length(dic), "\n"))
  return (dic)
}

## Transfer text into data feature.
make.data <- function(file.path, seq.len=32, max.vocab=10000, dic=NULL) {
  fi <- file(file.path, "r")
  text <- paste(readLines(fi), collapse="\n")
  close(fi)
  
  if (is.null(dic))
    dic <- make.dict(text, max.vocab)
  lookup.table <- list()
  for (c in names(dic)) {
    idx <- dic[[c]]
    lookup.table[[idx]] <- c 
  }
  
  char.lst <- strsplit(text, '')[[1]]
  num.seq <- as.integer(length(char.lst) / seq.len)
  char.lst <- char.lst[1:(num.seq * seq.len)]
  data <- array(0, dim=c(seq.len, num.seq))
  idx <- 1
  for (i in 1:num.seq) {
    for (j in 1:seq.len) {
      if (char.lst[idx] %in% names(dic))
        data[j, i] <- dic[[ char.lst[idx] ]]-1
      else {
        data[j, i] <- dic[["UNKNOWN"]]-1
      }
      idx <- idx + 1
    }
  }
  return (list(data=data, dic=dic, lookup.table=lookup.table))
}

## Move tail text.
drop.tail <- function(X, batch.size) {
  shape <- dim(X)
  nstep <- as.integer(shape[2] / batch.size)
  return (X[, 1:(nstep * batch.size)])
}

## get the label of X
get.label <- function(X) {
  label <- array(0, dim=dim(X))
  d <- dim(X)[1]
  w <- dim(X)[2]
  for (i in 0:(w-1)) {
    for (j in 1:d) {
      label[i*d+j] <- X[(i*d+j)%%(w*d)+1]
    }
  }
  return (label)
}

## get training data and eval data
# download.data("./data/")
# ret <- make.data("./data/input.txt", seq.len=seq.len)
ret <- make.data("input.txt", seq.len=seq.len)
X <- ret$data
dic <- ret$dic
lookup.table <- ret$lookup.table

vocab <- length(dic)

shape <- dim(X)
train.val.fraction <- 0.9
size <- shape[2]

X.train.data <- X[, 1:as.integer(size * train.val.fraction)]
X.val.data <- X[, -(1:as.integer(size * train.val.fraction))]
X.train.data <- drop.tail(X.train.data, batch.size)
X.val.data <- drop.tail(X.val.data, batch.size)

X.train.label <- get.label(X.train.data)
X.val.label <- get.label(X.val.data)

X.train <- list(data=X.train.data, label=X.train.label)
X.val <- list(data=X.val.data, label=X.val.label)

# Training Model
# --------------
# In `mxnet`, we have a function called `mx.lstm` so that users can build a general lstm model. 
# source("lstm.R")
model <- mx.rnn(X.train, X.val, 
                 ctx=mx.cpu(),
                 num.round=num.round, 
                 update.period=update.period,
                 num.rnn.layer = 1,
                 seq.len=seq.len,
                 num.hidden=num.hidden, 
                 num.embed=num.embed, 
                 num.label=vocab,
                 batch.size=batch.size, 
                 input.size=vocab,
                 initializer=mx.init.uniform(0.1), 
                 learning.rate=learning.rate,
                 wd=wd,
                 clip_gradient=clip_gradient)


# Inference from model
# --------------------
# helper function for random sample.
cdf <- function(weights) {
  total <- sum(weights)
  result <- c()
  cumsum <- 0
  for (w in weights) {
    cumsum <- cumsum+w
    result <- c(result, cumsum / total)
  }
  return (result)
}

search.val <- function(cdf, x) {
  l <- 1
  r <- length(cdf) 
  while (l <= r) {
    m <- as.integer((l+r)/2)
    if (cdf[m] < x) {
      l <- m+1
    } else {
      r <- m-1
    }
  }
  return (l)
}
choice <- function(weights) {
  cdf.vals <- cdf(as.array(weights))
  x <- runif(1)
  idx <- search.val(cdf.vals, x)
  return (idx)
}

## we can use random output or fixed output by choosing largest probability.
make.output <- function(prob, sample=FALSE) {
  if (!sample) {
    idx <- which.max(as.array(prob))
  }
  else {
    idx <- choice(prob)
  }
  return (idx)
  
}

# In `mxnet`, we have a function called `mx.lstm.inference` so that users can build a inference 
# from lstm model and then use function `mx.lstm.forward` to get forward output from the inference.
# Build inference from model.
infer.model <- mx.rnn.inference(num.rnn.layer=num.rnn.layer,
                                 input.size=vocab,
                                 num.hidden=num.hidden,
                                 num.embed=num.embed,
                                 num.label=vocab,
                                 arg.params=model$arg.params,
                                 ctx=mx.cpu())

## generate a sequence of 75 chars using function `mx.lstm.forward`.
start <- 's'
seq.len <- 75
random.sample <- TRUE

last.id <- dic[[start]]
out <- "a"
for (i in (1:(seq.len-1))) {
  input <- c(last.id-1)
  ret <- mx.rnn.forward(infer.model, input, FALSE)
  infer.model <- ret$model
  prob <- ret$prob
  last.id <- make.output(prob, random.sample)
  out <- paste0(out, lookup.table[[last.id]])
}
cat (paste0(out, "\n"))

