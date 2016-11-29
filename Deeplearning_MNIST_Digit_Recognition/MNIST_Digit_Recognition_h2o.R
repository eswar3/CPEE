  rm(list=ls(all=T))
setwd("F:\\TA\\Cognizant\\Batch-05\\Neural_Network")
load("im.Rdata")
# h2o
library(h2o)
# single thread
h2o.init()
set.seed(1234)


train_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/train.csv.gz"
test_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/test.csv.gz"

train <- h2o.importFile(train_file)
test  <- h2o.importFile(test_file)
  
# To see a brief summary of the data, run the following command
summary(train)
summary(test)

dim(train)
dim(test)

y <- "C785"
x <- setdiff(names(train), y)

# We encode the response column as categorical for multinomial
#classification
train[,y] <- as.factor(train[,y])
test[,y]  <- as.factor(test[,y])

train[,x] <- train[,x]/255
test[,x] <- test[,x]/255

# Train a Deep Learning model and valid
model_cv <- h2o.deeplearning(x = x,
                             y = y,
                             training_frame = train,
                             autoencoder = F,
                             initial_weight_distribution = "Uniform",
                             activation = "RectifierWithDropout",
                             loss = "CrossEntropy",
                             adaptive_rate = T,
                             hidden = c(100),
                             input_dropout_ratio = 0.2,
                             hidden_dropout_ratios = c(0.4),
                             l1 = 1e-5,
                             epochs = 120)

###### 
######
# Train Accuracy
pred_train <- h2o.predict(model_cv, train)
table(train[,785], pred_train[,1])
#head(pred_train[,1])
#dim(pred_train[,1])
dim(train)[1]
class(train[,785])
class(pred_train[,1])

p <- as.data.frame(pred_train[,1])
a <- as.data.frame(train[,785])
results_train <- cbind(a,p)

results_train$acc <- rep(0, dim(train)[1])
for (i in 1:nrow(results_train)){
  if (results_train[i,1] == results_train[i,2]){
    results_train[i,3] = 1
  }
}

sum(results_train[,3])/nrow(results_train)

###### 
######
# Test Accuracy
pred_test <- h2o.predict(model_cv, test)
#table(test[,785], pred_test[,1])

dim(test)[1]

p <- as.data.frame(pred_test[,1])
a <- as.data.frame(test[,785])
results_test <- cbind(a,p)

results_test$acc <- rep(0, dim(test)[1])
for (i in 1:nrow(results_test)){
  if (results_test[i,1] == results_test[i,2]){
    results_test[i,3] = 1
  }
}

sum(results_test[,3])/nrow(results_test)


test_df <- as.data.frame(test)
test_mat <- as.matrix(test_df)
inc <- which(results_test[,3] == 0)

## Color ramp def.
colors <- c('white','black')
cus_col <- colorRampPalette(colors=colors)

## Plot the first 12 images
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
for(di in 1:12)
{
  print(di)
  #   all_img[di+1,] <- apply(train[train[,785]==di,-785],2,sum)
  #   all_img[di+1,] <- all_img[di+1,]/max(all_img[di+1,])*255
  
  z <- array(test_mat[di,-785],dim=c(28,28))
  z <- z[,28:1] ##right side up
  z <- matrix(as.numeric(z), 28, 28)
  image(1:28,1:28,z,main=results_test[di,2],col=cus_col(256))
}

## Plot the first 12 incorrectly classified images
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
for(di in inc[1:12])
{
  print(di)
  #   all_img[di+1,] <- apply(train[train[,785]==di,-785],2,sum)
  #   all_img[di+1,] <- all_img[di+1,]/max(all_img[di+1,])*255
  
  z <- array(test_mat[di,-785],dim=c(28,28))
  z <- z[,28:1] ##right side up
  z <- matrix(as.numeric(z), 28, 28)
  image(1:28,1:28,z,main=results_test[di,2],col=cus_col(256))
}

#save.image(file=".RData")
