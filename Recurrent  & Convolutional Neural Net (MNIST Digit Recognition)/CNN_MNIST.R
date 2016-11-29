# Clean workspace
rm(list=ls())
setwd("F:\\TA\\Labs\\AML\\Batch17\\CNN_RNN\\CNN")
# Load MXNet
require(mxnet)

# Loading data and set up
#-------------------------------------------------------------------------------

# Load train and test datasets
train <- read.csv("train_sample.csv")
test <- read.csv("test_sample.csv")

###########################################
###########################################
train_mat <- as.matrix(train)
# inc <- which(results_test[,3] == 0)

## Color ramp def.
colors <- c('white','black')
cus_col <- colorRampPalette(colors=colors)

## Plot the first 12 images
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
sm = sample(nrow(train_mat), 12)
for(di in sm)
{
  print(di)
  #   all_img[di+1,] <- apply(train[train[,785]==di,-785],2,sum)
  #   all_img[di+1,] <- all_img[di+1,]/max(all_img[di+1,])*255
  
  z <- array(train_mat[di,-1],dim=c(28,28))
  z <- z[,28:1] ##right side up
  z <- matrix(as.numeric(z), 28, 28)
  image(1:28,1:28,z,main=train_mat[di,1],col=cus_col(256))
}
###########################################
###########################################

# Set up train and test datasets
train <- data.matrix(train)
train_x <- t(train[, -1])
train_y <- train[, 1]
train_array <- train_x
dim(train_array) <- c(28, 28, 1, ncol(train_x))

test_x <- t(test[, -1])
test_y <- test[, 1]
test_array <- test_x
dim(test_array) <- c(28, 28, 1, ncol(test_x))

# Set up the symbolic model
#-------------------------------------------------------------------------------

data <- mx.symbol.Variable('data')
# 1st convolutional layer
conv_1 <- mx.symbol.Convolution(data = data, kernel = c(5, 5), num_filter = 16)
relu_1 <- mx.symbol.Activation(data = conv_1, act_type = "relu")
pool_1 <- mx.symbol.Pooling(data = relu_1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))


# 2nd convolutional layer
conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5, 5), num_filter = 32)
relu_2 <- mx.symbol.Activation(data = conv_2, act_type = "relu")
pool_2 <- mx.symbol.Pooling(data=relu_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))


# 1st fully connected layer
flatten <- mx.symbol.Flatten(data = pool_2)
fc_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 128)
relu_3 <- mx.symbol.Activation(data = fc_1, act_type = "relu")


# 2nd fully connected layer
fc_2 <- mx.symbol.FullyConnected(data = relu_3, num_hidden = 10)
# Output. Softmax output since we'd like to get some probabilities.
NN_model <- mx.symbol.SoftmaxOutput(data = fc_2)

# Pre-training set up
#-------------------------------------------------------------------------------

# Set seed for reproducibility
mx.set.seed(100)

# Device used. CPU in my case.
devices <- mx.cpu()

# Training
#-------------------------------------------------------------------------------

# Train the model
model <- mx.model.FeedForward.create(NN_model,
                                     X = train_array,
                                     y = train_y,
                                     ctx = devices,
                                     num.round = 20,
                                     array.batch.size = 40,
                                     learning.rate = 0.01,
                                     momentum = 0.5,
                                     eval.metric = mx.metric.accuracy,
                                     epoch.end.callback = mx.callback.log.train.metric(100))

# Testing
#-------------------------------------------------------------------------------

# Predict labels
predicted <- predict(model, test_array)
# Assign labels
predicted_labels <- max.col(t(predicted)) - 1
# Get accuracy
sum(diag(table(test[, 1], predicted_labels)))/10
