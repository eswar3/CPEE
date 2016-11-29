# Remove environment variables
rm(list=ls(all=T))

#working directory

setwd("C:\\Users\\Admin\\Desktop\\20160828_Lab_Batch17")


#Importing data
data<-read.csv("germandata.csv",header=F)
head(data)

#Creating train, test data sets
set.seed(123)
trainrows = sample(1:nrow(data),round(0.7*nrow(data)))
train<-data[trainrows,]
test<-data[-trainrows,]

# Load h2o library

library(h2o)

# Initiate h2o process - can assign ip/port/max_mem_size(ram size)/
# nthreads(no. of processor cores; 2-2core;-1 -all cores available)
localh2o <- h2o.init(ip='localhost', port = 54321, max_mem_size = '1g',nthreads = 1)

#Converting R object to an H2O Object
train.hex <- as.h2o(localh2o, object = train, key = "train.hex")
test.hex <- as.h2o(localh2o, object = test, key = "test.hex")

#To extract features using autoencoder method
aec <- h2o.deeplearning(x = setdiff(colnames(train.hex), "V21"), 
                        y = "V21", data = train.hex,
                        autoencoder = T, activation = "RectifierWithDropout",
                        classification = T, hidden = c(50),
                        epochs = 100, l1 = 0.01)
# 0.5-0.8 retention (dropout 0.2 to 0.5) allowed to extract features
#Adding features to train and test data sets
# Extract features from train data
features_train <- as.data.frame.H2OParsedData(h2o.deepfeatures(train.hex[,-21], model = aec))
# Extract features from test data
features_test <- as.data.frame.H2OParsedData(h2o.deepfeatures(test.hex[,-21], model = aec))

# add extracted features with original data to train the model
train<-data.frame(train,features_train)
test<-data.frame(test,features_test)

#Converting new R object(incles features) to an H2O Object
train.hex <- as.h2o(localh2o, object = train, key = "train.hex")
test.hex <- as.h2o(localh2o, object = test, key = "test.hex")

#DeepLearning Model Implementation
model = h2o.deeplearning(x = setdiff(colnames(train.hex), "V21"), 
                         y = "V21",
                         data = train.hex, 
                         # activation =  "Tanh", 
                         hidden = c(10, 10, 10),
                         activation = "RectifierWithDropout",
                         input_dropout_ratio = 0.1, 
                         epochs = 100,seed=123)
# 0.8 0.9 retention allowed for DeepLearning

# Predictions and accuracy check
# makes prediction on test data
prediction = h2o.predict(model, newdata = test.hex)
# Convert prediction from h2o object to R object/dataframe
pred = as.data.frame(prediction)

# Confusion Matrix
a=table(test$V21, pred$predict)


################################################################
#train

# Build the classification model using randomForest
require(randomForest)
rf_DL <- randomForest(V21 ~ ., data=train, keep.forest=TRUE, ntree=30)

# importance of attributes
round(importance(rf_DL), 2)
importanceValues = data.frame(attribute=rownames(round(importance(rf_DL), 2)),MeanDecreaseGini = round(importance(rf_DL), 2))
importanceValues = importanceValues[order(-importanceValues$MeanDecreaseGini),]
# Top 20 Important attributes
Top20ImpAttrs = as.character(importanceValues$attribute[1:20])

Top20ImpAttrs

train_Imp = subset(train,select = c(Top20ImpAttrs,"V21"))
test_Imp = subset(test,select = c(Top20ImpAttrs,"V21"))

library(rpart)
model = rpart(V21~., data = train_Imp,method="class")
model

pred = predict(model, train_Imp,type="class")
conf_Matrix = table(train_Imp$V21,pred)


#Error Metrics
accuracy_train = sum(diag(conf_Matrix))/sum(conf_Matrix)
precision_train = conf_Matrix[2,2]/sum(conf_Matrix[,2])
recall_Train = conf_Matrix[2,2]/sum(conf_Matrix[2,])

pred = predict(model, test_Imp)
conf_Matrix =table(test_Imp$V21,pred)

#Error Metrics
accuracy_test = sum(diag(conf_Matrix))/sum(conf_Matrix)
precision_test = conf_Matrix[2,2]/sum(conf_Matrix[,2])
recall_Test = conf_Matrix[2,2]/sum(conf_Matrix[2,])

