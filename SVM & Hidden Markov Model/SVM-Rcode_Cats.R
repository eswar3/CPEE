library(MASS)
data(cats)
?cats
library(vegan) 
cats1 <- decostand(cats[,-1],"range")

head(cats)
cats1['Sex']=cats$Sex
cats1<-subset(cats1,select=c(Sex,Bwt,Hwt))
cats = cats1
inputData<-cats
library(e1071)
model <- svm(Sex~., data = inputData, kernel = "linear",cost = 10)
print(model)
summary(model)
plot(model, inputData)
table(inputData$Sex,inputData$Sex)
compareTable <- table(predict(model),inputData$Sex)  

model_kernel <- svm(Sex~., data = inputData, kernel = "radial",cost = 10)
print(model_kernel)
summary(model_kernel)
plot(model_kernel, inputData)
compareTable <- table(predict(model_kernel),inputData$Sex) 
mean(inputData$Sex != predict(model)) #20% misclassification error

### Tuning
# Prepare training and test data
set.seed(100) # for reproducing results
rowIndices <- 1 : nrow(inputData) # prepare row indices
sampleSize <- 0.8 * length(rowIndices) # training sample size
trainingRows <- sample (rowIndices, sampleSize) # random sampling
trainingData <- inputData[trainingRows, ] # training data
testData <- inputData[-trainingRows, ] # test data

tuned <- tune.svm(Sex ~., data = trainingData, gamma = 10^(-6:-1), cost = 10^(1:2)) # tune

summary (tuned) # to select best gamma and cost

svmfit <- svm (Sex ~ ., data = trainingData, kernel = "radial", cost = 100, gamma=0.01) # radial svm, scaling turned OFF
print(svmfit)
plot(svmfit, trainingData)
compareTable <- table (testData$Sex, predict(svmfit, testData[,-1]))  # comparison table
mean(testData$Sex != predict(svmfit, testData[,-1])) # 13.79% misclassification error


model1 <- svm(Sex~., data = trainingData,gamma=0.1,cost=100)
prediction1 <- predict(model1, testData[,-1])
tab1 <- table(pred = prediction1, true = testData[,1])

