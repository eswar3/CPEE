rm(list=ls())
setwd("F:/AA_Clases/DecisionTrees/20160717_CSE7405_Batch17_DecisionTrees")



data1<-read.csv("part1.csv",header=T,sep=",")
data2<-read.csv("part2.csv",header=T,sep=",")
data3<-merge(data1,data2,by="obs")
data3<-na.omit(data3)

data_Num<-data3[c(2,14,23)]
data_Cat<-data3[-c(1,2,14,23)]
data_Cat<- data.frame(apply(data_Cat,2,FUN=as.character))
data_Cat<- data.frame(apply(data_Cat,2,FUN=as.factor))
str(data_Cat)                        
data_Num<-data.frame(apply(data_Num,2,FUN=as.numeric))
library(infotheo)
duration<-discretize(data_Num[1],disc="equalfreq",nbins=5)
age<-discretize(data_Num[2],disc="equalfreq",nbins=5)
amount<-discretize(data_Num[3],disc="equalfreq",nbins=5)

data_final<-data.frame(data_Cat,duration,age,amount)
rm(list=setdiff(ls(),c("data_final","data_Num")))
summary(data_final)

#Splitting the data into train and test

Rows <- seq(from = 1, to = nrow(data_final), by = 1)
set.seed(1234)
TrainRows <- sample(x = Rows, size = nrow(data_final) * 0.6)
TestRows <- Rows[-TrainRows]

# create train and test data set
TrainData <- data_final[TrainRows,]
TestData <- data_final[TestRows,]
table(TrainData$response)
# remove files from environment
rm(Rows, TestRows, TrainRows)

#Applying decision trees
library(C50)
DT_C50=C5.0(response~., data=TrainData,rules=T)

#To get the rules
C5imp(DT_C50,pct=T)
myDtRule <- DT_C50$rules
summary(DT_C50)$Rules
DT_C50$rules
#to get the summary of the the tree and get rules
summary(DT_C50)


# Applying the model on train and test data to see efficiency of the model
a=table(TrainData$response, predict(DT_C50, newdata=TrainData, type="class"))
a
b=table(TestData$response, predict(DT_C50, newdata=TestData, type="class"))
accTrain = sum(diag(a))/sum(a)
accTest = sum(diag(b))/sum(b)

##Decision trees for Regression
#Removing the descretized amount from the data_final and adding the original amount values to it

data_final1<-data.frame(data_final[1:27],data_Num[3])
Rows <- seq(from = 1, to = nrow(data_final1), by = 1)
set.seed(1234)
TrainRows <- sample(x = Rows, size = nrow(data_final1) * 0.6)
TestRows <- Rows[-TrainRows]

# create train and test data set
TrainData1 <- data_final1[TrainRows,]
TestData1 <- data_final1[TestRows,]
# remove files from environment
rm(Rows, TestRows, TrainRows)

library(rpart)
DT_rpart<-rpart(amount~.,data=TrainData1,method="anova")
plot(DT_rpart,main="Regression tree for Amount",margin=0.15,uniform=T)
text(DT_rpart,use.n=T)

DT_rpart_PredTrain=predict(DT_rpart,newdata=TrainData1, type="vector")
A<-data.frame(TrainData1[28],DT_rpart_PredTrain)
DT_rpart_PredTest=predict(DT_rpart, newdata=TestData1,type="vector")
B<-data.frame(TestData1[28],DT_rpart_PredTest)   
library(DMwR)
regr.eval(TrainData1[28],DT_rpart_PredTrain)





