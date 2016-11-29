rm(list=ls(all=TRUE))
par(mfrow=c(1,1))
setwd("F:/AA_Clases/DecisionTrees/CSE 7405c (2)/CSE 7405c/CSE 7405c (2)/CSE 7405c/Day_03_DT_20150612")

source("05largeData.R")
write.csv(train, "train.csv")
library(C50)

dtC50= C5.0(loan ~ ., 
            data = train[], 
            rules=TRUE)
summary(dtC50)
C5imp(dtC50, pct=TRUE)

a=table(train$loan, 
         predict(dtC50, 
                 newdata=train, 
                 type="class"))
rcTrain=(a[2,2])/(a[2,1]+a[2,2])*100
a=table(test$loan, predict(dtC50, newdata=test, type="class"))
rcTest=(a[2,2])/(a[2,1]+a[2,2])*100

rm(a)
#based on c5 importance
dtC50= C5.0(loan ~ inc+infoReq+edu+family+cc+usage+online, 
            data = train, 
            rules=TRUE)
summary(dtC50)


#Smote model
dtC50= C5.0(loan ~ ., 
            data = trainS, 
            rules=TRUE)
summary(dtC50)
C5imp(dtC50, pct=TRUE)

a=table(trainS$loan, 
        predict(dtC50, 
                newdata=trainS, 
                type="class"))
rcTrain=(a[2,2])/(a[2,1]+a[2,2])*100
a=table(test$loan, predict(dtC50, newdata=test, type="class"))
rcTest=(a[2,2])/(a[2,1]+a[2,2])*100


#Experiment for best results
#mortgage equalfreq
#ccavg equalfreq
#ccavg equalfreq, 5
#All 10 bins
#All 10 bins equal width

a=table(eval$loan, predict(dtC50, 
                           newdata=eval, 
                           type="class"))
rcEval=(a[2,2])/(a[2,1]+a[2,2])*100

cat("Recall in Training", rcTrain, '\n',
    "Recall in Testing", rcTest, '\n',
    "Recall in Evaluation", rcEval)

#Test by increasing the number of bins in inc and ccavg to 10
#Test by changing the bin to euqalwidth in inc and ccavg

rm(a,rcEval,rcTest,rcTrain)
