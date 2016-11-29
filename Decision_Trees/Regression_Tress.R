#Let us retain the numeric variables as numeric.
#Let us build a regression tree to predict income from other
#attributes using rpart and learn to prune it

# setting the working directory
rm(list=ls(all=TRUE))
par(mfrow=c(1,1))
setwd("F:/AA_Clases/DecisionTrees/CSE 7405c (2)/CSE 7405c/CSE 7405c (2)/CSE 7405c/Day_03_DT_20150612")

univ=read.table('univComp.csv', 
                header=T,sep=',')


# removing the id, Zip and experience as experience 
#is correlated to age

univ=univ[,-c(1,3)]

class <- function(x){
  x <- as.factor(x)
  return(x)
}

univTemp <- data.frame(apply(univ[,9:17],2,class))
univ <- cbind(univ[,1:8], univTemp)
str(univ)
summary(univ)

#Splitting
rows=seq(1,4986,1)
set.seed(123)
trainRows=sample(rows,2986)
set.seed(123)
remainingRows=rows[-(trainRows)]
testRows=sample(remainingRows, 1000)
evalRows=rows[-c(trainRows,testRows)]

train = univ[trainRows,] 
test=univ[testRows,] 
eval=univ[evalRows,]

rm(univ,univTemp, evalRows,remainingRows,rows,testRows,trainRows, class)

summary(train)

library(rpart)
dtCart=rpart(inc ~.,data=train, 
             method="anova")    
plot(dtCart,main="Classification Tree for Income",margin=0.15,uniform=TRUE)
text(dtCart,use.n=T)

predCartTrain=predict(dtCart, 
                      newdata=train, 
                      type="vector")
predCartTest=predict(dtCart, 
                     newdata=test, 
                     type="vector")
predCartEval=predict(dtCart, 
                     newdata=eval, 
                     type="vector")

library(DMwR)
regr.eval(train[,"inc"], predCartTrain, train.y = train[,"inc"])
regr.eval(test[,"inc"], predCartTest, train.y = train[,"inc"])
regr.eval(eval[,"inc"], predCartEval, train.y = train[,"inc"])

plotcp(dtCart, minline = TRUE, lty = 3, col = 1, upper = c("size", "splits", "none"))
printcp(dtCart)

dtCart=rpart(inc ~.,data=train,
             method="anova", 
             cp=0.001)



dtCart=rpart(inc ~.,data=train,
             method="anova", cp=0.0069)
printcp(dtCart)

predCartTrain=predict(dtCart, newdata=train, type="vector")
predCartTest=predict(dtCart, newdata=test, type="vector")
predCartEval=predict(dtCart, newdata=eval, type="vector")


regr.eval(train[,"inc"], predCartTrain, train.y = train[,"inc"])
regr.eval(test[,"inc"], predCartTest, train.y = train[,"inc"])
regr.eval(eval[,"inc"], predCartEval, train.y = train[,"inc"])