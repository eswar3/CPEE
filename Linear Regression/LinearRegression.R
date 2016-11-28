# Problem Statement: 
# An online gaming portal wants to understand their customer patterns based on their 
# transactional behavior. For this, they have constructed a customer level data based 
# on the details they are tracking. The customer database consists of demographic and 
# transactional information for each customer. Building a regression model to predict 
# the customer revenue based on other factors.

rm(list=ls(all=TRUE))
setwd("C:/Users/Mahesh/Desktop/20160611")
#Reading data
data<-read.csv("CustomerData.csv",header=T)

#Data preparation
names(data)
str(data)
data<-data[,-1]
data$City<-as.factor(data$City)

####Splitting the Data as training  and testing
rows=seq(1,nrow(data),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(data))/100)
train = data[trainRows,] 
test = data[-trainRows,]

#Input all attributes into model
LinReg<- lm(TotalRevenueGenerated ~ City + NoOfChildren + MinAgeOfChild + 
              MaxAgeOfChild + Tenure + FrquncyOfPurchase + NoOfUnitsPurchased + 
              FrequencyOFPlay + NoOfGamesPlayed + NoOfGamesBought + FavoriteChannelOfTransaction + 
              FavoriteGame, data=train)
summary(LinReg)

#Multicollinearity check
#Variance inflation factor
library(car)
vif(LinReg)
LinReg<- lm(TotalRevenueGenerated ~ City + NoOfChildren + MinAgeOfChild + 
              MaxAgeOfChild + Tenure + NoOfUnitsPurchased + 
              FrequencyOFPlay + NoOfGamesPlayed  + FavoriteChannelOfTransaction + 
              FavoriteGame, data=train)
summary(LinReg)
# Stepwise Regression
library(MASS)
step <- stepAIC(LinReg, direction="both")
step
LinReg<- lm(formula = TotalRevenueGenerated ~ City + MinAgeOfChild + Tenure + 
     NoOfUnitsPurchased + FrequencyOFPlay + NoOfGamesPlayed + 
     FavoriteChannelOfTransaction + FavoriteGame, data = train)
summary(LinReg)

library(DMwR)
#Error verification on train data
regr.eval(train$TotalRevenueGenerated, LinReg$fitted.values) 
#Error verification on test data
Pred<-predict(LinReg,test)
regr.eval(test$TotalRevenueGenerated, Pred) 

#Analyzing residuals
par(mfrow=c(2,2))
plot((LinReg))

#Influential Observations 
#Leverage
lev= hat(model.matrix(LinReg))
plot(lev)
# obtain leverage values greater than 0.2
train[lev>0.2,]
train<-train[-(lev>0.2),]

#cooks distance
cook = cooks.distance(LinReg)
plot(cook,ylab="Cooks distances")
max=as.numeric(which.max(cook))
points(max,cook[max],col='red', pch=19)
train[max,]

#Residual outliers
residuals = LinReg$residuals
outliers <- boxplot(residuals,plot=T)$out
sort(outliers)
train[residuals%in% outliers,]

eval<-read.csv("Eval.csv",header=T)
#Error verification on test data
eval$City<-as.factor(eval$City)
Pred<-predict(LinReg,eval)
regr.eval(eval$TotalRevenueGenerated, Pred) 


