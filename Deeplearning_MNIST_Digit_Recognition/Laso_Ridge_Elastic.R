rm(list=ls(all=TRUE))
setwd("C:/Users/Admin/Desktop/20160828_Lab_Batch17")

data = read.csv("pbo_bin1.csv",header=TRUE)

str(data)
summary(data)
names(data)[20:22]  # categorical attributes
################################################
numeric_Variables = data[,c(1:18)]
target_variable = subset(data,select="pbo")
#Converting categorical attributes into dummy variables 
# removing the intercept column (1 index)
catDummies <- model.matrix(data$pbo ~ data$P2_Training_Store_Cnt + data$Freestyle_flag + data$IA_Flag)[,-1]
#####################################################
#Split the data into train and test data sets
rows=seq(1,nrow(data),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(data))/100)
train1 = data.frame(numeric_Variables, catDummies,pbo=data$pbo)[trainRows,]
test1 = data.frame(numeric_Variables, catDummies,pbo=data$pbo)[-trainRows,]

LinReg<- lm(pbo ~ ., data=train1)  
summary(LinReg)
#Evaluation of the model: Error metrics
library(DMwR)
lmtrain = regr.eval(train1$pbo, predict(LinReg,train1))
lmtest = regr.eval(test1$pbo, predict(LinReg,test1))
lmtrain
lmtest
#############################################################################
#Converted the data into matrix form to input into glm model
data2 <- as.matrix(data.frame(numeric_Variables, catDummies))
train = data2[trainRows,] 
test = data2[-trainRows,]

#Target Varaible
y=data$pbo[trainRows]
ytest = data$pbo[-trainRows]
library(glmnet)
#####################################################
# fit model
fit1=glmnet(train,y,alpha=1)  #Lasso
plot(fit1,xvar="lambda",label=TRUE)

fit2=glmnet(train,y,alpha=0)  #Ridge
plot(fit2,xvar="lambda",label=TRUE)

#######################################################
#cv.glmnet will help you choose lambda
cv <- cv.glmnet(train,y)
#lambda.min - value of lambda that gives minimum cvm - mean cross-validated error
###################################################
# Lasso Regression  using glmnet - L1 norm
fit1=glmnet(train,y,lambda=cv$lambda.min,alpha=1)
predict(fit1,train)
library(DMwR)
LASSOtrain = regr.eval(y, predict(fit1,train))
LASSOtest = regr.eval(ytest, predict(fit1,test))
LASSOtrain
LASSOtest


#Model Selection
coef(fit1)
cv.lasso=cv.glmnet(train,y)
plot(cv.lasso)
coef(cv.lasso)

#############################################################################
# Ridge Regression  using glmnet  - L2 norm
library(glmnet)
# fit model
fit2=glmnet(train,y,lambda=cv$lambda.min,alpha=0)
predict(fit2,train)
library(DMwR)
RIDGEtrain = regr.eval(y, predict(fit2,train))
RIDGEtest = regr.eval(ytest, predict(fit2,test))
RIDGEtrain
RIDGEtest
#Model Selection
coef(fit2) 
cv.ridge=cv.glmnet(train,y,alpha=0)
plot(cv.ridge)
coef(cv.ridge)

#cvfit = cv.glmnet(train,y,alpha=0, type.measure = "mae")
################################################################
# Elastic regression
fit3=glmnet(train,y,lambda=cv$lambda.min,alpha=0.5)
# summarize the fit
summary(fit3)
predict(fit3,train)

library(DMwR)
Elastictrain = regr.eval(y, predict(fit3,train))
Elastictest = regr.eval(ytest, predict(fit3,test))
Elastictrain
Elastictest

# make predictions
predictions <- predict(fit3, train, type="link")
# summarize accuracy
rmse <- mean((y - predictions)^2)
print(rmse)

#################################################
lmtrain
lmtest
LASSOtrain
LASSOtest
RIDGEtrain
RIDGEtest
Elastictrain
Elastictest

finalerros <- data.frame(rbind(lmtrain,lmtest,
                               LASSOtrain,LASSOtest,
                               RIDGEtrain,RIDGEtest,
                               Elastictrain,Elastictest))
finalerros
