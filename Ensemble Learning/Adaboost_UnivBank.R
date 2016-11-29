rm(list=ls(all.names=TRUE))
setwd("G:\\New folder\\Ensembles\\20160702_Batch15_CSE7405c_Ensembles")

# the Univ_bank dataset has 14 variables and 5000 records. Use loan as target variable. 

# read the dataset into R
U_Bank <-read.csv(file="UniversalBank.csv",header=T,sep=",",fill=T)

# drop unnecessary the attributes (ID & ZIP Code)
str(U_Bank)
U_Bank <-U_Bank[,-c(1,5)]
str(U_Bank)

# subset data into numeric and categorical
U_Bank_Num <- subset(x = U_Bank, select = c(Age, Experience, Income, Family, CCAvg, Mortgage))
U_Bank_Cat <- subset(x = U_Bank, select = -c(Age, Experience, Income, Family, CCAvg, Mortgage))

# convert categorical attributes into factors
U_Bank_Cat <- data.frame(apply(X = U_Bank_Cat, MARGIN = 2, FUN = as.factor))

# dummify all categorical attributes (except "Personal.Loan")
library(dummies)
data_cat_dummy <-dummy.data.frame(U_Bank_Cat[,-c(2)])
str(data_cat_dummy)

# convert numerical attributes into numerics
U_Bank_Num <- data.frame(apply(X = U_Bank_Num, MARGIN = 2, FUN = as.character))
U_Bank_Num <- data.frame(apply(X = U_Bank_Num, MARGIN = 2, FUN = as.numeric))

# descritize numerical attributes
#library(infotheo)
#Univ_num_binned <- apply(U_Bank_Num, 2, function(x) {discretize(x, disc = "equalfreq", nbins = 4)})
#Univ_num_binned <- data.frame(U_Bank_Num)
#names(Univ_num_binned)[1:5] <- c("Age", "Exp", "Income", "CCAvg", "Mortgage")
# or colnames(Univ_num_binned) = c("age", "Exp", "Income", "CCAvg", "Mortgage")

# Combining categorical and numerical datasets 
data_combine <- cbind(data_cat_dummy, U_Bank_Num)
str(data_combine)

# Standardize the combined datset
library(vegan)
data_std <- decostand(x = data_combine,method = "range")
str(data_std)

# Add "Personal.Loan" back into the dataset
data_final <- cbind(data_std,U_Bank_Cat$Personal.Loan)
str(data_final)
colnames(data_final)[18] <-c("Personal.Loan")

# Split data into train and test
rows <- seq(from = 1, to = nrow(data_final), by = 1)
set.seed(2020)
trainrows <- sample(x = rows, size = nrow(data_final) * 0.7)
trainR <- data_final[trainrows,] #all rows in trainrows & all columns of parent dataset
testR <- data_final[-trainrows,]

# build the classification model using Adaboost
library(ada) 
x = subset(trainR, select = -Personal.Loan) 
y = as.factor(trainR$Personal.Loan) 
a = subset(testR, select = -Personal.Loan) 
b = as.factor(testR$Personal.Loan) 

model = ada(x, y, iter=20, loss="logistic") # 20 Iterations 
model

# predict the values using model on test data sets. 
pred = predict(model, a);pred 

# calculate precision, recall and accuracy 
result <- table(pred, b);result # 0(-ve) and 1(+ve)
accuracy <- sum(diag(result))/sum(result)*100;accuracy
recall <- ((result[2,2])/(result[2,2]+result[1,2])*100);recall
precision <-((result[2,2])/(result[2,2]+result[2,1])*100);precision

# experiment with different number of iterations and find the best. 

