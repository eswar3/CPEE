rm(list = ls(all = T))
setwd("G:\\New folder\\Ensembles\\20160702_Batch15_CSE7405c_Ensembles")

#Read the data into R
hepatitis <- read.table('hepatitis.txt', header=F, dec='.',
                        col.names=c('target','age','gender','steroid','antivirals',
                                    'fatigue','malaise','anorexia','liverBig',
                                    'liverFirm','spleen','spiders','ascites',
                                    'varices','bili','alk','sgot','albu','protime',
                                    'histology'), na.strings=c('?'), sep=',')
#Study dataset
str(hepatitis)
table(hepatitis$target)
str(hepatitis$target) # 1: Die; 2: Live 

#Convert 1s and 2s into 1s and 0s 
hepatitis$target= ifelse(hepatitis$target==1,1,0 ) # 1: Die(+ve); 0: Live (-ve)

# The numerical variables are: age,bili,alk,sgot,albu and protime
# The categorical variables are: the remaining 14 variables

# Seperate numerical and categorical variables
hepatitis_num <- hepatitis[,c(2,15:19)]
hepatitis_cat <- hepatitis[,-c(2,15:19)]

# Convert subsetted categorical variables into factors 
hepatitis_cat <- data.frame(apply(hepatitis_cat,2,function(x){as.factor(x)}))
str(hepatitis_cat)

#convert subsetted data into numeric (not required)
#str(hepatitis_num) #mixture of num and int
#hepatitis_num <- data.frame(apply(hepatitis_num,2,function(x){as.character(x)}))
#hepatitis_num <- data.frame(apply(hepatitis_num,2,function(x){as.numeric(x)}))

# Combine the 2 datasets
hepatitis_merged <-cbind(hepatitis_num,hepatitis_cat)
str(hepatitis_merged)

# Handle missing values using knn imputation
sum(is.na(hepatitis_merged))
library(DMwR)
hepatitis_imputed <- knnImputation(data =hepatitis_merged,k = 5)
sum(is.na(hepatitis_imputed))

# Split dataset into train and test
rows <- seq(from = 1, to = nrow(hepatitis_imputed), by = 1)
set.seed(2020)
trainrows <- sample(x = rows, size = nrow(hepatitis_imputed) * 0.65)
trainR <- hepatitis_imputed[trainrows,] #all rows in trainrows & all columns of parent dataset
testR <- hepatitis_imputed[-trainrows,]

# Build the classification model using randomForest
library(randomForest)
hepatitis_rf <- randomForest(target ~ ., data=trainR, keep.forest=TRUE, ntree=30) 

# View results and understand important attributes
print(hepatitis_rf)
hepatitis_rf$predicted 
hepatitis_rf$importance  
# or importance(hepatitis_rf)
round(importance(hepatitis_rf), 2)   

# Extract and store important variables obtained from the random forest model
Imp_hepatitis_rf <- data.frame(hepatitis_rf$importance)
Imp_hepatitis_rf <- data.frame(row.names(Imp_hepatitis_rf),Imp_hepatitis_rf[,1])
colnames(Imp_hepatitis_rf) = c('Attributes','Importance')
Imp_hepatitis_rf <- Imp_hepatitis_rf[order(Imp_hepatitis_rf$Importance , decreasing = TRUE),]
Imp_hepatitis_rf <- Imp_hepatitis_rf[1:6,]

# plot (directly prints the important attributes) 
varImpPlot(hepatitis_rf)

# Predict on Train data 
pred_model_train <-predict(hepatitis_rf,trainR[,-c(7)],type="response", norm.votes=TRUE)
result_train <- table("actual _values"= trainR$target,pred_model_train);result_train
#OR
#table(trainR$target, predict(hepatitis_rf, trainR, type="response", norm.votes=TRUE)) 

# Predicton Test Data
pred_model_test <-predict(hepatitis_rf,testR[,-c(7)],type="response", norm.votes=TRUE)
result_test <- table("actual _values"= testR$target,pred_model_test);result_test

# Accuracy,Precision and Recall on testR
test_accuracy <- sum(diag(result_test))/sum(result_test)*100;test_accuracy
test_recall <- ((result_test[2,2])/(result_test[2,2]+result_test[2,1])*100);test_recall
test_precision <-((result_test[2,2])/(result_test[2,2]+result_test[1,2])*100);test_precision

##Parameter Tuning in RandomForest 

# increase/decrease "ntree", mtry, nodesize and see if there is any improvement in the above metrics

hepatitis_rf2 <- randomForest(target ~ ., data=trainR, keep.forest=TRUE, ntree=150, mtry = 5, nodesize = 3) 

pred_model_train2 <-predict(hepatitis_rf2,trainR[,-c(7)],type="response", norm.votes=TRUE)
result_train2 <- table("actual _values2"= trainR$target,pred_model_train2);result_train2
#OR
#table(trainR$target, predict(hepatitis_rf, trainR, type="response", norm.votes=TRUE)) 

# Predicton Test Data
pred_model_test2 <-predict(hepatitis_rf2,testR[,-c(7)],type="response", norm.votes=TRUE)
result_test2 <- table("actual _values"= testR$target,pred_model_test2);result_test2

# Accuracy,Precision and Recall on testR
test_accuracy2 <- sum(diag(result_test2))/sum(result_test2)*100;test_accuracy2
test_recall2 <- ((result_test2[2,2])/(result_test2[2,2]+result_test2[2,1])*100);test_recall2
test_precision2 <-((result_test2[2,2])/(result_test2[2,2]+result_test2[1,2])*100);test_precision2



