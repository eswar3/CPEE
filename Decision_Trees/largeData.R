# setting the working directory
rm(list=ls(all=TRUE))
setwd("F:/AA_Clases/DecisionTrees/CSE 7405c (2)/CSE 7405c/CSE 7405c (2)/CSE 7405c/Day_03_DT_20150612")
par(mfrow=c(1,1))
# setwd("E:\\CPEE\\Batch 11\\7305c\\Day_03_DT")

univ=read.table('univComp.csv', 
                header=T,sep=',')

cor(univ$age, univ$exp)
# removing the id and experience as experience 
#is correlated to age
univ=univ[,-c(1,3)]
summary(univ)

# hist(univ$mortgage, breaks=50)
# mort2 <- (univ$mortgage)^3
# lmort <- log(univ$mortgage)
# hist(mort2, breaks=50)
# hist(lmort, breaks=50)

library(infotheo)

# binning <- function(x,bins, disc){
#   a <- discretize(x, disc=disc, nbins=bins)
#   return(a)
# }
# 
# univTemp <- as.data.frame(matrix(unlist(apply(univ[,1:8],2, binning, 
#                          bins=5, disc="equalfreq")), ncol=8))
# colnames(univTemp)=c(names(univ[,1:8]))


names(univ)
# 
# age <- discretize(univ$age, disc="equalfreq", 
#                   nbins=5)$X
# inc <- discretize(univ$inc, disc="equalfreq", nbins=5)$X
# mortgage <- discretize(univ$mortgage, disc="equalfreq", nbins=5)$X
# ccAvg <- discretize(univ$ccAvg, disc="equalfreq", nbins=5)$X
# MaxCC <- discretize(univ$MaxCC, disc="equalfreq", nbins=5)$X
# MinCC <- discretize(univ$MinCC, disc="equalfreq", nbins=5)$X
# sdCC <- discretize(univ$sdCC, disc="equalfreq", nbins=5)$X
# usage <- discretize(univ$usage, disc="equalfreq", nbins=5)$X

age <- discretize(univ$age, disc="equalfreq", nbins=5)$X
inc <- discretize(univ$inc, disc="equalwidth", nbins=15)$X
mortgage <- discretize(univ$mortgage, disc="equalfreq", nbins=5)$X
ccAvg <- discretize(univ$ccAvg, disc="equalwidth", nbins=10)$X
MaxCC <- discretize(univ$MaxCC, disc="equalfreq", nbins=5)$X
MinCC <- discretize(univ$MinCC, disc="equalfreq", nbins=5)$X
sdCC <- discretize(univ$sdCC, disc="equalfreq", nbins=5)$X
usage <- discretize(univ$usage, disc="equalwidth", nbins=10)$X

univTemp <- data.frame(age, inc, mortgage, ccAvg, MaxCC, MinCC, sdCC, usage)
univ1 <- cbind(univTemp,univ[,9:17])


class <- function(x){
  x <- as.factor(x)
  return(x)
}

univ1 <- data.frame(apply(univ1,2,class))
summary(univ1)

rm(univTemp,class, age, ccAvg, inc, mortgage, MaxCC, MinCC,
   sdCC, usage)

#Let us divide the data into training (3000, testing 
#and cross validation sets)

rows=seq(1,4986,1)
set.seed(123)
trainRows=sample(rows,2986)
set.seed(123)
remainingRows=rows[-(trainRows)]
testRows=sample(remainingRows, 1000)
evalRows=rows[-c(trainRows,testRows)]

train = univ1[trainRows,] 
test=univ1[testRows,] 
eval=univ1[evalRows,]

library(DMwR)
summary(train)

#Ensure that the loan (smoted variable) is the last column in your data frame.
trainS = SMOTE(loan ~ ., 
               train, 
               perc.over = 400, 
               k=5)
#rownames(trainS)=as.character(seq(1:length(trainS$age)))
table(trainS$loan)

table(test$loan)
table(eval$loan)

rm(univ, rows, trainRows, testRows, 
   evalRows, remainingRows)


