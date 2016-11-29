#To remove previous history and data
rm(list=ls(all=T))
#Set the working directory
setwd("F:/20161001_Batch17_CSE7219c_Lab04_SVD_SpecClust")

#Read data into R
data=read.csv("MovieRatings.csv",header=T)

#Data Pre-processing
#Covert the given data into matrix form
#Users into rows & movies into columns
library(reshape2)
data2=dcast(data, UserID ~ Movie, value="Rating")
data2=data2[,c("UserID","M1","M2","M3","M4","M5","M6","M7","M8","M9","M10","M11","M12","M13","M14","M15","M16","M17","M18","M19")]

#Compute the singular-value decomposition
head(data2[,-1])
SVD = svd(data2[,-1])
s <- diag(SVD[[1]])
u <- SVD[[2]]
v <- SVD[[3]]
vt = t(v)

N_hat = u %*% s %*% vt

head(N_hat)

eigenval = SVD$d
e_sqare_energy = (eigenval/sum(eigenval))*100
cumsum(e_sqare_energy)   # may be we can take only 15 dimensions (almost 85% covered) instead 19


#Function to extract top Z nearest users with respect to the given user
SimilarUsers<-function(x,z){
  library(lsa)
  score= 0
  user = 0
  for (i in 1:nrow(u)) {
    score[i] =cosine(u[x,],as.vector(u[i,]))
    user[i] = i
  }
  Distance<-data.frame(user,score)
  Distance<-Distance[order(-Distance$score),]
  z=z+1
  TopZUsers<-Distance$user[1:z][!Distance$user[1:z]%in%c(x)]
  subset<-data2[which(data2$UserID%in%TopZUsers),]
  return(subset)
}
#Extract similar users
SimilarUsers(1,10)



#Function to return expected rating
ExpectedRating<-function(x,y,z){
  library(lsa)
  score= 0
  user = 0
  for (i in 1:nrow(u)) {
    score[i] =cosine(u[x,],as.vector(u[i,]))
    user[i] = i
  }
  Distance<-data.frame(user,score)
  Distance<-Distance[order(-Distance$score),]
  TopZUsers<-Distance$user[1:z][!Distance$user[1:z]%in%c(x)]
  subset<-data2[which(data2$UserID%in%TopZUsers),]
  print(data2[which(data2$UserID%in%x),])
  return("ExpectedRating"=mean(subset[,y]))
}

#Expected rating for a given 'User and Movie'
names(data2)
#Input "User" "Movie" "NNs"

ExpectedRating(1,"M15",3)
x=1
y="M15"
z=3



