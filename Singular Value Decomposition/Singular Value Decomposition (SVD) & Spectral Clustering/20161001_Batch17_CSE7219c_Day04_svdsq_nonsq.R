rm(list=ls(all=TRUE))

#SVD of a square matrix
rotMatrix <- matrix(c(seq(1:25)),
                    nrow=5)
rotMatrix

singValDec <- svd(rotMatrix)

s <- diag(singValDec[[1]])
u <- singValDec[[2]]
v <- singValDec[[3]]

vt=t(v)


s
u
vt

Val=c(1,1,1,1,1)
compTran=rotMatrix%*%Val
compTran

firstTrans=vt%*%Val
secTrans=s%*%firstTrans
finTrans=u%*%secTrans
finTrans

s
vtR1=vt[1:2,]
sR1=s[1:2,1:2]
uR1=u[,1:2]

firstTransR1=vtR1%*%Val
secTransR1=sR1%*%firstTransR1
finTransR1=uR1%*%secTransR1
finTransR1

vtR=vt[1,]
sR=as.matrix(s[1,1])
uR=u[,1]

firstTransR=vtR%*%Val
secTransR=sR%*%firstTransR
finTransR=uR%*%secTransR
finTransR

#SVD of a non-Square matrix

rotMatrix=matrix(c(seq(1:30)),nrow=6)
rotMatrix
singValDec=svd(rotMatrix)
s <- diag(singValDec[[1]])
u <- singValDec[[2]]
v <- singValDec[[3]]
vt=t(v)

diag(s)
dim(u)
dim(vt)
dim(s)

Val=c(1,1,1,1,1)
compTran=rotMatrix%*%Val
compTran

firstTrans=vt%*%Val
secTrans=s%*%firstTrans
finTrans=u%*%secTrans
finTrans

diag(s)
vtR1=vt[1:2,]
sR1=s[1:2,1:2]
uR1=u[,1:2]

firstTransR1=vtR1%*%Val
secTransR1=sR1%*%firstTransR1
finTransR1=uR1%*%secTransR1
finTransR1


vtR=vt[1,]
sR=as.matrix(s[1,1])
uR=u[,1]

firstTransR=vtR%*%Val
secTransR=sR%*%firstTransR
finTransR=uR%*%secTransR
finTransR

#Image compression

image=matrix(c(rep(1,375)),nrow=25)
for(i in 6:20){
  for(j in 3:13){
    image[i,j]=0
  }
}

for(i in 9:17){
  for(j in 6:10){
    image[i,j]=1
  }
}

image

imageSVD=svd(image)

s <- diag(imageSVD[[1]])
s

u <- imageSVD[[2]]
vt <- t(imageSVD[[3]])
s=s[1:3,1:3]
u=u[,1:3]
vt=vt[1:3,]
dim(u)
dim(vt)
newImage=u%*%s%*%vt
newImage

#noise reduction in data

x <- c(-1.03,0.74,-0.02,0.51,-1.31,0.99,0.69,-0.12,-0.72,1.11)
y <- c(-2.23,1.61,-0.02,0.88,-2.39,2.02,1.62,-0.35,-1.67,2.46)

plot(y~x, xlim=c(-3,3), ylim=c(-3,3), pch=16)

data <- cbind(x,y)

dataSVD <- svd(data)

s <- diag(dataSVD[[1]])
s
s <- as.matrix(s[1,1])
u=dataSVD[[2]]
u=as.matrix(u[,1])
v=dataSVD[[3]]
vt=t(v)
vt=t(as.matrix(vt[1,]))

newData=u%*%s%*%vt

plot(newData[,2]~newData[,1], xlim=c(-3,3), ylim=c(-3,3), pch=16)
