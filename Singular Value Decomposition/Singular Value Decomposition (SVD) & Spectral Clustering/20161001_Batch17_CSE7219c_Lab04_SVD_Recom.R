rm(list=ls(all=TRUE))

##Matrix
M <- matrix(c(2,4,1,3,0,0,0,0),byrow = 4, ncol=2)
M
# SVD computation
svdM = svd(M)
s= diag(svdM$d)
u = svdM$u
v = svdM$v
vt = t(v)

u
s
vt

M_hat = u %*% s %*% vt

M_hat

#  Mathematical Computation, u, v , s 

##Transpose of a matrix
TM = t(M)
TM
#M*t(M)
exTex = M%*%TM
exTex
##Eigen values (M-lamda)V = 0 compute for Lamda and V
#Lamda is the eigen value and V is the eigenvector
Meigen = eigen(exTex)
U = Meigen$vectors
U
##now let us get the eigen values and eigen vectors of t(M)*M
exTM = TM%*%M
exTM
##Eigen value of exTM
Meigenv = eigen(exTM)
V = Meigenv$vector
V

## The diagnonal matrix (singular values) can be 
# obtained by taking the squareroot of the eigen values


sqrt(Meigen$values)
sqrt(Meigenv$values)
S = sqrt(Meigen$values)  
S = S[which(S>0)]  # consider non-zero singular values
S1 = matrix(rep(0, nrow(u)*nrow(v)),byrow=T,ncol=nrow(v))
for( i in 1:length(S)){
  S1[i,i]=S[i]
}
S1
V
U
Vt = t(V)
U %*% S1 %*% Vt

#################################################


# Rows are  users (5 users) and # columns are songs (6 songs)
N=matrix(c(5,0,3,4,3,2,
           2,1,4,0,0,5,
           1,1,1,0,2,1,
           1,0,0,0,5,5,
           4,3,2,3,4,1),byrow=T,ncol=6)
rownames(N)= c("User1","User2","User3","User4","User5")
colnames(N) = c("Song1","Song2","Song3","Song4","Song5","Song6")

# UserId = c("User1","User2","User3","User4","User5")
# N= cbind(UserId,N)
rownames(N)= c("User1","User2","User3","User4","User5")
Users = c(1,2,3,4,5)
colnames(N) = c("Song1","Song2","Song3","Song4","Song5","Song6")
songs = colnames(N)
# Compute the Singular-value decomposition

svd<-svd(N)
S <- diag(svd$d)
u <- svd$u
v <- svd$v
vt = t(v)

N_hat = u %*% S %*% vt

eigenval = svd$d
e_sqare_energy = (eigenval/sum(eigenval))*100
cumsum(e_sqare_energy)


# with first 3 values itself, almost 90% is covered hence, 3 dimensions are enough
svd <- svd(N,nu=3,nv=3)
S <- diag(svd$d[1:3])
svd$u %*% S %*% t(svd$v) 

SVDresultusers = svd$u
SVDresultsongs = svd$v

#New user ratings  0,0,2,0,4,1
Newuserratings = c(0,0,2,0,4,1)

# ratings should be multipled with songs space matrix
# A = us(t(v))
# Av = us (t(v))(v)   & (t(v))(v) = I  
#  =>  Av = us(I) 
#  =>  Av = us
# => AV(inverse of S) = u, new user's user space = newuserrmatrix * svd$v * inverse of s
b <-matrix(Newuserratings,byrow=T, ncol=6)
S1 <- diag(1/svd$d[1:3])
# building a decomposition of new user ratings
newuserrating = b %*% svd$v %*% S1   



### correlation

# Creating function to calculate the cosine between two vectors
getCosine <- function(x,y) 
{
  cosine <- abs(sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y))))
  return(cosine)
}



#user based recommendation

distances = apply(svd$u,1,getCosine,newuserrating) 
userid = which((distances==max(distances))==TRUE)
similaruser = N[userid,]
Newuserratings
zeroposofNewUser = which((Newuserratings==0)==TRUE)
recom = which(similaruser ==max(similaruser[zeroposofNewUser]))
recom
# Example : recom = c("song1"=1,"song6" = 6)
if(length(recom)>1){
 # do popular movie/song
  moviewiseavgratings = apply(N[,recom],2,median)
  pos = which((moviewiseavgratings == max(moviewiseavgratings))==TRUE)
  Recommendation = names(recom[pos])
  
}else{
  Recommendation = names(recom[1])
  }


Recommendation

