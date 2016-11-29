rm(list=ls(all=TRUE))

group.one = cbind(runif(100,0,15),
                  runif(100,0,1))
group.two = cbind(rnorm(100,15,1),
                  rnorm(100,5,1))
uniform.pts = runif(100,4,15)
group.three = cbind(uniform.pts+
                      rnorm(100,0,.5),
                    uniform.pts+
                      rnorm(100,0,.5))
x = rbind(group.one,group.two,group.three)

plot(x, xlab="",ylab="")

km = kmeans(x, centers=3,
            iter.max=20)

plot(x,xlab="",ylab="",
     col=c("red","black","blue")[km$cluster],
     main="k-means")

distances = as.matrix(dist(x))
W = exp(-distances^2)  # adjacency matrix
head(W,10)
G = diag(rowSums(W))  # degree matrix
L = G - W   # Laplacian matrix
eig = eigen(L)
eig
eigvecmat = eig$vectors
eig$vectors[,300] 
# use the k-means algorithm on the eigenvalues corresponding to the k smallest eigenvectors.
# using connectivity strength, take last two eigen vectors to be considered (last two vectors) or 
# as per algebraic connectivity, take two eigen vectors from last but one ( last 2nd and 3rd vectors)
# these will explain maximum variance 


km2 = kmeans(eig$vectors[,298:299],
             centers=3,
             iter.max=20)

plot(x,xlab="",ylab="",
     col=c("red","black","blue","green")[km2$cluster],
     main="spectral clustering")


###############################################
require(kernlab)
specclu = specc(x, kernel = "laplacedot",centers=3)
plot(x, col=specclu)

