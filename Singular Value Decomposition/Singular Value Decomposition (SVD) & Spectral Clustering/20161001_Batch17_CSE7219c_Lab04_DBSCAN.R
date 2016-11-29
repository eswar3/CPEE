#install.packages("fpc", dependencies = TRUE)
rm(list=ls(all=T))
library('fpc')

iris2 = iris[-5]  #removed class tags
ds=dbscan(iris2,eps=0.42,MinPts=5)
table(ds$cluster,iris$Species)

# 1:3 are identified cluster::
#   0: noises or outliers  ie., objects that are not assigned to any clusters
################################################################
# Another Example  & Comparison with k-means & spectral clustering
##############################################################
par(mfrow=c(1,3))
# k-means
library(factoextra)
data("multishapes")
df <- multishapes[, 1:2]
set.seed(123)
km.res <- kmeans(df, 5, nstart = 25)
fviz_cluster(km.res, df, frame = FALSE, geom = "point")

plot(df,xlab="",ylab="",
     col=c("blue","black","red","green","brown")[km.res$cluster],
     main="K-means clustering")

#  Spectral Clustering
# 
require(kernlab)
set.seed(123)
specclu = specc(as.matrix(df), kernel = "laplacedot",centers=5)
plot(df, col=specclu, main="spectral clustering")


#The function dbscan() can be used as follow:
  
library("fpc")
# Compute DBSCAN using fpc package
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
# Plot DBSCAN results
plot(db, df, main = "DBSCAN", frame = FALSE)

