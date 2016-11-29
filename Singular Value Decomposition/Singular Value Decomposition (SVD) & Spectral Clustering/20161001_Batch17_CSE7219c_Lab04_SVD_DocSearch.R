##Creating textmatrix and computing LSA
setwd("C:/Users/Mahesh/Desktop/DocSearch/text_docs")
tem_dir = tempfile()
dir.create(tem_dir)
doc1=read.table("doc1.txt", header=FALSE, sep="")
doc1<-t(doc1)
write(doc1, file=paste(tem_dir, "doc1", sep="/"))

doc2=read.table("doc2.txt", header=FALSE, sep="")
doc2<-t(doc2)
write(doc2, file=paste(tem_dir, "doc2", sep="/"))

doc3=read.table("doc3.txt", header=FALSE, sep="")
doc3<-t(doc3)
write(doc3, file=paste(tem_dir, "doc3", sep="/"))

doc4=read.table("doc4.txt", header=FALSE, sep="")
doc4<-t(doc4)
write(doc4, file=paste(tem_dir, "doc4", sep="/"))

doc5=read.table("doc5.txt", header=FALSE, sep="")
doc5<-t(doc5)
write(doc5, file=paste(tem_dir, "doc5", sep="/"))

doc6=read.table("doc6.txt", header=FALSE, sep="")
doc6<-t(doc6)
write(doc6, file=paste(tem_dir, "doc6", sep="/"))
library(lsa)
#Creating a term document matrix using text matrix function in lsa
Term_document_matrix = textmatrix(tem_dir, minWordLength=6)

#inspecting the matrix
Term_document_matrix

#Computing tf-idf for this
Weights_Matrix = lw_bintf(Term_document_matrix) * gw_idf(Term_document_matrix)

#Applying singular value decomposition on this matrix
LSA_space = lsa(Weights_Matrix, dims=2) ##considering only two singular values

##Seach using LSA
temp=t(LSA_space$sk)
sk=array(dim=c(2,2))
sk[1,1]=temp[1,1]
sk[1,2]=0
sk[2,1]=0
sk[2,2]=temp[1,2]

#Forming the query matrix for a given term.  Here, the term is "analysts".

q =query("analysts", rownames(Weights_Matrix))

#The following code accomplishes q = qTUS-1
qv=t(q)%*%LSA_space$tk%*%solve(sk)

#In this step, we are finding cosine with all document matrices
score= 0
doc = 0
for (i in 1:nrow(LSA_space$dk)) {
  score[i] =cosine(as.vector(qv),as.vector(LSA_space$dk[i,]))
  doc[i] = i
}
Result=cbind(doc,score)
Result
