#tiene q tener todo el mismo tipo de dato
library(bigmemory)
library(biganalytics)
 options(bigmemory.typecast.warning=FALSE)

 A <- big.matrix(5000, 5000, type="char", init=0)
 #Fill the matrix by randomly picking 20% of the positions for a 1.
 x <- sample(1:5000,size=5000,replace=TRUE)
 y <- sample(1:5000,size=5000,replace=TRUE)
 for(i in 1:5000) {
   A[x[i],y[i]] <- 1
 }
 #Get the location in RAM of the pointer to A.
 desc <- describe(A)
 #Write it to disk.
 dput(desc , file="/tmp/A.desc")
 sums <- colsum(A, 1:20)