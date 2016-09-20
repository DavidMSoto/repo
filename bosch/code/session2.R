library(bigmemory)
 library(biganalytics)

 #Read the pointer from disk.
 desc <- dget("/tmp/A.desc")
 #Attach to the pointer in RAM.
 A <- attach.big.matrix(desc)
 #Check our results.
  sums <- colsum(A, 1:20)