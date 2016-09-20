#tiene q tener todo el mismo tipo de dato 
library(bigmemory)
library(biganalytics)
library(data.table)
 options(bigmemory.typecast.warning=FALSE)
 setwd("/Users/davidmonteagudo/kaggle/data/bosch/input/")
 rm(list = ls())
 
 gc(reset=T)
 
 start.time<-proc.time()
 
 # creating the file
 cat.matrix <- read.big.matrix(
   "../input/train_categorical.csv", 
   type ="integer", header = TRUE, backingfile = "cat.bin", 
   descriptorfile ="cat.desc", extraCols =NULL) 
 #creates a ffdf object 
 class(cat.ff)
 end.time<-proc.time()
 
 save.time<-end.time-start.time
 cat("\n Number of minutes running:", save.time[3]/60, "\n \n")