
rm(list = ls())

library(data.table)
library(tidyr)   
library(dplyr)   
WIN <- TRUE
if (WIN) {setwd("c:/repos/repo/allState/code/")} else{setwd("~/dataScience/projects/repo/AllState/code/")}


s=fread('./../input/train.csv', 
        header=T, sep=",", verbose = FALSE, showProgress = FALSE) %>% as.data.frame() # SILENCE

str(s)
unique(names(s))