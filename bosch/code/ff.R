

library(data.table)
setwd("/Users/davidmonteagudo/kaggle/data/bosch/input/")

library(doMC)
registerDoMC(cores = 4)

system.time(DT1 <- fread("../input/train_categorical.csv"))

library(ff)

# creating the file
school.ff <- read.csv.ffdf(file="/Users/sundar/dev/mixed_matrix_SAT__College_Board__2010_School_Level_Results.csv")

#creates a ffdf object 
class(school.ff)

summary(DT1)