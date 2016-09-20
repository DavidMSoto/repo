library(data.table)
#install.packages("h2o")
library(h2o)
setwd("c:/repo/redtraindata/")

# load source data
act_train <- fread('act_train.csv')
act_test <- fread('../input/act_test.csv')
act_people <- fread('../input/people.csv')

# enrich with supporting indicators
act_train$train <- 1
act_test$outcome <- -1
act_test$train <- 0

# combine data sources to one large dataset
act_data <- rbind(act_train, act_test)
full_data <- merge(act_data, act_people, by='people_id', suffixes = c('.act','.ppl'))

#
# space for variable transformations and/or feature engineering over the full_data
# one space for feature engineering is the main reason why first merge all sources
# to one data.table to split it into two H2O frames later
#

# dump train/test data tables as gzipped csvs for faster upload to h2o server (should
# be more efficient when not running H2O locally)
write.table(full_data[full_data$train==1,], gzfile('./transformed_train.csv.gz'),quote=F,sep=',',row.names=F)
write.table(full_data[full_data$train==0,], gzfile('./transformed_test.csv.gz'),quote=F,sep=',',row.names=F)

# initialize connection to H2O server
h2o.init(nthreads = -1)
train.hex <- h2o.uploadFile('./transformed_train.csv.gz', destination_frame='redhat_train')
test.hex <- h2o.uploadFile('./transformed_test.csv.gz', destination_frame='redhat_test')

# list of features for training
feature.names <- names(train.hex)
feature.names <- feature.names[! feature.names %in% c('people_id','outcome','train','activity_id')]

# train random forest model, use ntrees = 100 to get LB score ~0.96 (0.96004 in my case)
drf <- h2o.randomForest(x=feature.names, y='outcome', training_frame = train.hex, ntrees = 2)

# create output for making submission
sub <- data.frame(activity_id = as.vector(test.hex$activity_id), outcome = as.vector(predict(drf,test.hex)))
write.table(sub, './sub_h2o_drf.csv',quote=F,sep=',',row.names=F)
