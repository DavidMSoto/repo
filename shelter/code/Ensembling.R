

library(rpart)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(xgboost)
library(randomForest)

###############################################################################
# This section follows Megan Risdal's 'Quick and Dirty Random Forest' Script  #
# See her excellent script for more details                                   #
###############################################################################

train <- read.csv('perretes/input/train.csv', stringsAsFactors=FALSE)
test <- read.csv('perretes/input/test.csv', stringsAsFactors=FALSE)

train %>% head()
test %>% head()

colnames(train)[1] <- 'ID'
test$ID <- as.character(test$ID)

full <- bind_rows(train, test)

# Get the time value:
full$TimeValue <- sapply(full$AgeuponOutcome,  
                         function(x) strsplit(x, split = ' ')[[1]][1])

# Now get the unit of time:
full$UnitofTime <- sapply(full$AgeuponOutcome,  
                          function(x) strsplit(x, split = ' ')[[1]][2])

# Fortunately any "s" marks the plural, so we can just pull them all out
full$UnitofTime <- gsub('s', '', full$UnitofTime)

full$TimeValue  <- as.numeric(full$TimeValue)
full$UnitofTime <- as.factor(full$UnitofTime)

# Make a multiplier vector
multiplier <- ifelse(full$UnitofTime == 'day', 1,
                     ifelse(full$UnitofTime == 'week', 7,
                            ifelse(full$UnitofTime == 'month', 30, # Close enough
                                   ifelse(full$UnitofTime == 'year', 365, NA))))

# Apply our multiplier
full$AgeinDays <- full$TimeValue * multiplier

# Replace blank names with "Nameless"
full$Name <- ifelse(nchar(full$Name)==0, 'Nameless', full$Name)

# Make a name v. no name variable
full$HasName[full$Name == 'Nameless'] <- 0
full$HasName[full$Name != 'Nameless'] <- 1

# Replace blank sex with most common
full$SexuponOutcome <- ifelse(nchar(full$SexuponOutcome)==0, 
                              'Spayed Female', full$SexuponOutcome)

# Extract time variables from date (uses the "lubridate" package)
full$Hour    <- hour(full$DateTime)
full$Weekday <- wday(full$DateTime)
full$Month   <- month(full$DateTime)
full$Year    <- year(full$DateTime)

# Time of day may also be useful
full$TimeofDay <- ifelse(full$Hour > 5 & full$Hour < 11, 'morning',
                         ifelse(full$Hour > 10 & full$Hour < 16, 'midday',
                                ifelse(full$Hour > 15 & full$Hour < 20, 'lateday', 'night')))

# Put factor levels into the order we want
full$TimeofDay <- factor(full$TimeofDay, 
                         levels = c('morning', 'midday',
                                    'lateday', 'night'))

summary(full$AgeinDays)

# Use "grepl" to look for "Mix"
full$IsMix <- ifelse(grepl('Mix', full$Breed), 1, 0)

# Split on "/" and remove " Mix" to simplify Breed
full$SimpleBreed <- sapply(full$Breed, 
                           function(x) gsub(' Mix', '', 
                                            strsplit(x, split = '/')[[1]][1]))

# Use strsplit to grab the first color
full$SimpleColor <- sapply(full$Color, 
                           function(x) strsplit(x, split = '/| ')[[1]][1])

levels(factor(full$SimpleColor))

# Use "grepl" to look for "Intact"
full$Intact <- ifelse(grepl('Intact', full$SexuponOutcome), 1,
                      ifelse(grepl('Unknown', full$SexuponOutcome), 'Unknown', 0))

# Use "grepl" to look for sex
full$Sex <- ifelse(grepl('Male', full$SexuponOutcome), 'Male',
                   ifelse(grepl('Unknown', full$Sex), 'Unknown', 'Female'))

# Use rpart to predict the missing age values
age_fit <- rpart(AgeinDays ~ AnimalType + Sex + Intact + SimpleBreed + HasName, 
                 data = full[!is.na(full$AgeinDays), ], 
                 method = 'anova')

# Impute predicted age values where missing using "predict"
full$AgeinDays[is.na(full$AgeinDays)] <- predict(age_fit, full[is.na(full$AgeinDays), ])

# All gone? Yes.
sum(is.na(full$AgeinDays))

# Use the age variable to make a puppy/kitten variable
full$Lifestage[full$AgeinDays < 365] <- 'baby'
full$Lifestage[full$AgeinDays >= 365] <- 'adult'

full$Lifestage <- factor(full$Lifestage)

factorVars <- c('Name','OutcomeType','OutcomeSubtype','AnimalType',
                'SexuponOutcome','AgeuponOutcome','SimpleBreed','SimpleColor',
                'HasName','IsMix','Intact','Sex','TimeofDay','Lifestage')

full[factorVars] <- lapply(full[factorVars], function(x) as.factor(x))

# Split up train and test data
train <- full[1:26729, ]
test  <- full[26730:nrow(full), ]



######################### Break from Risdal script ############################
#                            Build xgboost model                              #
###############################################################################

# prepare for xgboost model - is data correct type?
class(train$OutcomeType)

# No. Need to change to numeric
y_train <- as.numeric(as.factor(train$OutcomeType)) - 1

# keep track of the labels
labels_train <- data.frame(train$OutcomeType, y_train)

# xgboost-specific design matrices
xgb_train <- xgb.DMatrix(model.matrix(~AnimalType+AgeinDays+Intact+
                                        HasName+Hour+Weekday+TimeofDay+
                                        SimpleColor+IsMix+Sex+Month, data=train),
                         label=y_train, missing=NA)
xgb_test <- xgb.DMatrix(model.matrix(~AnimalType+AgeinDays+Intact+
                                       HasName+Hour+Weekday+TimeofDay+
                                       SimpleColor+IsMix+Sex+Month, data=test), missing=NA)




# tune nrounds - uncomment first
# xgb.cv(data=xgb_train, label=y_train, nfold=5, nround=200, objective='multi:softprob',
#       num_class=5, eval_metric='mlogloss')

# looks like nrounds should be around 45

# build model
xgb_model <- xgboost(xgb_train, y_train, nrounds=45, objective='multi:softprob',
                     num_class=5, eval_metric='mlogloss',
                     early.stop.round=TRUE)

# make predictions
predictions <- predict(xgb_model, xgb_test)

# reshape predictions
xgb_preds <- data.frame(t(matrix(predictions, nrow=5, ncol=length(predictions)/5)))

# name columns
colnames(xgb_preds) <- c('Adoption', 'Died', 'Euthanasia', 'Return_to_owner', 'Transfer')

# attach ID column
xgb_preds['ID'] <- test['ID']

# quick peek - looks good
head(xgb_preds)

#################### Risdal random forest model ###############################
#                      Minor parameter tweaks                                 #
###############################################################################

# build model - i like verbose output ;-)
rf <- randomForest(OutcomeType~AnimalType+AgeinDays+Intact+
                     HasName+Hour+Weekday+TimeofDay+
                     SimpleColor+IsMix+Sex+Month, data=train,
                   importance=FALSE, do.trace=1, ntree=550)

# make predictions
rf_preds <- data.frame(predict(rf, test, type='vote'))

# take a look
head(rf_preds)


################## Break from Risdal script ###################################
#           How similar are predictions from different models?                #
###############################################################################

library(gridExtra)

g1 <- ggplot() + geom_point(aes(x=rf_preds$Adoption, y=xgb_preds$Adoption),
                            color='steelblue', alpha=0.3) +
  labs(x='Random Forest', y='XGBoost') +
  ggtitle('Adoption Predictions')
g2 <- ggplot() + geom_point(aes(x=rf_preds$Died, y=xgb_preds$Died),
                            color='steelblue', alpha=0.3) +
  labs(x='Random Forest', y='XGBoost') +
  ggtitle('Died Predictions')
g3 <- ggplot() + geom_point(aes(x=rf_preds$Euthanasia, y=xgb_preds$Euthanasia),
                            color='steelblue', alpha=0.3) + 
  labs(x='Random Forest', y='XGBoost') +
  ggtitle('Euthanasia Predictions')
g4 <- ggplot() + geom_point(aes(x=rf_preds$Return_to_owner, y=xgb_preds$Return_to_owner),
                            color='steelblue', alpha=0.3) + 
  labs(x='Random Forest', y='XGBoost') + 
  ggtitle('Return To Owner Predictions')
g5 <- ggplot() + geom_point(aes(x=rf_preds$Transfer, y=xgb_preds$Transfer),
                            color='steelblue', alpha=0.3) + 
  labs(x='Random Forest', y='XGBoost') +
  ggtitle('Transfer Predictions')

grid.arrange(g1, g2, g3, g4, g5, ncol=3, nrow=2)

##############################################
# Similar, but I bet we'll gain from an ensemble. We'll do a simple average of the predictions.
##############################################


ave_pred <- xgb_preds

# drop ID column for averaging
ave_pred <- ave_pred[,1:5]
ave_pred %>% head()

# average predictions
ave_pred <- 0.5*(ave_pred+rf_preds)

ave_pred %>% head()

# write the submission file
write.csv(ave_pred, 'rf_xgb_avg.csv', row.names=FALSE)


