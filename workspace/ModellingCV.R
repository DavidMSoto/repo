


# bechmark -------------------------------------------------------------------


train.keeps <- c("Survived", "Sex", "Boat.dibs", "Age", "Title", "Class", "Deck", 
                 "Side", "Fare", "Fare.pp", "Embarked", "Family", "FamilySize", "FamilySizeAdj")
df.train.munged <- df.train[train.keeps]

### Models  -------
require(caret) 

set.seed(23)
training.rows <- createDataPartition(df.train.munged$Survived,  p = 0.8, list = FALSE) # *** changed
## split training data into train batch and test batch

train.batch <- df.train.munged[training.rows, ]
test.batch <- df.train.munged[-training.rows, ]



# rpart  -------------------------------------------------------------------

modelaccuracy <- function(test, rpred) {
  result_1 <- test$Survived == rpred
  sum(result_1) / length(rpred)
}

checkaccuracy <- function(accuracy) {
  if (accuracy > bestaccuracy) {
    bestaccuracy <- accuracy
    assign("bestaccuracy", accuracy, envir = .GlobalEnv)
    label <- 'better'
  } else if (accuracy < bestaccuracy) {
    label <- 'worse'
  } else {
    label <- 'no change'
  }
  label
}


fol <- formula(Survived ~ Age + Sex  + FamilySizeAdj + I(Title=="Jonkheer"&Class=="Third") )

rmodel <- rpart(fol, method="class", data=train.batch)
rpred <- predict(rmodel, newdata=test.batch, type="class")
accuracy <- modelaccuracy(test.batch, rpred)
accuracyLabel <- checkaccuracy(accuracy)
print(c("accuracy n", accuracy, accuracyLabel))				# label


# rpart cv  -------------------------------------------------------------------
# pero esto para que vale para inferir variables como el mice o es un modelo ?
library(caret)

tc <- trainControl("cv",10)
rpart.grid <- expand.grid(.cp=0.2)
 
   (train.rpart <- train(Species ~., data=iris, method="rpart",trControl=tc,tuneGrid=rpart.grid))

# glm  -------------------------------------------------------------------


Titanic.logit.2 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare.pp,                        
                       data = train.batch, family=binomial("logit"))

anova(Titanic.logit.2, test="Chisq")

# glm cv  -------------------------------------------------------------------


cv.ctrl <- trainControl(method = "repeatedcv", repeats = 10,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)

glm.tune.5 <- train(Fate ~ Class + I(Title=="Mr") + I(Title=="Jonkheer") 
                    + Age + Family + I(Embarked=="S") 
                    + I(Title=="Mr"&Class=="Third"), 
                    data = train.batch, 
                    method = "glm", metric = "ROC", 
                    trControl = cv.ctrl)

summary(glm.tune.5)


