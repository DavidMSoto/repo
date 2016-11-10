

# notas -------------------------------------------------------------------


# git hub
# cd /c/repo/root
#git init
#git commit -am "v2"
#git push origin master
#git add *


#setwd("c:/repo/root/")
#https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md
#http://lifehacker.com/5812578/the-coffee-lovers-guide-to-tea

#http://trevorstephens.com/kaggle-titanic-tutorial/getting-started-with-r/


rm(list = ls())


# read --------------------------------------------------------------------


readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv( url( paste(path.name, file.name, sep="") ), 
            colClasses=column.types,
            na.strings=missing.types )
}


Titanic.path <- "https://raw.github.com/wehrley/Kaggle_Titanic/master/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA", "")
train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
test.column.types <- train.column.types[-2]     # # no Survived column in test.csv

train.raw <- readData(Titanic.path, train.data.file, 
                      train.column.types, missing.types)
df.train <- train.raw

test.raw <- readData(Titanic.path, test.data.file, 
                     test.column.types, missing.types)
df.infer <- test.raw 


# Data Munging feature enginering  ------------------------------------------------------------


###########AGE 
## function for extracting honorific (i.e. title) from the Name feature
head(df.train$Name, n=10L)
getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
  data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
  return (data$Title)
}   

df.train$Title <- getTitle(df.train)
unique(df.train$Title)

options(digits=2)

  library(Hmisc)

bystats(df.train$Age, df.train$Title,   fun=function(x)c(Mean=mean(x),Median=median(x)))


## list of titles with missing Age value(s) requiring imputation
titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")

# hace la media separada por las categorias de los titulos nobiliarios
imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[ which( filter.var == v)] <- impute(impute.var[ 
      which( filter.var == v)])
  }
  return (impute.var)
}
#For example, the single record with a missing Age value and Title="Dr" will be assigned the median of the ages from the 6 records with Title="Dr" which do have age data.
df.train$Age[which(df.train$Title=="Dr")]
df.train$Age <- imputeMedian(df.train$Age, df.train$Title, titles.na.train)
df.train$Age[which(df.train$Title=="Dr")]
summary(df.train$Age)
#end of Age enginering feature, for now .. we have used a new variable (Tittle) to be able to do it. 


############Embarked / PORT two missings - a lo mejor esq habia 2 polizontes q sobrevivien ...
#it should be fine to replace those missings with "S", the most common value.

#que edades tienen los que se colaron ??

summary(df.train$Embarked)
df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'


############Fare / many of them are 0 
summary(df.train$Fare)

subset(df.train, Fare < 7)[order(subset(df.train, Fare < 7)$Fare, 
                                 subset(df.train, Fare < 7)$Pclass), 
                           c("Age", "Title", "Pclass", "Fare")]

## impute missings on Fare feature with median fare by Pclass
df.train$Fare[ which( df.train$Fare == 0 )] <- NA
df.train$Fare <- imputeMedian(df.train$Fare, df.train$Pclass, 
                              as.numeric(levels(df.train$Pclass)))


df.train$Title <- factor(df.train$Title,
                         c("Capt","Col","Major","Sir","Lady","Rev", "Dr","Don","Jonkheer","the Countess","Mrs",
                           "Ms","Mr","Mme","Mlle","Miss","Master"))


boxplot(df.train$Age ~ df.train$Title,     main="Passenger Age by Title", xlab="Title", ylab="Age")

############title
## function for assigning a new title value to old title(s) 

##ver los viejos titulos



changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {

    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data$Title)
}

describe(df.train$Title )
summary(df.train$Title)
describe(df.train$Title)
## Title consolidation

#sapply(df.train$Title, class)

## cambiado Noble por Jonkheer
df.train$Title <- changeTitles(df.train, 
                               c("Capt", "Col", "Don", "Dr", 
                                 "Jonkheer", "Lady", "Major", 
                                 "Rev", "Sir"),
                               "Jonkheer")
##nueva pieza de codigo
df.train$Title[which(is.na(df.train$Title))] <- 'Jonkheer'



df.train$Title <- changeTitles(df.train, c("the Countess", "Ms"),            "Mrs")
df.train$Title <- changeTitles(df.train, c("Mlle", "Mme"), "Miss")
df.train$Title <- as.factor(df.train$Title)

require(plyr)     # for the revalue function 
require(stringr)  # for the str_sub function

## test a character as an EVEN single digit
isEven <- function(x) x %in% c("0","2","4","6","8") 
## test a character as an ODD single digit
isOdd <- function(x) x %in% c("1","3","5","7","9") 

## function to add features to training or test data frames
featureEngrg <- function(data) {
 
   head(data$PassengerId, n = 1L)

  ## Using Fate ILO Survived because term is shorter and just sounds good
  data$Fate <- data$Survived
  ## Revaluing Fate factor to ease assessment of confusion matrices later
  data$Fate <- revalue(data$Fate, c("1" = "Survived", "0" = "Perished"))
  ## Boat.dibs attempts to capture the "women and children first"
  ## policy in one feature.  Assuming all females plus males under 15
  ## got "dibs' on access to a lifeboat
  data$Boat.dibs <- "No"
  data$Boat.dibs[which(data$Sex == "female" | data$Age < 15)] <- "Yes"
  data$Boat.dibs <- as.factor(data$Boat.dibs)
  ## Family consolidates siblings and spouses (SibSp) plus
  ## parents and children (Parch) into one feature
  data$Family <- data$SibSp + data$Parch
  ## Fare.pp attempts to adjust group purchases by size of family
  data$Fare.pp <- data$Fare/(data$Family + 1)
  ## Giving the traveling class feature a new look
  data$Class <- data$Pclass
  data$Class <- revalue(data$Class, 
                        c("1"="First", "2"="Second", "3"="Third"))
  ## First character in Cabin number represents the Deck 
  data$Deck <- substring(data$Cabin, 1, 1)
  data$Deck[ which( is.na(data$Deck ))] <- "UNK"
  data$Deck <- as.factor(data$Deck)
  ## Odd-numbered cabins were reportedly on the port side of the ship
  ## Even-numbered cabins assigned Side="starboard"
  data$cabin.last.digit <- str_sub(data$Cabin, -1)
  data$Side <- "UNK"
  data$Side[which(isEven(data$cabin.last.digit))] <- "port"
  data$Side[which(isOdd(data$cabin.last.digit))] <- "starboard"
  data$Side <- as.factor(data$Side)
  data$cabin.last.digit <- NULL
  return (data)
}

## add remaining features to training data frame
df.train <- featureEngrg(df.train)




train.keeps <- c("Fate", "Sex", "Boat.dibs", "Age", "Title", "Class", "Deck", "Side", "Fare", "Fare.pp", "Embarked", "Family")
df.train.munged <- df.train[train.keeps]


# visualization -----------------------------------------------------------


require(Amelia)
missmap(df.train, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

barplot(table(df.train$Survived), names.arg = c("Perished", "Survived"), 
        main="Survived (passenger fate)", col="black")
barplot(table(df.train$Pclass), names.arg = c("first", "second", "third"), 
        main="Pclass (passenger traveling class)", col="firebrick")
barplot(table(df.train$Sex), 
        main="Sex (gender)", col="darkviolet")

hist(df.train$Age, main="Age", xlab = NULL, col="brown")

barplot(table(df.train$SibSp), 
        main="SibSp (siblings + spouse aboard)", col="darkblue")
barplot(table(df.train$Parch), main="Parch (parents + kids aboard)", col="gray50")

hist(df.train$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")

barplot(table(df.train$Embarked), names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="sienna")
# more passengers perished than survived
# about twice as many passengers in 3rd class than in either 1st or 2nd
# male passengers far outnumbered females

mosaicplot(df.train$Pclass ~ df.train$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

mosaicplot(df.train$Sex ~ df.train$Survived, 
           main="Passenger Fate by Gender", shade=FALSE, color=TRUE, 
           xlab="Sex", ylab="Survived")

boxplot(df.train$Age ~ df.train$Survived, 
        main="Passenger Fate by Age",
        xlab="Survived", ylab="Age")

mosaicplot(df.train$Embarked ~ df.train$Survived, 
           main="Passenger Fate by Port of Embarkation",
           shade=FALSE, color=TRUE, xlab="Embarked", ylab="Survived")

require(corrgram)
#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("corrgram")

corrgram.data <- df.train
## change features of factor type to numeric type for inclusion on correlogram
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
#corrgram.data$Embarked <- revalue(corrgram.data$Embarked, 
#                                  c("C" = 1, "Q" = 2, "S" = 3))
## generate correlogram
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age", 
                   "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.ellipse, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Titanic Training Data")




summary(df.train$Age)
names(df.train)


boxplot(df.train$Age ~ df.train$Pclass, 
        main="Passenger traveling calss by age",
        xlab="Survived", ylab="Age")

head(df.train$Name, n=10L)



### Models  -------
require(caret) 

set.seed(23)
training.rows <- createDataPartition(df.train.munged$Fate,  p = 0.8, list = FALSE) # *** changed
## split training data into train batch and test batch

train.batch <- df.train.munged[training.rows, ]
test.batch <- df.train.munged[-training.rows, ]


## GLM Logistic regression   ---------------------------------------------------------

Titanic.logit.1 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare, 
                       data = train.batch, family=binomial("logit"))
# chi-square statistic, which is basically a measure of the goodness of fit of observed values to expected values
Titanic.logit.1
1 - pchisq(332.2, df=8)

anova(Titanic.logit.1, test="Chisq")

Titanic.logit.2 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare.pp,                        
                       data = train.batch, family=binomial("logit"))

anova(Titanic.logit.2, test="Chisq")

glm(Fate ~ Sex + Class + Age + Family + Embarked, 
    data = train.batch, family=binomial("logit"))



# using  cv over glm  ---------------------------------------------------------
## Define control function to handle optional arguments for train function
## Models to be assessed based on largest absolute area under ROC curve
library(pROC)

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 10,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)
set.seed(35)
glm.tune.1 <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)

glm.tune.1
summary(glm.tune.1)


set.seed(35)
glm.tune.2 <- train(Fate ~ Sex + Class + Age + Family + I(Embarked=="S"),
                    data = train.batch, method = "glm",
                    metric = "ROC", trControl = cv.ctrl)
summary(glm.tune.2)

set.seed(35)


glm.tune.3 <- train(Fate ~ Sex + Class + Title + Age + Family + I(Embarked=="S"), 
                    data = train.batch, method = "glm",
                    metric = "ROC", trControl = cv.ctrl)
summary(glm.tune.3)


set.seed(35)
glm.tune.4 <- train(Fate ~ Class + I(Title=="Mr") + I(Title=="Jonkheer") 
                    + Age + Family + I(Embarked=="S"), 
                    data = train.batch, method = "glm",
                    metric = "ROC", trControl = cv.ctrl)

summary(glm.tune.4)


set.seed(35)

glm.tune.5 <- train(Fate ~ Class + I(Title=="Mr") + I(Title=="Jonkheer") 
                    + Age + Family + I(Embarked=="S") 
                    + I(Title=="Mr"&Class=="Third"), 
                    data = train.batch, 
                    method = "glm", metric = "ROC", 
                    trControl = cv.ctrl)

summary(glm.tune.5)



#  boosting (ada) cv -----------------------------------------------------
#tarda un cojon ** creo que a partir de aqui por simplicidad
#usa la misma formula que para el GLM pero esta usando CV, 
#es decir en lugar de hacer un analisis inical con las salidas
#del modelo se pone a hacer la cv usando la formula que ha deducido
#del primer analisis *using cv over glm*

library(ada)
ada.grid <- expand.grid(.iter = c(50, 100),
                        .maxdepth = c(4, 8),
                        .nu = c(0.1, 1))

set.seed(35)
ada.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, 
                  data = train.batch,
                  method = "ada",
                  metric = "ROC",
                  tuneGrid = ada.grid,
                  trControl = cv.ctrl)

ada.tune

plot(ada.tune) 


# Random Forest (RF) cv ------------------------------------------------------

rf.grid <- data.frame(.mtry = c(2, 3))
set.seed(35)
rf.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, 
                 data = train.batch,
                 method = "rf",
                 metric = "ROC",
                 tuneGrid = rf.grid,
                 trControl = cv.ctrl)

rf.tune


# support vector machine (SVM) cv ---------------------------------------------

set.seed(35)
svm.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, 
                  data = train.batch,
                  method = "svmRadial",
                  tuneLength = 9,
                  preProcess = c("center", "scale"),
                  metric = "ROC",
                  trControl = cv.ctrl)

svm.tune

#  Model Evaluation  ------------------------------------------------------
#  confusion Matrix  ------------------------------------------------------

## Logistic regression model
glm.pred <- predict(glm.tune.5, test.batch)
confusionMatrix(glm.pred, test.batch$Fate)

## Boosted model
ada.pred <- predict(ada.tune, test.batch)
confusionMatrix(ada.pred, test.batch$Fate)
## Random Forest model
rf.pred <- predict(rf.tune, test.batch)
 confusionMatrix(rf.pred, test.batch$Fate)

 ## SVM model 
 svm.pred <- predict(svm.tune, test.batch)
 confusionMatrix(svm.pred, test.batch$Fate)
 
 
 
 
#  ROC curves --------------------------------------------------------------
 #install.packages("ROCR")
 #require("something")
 #library(ROCR)
## Logistic regression model (BLACK curve)
 glm.probs <- predict(glm.tune.5, test.batch, type = "prob")
 glm.ROC <- roc(response = test.batch$Fate,  
                predictor = glm.probs$Survived,
                levels = levels(test.batch$Fate))
 
 plot(glm.ROC, type="S")   
 ## Area under the curve: 0.8609 
 
 
## Boosted model (GREEN curve)
 ada.probs <- predict(ada.tune, test.batch, type = "prob")
 ada.ROC <- roc(response = test.batch$Fate,
                predictor = ada.probs$Survived,
                levels = levels(test.batch$Fate))
 plot(ada.ROC, add=TRUE, col="green")    
 ## Area under the curve: 0.8759
 
## Random Forest model (RED curve)
 rf.probs <- predict(rf.tune, test.batch, type = "prob")
 rf.ROC <- roc(response = test.batch$Fate,
               predictor = rf.probs$Survived,
               levels = levels(test.batch$Fate))
 plot(rf.ROC, add=TRUE, col="red") 
 ## Area under the curve: 0.8713
 
 ## SVM model (BLUE curve)
 svm.probs <- predict(svm.tune, test.batch, type = "prob")
 svm.ROC <- roc(response = test.batch$Fate,
                predictor = svm.probs$Survived,
                levels = levels(test.batch$Fate))
 plot(svm.ROC, add=TRUE, col="blue")
 ## Area under the curve: 0.8077
 
 
 
 cv.values <- resamples(list(Logit = glm.tune.5, Ada = ada.tune, 
                             RF = rf.tune, SVM = svm.tune))
 dotplot(cv.values, metric = "ROC")
 
 
 

# submision  glm.tune.5---------------------------------------------------------------
 
 
 # get titles
 df.infer$Title <- getTitle(df.infer)
 # impute missing Age values
 df.infer$Title <- changeTitles(df.infer, c("Dona", "Ms"), "Mrs")
 
 titles.na.test <- c("Master", "Mrs", "Miss", "Mr")
 df.infer$Age <- imputeMedian(df.infer$Age, df.infer$Title, titles.na.test)
 
 
 # consolidate titles
 df.infer$Title <- changeTitles(df.infer, c("Col", "Dr", "Rev", "Master"), "Noble") #cambiado 
 df.infer$Title <- changeTitles(df.infer, c("Mlle", "Mme"), "Miss")
 df.infer$Title <- as.factor(df.infer$Title)
 unique(df.infer$Title)
 
 # impute missing fares
 df.infer$Fare[ which( df.infer$Fare == 0)] <- NA
 df.infer$Fare <- imputeMedian(df.infer$Fare, df.infer$Pclass, 
                               as.numeric(levels(df.infer$Pclass)))
 # add the other features
 df.infer <- featureEngrg(df.infer)

 head(df.infer$PassengerId, n = 1L)
 # data prepped for casting predictions
 test.keeps <- train.keeps[-1]
 pred.these <- df.infer[test.keeps]
 
 # use the logistic regression model to generate predictions
 Survived <- predict(rf.tune, newdata = pred.these)
 
 # reformat predictions to 0 or 1 and link to PassengerId in a data frame
 Survived <- revalue(Survived, c("Survived" = 1, "Perished" = 0))
 predictions <- as.data.frame(Survived)
 predictions$PassengerId <- df.infer$PassengerId
 
 # write predictions to csv file for submission to Kaggle
 setwd("/Users/davidmonteagudo/kaggle/data/titanic/output/")
 write.csv(predictions[,c("PassengerId", "Survived")], 
file="Titanic_predictions_rf4.csv", row.names=FALSE, quote=FALSE)

 
 
 
 
 
 
 
 
 
 
 
 
 
 