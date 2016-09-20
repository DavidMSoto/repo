#https://www.kaggle.com/mrisdal/shelter-animal-outcomes/quick-dirty-randomforest


# Load packages 2
library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm

install.packages("randomForest")

# Read the data
#setwd("~/Kaggle/shelter/input")
train <- read.csv('perretes/input/train.csv', stringsAsFactors = F)
test <- read.csv('perretes/input/test.csv', stringsAsFactors = F)

# Rename the ID column so train & test match
names(train)[1] <- 'ID'

# And change ID in test to character
test$ID <- as.character(test$ID)

# Combine test & training data
full <- bind_rows(train, test)

#Before I start anything, I’m interested to see how the outcomes are distributed for the 4800 cats and 6656 dogs in the training set.


# Reshape
outcomes <- full[1:26729, ] %>%
  group_by(AnimalType, OutcomeType) %>%
  summarise(num_animals = n())

# Plot
ggplot(outcomes, aes(x = AnimalType, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Animal',
       title = 'Outcomes: Cats & Dogs') +
  theme_few()

#Looking at the variables, the first thing I notice is that the AgeuponOutcome variable is not in a format that will be easily usable for us. I’ll show you the first ten levels of 46 of this variable:

factor(full$AgeuponOutcome)[1:10]

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

summary(full$AgeinDays)


 # Cool, now everyone is the same age in cat days, dog days, fish days, siamang days, etc. It’s all animal days.
#We have a few more things to tweak. First, there are quite a few animals who sadly don’t have names. Maybe animals without names have … different … outcomes? There’s also one animal without SexuponOutcome specified. Finally, the DateTime variable isn’t very useful at the moment, but I want to keep things simple for now, too. I’ll just extract information at different levels of detail from the DateTime variable. My guess is that time of day and day of week will be the best predictors. I will fix all of these and a few more things in the next few code chunks below. Once we have all of our variables in a good place, I will use them (as appropriate) to predict those few missing age values.

#In case you get lost, here is a list of variables I’m tweaking or adding:
  
 # Name (no name versus named)
#Missing sex value for one animal
#Time variables extracted from DateTime
#Mix (is mix versus not a mix)
#Intactness (intact versus not versus unknown)
#Sex (M versus F versus unknown)


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

# Reshape
daytimes <- full[1:26729, ] %>%
  group_by(AnimalType, TimeofDay, OutcomeType) %>%
  summarise(num_animals = n())

# Plot
ggplot(daytimes, aes(x = TimeofDay, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  facet_wrap(~AnimalType) +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Animal',
       title = 'Outcomes by Time of Day: Cats & Dogs') +
  theme_few()


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


#Before I start anything, I’m interested to see how the outcomes are distributed for the 4800 cats and 6656 dogs in the training set.

# Reshape
intact <- full[1:26729, ] %>%
  group_by(AnimalType, Intact, OutcomeType) %>%
  summarise(num_animals = n())

# Plot
ggplot(intact, aes(x = Intact, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  facet_wrap(~AnimalType) +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Animal',
       title = 'Outcomes by Intactness: Cats & Dogs') +
  theme_few()

# Use rpart to predict the missing age values
#The analysis of variance is a commonly used method to determine differences between several samples. R provides a function to conduct ANOVA so: aov(model, data)
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

# Plot in ggplot2
ggplot(full[1:26729, ], aes(x = Lifestage, fill = OutcomeType)) + 
  geom_bar(position = 'fill', colour = 'black') +
  labs(y = 'Proportion', title = 'Animal Outcome: Babies versus Adults') +
  theme_few()

factorVars <- c('Name','OutcomeType','OutcomeSubtype','AnimalType',
                'SexuponOutcome','AgeuponOutcome','SimpleBreed','SimpleColor',
                'HasName','IsMix','Intact','Sex','TimeofDay','Lifestage')

full[factorVars] <- lapply(full[factorVars], function(x) as.factor(x))


#Let’s fit a randomForest model predicting OutcomeType. Do you think about 80% of my text will have ended up being dedicated to messing around with features once we reach the end?
# Split up train and test data
train <- full[1:26729, ]
test  <- full[26730:nrow(full), ]

# Set a random seed
set.seed(731)

# Build the model
rf_mod <- randomForest(OutcomeType ~ AnimalType+AgeinDays+Intact+HasName+Hour+Weekday+TimeofDay+SimpleColor+IsMix+Sex+Month, 
                       data = train, 
                       ntree = 600, 
                       importance = TRUE)

# Show model error
plot(rf_mod, ylim=c(0,1))
legend('topright', colnames(rf_mod$err.rate), col=1:6, fill=1:6)


# Get importance
importance    <- importance(rf_mod)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))



# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance)) +
  geom_bar(stat='identity', colour = 'black') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'lavender',
            fontface = 'bold') +
  labs(x = 'Variables', title = 'Relative Variable Importance') +
  coord_flip() + 
  theme_few()

# Predict using the test set
prediction <- predict(rf_mod, test, type = 'vote')

# Save the solution to a dataframe
solution <- data.frame('ID' = test$ID, prediction)

# Write it to file
write.csv(solution, 'rf_solution.csv', row.names = F)