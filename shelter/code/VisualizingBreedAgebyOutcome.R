

library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)

train <- read.csv('perretes/input/train.csv', stringsAsFactors = F)



# Get the time value:
train$TimeValue <- sapply(train$AgeuponOutcome,  
                         function(x) strsplit(x, split = ' ')[[1]][1])

# Now get the unit of time:
train$UnitofTime <- sapply(train$AgeuponOutcome,  
                          function(x) strsplit(x, split = ' ')[[1]][2])

# Fortunately any "s" marks the plural, so we can just pull them all out
train$UnitofTime <- gsub('s', '', train$UnitofTime)

train$TimeValue  <- as.numeric(train$TimeValue)
train$UnitofTime <- as.factor(train$UnitofTime)

# Make a multiplier vector
multiplier <- ifelse(train$UnitofTime == 'day', 1,
                     ifelse(train$UnitofTime == 'week', 7,
                            ifelse(train$UnitofTime == 'month', 30, # Close enough
                                   ifelse(train$UnitofTime == 'year', 365, NA))))

# Apply our multiplier
train$age <- train$TimeValue * multiplier / 365



summary(train$age)



cat <- filter(train, AnimalType == "Cat")



cat %>% count(age) %>%  
  ggplot(aes(x = age, y = n)) + 
  geom_bar(stat = "identity", alpha = 0.5) +
  theme_void() + 
  ylab("Count") + 
  ggtitle("Outcome Type by Age upon Outcome - Cats") +
  theme(axis.title.y = element_text(angle = 90, color = "#737373" )) -> g1

ggplot(cat, aes(x = age, fill = OutcomeType)) +
  geom_bar(stat = "count", position = "fill",  alpha = 0.9) +
  scale_fill_brewer(palette = "Set1") +
  theme_void() + 
  ylab("Relative Count by Outcome") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 11, color = "#737373"),
        legend.position = "bottom",
        axis.title.y = element_text(angle = 90, color = "#737373")) -> g2

grid.arrange(g1, g2, ncol = 1, heights = c(1,3))