# J Pocahontas Olson    April 2016
library(rpart)
library(randomForest)
library(plyr)           # Remapping factor levels

# train and test data set
train <- read.csv("train.csv")
test  <- read.csv("test.csv")

# Add variable Survived to the test dataset with 0 Values (gets overwritten later)
test$Survived <- 0

# Combine train and test datasets, name all_data 
all_data <- rbind(train,test)

# Add new variable Title to all_data from Name
# First we should convert Name to a character variable
all_data$Name <- as.character(all_data$Name)
all_data$Title <- sapply(all_data$Name,FUN=function(x){strsplit(x,split="[,.]")[[1]][2]})
all_data$Title <- sub(" ","",all_data$Title)
# Group some of those titles
all_data$Title[all_data$Title %in% c("Capt","Col","Don","Dr","Major","Rev")] <- "Sir"
all_data$Title[all_data$Title %in% c("Dona","Jonkheer","Lady","the Countess", "Mme","Mlle","Ms")] <- "Lady"
all_data$Title[is.na(all_data$Title)] <- "NoTitle"
# Factor tweaked values
all_data$Title <- factor(all_data$Title)

# Family size
all_data$FamilySize = all_data$SibSp + all_data$Parch + 1
all_data$Surname <- sapply(all_data$Name, FUN=function(x){strsplit(x, split='[,.]')[[1]][1]})
all_data$FamilyID <- paste(as.character(all_data$FamilySize), all_data$Surname, sep="")
all_data$FamilyID[all_data$FamilySize <= 2] <- 'Small'
famIDs <- data.frame(table(all_data$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
all_data$FamilyID[all_data$FamilyID %in% famIDs$Var1] <- 'Small'
all_data$FamilyID <- factor(all_data$FamilyID)


# Add new variable Deck to all_data from Cabin
# First, remove incorrect input in Cabin[340] which is T
all_data$Cabin[340] <- ''
all_data$Deck <- sapply(all_data$Cabin, FUN= function(x){substr(x,1,1)})

# Write a function for filling missing values in Deck
# the vector and probabilities in sample() are extracted from dataset
deck_missing <- function(x){
if (all_data$Pclass[x] == 1) {
x <- sample(c("A", "B", "C", "D", "E"), size = 1, replace = TRUE, prob = c(0.09, 0.25, 0.37, 0.16, 0.13))
} else if( all_data$Pclass[x] == 2){
x <- sample(c("D", "E", "F"), size = 1, replace = TRUE, prob = c(0.2, 0.3, 0.5))
}else {
x <- sample(c("E", "F", "G"), size = 1, replace = TRUE, prob = c(0.1, 0.3, 0.6))
}}

# Fill missing values in Deck
all_data$Deck[all_data$Deck == ""] <- sapply(which(all_data$Deck == ""), deck_missing) 
all_data$Deck <- factor(all_data$Deck)

# Fill missing values in Age
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + Deck + FamilySize + FamilyID, data = all_data[!is.na(all_data$Age),], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])
# Fill missing values in Fare
predicted_fare <- rpart(Fare ~ Pclass + Sex +  Age + SibSp + Parch + Embarked + Title + Deck + FamilySize + FamilyID, data = all_data[!is.na(all_data$Age),], method = "anova")
all_data$Fare[is.na(all_data$Fare)] <- predict(predicted_fare, all_data[is.na(all_data$Fare),])

# Fill missing values in Embarked
all_data$Embarked[which(all_data$Embarked == '')] <- "S"


# Split the data back into train and test
train <- all_data[1:891,]
test <- all_data[892:1309,]
# set seed for reproducibility
set.seed(88)

# Apply Random Forest
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Embarked + Title + Deck + FamilySize + FamilyID, data = train, importance = TRUE, ntree = 750, mtry=3, nodesize=2)


# Use my_forest to predict Survived in test
my_prediction <- predict(my_forest, newdata = test)

# Accuracy
## This isn't meaningful, I don't have a known-survival test
## calculate the confusion matrix
titanic.rf.confusion <- table(my_prediction, test$Survived)
print(titanic.rf.confusion)
## accuracy
titanic.rf.accuracy <- sum(diag(titanic.rf.confusion)) / sum(titanic.rf.confusion)
print(titanic.rf.accuracy)

# Create data.frame and csv file for submission
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "Kaggle submission - Titanic.csv", row.names = FALSE, quote = FALSE)

