# J. Pocahontas Olson  April 2016

# Load in libraries
library(plyr)      # for join()
library(lattice)   # for histogram()
library(ggplot2)   # for ggplot()
library(rpart)     # for predictive model building

#3.) Load in .data and .test files
adult.train <- read.csv("adult.data", header = FALSE)
adult.test <- read.csv("adult.test", header = FALSE)

#3.) Use the .names file to assign column names to all the columns
column.names <- c("age","workclass", "final-weight", "education", "education-num", "marital-status", "occupation", "relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country", "salary split")
names(adult.train) <- column.names
names(adult.test) <- column.names

head(adult.train)
head(adult.test)

#3.) Combine the .data and .test into one data frame.
adult.all <- join(adult.train, adult.test, type = "full")

#3.) Cast categorical columns to factor and assign human-meaningful names to the levels if needed.
names(adult.all)
str(adult.all)
adult.all$age              <- as.numeric(adult.all$age)
adult.all$`final-weight`   <- as.numeric(adult.all$`final-weight`)
adult.all$`education-num`  <- as.numeric(adult.all$`education-num`)
adult.all$`capital-gain`   <- as.numeric(adult.all$`capital-gain`)
adult.all$`capital-loss`   <- as.numeric(adult.all$`capital-loss`)
adult.all$`hours-per-week` <- as.numeric(adult.all$`hours-per-week`)
levels(adult.all$`salary split`) <- c("<=50K",">50K","<=50K",">50K")

#4.)  Write new data frame out to "adult.csv", including column headers.
write.csv(adult.all, file = "adult.csv", col.names = TRUE)


##  VISUALIZATION  ##
#5.) Investigate the proportions of several categorical columns
plot(adult.all)
#      sex
summary(adult.all)
#      marital-status
pie(table(adult.all$`marital-status`))
ggplot(data=adult.all, aes(x=`marital-status`)) + geom_bar() + 
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + xlab("Marital Status")

#      income
ggplot(data = adult.all, aes(x=adult.all$`salary split`, fill=sex)) + geom_bar() + xlab("Salary")

#      occupation
pie(table(adult.all$occupation))
ggplot(data=adult.all, aes(x=occupation)) + geom_bar() + 
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + xlab("Occupation")

#      race
pie(table(adult.all$race))
ggplot(data=adult.all, aes(x=race, fill=`salary split`)) + geom_bar() + 
  theme(axis.text.x = element_text(angle=60, hjust = 1)) + xlab("Race")

#      relationship
histogram(~ adult.all$relationship, xlab = "Relationship")  # technically, shouldn't use w/ categorical, b/c it wants to bin itself
histogram(~ adult.all$relationship | adult.all$sex, xlab = "Relationship")
ggplot(adult.all, aes(x=relationship, fill=sex)) + geom_bar() #+ scale_y_continuous(labels="percent")

# Make density and box plots of age, education-num, and hours-per-week
#      age
densityplot(~ age, data=adult.all, groups=sex, plot.points=F, lwd=3,
            auto.key=list(corner=c(0,0), x=0.7, y=0.8))
boxplot(age ~ sex, data=adult.all)

#      education-num
densityplot(~ `education-num`, data = adult.all)
boxplot(`education-num` ~ sex, data=adult.all)

#      hours-per-week
densityplot(~ `hours-per-week`, data = adult.all)
boxplot(`hours-per-week` ~ sex, data=adult.all)
sort(unique(adult.all$`hours-per-week`))
histogram(~ adult.all$`hours-per-week`)


# Segment box plots by the categorical variables you investigated in part 5. 
boxplot(age ~ sex, data=adult.all)
boxplot(`education-num` ~ sex, data=adult.all)
boxplot(`hours-per-week` ~ sex, data=adult.all)


# Predict Income column
## Specify the stopping conditions
adult.dt.parameters <- rpart.control(minsplit=20, minbucket=7, cp=0.01, maxdepth=30)

## Fit decision model to training set
## Use parameters from above and Gini index for splitting
adult.dt.model <- rpart(`salary split` ~ ., data = adult.train, 
                       control=adult.dt.parameters, parms=list(split="gini"))




