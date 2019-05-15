# sys6018-competition-titanic

############ Training Model ###############
# Importing the dataset
dataset = read.csv('train.csv')
# drop 'Name' and 'Cabin' columns
dataset <- dataset[, !(colnames(dataset) %in% c('Name', 'Cabin', 'Ticket'))]

# missing values
# find which column has missing values
indx <- apply(dataset, 2, function(x) any(is.na(x)))
# names of columns with missing values
colnames(dataset)[indx]
# "Age"  

# impute missing values of "Age"
dataset_male <- dataset[dataset$Sex == 'male',]
dataset_female <- dataset[dataset$Sex == 'female',]
dataset_male$Age = ifelse(is.na(dataset_male$Age),
                          ave(dataset_male$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                          dataset_male$Age)
dataset_female$Age = ifelse(is.na(dataset_female$Age),
                            ave(dataset_female$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                            dataset_female$Age)

# the new dateset after preprocessing
df <- rbind(dataset_male, dataset_female)

# encoding categorical data:
df$Pclass <- factor(df$Pclass)
df$Sex <- factor(df$Sex)
df$SibSp <- factor(df$SibSp)
df$Embarked <- factor(df$Embarked)
df$Survived <- factor(df$Survived)

# split training data for cross validation
library(caTools)                
set.seed(123)
# returned value is True/False for each observations
split = sample.split(df$Survived, SplitRatio = 0.8) 
# if it is True, the value goes to training set
training_set = subset(df, split == TRUE)              
test_set = subset(df, split == FALSE)

# Random Forest Classification
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-2],
                          y = training_set$Survived,
                          ntree = 90)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-2])

# Making the Confusion Matrix
cm = table(test_set[, 2], y_pred)
cm

set.seed(123)
classifier_train = randomForest(x = df[-2],
                                y = df$Survived,
                                ntree = 90)

############ Prediction ###############
# Final Prediction
test = read.csv('test.csv')

# drop 'Name' and 'Cabin' columns
test <- test[, !(colnames(test) %in% c('Name', 'Cabin', 'Ticket'))]

# missing values
# find which column has missing values
indx <- apply(test, 2, function(x) any(is.na(x)))
# names of columns with missing values
colnames(test)[indx]
# "Age"  "Fare"

# impute missing values of "Fare"
test$Fare = ifelse(is.na(test$Fare),
                   ave(test$Fare, FUN = function(x) mean(x, na.rm = TRUE)),
                   test$Fare)

# impute missing values of "Age"
test_male <- test[test$Sex == 'male',]
test_female <- test[test$Sex == 'female',]

test_male$Age = ifelse(is.na(test_male$Age),
                       ave(test_male$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                       test_male$Age)
test_female$Age = ifelse(is.na(test_female$Age),
                         ave(test_female$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                         test_female$Age)
# the new dateset after preprocessing
df_2 <- rbind(test_male, test_female)

# encoding categorical data
df_2$Pclass <- factor(df_2$Pclass)
df_2$Sex <- factor(df_2$Sex)
df_2$SibSp <- factor(df_2$SibSp)
df_2$Embarked <- factor(df_2$Embarked, levels = levels(df$Embarked)) 

# Predicting the Test set results
y_predicted = predict(classifier_train, df_2)

df_result <- data.frame(df_2$PassengerId, y_predicted)
colnames(df_result) <- c("PassengerId", "Survived")
df_result
write.csv(df_result, file = "~/Desktop/SYS6018/homework/competiton01_Titanic/titanic_prediction.csv", row.names = FALSE)