library(MASS)
library(caret)
library("C50")
setwd("D:/MGR/APU/lab4")
# split database for train and test
file_data <- read.csv("macbooki.csv")

train_data_precent <- (0.7)
ind <- createDataPartition(file_data$ocena_klientow, p = train_data_precent, list = FALSE)
train_data <- file_data[ind,]
test_data <- file_data[-ind,]

# convert outcome variable to factor
train_data$ocena_klientow <- as.factor(train_data$ocena_klientow)
test_data$ocena_klientow <- as.factor(test_data$ocena_klientow)

oneTree <- C5.0(formula = ocena_klientow ~ ., data = train_data)
summary(oneTree)
plot(oneTree)

# set factor levels in test data to be the same as in train data
test_data$nazwa <- factor(test_data$nazwa, levels = levels(train_data$nazwa))

bstTreePred <- predict(oneTree, test_data)
postResample(bstTreePred, test_data$ocena_klientow)