setwd("D:/MGR/APU/lab5/zad1")
library(rpart)

data(infert)

summary(infert)

set.seed(123)
train_indices <- sample(1:nrow(infert), nrow(infert) * 0.7)
train <- infert[train_indices, ]
test <- infert[-train_indices, ]

model <- rpart(case~., data=train, method="class")

plot(model)
text(model)

predictions <- predict(model, newdata=test, type="class")

table(predictions, test$case)
