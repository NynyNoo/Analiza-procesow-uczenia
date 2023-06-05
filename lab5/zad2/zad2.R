setwd("D:/MGR/APU/lab5/zad2")
library("rFerns")
library("randomForestSRC")
library("mlr")
data <- read.csv("macbooki.csv")
data <- data[3:8]
data[, 6] <- factor(data[, 6])

# Ustaw zadanie
task = makeClassifTask(
    id = deparse(substitute(data)), 
    data,                              
    target = "ocena_klientow",              
    weights = NULL, 
    blocking = NULL, 
    coordinates = NULL,
    positive = NA_character_,
    fixup.data = "warn",
    check.data = TRUE
)

# Utwórz listę algorytmów do sprawdzenia
lrns <- makeLearners(
    c(
        "lda",
        "rpart",
        "C50",
        "rFerns",
        "h2o.randomForest"
    ),
    type = "classif"
)

# Wykonaj benchmark
bench <- benchmark(
    learners = lrns,
    tasks = task,
    resamplings = cv5
)
bench
library(ggplot2)
library(dplyr)

df <- as.data.frame(bench)

plot <- ggplot(df, aes(x = learner.id, y = mmce)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Średni błąd klasyfikacji",
         x = "Algorytm",
         y = "Błąd klasyfikacji") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot)