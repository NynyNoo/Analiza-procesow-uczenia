library(MASS)
library(caret)
library("C50")
setwd("D:/MGR/APU/lab4")
# podział bazy danych na zbiór treningowy i testowy
file_data <- read.csv("macbooki.csv")

train_data_precent <- (0.7)# ustalenie procentowego udziału danych treningowych
ind <- createDataPartition(file_data$ocena_klientow, p = train_data_precent, list = FALSE)
train_data <- file_data[ind,]# dane treningowe
test_data <- file_data[-ind,]# dane testowe

# konwersja zmiennej wynikowej na czynnik (factor)
train_data$ocena_klientow <- as.factor(train_data$ocena_klientow)
test_data$ocena_klientow <- as.factor(test_data$ocena_klientow)

oneTree <- C5.0(formula = ocena_klientow ~ ., data = train_data)
summary(oneTree)# podsumowanie modelu
plot(oneTree)# wyświetlenie graficznego przedstawienia modelu

# predykcja na danych testowych
test_data$nazwa <- factor(test_data$nazwa, levels = levels(train_data$nazwa))

bstTreePred <- predict(oneTree, test_data)# ustawienie poziomów zmiennej 'nazwa' w danych testowych na poziomy zmiennej 'nazwa' w danych treningowych
postResample(bstTreePred, test_data$ocena_klientow)# wyświetlenie wyników predykcji na danych testowych