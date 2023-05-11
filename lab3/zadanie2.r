library(neuralnet)
library(ggplot2)

setwd("D:/MGR/APU/lab3/")
dataset <-read.csv("macbooki.csv")
ocena_klientow <- dataset[["ocena_klientow"]]  
cena <- dataset[["cena"]]  

#przygotowanie danych
compare.trainingdata <- cbind(ocena_klientow, cena)
#scalowanie ocen
scaled.ocena_klientow <- as.data.frame(scale(ocena_klientow))
#trenowanie
trainingdata <- cbind(scaled.ocena_klientow, cena)
colnames(trainingdata) <- c("ocena_klientow", "cena")
#(error ≤ 100 z l)
net.price <- neuralnet(cena~ocena_klientow,trainingdata, hidden<-c(5,3,3), threshold<-100, lifesign <- "full")
plot(net.price)

testdata <- data.frame(c(20,130))
scaled.testdata <- as.data.frame(scale(testdata))
#prognozowanie
net.results <- compute(net.price, scaled.testdata)
fixed_cena <- cbind(testdata, as.data.frame(net.results$net.result))
colnames(fixed_cena) <- c("ocena_klientow", "cena")

print(fixed_cena)