# install and load packages
install.packages("kernlab")
library(kernlab)
library(ggplot2)

# load dataset
setwd('/Users/Dylan Rivera/Desktop/OMSA/Spring 2025/ISYE 6501/HW1/')
ccData <- read.csv("credit_card_data-headers.txt", sep = "")

# initialize empty data frame
table = data.frame()

# set initial value of C
C <- 0.00001

# call ksvm, loop through different values of C
while (C <= 100000) {
  
  # call ksvm function
  model <- ksvm(as.matrix(ccData[,1:10]), as.factor(ccData[,11]), type="C-svc", kernel="vanilladot", C = C, scaled=TRUE)
  
  # calculate a1…am
  a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
  a
  
  # calculate a0
  a0 <- -(model@b)
  a0
  
  # see what the model predicts
  pred <- predict(model,ccData[,1:10])
  pred
  
  # see what fraction of the model’s predictions match the actual classification
  accuracy <- sum(pred == ccData[,11]) / nrow(ccData)
  
  # add value of C and model accuracy into table
  row <- 
  table <-
  
  # iterate to next value of C
  C = C * 10
}

# plot graph of accuracy vs C value