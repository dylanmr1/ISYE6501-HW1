# install and load packages
install.packages("kernlab")
library(kernlab)
library(ggplot2)

path <- "~/Downloads/hw1/data 2.2/"
# load dataset
ccData <- read.csv(file.path(path,"credit_card_data-headers.txt"), sep="")

# initialize empty data frame
table = data.frame(C = numeric(), Accuracy = numeric())

# set initial value of C
C <- 1e-15

# call ksvm, loop through different values of C
while (C <= 1e15) {
  
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
  
  # add value of C and model's accuracy into table
  row <- c(C, accuracy)
  table <- rbind(table, row)
  colnames(table) <- c("C", "Accuracy")
  
  # iterate to next value of C
  C = C * 10
}

# plot graph of accuracy vs C value
plot <- ggplot(table, aes(C, Accuracy))
plot + geom_point() + scale_x_log10() + ylim(0,1) + labs(title = "SVM Model Accuracy for different values of C")