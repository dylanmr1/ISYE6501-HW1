# install packages
install.packages("kknn")
library(kknn)

path <- "~/Downloads/hw1/data 2.2/"
# load dataset
ccData <- read.csv(file.path(path,"credit_card_data-headers.txt"), sep="")

# initialize empty results table
results <- data.frame(Prediction = numeric(), Actual = numeric())

# call kknn
for (i in 1:654){
  model_kknn <- kknn(R1~A1+A2+A3+A8+A9+A10+A11+A12+A14+A15, ccData[-i,], ccData[i,], k = 10, distance = 2, kernel = "optimal", scale = TRUE)

  # compare prediction with actual value
  prediction <- fitted.values(model_kknn)
  actual <- ccData[i,11]
  
  if (prediction < 0.5){
    prediction = 0
  } else {
    prediction = 1
  }
  # add results into table
  row <- c(prediction, actual)
  results <- rbind(results, row)
  colnames(results) <- c("Prediction", "Actual")
  
  # iterate to next row
  i = i+1
}
  # rough estimate of accuracy based on rounding up or down
  accuracy <- sum(results[,1]==results[,2]) / nrow(results)
  