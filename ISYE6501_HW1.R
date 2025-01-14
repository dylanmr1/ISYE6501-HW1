# install and load package
install.packages("kernlab")
library(kernlab)

path <- "~/Downloads/hw1/data 2.2/"
# load dataset
ccData <- read.csv(file.path(path,"credit_card_data-headers.txt"), sep="")

# call ksvm. Vanilladot is a simple linear kernel.
model <- ksvm(as.matrix(ccData[,1:10]), as.factor(ccData[,11]), type="C-svc", kernel="vanilladot", C=100000, scaled=TRUE)

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
fraction <- sum(pred == ccData[,11]) / nrow(ccData)

