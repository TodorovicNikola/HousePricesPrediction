#Ridge, Lasso & Elastic Net Regression
#POWERED BY NKL, USE FOR SIAP ONLY!


require(glmnet)
require(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#setwd("../CSV Files/Start point")
#setwd("../CSV Files/Attrs with bad dist removed")
#setwd("../CSV Files/AWBD and outliers removed")
#setwd("../CSV Files/AWBD and outliers removed, remod attr added")
#setwd("../CSV Files/AWBD, extreme outlier removing,  remod added")
#setwd("../CSV Files/Outliers removed, remod attr added")

#setwd("../CSV Files/Clusterized/Attrs with bad dist removed")
#setwd("../CSV Files/Clusterized/AWBD and outliers removed")
#setwd("../CSV Files/Clusterized/AWBD and outliers removed, remod attr added")
#setwd("../CSV Files/Clusterized/AWBD, extreme outlier removing,  remod added")
setwd("../CSV Files/Clusterized/Outliers removed, remod attr added")



trainData <- read.csv(file="train.csv", header=TRUE, sep=",")
salePrice.tr <- trainData$SalePrice
data.tr <- as.matrix(trainData[, !names(trainData) %in% c("SalePrice", "Id")])
#print(data.tr)

testData <- read.csv(file="test.csv", header=TRUE, sep=",")
salePrice.te <- testData$SalePrice
data.te <- as.matrix(testData[, !names(testData) %in% c("SalePrice", "Id")])
#print(data.te)

workingDirectory <- getwd()

if (grepl("Clusterized", getwd())){
  setwd("../../../Log Files/Ridge_Lasso_ENet")
}else{
  setwd("../../Log Files/Ridge_Lasso_ENet")
}

#FOR ASSOCIATING COEF FILE WITH LOG FILE
ExecTime <- gsub(":", "-",Sys.time())

calculate_prediction <- function(alpha_par){

  set.seed(999)
  rr.cv <- cv.glmnet(data.tr, salePrice.tr, alpha=alpha_par)
  plot(rr.cv, main = alpha_par)

  zaStampu <- as.matrix(coef(rr.cv, s="lambda.min"))
  print(zaStampu)

  strAlpha <- paste("\n\n\n\n\nAlpha =", alpha_par, sep=" ")

  forFile <- paste("coef", ExecTime, sep = "_")
  forFile <- paste(forFile, ".txt", sep="")

  write(strAlpha, file=forFile, append=TRUE)
  write.table(zaStampu, file=forFile,append=TRUE)

  rr.bestlam <- rr.cv$lambda.min
  rr.goodlam <- rr.cv$lambda.1se

  # predict validation set using best lambda and calculate RMSE
  rr.fit <- glmnet(data.tr, salePrice.tr, alpha = alpha_par)
  plot(rr.fit, xvar = "lambda", label = TRUE, main = alpha_par)

  rr.pred <- predict(rr.fit, s = rr.bestlam, newx = data.te)

  str <- paste("MSE with alpha =", alpha_par, "is",mean((rr.pred - salePrice.te)^2), sep=" ")

  print(str)

  return (str)

}

#PREDIKCIJE
pred.r <- calculate_prediction(0)
pred.l <- calculate_prediction(1)
pred.en <- calculate_prediction(0.58)

#ISPIS U FAJL I NA EKRAN
vreme <- paste("Vreme: ", ExecTime, sep=" ")
blockSeparator <- "###############################################################"
line <- paste(workingDirectory, vreme, pred.r, pred.l, pred.en, " ", blockSeparator, " ", sep = "\n")
write(line,file="rezultati_R_L_EN.txt",append=TRUE)
