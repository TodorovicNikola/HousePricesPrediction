require(randomForest)
require(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

setwd("../CSV Files/Clusters fully separated/AWBD removed")
#setwd("../CSV Files/Clusters fully separated/AWBD and outliers removed")
#setwd("../CSV Files/Clusters fully separated/AWBD and outliers removed, remod attr added")
#setwd("../CSV Files/Clusters fully separated/AWBD, extreme outlier removing,  remod added")
#setwd("../CSV Files/Clusters fully separated/Outliers removed, remod attr added")



# 0 ZNACI DA SE RADI DEFAULT
numberOfTrees <- 120
numberOfVars <- 90
maxTermNodes <- 0
minSizeOfTermNodes <- 0 #controlles pruning
#xtest, ytest


set.seed(58)


trainData.cluster0 <- read.csv(file="cluster0_train.csv", header=TRUE, sep=",")
trainData.cluster1 <- read.csv(file="cluster1_train.csv", header=TRUE, sep=",")

testData.cluster0 <- read.csv(file="cluster0_test.csv", header=TRUE, sep=",")
testData.cluster1 <- read.csv(file="cluster1_test.csv", header=TRUE, sep=",")

testData.cluster0 <- testData.cluster0[, !(names(testData.cluster0) %in% c("Id"))]
testData.cluster1 <- testData.cluster1[, !(names(testData.cluster1) %in% c("Id"))]


calculate_prediction <- function(trainData, testData){
  varNames <- names(trainData)
  varNames <- varNames[!varNames %in% c("SalePrice", "Id")]
  
  formulaNames <- paste(varNames, collapse = "+")
  
  rf.form <- as.formula(paste("SalePrice", formulaNames, sep = " ~ "))
  

  if(numberOfTrees != 0 && numberOfVars != 0){
    res.rf <- randomForest(rf.form, trainData, importance = TRUE, ntree = numberOfTrees, mtry = numberOfVars)#, nodesize = minSizeOfTermNodes)
  }else if(numberOfTrees != 0){
    res.rf <- randomForest(rf.form, trainData, importance = TRUE, ntree = numberOfTrees)
  }else if(numberOfVars != 0){
    res.rf <- randomForest(rf.form, trainData, importance = TRUE, mtry = numberOfVars)
  }else{
    res.rf <- randomForest(rf.form, trainData, importance = TRUE)
  }
  plot(res.rf)
  
  varImpPlot(res.rf,
             sort = T,
             main="Variable Importance",
             n.var=15)
  
  testData$predicted.response <- predict(res.rf , testData)

  return (mean((testData$predicted.response - testData$SalePrice)^2))
  
}

res0.mse <- paste("MSE is on cluster0 is ", calculate_prediction(trainData.cluster0, testData.cluster0), sep=" ")
res1.mse <- paste("MSE is on cluster1 is ", calculate_prediction(trainData.cluster1, testData.cluster1), sep=" ")


#RADI ISPISA
workingDirectory <- getwd()

#RADI PISANjA U ODGOVARAJUCI FAJL
if (grepl("Clusters", getwd())){
  setwd("../../../Log Files/Random Forest")
}else{
  setwd("../../Log Files/Random Forest")
}

blockSeparator <- "#########################################################"
parameters <- paste("Number of Trees = ", numberOfTrees, " ,", "Number of Variables = ", numberOfVars, ", ", "Max number of nodes = ", maxTermNodes, ", ", "Min size of terminal nodes = ", minSizeOfTermNodes, "(NOTE: if ommited or zero -> default)")
line <- paste(workingDirectory, parameters, res0.mse, res1.mse, " ", blockSeparator, " ", sep = "\n")
write(line,file="rezultati_RF_MSE_divided_clusters.txt",append=TRUE)
print(res0.mse)
print(res1.mse)