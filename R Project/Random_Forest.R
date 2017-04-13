require(randomForest)
require(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#setwd("../CSV Files/Start point")
#setwd("../CSV Files/Attrs with bad dist removed")
#setwd("../CSV Files/AWBD and outliers removed")
#setwd("../CSV Files/AWBD and outliers removed, remod attr added")
setwd("../CSV Files/AWBD, extreme outlier removing,  remod added")
#setwd("../CSV Files/Outliers removed, remod attr added")

#setwd("../CSV Files/Clusterized/Attrs with bad dist removed")
#setwd("../CSV Files/Clusterized/AWBD and outliers removed")
#setwd("../CSV Files/Clusterized/AWBD and outliers removed, remod attr added")
#setwd("../CSV Files/Clusterized/AWBD, extreme outlier removing,  remod added")
#setwd("../CSV Files/Clusterized/Outliers removed, remod attr added")



# 0 ZNACI DA SE RADI DEFAULT
numberOfTrees <- 120
numberOfVars <- 90
maxTermNodes <- 0
minSizeOfTermNodes <- 0 #controlles pruning
#xtest, ytest





trainData <- read.csv(file="train.csv", header=TRUE, sep=",")
testData <- read.csv(file="test.csv", header=TRUE, sep=",")
testData <- testData[, !(names(testData) %in% c("Id"))]
varNames <- names(trainData)
varNames <- varNames[!varNames %in% c("SalePrice", "Id")]

formulaNames <- paste(varNames, collapse = "+")

rf.form <- as.formula(paste("SalePrice", formulaNames, sep = " ~ "))

set.seed(58)

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

rez.rmse <- paste("MSE is",mean((testData$predicted.response - testData$SalePrice)^2), sep=" ")


#RADI ISPISA
workingDirectory <- getwd()

#RADI PISANjA U ODGOVARAJUCI FAJL
if (grepl("Clusterized", getwd())){
  setwd("../../../Log Files/Random Forest")
}else{
  setwd("../../Log Files/Random Forest")
}

blockSeparator <- "#########################################################"
parameters <- paste("Number of Trees = ", numberOfTrees, " ,", "Number of Variables = ", numberOfVars, ", ", "Max number of nodes = ", maxTermNodes, ", ", "Min size of terminal nodes = ", minSizeOfTermNodes, "(NOTE: if ommited or zero -> default)")
line <- paste(workingDirectory, parameters, rez.rmse, " ", blockSeparator, " ", sep = "\n")
write(line,file="rezultati_RF_MSE.txt",append=TRUE)
print(rez.rmse)