# Please install the library listed below
#
# library(EBImage)
# library(glcm)
# library(raster)
# library(jpeg)
# library(class)
# library(randomForest)
# library(dplyr)
# library(caret)
# library(abind)
# library(foreach)
# library(doParallel)
#
# Use parallel computation, make it 2~4 times faster.
# Default setting: number of cores = 4

# Please be noted that the function "Mytrain()" cannot be renamed as "train()", because "train()" is 
# a function from package caret, and is used inside the function Mytrain()


Mytrain=function(SomepredictorX,responseY,no.cores=4){
  cl<-makeCluster(no.cores)
  registerDoParallel(cl)
  gbm_final=train(x=SomepredictorX, y=responseY, method="gbm", verbose=F, trControl=trainControl(method="repeatedcv")) # way faster and more accurate
  stopCluster(cl)
  return(gbm_final)
}

