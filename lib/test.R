# Please install the library listed below
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

# Since the Mytrain() function is named that way, here we use "Mytest()" as test function name
# Not sure what will happened if changed to "test()", hopefully won't mask any others

# We use the same test function for advanced and baseline model

Mytest=function(gbm_final,testX){
  library(caret)
  gbm_result <- predict(gbm_final,testX)
  return(gbm_result)
}

