
library(EBImage)
library(glcm)
library(raster)
library(jpeg)
library(class)
library(randomForest)
library(dplyr)
library(caret)
library(abind)
library(foreach)
library(doParallel)


TrainRoot= "C:/Users/Administrator/Desktop/proj 3/ForSubmit/trainImage"
TestRoot= "C:/Users/Administrator/Desktop/proj 3/ForSubmit/testImage"
setwd(TrainRoot) #You can find setwd(TrainRoot) somewhere below

source("C:/Users/Administrator/Desktop/proj 3/ForSubmit/feature.R")

load("feature_evual.Rdata")

source("C:/Users/Administrator/Desktop/proj 3/good2/train.R")
source("C:/Users/Administrator/Desktop/proj 3/good2/test.R")
system.time({
  m1=Mytrain(SomepredictorX,responseY,no.cores=4)
  pred=Mytest(m1,testX)
})

pred
mean(pred == c(rep(0,500),rep(1,500)))

