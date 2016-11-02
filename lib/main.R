
library(EBImage)
library(glcm)
library(raster)
library(jpeg)
library(randomForest)
library(dplyr)
library(caret)
library(e1071)
library(gbm)
library(abind)
library(foreach)
library(doParallel)

# Set project working directory here, SPECIFY THIS DIRECTORY WHENEVER YOU MAKE A COPY OF THIS PROJECT
Root <- "D:/Yinxiang Gao/2015 M.A.STAT Courses/STATGR5243_Applied Data Science/Fall2016-proj3-grp11"
setwd(Root)

### NOTE: this line takes about an hour to run ###
source("./lib/feature.R")


setwd(Root)
load("./output/feature_eval.Rdata")
source("./lib/train.R")
source("./lib/test.R")

# Our model: gbm on glcm+rgb features
system.time({
  m1=Mytrain(SomepredictorX,responseY,no.cores=4)
  pred1=Mytest(m1,testX)
})
pred1
mean(pred1 == c(rep(1,500),rep(0,500)))

# baseline model: gbm on sift features
system.time({
  m2=Mytrain(sift_train,sift_label,no.cores=4)
  pred2=Mytest(m1,testX)
})
pred2
mean(pred2 == c(rep(1,500),rep(0,500)))


