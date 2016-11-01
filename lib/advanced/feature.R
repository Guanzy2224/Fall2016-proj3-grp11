
# This code takes as input the training picture, training label, and testing picture.
# Cannot extract the testing feature independently because it needs the dictionary which is selected
# from the training images.
# For more detail, please refer to our slides.
# If meet any problem running this code, please email: gy2224@columbia.edu


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

# Set working directory here, please specify the address for training image folder and 
# testing image folder respectively.
# And specify the file name for dog images and chicken images respectively

TrainRoot= "C:/Users/Administrator/Desktop/proj 3/ForSubmit/trainImage"
TestRoot= "C:/Users/Administrator/Desktop/proj 3/ForSubmit/testImage"
setwd(TrainRoot) #You can find setwd(TrainRoot) somewhere below
save_Rdata_Name="feature_evual.Rdata" #You can also specify the address to store the .Rdata file

#Specify the file names for picture.
dog_train_files=sapply(c(1:500),function(i)sprintf("dog_%04d.jpg",i));
chick_train_files=sapply(c(1:500),function(i)sprintf("chicken_%04d.jpg",i));
dog_test_files=sapply(c(501:1000),function(i)sprintf("dog_%04d.jpg",i));
chick_test_files=sapply(c(501:1000),function(i)sprintf("chicken_%04d.jpg",i));

train_files=c(dog_train_files,chick_train_files)
test_files=c(dog_test_files,chick_test_files)
train_labels=c(rep(0,500),rep(1,500)) #Please specify the tag for each training picture

no.cores=4 # number of cores in parallel computation, which save your life 2~4 times (though) in running this codes


#---------------please specify above information-------------------#

# Here we have 3 kinds of files(images,obvervations):
# The images used to extract feature pool (words pool,dictionary): dog_pool_files, chick_pool_files
# The images used to select feature pool (the thinner dictionary), which is different from the first type:
#  dog_learn_files,chick_learn_files
# The images to be labeled, i.e. test images: test_files

n_dog_train=length(train_files[train_labels==0])
n_chick_train=length(train_files[train_labels==1])
n_dog_learn=min(max(round(n_dog_train*0.5),50),400) #400 takes a longer time for randomforest than 200, but more accurate
n_chick_learn=min(max(round(n_chick_train*0.5),50),400)
n_dog_pool= n_dog_train-n_dog_learn
n_chick_pool=n_chick_train-n_chick_learn
n_test=length(test_files)

dog_pool_files=train_files[train_labels==0][1:n_dog_pool]
chick_pool_files=train_files[train_labels==1][1:n_chick_pool]
dog_learn_files=train_files[train_labels==0][n_dog_pool+1:n_dog_train]
chick_learn_files=train_files[train_labels==1][n_chick_pool+1:n_chick_train]

### set global parameters
WINDOW <- c(21,21) # glcm window size
SHIFT <- c(1,1) # glcm shift direction in each window
RESIZE <- 30000 # resize the image if it has more than this amount of pixels
clusterN <- 100 # number of clusters to extract for each image
distance.as.near <- 1 # threshold for deciding if two features are near to each other


# get img features
getFeature <- function(imgName = "name of feature image",window=WINDOW,shift=SHIFT){
  img0 <-  readImage(imgName)
  imgDim=dim(img0)
  # resize
  if (imgDim[1]*imgDim[2]>RESIZE) {
    ratio=sqrt((imgDim[1]*imgDim[2])/RESIZE)
    w=round(imgDim[1]/ratio)
    h=round(imgDim[2]/ratio)
    img0=resize(img0,w=w,h=h)
  }
  imgDim=dim(img0)
  
  # to gray,R,G,B mode
  img1 <- matrix(channel(img0, mode='gray'),imgDim[1],imgDim[2])
  imgR <- matrix(channel(img0, mode='red'),imgDim[1],imgDim[2])
  imgG <- matrix(channel(img0, mode='green'),imgDim[1],imgDim[2])
  imgB <- matrix(channel(img0, mode='blue'),imgDim[1],imgDim[2])
  # get glcm feature
  feature_texture <- glcm(raster(img1) , window = window, shift = shift)
  feature_R       <- glcm(raster(imgR) , window = window, shift = shift, statistics = c( 'mean_ENVI', 'variance_ENVI'))
  feature_G       <- glcm(raster(imgG) , window = window, shift = shift, statistics = c( 'mean_ENVI', 'variance_ENVI'))
  feature_B       <- glcm(raster(imgB) , window = window, shift = shift, statistics = c( 'mean_ENVI', 'variance_ENVI'))
  # bind features
  feature <- abind(as.array(feature_texture),as.array(feature_R),as.array(feature_G),as.array(feature_B))
  # flatten 3d array
  ndim=dim(feature)
  dim(feature)=c(ndim[1]*ndim[2],ndim[3])
  feature=as.matrix(feature)
  # deal with anomalies
  feature=na.omit(feature) # delete any row that has NA value
  feature=feature[abs(feature[,8])<=1,] # delete any row that has a correlation value out of normal range [0,1]
  
  return(feature)
}

# calculate img features clusters from all image features
makeFeatureCluster=function(features,clusterNum){
  featuresSD=apply(features,2,sd)
  featuresMEAN=colMeans(features)
  features=t( (t(features)-featuresMEAN) / featuresSD + featuresMEAN ) 
  featuresClsuters=kmeans(features,centers = clusterNum,iter.max = 20) # Of course here we can use other faster methods
  featuresClsuters=featuresClsuters$centers
  featuresClsuters=t( (t(featuresClsuters) -  featuresMEAN) * featuresSD + featuresMEAN)
  return(featuresClsuters)
}
### construct feature pool
### parallel version
cl<-makeCluster(no.cores)
registerDoParallel(cl)

  dogFeature <- foreach(i=1:n_dog_pool, .combine = "rbind", .packages = c("EBImage","glcm","raster","jpeg","abind")) %dopar% {
    imgName=dog_pool_files[i]
    temp=getFeature(imgName)
    temp=makeFeatureCluster(temp,clusterNum = clusterN)
  }
  chickFeature <- foreach(i=1:n_chick_pool, .combine = "rbind", .packages = c("EBImage","glcm","raster","jpeg","abind")) %dopar% {
    imgName=chick_pool_files[i]
    temp=getFeature(imgName)
    temp=makeFeatureCluster(temp,clusterNum = clusterN)
  }
  
stopCluster(cl)

# To scale each column so that each glcm character has the same weight in calculating distances.
temp2=rbind(dogFeature,chickFeature)
scaleSD=apply(temp2,2,sd)
featuresMean=colMeans(temp2)
temp2=t( (t(temp2)-featuresMean) / scaleSD + featuresMean )
allFeature <- temp2 #allfeature is the feature pool or the big dictionary

### feature selection
# squared Euclidean distance (or just Euclidean dist cuz it makes no difference)
# In this function DISTANCE2(), we use matrix operation to make it way faster
DISTANCE2=function(x,y){
  x2=rowSums(x^2)
  y2=rowSums(y^2)
  MAT=-2*x%*%t(y)
  MAT=MAT+x2
  MAT=t(t(MAT)+y2)
  return(MAT)
}

# for an image, calculate the number of windows that has similar stats to each feature in the pool
# returns a p-dimension vector where p is the total number of features in feature pool
findSimilarFeatureNumber=function(TheFeature,Dictionary,distance=distance.as.near){
  theDIST=DISTANCE2(TheFeature,Dictionary)
  ans=colSums(theDIST<distance)
  return(ans)
}
### training random forest for feature selection
### parallel version
cl<-makeCluster(no.cores)
registerDoParallel(cl)

  predictorX_dog <- foreach(i=1:n_dog_learn,.combine = "rbind", .packages = c("EBImage","glcm","raster","jpeg","abind")) %dopar% {
    imgName=dog_learn_files[i]
    temp=getFeature(imgName)
    temp=makeFeatureCluster(temp,clusterNum = clusterN)
    temp=t( (t(temp)-featuresMean) / scaleSD + featuresMean )
    predictorX=findSimilarFeatureNumber(TheFeature = temp,Dictionary = allFeature)
  }
  predictorX_chick <- foreach(i=1:n_chick_learn,.combine = "rbind", .packages = c("EBImage","glcm","raster","jpeg","abind")) %dopar% {
    imgName=chick_learn_files[i]
    temp=getFeature(imgName)
    temp=makeFeatureCluster(temp,clusterNum = clusterN)
    temp=t( (t(temp)-featuresMean) / scaleSD + featuresMean )
    predictorX=findSimilarFeatureNumber(TheFeature = temp,Dictionary = allFeature)
  }

stopCluster(cl)

predictorX <- rbind(predictorX_dog, predictorX_chick)
rm(predictorX_dog, predictorX_chick)
responseY <- as.factor(c(rep(0,n_dog_learn),rep(1,n_chick_learn)))

cl<-makeCluster(no.cores)
registerDoParallel(cl)
rf=randomForest(x=predictorX,y=responseY,importance = T)
stopCluster(cl)

# show result for random forest
# !!! IMPORTANT !!!
# IF THE CUT Ind=(rf$importance[,4]>0.03) DOES NOT LOCATE AROUND THE TURNING POINT, THE ACCURACY DROPS
# ALSO, IF THE NUMBER OF IMAGES FOR EXTRACTING FEATURE POOLS IS WAY MORE THAN THE NUMBER OF IMAGE USED IN 
# RANDOM FOREST FOR FEATURE SELECTION, THE ACCURACY DROPS, TOO. (1:1 IS THE BEST RATIO)

rfImpt=rf$importance %>%
  as.data.frame() %>%
  arrange(desc(MeanDecreaseGini))
plot(rfImpt$MeanDecreaseGini)
Ind=(rf$importance[,4]>0.03) # leave words about 3 times of the number of learning data
SomeFeature=allFeature[Ind,] # SomeFeature means some selected "words", while the variable "predictorX" means word frequency
SomepredictorX=predictorX[,Ind] # training data on selected features


#Now use the just-made dictionary to construct a word frequency predictor for each image.

setwd(TestRoot)  

cl<-makeCluster(no.cores)
registerDoParallel(cl)
  
  testX <- foreach(i=1:n_test, .inorder = T, .combine = "rbind", .packages = c("EBImage","glcm","raster","jpeg","abind")) %dopar% {
    imgName=test_files[i]
    temp=getFeature(imgName)
    temp=makeFeatureCluster(temp,clusterNum = clusterN)
    temp=t( (t(temp)-featuresMean) / scaleSD + featuresMean )
    testX=findSimilarFeatureNumber(TheFeature = temp, Dictionary = SomeFeature)
  }

stopCluster(cl)

save(SomepredictorX,responseY,testX,file=save_Rdata_Name)

