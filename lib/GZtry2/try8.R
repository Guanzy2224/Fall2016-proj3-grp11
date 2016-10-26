library(EBImage)
library(glcm)
library(raster)
library(jpeg)
library(class)
library(randomForest)
library(dplyr)

getFeature <- function(imgName = "name of feature image",WINDOW=c(9,9),SHIFT=c(1,1)){
  img0 <-  readImage(imgName)
  img1 <- channel(img0, mode='gray')
  imgDim=dim(img1)
  if (imgDim[1]*imgDim[2]>30000) {
    ratio=sqrt((imgDim[1]*imgDim[2])/40000)
    w=round(imgDim[1]/ratio)
    h=round(imgDim[2]/ratio)
    img1=resize(img1,w=w,h=h)
  }
  class(img1) <- "matrix"
  feature <- glcm(raster(img1) , window = WINDOW, shift = SHIFT)
  ndim=dim(feature)  
  feature=as.array(feature)
  dim(feature)=c(ndim[1]*ndim[2],ndim[3])
  feature=as.matrix(feature)
  #feature=na.omit(feature)
  return(feature)
}

makeFeatureCluster=function(features,clusterNum,SCALE=F){
  features[is.na(features)]=0
  features[abs(features)==Inf]=0
  featuresSD=apply(features,2,sd)
  features=t(t(features)/featuresSD)
  featuresClsuters=kmeans(features,centers = clusterNum)
  featuresClsuters=featuresClsuters$centers
  if (!SCALE) featuresClsuters=t(t(featuresClsuters)*featuresSD)
  return(featuresClsuters)
}


clusterN=10
setwd("C:/Users/Administrator/Desktop/proj 3/try4/ChickDog")
dogFeature=matrix(0,ncol=8)
for(i in 1:25){
  imgName=sprintf("dog_%04d.jpg",i)
  temp=getFeature(imgName)
  temp=makeFeatureCluster(temp,clusterNum = clusterN)
  dogFeature=rbind(dogFeature,temp)
}
dogFeature=dogFeature[-1,]

chickFeature=matrix(0,ncol=8)
for(i in 1:25){
  imgName=sprintf("chicken_%04d.jpg",i)
  temp=getFeature(imgName)
  temp=makeFeatureCluster(temp,clusterNum = clusterN)
  chickFeature=rbind(chickFeature,temp)
}
chickFeature=chickFeature[-1,]

temp2=rbind(dogFeature,chickFeature)
scaleSD=apply(temp2,2,sd)
dogFeature1=t(t(dogFeature)/scaleSD)
chickFeature1=t(t(chickFeature)/scaleSD)
dogFeature2=kmeans(dogFeature1,centers=100)$centers
chickFeature2=kmeans(chickFeature1,centers=100)$centers
allFeature=rbind(dogFeature2,chickFeature2)

DISTANCE=function(a,b){
  return(sum((a-b)^2))
}

findSimilarFeatureNumber=function(TheFeature,Dictionary,distance=10){
  n=nrow(TheFeature)
  l=nrow(Dictionary)
  ans=rep(0,l)
  for (i in 1:l){
    ans[i]=sum(apply(TheFeature,1,DISTANCE,b=Dictionary[i,])<distance)
  }
  return(ans)
}

setwd("C:/Users/Administrator/Desktop/proj 3/try4/test")
predictorX=matrix(0,ncol=nrow(allFeature),nrow=20)
for(i in 26:35){
  imgName=sprintf("dog_%04d.jpg",i)
  temp=getFeature(imgName)
  temp=makeFeatureCluster(temp,clusterNum = 30,SCALE=T)
  predictorX[i-25,]=findSimilarFeatureNumber(TheFeature = temp,Dictionary = allFeature,distance = 10)
}

for(i in 26:35){
  imgName=sprintf("chicken_%04d.jpg",i)
  temp=getFeature(imgName)
  temp=makeFeatureCluster(temp,clusterNum = 30,SCALE=T)
  predictorX[i-15,]=findSimilarFeatureNumber(TheFeature = temp,Dictionary = allFeature,distance = 10)
}
responseY=as.factor(c(rep(1,10),rep(2,10)))

rf=randomForest(x=predictorX,y=responseY,importance = T)
rfImpt=rf$importance%>%
  as.data.frame()%>%
  arrange(desc(MeanDecreaseGini))
plot(rfImpt$MeanDecreaseGini)
sum(rfImpt$MeanDecreaseGini>0.1)
Ind=(rf$importance[,4]>0.05)
SomeFeatures=allFeature[Ind,]


setwd("C:/Users/Administrator/Desktop/proj 3/try4/testtest")
testX=matrix(0,ncol=nrow(allFeature),nrow=20)
for (i in 1:10){
  imgName=sprintf("chicken_%04d.jpg",i+120)
  temp=getFeature(imgName)
  temp=makeFeatureCluster(temp,clusterNum = 30,SCALE=T)
  testX[i,]=findSimilarFeatureNumber(TheFeature = temp,Dictionary = allFeature,distance = 10)
}

predict(rf,testX)

