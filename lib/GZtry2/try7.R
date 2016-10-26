library(EBImage)
library(glcm)
library(raster)
library(jpeg)
library(class)

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

makeFeatureCluster=function(features,clusterNum){
  features[is.na(features)]=0
  features[abs(features)==Inf]=0
  featuresSD=apply(features,2,sd)
  features=t(t(features)/featuresSD)
  featuresClusters=kmeans(features,centers = clusterN)
  featuresClusters=t(t(featuresClusters$centers)*featuresSD)
  return(featuresClusters)
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

