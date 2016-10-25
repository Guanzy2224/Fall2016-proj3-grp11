library(EBImage)
library(glcm)
library(raster)
library(jpeg)
library(class)

getFeature <- function(imgName = "name of feature image",WINDOW=c(31,31),SHIFT=c(10,10)){
  img0 <-  readImage(imgName)
  img1 <- as.matrix(img0[,,1])
  img2 <- as.matrix(img0[,,2])
  img3 <- as.matrix(img0[,,3])
  class(img1) <- "matrix"
  class(img2) <- "matrix"
  class(img3) <- "matrix"
  feature1 <- glcm(raster(img1) , window = WINDOW, shift = SHIFT)
  feature2 <- glcm(raster(img2) , window = WINDOW, shift = SHIFT)
  feature3 <- glcm(raster(img3) , window = WINDOW, shift = SHIFT)
  ndim=dim(feature1)  
  feature1=as.array(feature1)
  feature2=as.array(feature2)
  feature3=as.array(feature3)
  dim(feature1)=c(ndim[1]*ndim[2],ndim[3])
  dim(feature2)=c(ndim[1]*ndim[2],ndim[3])
  dim(feature3)=c(ndim[1]*ndim[2],ndim[3])
  feature1=as.matrix(feature1)
  feature2=as.matrix(feature2)
  feature3=as.matrix(feature3)
  feature=cbind(feature1,feature2,feature3)
  feature=na.omit(feature)
  return(feature)
}

setwd("C:/Users/Administrator/Desktop/proj 3/try4/fur")
dogFeature=matrix(0,ncol=24)
for(i in 1:6){
  imgName=sprintf("%d.jpg",i)
  temp=getFeature(imgName)
  dogFeature=rbind(dogFeature,temp)
}
dogFeature=dogFeature[-1,]

setwd("C:/Users/Administrator/Desktop/proj 3/try4/chick")
chickFeature=matrix(0,ncol=24)
for(i in 1:6){
  imgName=sprintf("%d.jpg",i)
  temp=getFeature(imgName)
  chickFeature=rbind(chickFeature,temp)
}
chickFeature=chickFeature[-1,]

setwd("C:/Users/Administrator/Desktop/proj 3/try4/noise")
noiseFeature=matrix(0,ncol=24)
for(i in 1:4){
  imgName=sprintf("%d.jpg",i)
  temp=getFeature(imgName)
  noiseFeature=rbind(noiseFeature,temp)
}
noiseFeature=noiseFeature[-1,]

temp2=rbind(dogFeature,chickFeature)
scaleSD=apply(temp2,2,sd)
dogFeature=t(t(dogFeature)/scaleSD)
chickFeature=t(t(chickFeature)/scaleSD)
noiseFeature=t(t(noiseFeature)/scaleSD)






getTestFeature <- function(imgName = "name of feature image",WINDOW=c(31,31)){#,SHIFT=c(10,10)){
  img0 <-  readImage(imgName)
  img1 <- as.matrix(img0[,,1])
  img2 <- as.matrix(img0[,,2])
  img3 <- as.matrix(img0[,,3])
  class(img1) <- "matrix"
  class(img2) <- "matrix"
  class(img3) <- "matrix"
  feature1 <- glcm(raster(img1) , window = WINDOW)#, shift = SHIFT)
  feature2 <- glcm(raster(img2) , window = WINDOW)#, shift = SHIFT)
  feature3 <- glcm(raster(img3) , window = WINDOW)#, shift = SHIFT)
  ndim=dim(feature1)  
  feature1=as.array(feature1)
  feature2=as.array(feature2)
  feature3=as.array(feature3)
  dim(feature1)=c(ndim[1]*ndim[2],ndim[3])
  dim(feature2)=c(ndim[1]*ndim[2],ndim[3])
  dim(feature3)=c(ndim[1]*ndim[2],ndim[3])
  feature1=as.matrix(feature1)
  feature2=as.matrix(feature2)
  feature3=as.matrix(feature3)
  feature=cbind(feature1,feature2,feature3)
  feature[is.na(feature)]=0
  return(feature)
}


setwd("C:/Users/Administrator/Desktop/proj 3/try4/test")
imgName="dog_0649.jpg"
testFeature=getTestFeature(imgName)
testFeature[!abs(testFeature)<Inf]=0
testFeature=t(t(testFeature)/scaleSD)

dogInd=sample(1:173262,size=30000,replace = F)
chickInd=sample(1:117518,size=10000,replace = F)
dogFeature0=dogFeature[dogInd,]
chickFeature0=chickFeature[chickInd,]
pts=runif(2400000,min=-2,max=13)
pts=matrix(pts,ncol=24)
#tag=c(rep(0,133206),rep(0,10000),rep(1,10000))
#trainFeature=rbind(pts,noiseFeature,chickFeature0,dogFeature0)
tag=c(rep(0,100000),rep(1,30000))
trainFeature=rbind(pts,dogFeature0)

fit_gbm=gbm.fit(x=trainFeature, y=tag,
        n.trees=24,
        distribution="bernoulli",
        interaction.depth=5, 
        bag.fraction = 0.5,
        verbose=FALSE)
pred <- predict(fit_gbm, newdata=as.data.frame(testFeature),
                n.trees=24, type="response")
imgCluster=kmeans(testFeature,centers = 5)

ff=knn(train=trainFeature,test=testFeature,cl=tag,k=3)
ff0=ff
ff0=as.matrix(as.numeric(imgCluster$cluster),ncol=1)
img000=readImage(imgName)
theDim=dim(img000)
dim(ff0)=c(theDim[1],theDim[2])
#image(ff0)
#save.image(ff0,file="ff0.Rdata")
par(mfrow=c(1,3))

image(img000[,,3]+ff0/8)
image(img000[,,3])
image(ff0)
