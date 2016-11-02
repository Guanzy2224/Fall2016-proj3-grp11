# Project: Labradoodle or Fried Chicken? 
![image](https://s-media-cache-ak0.pinimg.com/236x/6b/01/3c/6b013cd759c69d17ffd1b67b3c1fbbbf.jpg)
### [Full Project Description](doc/project3_desc.html)

Term: Fall 2016

+ Team #11
+ Team members
     + Hu, Yifei
     + Song, Shuli
     + You, Guanzhong 
     + Zeng, Cen 
     + Gao, Yinxiang

## Project Summary

In this project, we carried out model evaluation and selection for predictive analytics on image data. We received a set of 2000 images of poodle dogs and fried chickens and worked on how to distinguish them accurately and effectively.

The baseline model uses boosted decision stumps on SIFT features with an accuracy of 72%.

The advanced model we created uses GLCM+COLOR as our new feature resulting in shorter time of extracting features. In addition, the advanced model changes key point selection in SIFT to clustering and adds random forest for feature selection.  As for the classifier we stick with boosted decision stumps using R packages gbm. In all, our new model reaches a much higher accuracy at 80%.

## Process Flow

**Divide**:	Divide the training image data into two parts: 	One for forming feature pool: pool_image;The other for supervised feature selection: learn_image.

**Resize**:	Resize all large images to around 30000 pixels

**Calculate statistics**:	For each p*p window, calculate its statistics (glcm and color)

**Cluster**: Cluster the windows by their statistics, keep 100 cluster means for each image

**Generate words**:	Pool cluster means from all  pool_image together and form a dictionary; each statistics (column) is scaled

**Word Frequency**:	For each learn_image, calculate word frequency

**Feature Selection**: Find the most useful words in distinguishing chicken and dog (Random Forest)

**Train Model**: Still use GBM

## Key Parameters

In this algorithm, some key parameters in feature extraction should be corrected setted, otherwise the accuracy will drop down.

**0.03**: This number in Ind=(rf$importance[,4]>0.03) is an essential threshold for choosing useful words. 0.03 is usually the turning point of importance plot, but it could be larger when the test sample size smaller.

**n_dog_learn=min(max(round(n_dog_train*0.5),50),400)
n_chick_learn=min(max(round(n_chick_train*0.5),50),400)**: This two lines define the ratio of the size of pooling part and learning part. Closer to 0.5 means a better result but a slower feature extraction for training data.

**WINDOW**: window size. The window size for calculating GLCM statistics and color statistics. Good choice should be between c(15,15) and c(31,31)

**RESIZE**: If the image size is larger than RESIZE, then scale them to the size of RESIZE.

**distance.as.near**: This variable measures how closed are two "words" considered the same. Approximately chi-square distribution with df equal to the number of statistics (because each stat is appx (iid) normal, distance sums their squared difference up)

**TrainRoot**: The address to folder containing pictures of training set

**TestRoot**: The address to folder containing pictures of training set

**train_labels**: The label of training data, please specify it before extracting training features. This is VERY important!

**save_Rdata_Name**: By default "feature_evual.Rdata", the output filename of .Rdata file for train.R and test.R. This file contains 6 key elements: 3 for our model, 3 for baseline model

**no.cores**: number of cores used in parallel computation

**allFeature**: The 14 statistics for each word for all words extracted from all pool_images

**SomeFeature**: The 14 statistics for each word for ONLY selected (by RandomForest GiniDecreasing) words

**scaleSD**: A vector of length 14. The scaling parameter for pool_images features(14 columns) and testing features(14 columns).

**predictorX**: Word frequency (like document to term matrix, dtm) in each learn_images for ALL words extracted from pool_images.

**SomepredictorX**: Word frequcney in each learn_images for ONLY selected (by RandomForest GiniDecreasing) words

**responseY**: The label for each line in the "dtm" mentioned above

**testX**: Word frequcney in each test_images for selected words
	
## Packages
This algorithm uses packages:
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

The installation instruction for EBImage:	
https://bioconductor.org/packages/release/bioc/html/EBImage.html
	
## R Version
Not very version sensitive, just use some new version.

## Contribution statement : 
All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 



