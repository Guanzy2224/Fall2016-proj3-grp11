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

The baseline model uses boosted decision stumps on SIFT features with an accuracy of 67%.

The advanced model we created uses GLCM+COLOR as our new feature resulting in shorter time of extracting features.
In addition, the advanced model changes key point selection in SIFT to clustering and adds random forest for feature selection. 
As for the classifier we stick with boosted decision stumps using R packages gbm.
In all, our new model reaches a much higher accuracy at 80%.

## Process Flow

Divide:	Divide the training image data into two parts: 	One for forming feature pool: pool_image;The other for supervised feature selection: learn_image.

Resize:	Resize all large images to around 30000 pixels

Calculate statistics:	For each p*p window, calculate its statistics (glcm and color)

Cluster: Cluster the windows by their statistics, keep 100 cluster means for each image

Generate words:	Pool cluster means from all  pool_image together and form a dictionary; each statistics (column) is scaled

Word Frequency:	For each learn_image, calculate word frequency

Feature Selection: Find the most useful words in distinguishing chicken and dog (Random Forest)

Train Model: Still use GBM

	
**Contribution statement**: All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 



