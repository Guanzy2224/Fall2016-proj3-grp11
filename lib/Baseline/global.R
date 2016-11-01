library(gbm)

#Read in the SIFT
setwd("/Users/yifeihu/Documents/semester_3/applied_data_science/project_3/Project3_poodleKFC_train")
data<-read.csv("sift_features.csv")
#label the data, chicken is 1 and dog is 0
data1<-t(data)
y<-c(rep(0,2000))
for(i in 1:1000){
  y[i]<-1
}
data1<-cbind(data1,y==1)
#make the data suitable for GBM
data1 <- as.data.frame(data1)
var <- colnames(data1)[1:(length(data1[1,]) - 1)]
yColumn <- "isChick"
colnames(data1) <- c(var, yColumn)
train_num<-sample(c(1:2000),1500)
#split train data and test data
train<-data1[train_num,]
test<-data1[-train_num,]

formula <- paste(yColumn,paste(var,collapse=' + '),sep=' ~ ')
test<-as.data.frame(test)
train <- as.data.frame(train)
train$isTest<-FALSE
test$isTest<-TRUE
data2<-rbind(train,test)
