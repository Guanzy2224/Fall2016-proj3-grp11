#predict by using the best number of trees
data2$fit1 <- predict(fit1, newdata = data2[data2$isTest, 1:5000],type = "response",n.tree = ntrees)
dSub <- data2[data2$isTest==TRUE,, drop = FALSE]
modelName <- "fit1"
tab <- table(truth=dSub[,yColumn],pred=dSub[,modelName]>0.5)
#get the prediction accuracy rate
sum(dSub[,yColumn] == (dSub[,modelName]>0.5)) / 500
