#run GBM
fit1<-gbm(as.formula(formula),
          data = data2[!data2$isTest,1:5001],
          distribution="bernoulli",
          n.trees=1000,
          interaction.depth=3,
          shrinkage=0.05,
          bag.fraction=0.5,
          keep.data=FALSE,
          cv.folds=5
)
#get the best number of trees by cross-validation
ntrees<-gbm.perf(fit1)
