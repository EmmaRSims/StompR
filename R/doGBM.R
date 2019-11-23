#
# GBM - Generalised Boosted Regression
#

doGBM <- function(train,test,ntree,shrink,dist,nnode){
  test.x <- test[,-1]
  test.y <- test[,1]

  invisible(model <- gbm(train.y ~. , data = train, n.trees = ntree, shrinkage = shrink, distribution = dist, n.minobsinnode = nnode))
  pred <- predict(model, test.x, ntree)

  return(getModelError(pred,test.y))
}
