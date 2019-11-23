#
# RLR - Robust Linear Regression
#

doRLR <- function(train,test,mscale){
  test.x <- test[,-1]
  test.y <- test[,1]

  invisible(model <- lmrob(train.y ~. , data = train, maxit.scale = mscale, na.action = na.roughfix))
  pred <- predict(model, test.x)

  return(getModelError(pred,test.y))
}
