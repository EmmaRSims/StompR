#
# PLSR - Partial Least Squares Regression
#

doPLSR <- function(train,test,ncomp){
  test.x <- test[,-1]
  test.y <- test[,1]

  invisible(model <- plsr(train.y ~. , data = train))
  suppressWarnings(pred <- as.numeric(predict(model, test.x, ncomp=ncomp)))

  return(getModelError(pred,test.y))
}
