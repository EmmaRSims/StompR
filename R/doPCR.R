#
# PCR - Principal Components Regression
#

doPCR <- function(train,test){
  test.x <- test[,-1]
  test.y <- test[,1]

  invisible(model <- pcr(train.y ~. , data = train, scale=F))
  suppressWarnings(pred <- predict(model, test.x))

  return(getModelError(pred,test.y))
}
