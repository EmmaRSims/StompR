#
# OLSR - Ordinary Least Squares Regression
#

doOLSR <- function(train,test){
  test.x <- test[,-1]
  test.y <- test[,1]

  invisible(model <- lm(train.y ~. , data = train))
  pred <- predict(model, test.x)

  return(getModelError(pred,test.y))
}
