#
# SLR - Stepwise Linear Regression
#

#Both
doSLR <- function(train,test){
  test.x <- test[,-1]
  test.y <- test[,1]

  model <- lm(train.y ~. , data = train)
  invisible(smodel <- step(model, direction = "both"))
  suppressWarnings(pred <- predict(smodel, test.x))

  return(getModelError(pred,test.y))
}

#Forward
doSLRf <- function(train,test){
  test.x <- test[,-1]
  test.y <- test[,1]

  model <- lm(train.y ~. , data = train)
  invisible(smodel <- step(model, direction = "forward"))
  suppressWarnings(pred <- predict(smodel, test.x))

  return(getModelError(pred,test.y))
}

#Backward
doSLRb <- function(train,test){
  test.x <- test[,-1]
  test.y <- test[,1]

  model <- lm(train.y ~. , data = train)
  invisible(smodel <- step(model, direction = "backward"))
  suppressWarnings(pred <- predict(smodel, test.x))

  return(getModelError(pred,test.y))
}
