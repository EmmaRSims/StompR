###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

#
# SLR - Stepwise Linear Regression
#

#Both
doSLR <- function(train,test){
  test.x <- test[,-1]
  test.y <- test[,1]

  model <- lm(train.y ~. , data = train)
  invisible(smodel <- step(model, direction = "both", trace = F))
  suppressWarnings(pred <- predict(smodel, test.x))

  return(getModelError(pred,test.y))
}

#Forward
doSLRf <- function(train,test){
  test.x <- test[,-1]
  test.y <- test[,1]

  model <- lm(train.y ~. , data = train)
  invisible(smodel <- step(model, direction = "forward", trace = F))
  suppressWarnings(pred <- predict(smodel, test.x))

  return(getModelError(pred,test.y))
}

#Backward
doSLRb <- function(train,test){
  test.x <- test[,-1]
  test.y <- test[,1]

  model <- lm(train.y ~. , data = train)
  invisible(smodel <- step(model, direction = "backward", trace = F))
  suppressWarnings(pred <- predict(smodel, test.x))

  return(getModelError(pred,test.y))
}
