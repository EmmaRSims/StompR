###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

#
# RFR - Random Forest Regression
#

doRFR <- function(train,test,ntree){
  test.x <- test[,-1]
  test.y <- test[,1]

  invisible(model <- randomForest(train.y ~. , data = train, ntree=ntree))
  suppressWarnings(pred.all <- predict(model, test.x, predict.all=T)$individual)
  pred <- apply(pred.all, 1, mean)

  return(getModelError(pred,test.y))
}
