#
# SVM - Support Vector Machine
#

doSVM <- function(train,test, gamma, epsilon, cost){
  test.x <- test[,-1]
  test.y <- test[,1]

  bestRmse<-1000000000000
  bestMape<-1000000000000
  best_c  <-1000000000000
  best_g  <-1000000000000
  best_e  <-1000000000000
  for (c in cost){
    for (e in epsilon){
      for (g in gamma){
        invisible(model<-svm(train.y ~. , data = train, type = "eps-regression", kernel = "radial", cost = c, gamma = g, epsilon = e))
        suppressWarnings(pred <- predict(model, test.x))

        error_model <- getModelError(pred, test.y)
        if(bestRmse > error_model$rmse){
          bestRmse <- error_model$rmse
          bestMape <- error_model$mape
          best_c   <- c
          best_g   <- g
          best_e   <- e
          }
      }
    }
  }

  cat(paste("\n~~~\nBest SVM parameters:\nGamma = ",best_g,"\nEpsilon = ",best_e,"\nCost = ", best_c))

  return(list("rmse" = bestRmse, "mape" = bestMape))
}
