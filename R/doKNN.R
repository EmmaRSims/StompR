#
# KNN - K-Nearest Neighbours
#

doKNN <- function(train,test,knum){
  test.x <- test[,-1]
  test.y <- test[,1]

  train.x <- train[,-1]
  train.y <- train[,1]

  bestK   <-1000000000000
  bestRmse<-1000000000000
  bestMape<-1000000000000

  for(k in knum){
    pred<-knn.reg(train.x, test=test.x, y=train.y, k=k)$pred
    model_error <- getModelError(pred,test.y)

    if(bestRmse > model_error$rmse){
      bestRmse <- model_error$rmse
      bestMape <- model_error$mape
      bestK    <- k
    }
  }

  cat(paste("\n~~~\nBest KNN Parameters:\nK = ",bestK,"\n"))

  return(list("rmse" = bestRmse, "mape" = bestMape))
}
