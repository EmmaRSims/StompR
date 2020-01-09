###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

#
# KNN - K-Nearest Neighbours
#

#' doKNN Applies the K-Nearest Neighbours Regression model to the training data,
#' and calculates the Root Mean Square Error (RMSE) and Mean Absolute Percentage Error (MAPE) from the predicted values of the testing data.
#' @details This function performs K-Nearest Neighbours regression \code{\link[FNN]{knn.reg}} on the training data, for each K specified.
#' The testing data is used to extract a Root Mean Square Error (RMSE) and Mean Absolute Percentage Error (MAPE) with the \code{\link[StompR]{getModelError}} function.
#' Only the lowest RMSE and corresponding MAPE and best K value are returned.
#'
#' @import FNN
#'
#' @param train Dataframe of training data, the first column is the response factor to be modelled by the remaining columns
#' @param test Dataframe of testing data, the first column is the response factor, the rest of the columns contain factors used to predict the response column with the built model
#' @param knum The number of K-Centroids used in the K-Nearest Neighbours method (see \code{\link[FNN]{knn.reg}})
#'
#' @return Returns:
#' \itemize{
#' \item mape - the Mean Absolute Percentage Error (MAPE) of the current model
#' \item rmse - the Root Mean Square Error (RMSE) of the current model
#' \item bestK - the value of K which returned the best RMSE and MAPE values
#' }
#' @export
doKNN <- function(train,test,knum){
  colnames(train)[1] <- "train.y"

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

  #cat(paste("\n~~~\nBest KNN Parameters:\nK = ",bestK,"\n"))

  return(list("rmse" = bestRmse, "mape" = bestMape, "bestK" - bestK))
}
