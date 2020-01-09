###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University        #
#     2019/2020                   #
###################################

#
# SVM - Support Vector Machine
#

#' doSVM Applies the Support Vector Machine algorithm (see \code{\link[e1071]{svm}}),
#' and then calculates the RMSE and MAPE with \code{\link[StompR]{getModelError}} using the predicted values calculated from the testing data.
#' @details This function iterates over each cost, epsilon, and gamma value given, applying the SVM algorithm to the training data (see \code{\link[e1071]{svm}}).
#' Calculating the RMSE and MAPE with \code{\link[StompR]{getModelError}} using the predicted values calculated from the testing data,
#' the RMSE, MAPE, cost, gamma, and epsilon of the best performing model is returned.
#'
#' @import e1071
#'
#' @param train Dataframe of training data, the first column is the response factor to be modelled by the remaining columns
#' @param test Dataframe of testing data, the first column is the response factor, the rest of the columns contain factors used to predict the response column with the built model
#' @param gamma Parameter of the Gaussian Kernel for non-linear classification; default is 1/(data dimension). This can be a list of values
#' @param epsilon The margin of tolerance for the epsilon-insensitive loss function; default is 0.1. This can be a list of values
#' @param cost The cost of misclassification; default is 1. This can be a list of values
#'
#' @return Returns the output of the function getModelError, which is a list consisting of:
#' \itemize{
#' \item mape - the Mean Absolute Percentage Error (MAPE) of the best model
#' \item rmse - the Root Mean Square Error (RMSE) of the best model
#' \item gamma - the gamma used for the best model
#' \item epsilon - the epsilon used for the best model
#' \item cost - the cost used for the best model
#' }
#' @export
doSVM <- function(train,test, gamma, epsilon, cost){
  colnames(train)[1] <- "train.y"

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

  #cat(paste("\n~~~\nBest SVM parameters:\nGamma = ",best_g,"\nEpsilon = ",best_e,"\nCost = ", best_c))

  return(list("rmse" = bestRmse, "mape" = bestMape, "gamma" = best_c, "epsilon" = best_e, "cost" = best_c))
}
