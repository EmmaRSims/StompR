###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

#
# PLSR - Partial Least Squares Regression
#

#' doPLSR Applies the Partial Least Squares Regression (PLSR) model to the training data,
#' and calculates the Root Mean Square Error (RMSE) and Mean Absolute Percentage Error (MAPE) from the predicted values of the testing data.
#' @details This function uses the PLSR method to build a model from the training data, with the function \code{\link[pls]{plsr}},
#' then calculates the RMSE and MAPE with \code{\link[StompR]{getModelError}} using the predicted values calculated from the testing data.
#'
#' @import pls
#'
#' @param train Dataframe of training data, the first column is the response factor to be modelled by the remaining columns
#' @param test Dataframe of testing data, the first column is the response factor, the rest of the columns contain factors used to predict the response column with the built model
#' @param ncomp The number of components to include in the model (see \code{\link[pls]{plsr}})
#'
#' @return Returns the output of the function getModelError, which is a list consisting of:
#' \itemize{
#' \item mape - the Mean Absolute Percentage Error (MAPE) of the current model
#' \item rmse - the Root Mean Square Error (RMSE) of the current model
#' }
#' @export
doPLSR <- function(train,test,ncomp){
  colnames(train)[1] <- "train.y"

  test.x <- test[,-1]
  test.y <- test[,1]

  invisible(model <- plsr(train.y ~. , data = train))
  suppressWarnings(pred <- as.numeric(predict(model, test.x, ncomp=ncomp)))

  return(getModelError(pred,test.y))
}
