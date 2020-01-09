###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University        #
#     2019/2020                   #
###################################

#
# RLR - Robust Linear Regression
#

#' doRLR Applies a Robust Linear Regression (RLR) model to the training data,
#' and calculates the Root Mean Square Error (RMSE) and Mean Absolute Percentage Error (MAPE) from the predicted values of the testing data.
#' @details This function applies the RLR algorithm to the training data (see \code{\link[robustbase]{lmrob}}),
#' then calculates the RMSE and MAPE with \code{\link[StompR]{getModelError}} using the predicted values calculated from the testing data.
#'
#' @import robustbase
#'
#' @param train Dataframe of training data, the first column is the response factor to be modelled by the remaining columns
#' @param test Dataframe of testing data, the first column is the response factor, the rest of the columns contain factors used to predict the response column with the built model
#' @param mscale The maximum number of iterations for the Iteratively Reweighted Least Squares algorithm; default is 50
#'
#' @return Returns the output of the function getModelError, which is a list consisting of:
#' \itemize{
#' \item mape - the Mean Absolute Percentage Error (MAPE) of the current model
#' \item rmse - the Root Mean Square Error (RMSE) of the current model
#' }
#' @export
doRLR <- function(train,test,mscale){
  colnames(train)[1] <- "train.y"

  test.x <- test[,-1]
  test.y <- test[,1]

  invisible(model <- lmrob(train.y ~. , data = train, maxit.scale = mscale, na.action = na.roughfix))
  pred <- predict(model, test.x)

  return(getModelError(pred,test.y))
}
