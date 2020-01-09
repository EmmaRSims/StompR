###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

#
# OLSR - Ordinary Least Squares Regression
#

#' doOLSR Applies the Ordinary Least Squares Regression (OLSR) model on the training data,
#' and calculates the Root Mean Square Error (RMSE) and Mean Absolute Percentage Error (MAPE) from the predicted values of the testing data.
#' @details This function applies a basic linear model to the input training data (see \code{\link[stats]{lm}}),
#' then calculates the RMSE and MAPE with \code{\link[StompR]{getModelError}} using the predicted values calculated from the testing data.
#'
#' @import stats
#'
#' @param train Dataframe of training data, the first column is the response factor to be modelled by the remaining columns
#' @param test Dataframe of testing data, the first column is the response factor, the rest of the columns contain factors used to predict the response column with the built model
#'
#' @return Returns the output of the function getModelError, which is a list consisting of:
#' \itemize{
#' \item mape - the Mean Absolute Percentage Error (MAPE) of the current model
#' \item rmse - the Root Mean Square Error (RMSE) of the current model
#' }
#' @export
doOLSR <- function(train,test){
  colnames(train)[1] <- "train.y"

  test.x <- test[,-1]
  test.y <- test[,1]

  invisible(model <- lm(train.y ~. , data = train))
  pred <- predict(model, test.x)

  return(getModelError(pred,test.y))
}
