###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

#
# SLR - Stepwise Linear Regression
#


#' doSLR performs a Stepwise Linear Regression (SLR) model in both directions on the training data,
#' and calculates the Root Mean Square Error (RMSE) and Mean Absolute Percentage Error (MAPE) from the predicted values of the testing data.
#' @details This function applies the SLR Step algorithm to the training data (see \code{\link[stats]{step}}) with derection set to "both",
#' and then calculates the RMSE and MAPE with \code{\link[StompR]{getModelError}} using the predicted values calculated from the testing data.
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
doSLR <- function(train,test){#Both
  colnames(train)[1] <- "train.y"

  test.x <- test[,-1]
  test.y <- test[,1]

  model <- lm(train.y ~. , data = train)
  invisible(smodel <- step(model, direction = "both", trace = F))
  suppressWarnings(pred <- predict(smodel, test.x))

  return(getModelError(pred,test.y))
}

#' doSLRf performs a Stepwise Linear Regression (SLR) model in the forward direction on the training data,
#' and calculates the Root Mean Square Error (RMSE) and Mean Absolute Percentage Error (MAPE) from the predicted values of the testing data.
#' @details This function applies the SLR Step algorithm to the training data (see \code{\link[stats]{step}}) with derection set to "forward",
#' and then calculates the RMSE and MAPE with \code{\link[StompR]{getModelError}} using the predicted values calculated from the testing data.
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
doSLRf <- function(train,test){#Forward
  colnames(train)[1] <- "train.y"

  test.x <- test[,-1]
  test.y <- test[,1]

  model <- lm(train.y ~. , data = train)
  invisible(smodel <- step(model, direction = "forward", trace = F))
  suppressWarnings(pred <- predict(smodel, test.x))

  return(getModelError(pred,test.y))
}

#' doSLRb performs a Stepwise Linear Regression (SLR) model in the backwards direction on the training data,
#' and calculates the Root Mean Square Error (RMSE) and Mean Absolute Percentage Error (MAPE) from the predicted values of the testing data.
#' @details This function applies the SLR Step algorithm to the training data (see \code{\link[stats]{step}}) with derection set to "backward",
#' and then calculates the RMSE and MAPE with \code{\link[StompR]{getModelError}} using the predicted values calculated from the testing data.
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
doSLRb <- function(train,test){#Backward
  colnames(train)[1] <- "train.y"

  test.x <- test[,-1]
  test.y <- test[,1]

  model <- lm(train.y ~. , data = train)
  invisible(smodel <- step(model, direction = "backward", trace = F))
  suppressWarnings(pred <- predict(smodel, test.x))

  return(getModelError(pred,test.y))
}
