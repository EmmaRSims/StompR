###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University        #
#     2019/2020                   #
###################################

#
# RFR - Random Forest Regression
#

#' doRFR Applies the Random Forest Regression (RFR) method to the training data,
#' and calculates the Root Mean Square Error (RMSE) and Mean Absolute Percentage Error (MAPE) from the predicted values of the testing data.
#' @details This function applies Breiman's RFR algorithm to the training data (see \code{\link[randomForest]{randomForest}}),
#' then calculates the RMSE and MAPE with \code{\link[StompR]{getModelError}} using the predicted values calculated from the testing data.
#'
#' @import randomForest
#'
#' @param train Dataframe of training data, the first column is the response factor to be modelled by the remaining columns
#' @param test Dataframe of testing data, the first column is the response factor, the rest of the columns contain factors used to predict the response column with the built model
#' @param ntree The number of trees to grow, should not be a small number (see \code{\link[randomForest]{randomForest}})
#'
#' @return Returns the output of the function getModelError, which is a list consisting of:
#' \itemize{
#' \item mape - the Mean Absolute Percentage Error (MAPE) of the current model
#' \item rmse - the Root Mean Square Error (RMSE) of the current model
#' }
#' @export
doRFR <- function(train,test,ntree){
  colnames(train)[1] <- "train.y"

  test.x <- test[,-1]
  test.y <- test[,1]

  invisible(model <- randomForest(train.y ~. , data = train, ntree=ntree))
  suppressWarnings(pred.all <- predict(model, test.x, predict.all=T)$individual)
  pred <- apply(pred.all, 1, mean)

  res <- getModelError(pred,test.y)

  return(list("mape" = res$mape, "rmse" = res$rmse, 'model' = model))
}
