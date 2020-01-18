###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University        #
#     2019/2020                   #
###################################

#
# GBM - Generalised Boosted Regression
#

#' doGBM fits the Generalised Boosted Regression model to the training data,
#' and calculates the Root Mean Square Error (RMSE) and Mean Absolute Percentage Error (MAPE) from the predicted values of the testing data.
#' @details This function uses the gbm package to fit a Generalised Boosted Regression model to the input training data,
#' then predicts the reponse variable using the all but the first column of the test dataframe,
#' and calculates the Mean Absolute Percentage Error (MAPE) and the Root Mean Square Error (RMSE) of that particular model,
#' using the difference in predicted and real (first column) values of the test dataset. (See documentation from \code{\link[gbm]{gbm}} and \code{\link[StompR]{getModelError}} for more information.)
#'
#' @import gbm
#'
#' @param train Dataframe of training data, the first column is the response factor to be modelled by the remaining columns
#' @param test Dataframe of testing data, the first column is the response factor, the rest of the columns contain factors used to predict the response column with the built model
#' @param ntree Integer which specifies the number of trees to be fitted within the model. The default is 100; see the \code{\link[gbm]{gbm}} for more information.
#' @param shrink The learning rate/step size reduction. The default is 0.1; see \code{\link[gbm]{gbm}} for more information.
#' @param dist The distribution of the data. Default is "bernoulli", unless used as part of startStomping, in which case "gaussian" is default; the available ones are:
#' \itemize{
#' \item "gaussian"
#' \item "laplace"
#' \item "tdist"
#' \item "bernoulli"
#' \item "huberized"
#' \item "adaboost"
#' \item "poisson"
#' \item "coxph"
#' \item "quantile"
#' \item "pairwise"
#' }
#' See \code{\link[gbm]{gbm}} for more information.
#' @param nnode Integer specifying the the minimum number of observations in the terminal nodels of the trees; see \code{\link[gbm]{gbm}} for more information.
#'
#' @return Returns the output of the function getModelError, which is a list consisting of:
#' \itemize{
#' \item mape - the Mean Absolute Percentage Error (MAPE) of the current model
#' \item rmse - the Root Mean Square Error (RMSE) of the current model
#' }
#' @export
doGBM <- function(train,test,ntree,shrink,dist,nnode){
  test.x <- test[,-1]
  test.y <- test[,1]

  colnames(train)[1] <- "train.y"

  invisible(model <- gbm(train.y ~. , data = train, n.trees = ntree, shrinkage = shrink, distribution = dist, n.minobsinnode = nnode))
  pred <- predict(model, test.x, ntree)
  res <- getModelError(pred,test.y)

  return(list("mape" = res$mape, "rmse" = res$rmse, 'model' = model))
}
