###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University        #
#     2019/2020                   #
###################################

#
# Get the Mean Absolute Percentage Error (MAPE)
# And Get the Root Mean Square Error (RMSE)
#

#' getModelError is a function that calculates the Root Mean Square Error (RMSE) and Mean Absolute Percentage Error (MAPE) from the Predicted values and corresponding Real values
#'
#' @param Predicted The output of the function \code{\link[stats]{predict}}
#' @param Actual The corresponding real values to the testing data used to calculate the Predicted values
#'
#' @return Returns:
#' \itemize{
#' \item mape - the Mean Absolute Percentage Error (MAPE) of the current model
#' \item rmse - the Root Mean Square Error (RMSE) of the current model
#' }
#' If they are infinite values, this function changes them to 999 to avoid Inf errors
#' @export
getModelError <- function(Predicted, Actual){
  diff_ <- abs(Predicted - Actual)

  MAPE <- 100*(mean((diff_/abs(Actual))))
  RMSE <- sqrt(mean((diff_)^2))

  if(is.infinite(MAPE)){MAPE = 999}
  if(is.infinite(RMSE)){RMSE = 999}

  return(list("mape" = MAPE, "rmse" = RMSE))
}
