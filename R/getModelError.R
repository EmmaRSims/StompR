###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

#
# Get the Mean Absolute Percentage Error (MAPE)
# And Get the Root Mean Square Error (RMSE)
#

getModelError <- function(Predicted, Actual){
  diff_ <- abs(Predicted - Actual)

  MAPE <- 100*(mean((diff_/abs(Actual))))
  RMSE <- sqrt(mean((diff_)^2))

  return(list("mape" = MAPE, "rmse" = RMSE))
}
