###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University        #
#     2019/2020                   #
###################################

#' splitData Splits the Data Sets into Training and Testing data
#' @details Uses the proportion to generate an index vector of random integers between 1 and proportion*length(yVector).
#' Then xMatrix is split using the index vector to extract rows, and stored as train.x and test.x.
#' The same is done to the yVector to generate train.y and test.y.
#' These are then combined into dataframs, where train contains train.x and train.y, and test contains test.x and test.y.
#' These two dataframes are returned in a list.
#'
#' @param xMatrix a matrix which assumes the columns are factors, and the rows are samples
#' @param yVector a vector which should be the same length as
#' @param proportion the proportion of rows to keep in the training dataset to build the model, value between 0 and 1
#'
#' @return returns a list of the following:
#' \itemize{
#' \item train, a dataframe consisting of:
#' \itemize{
#' \item train.x - a matrix with rows from the xMatrix input to build the model
#' \item train.y - a vector with the corresponding yVector rows to train.x
#' }
#' \item test, a dataframe consisting of:
#' \itemize{
#' \item test.x - a matrix with rows from the xMatrix input to test the model performance
#' \item test.y - a vector with the corresponding yVector rows to test.x
#' }
#' }
#'
#' @export
splitData <- function(xMatrix, yVector, proportion){
  if(missing(xMatrix)){stop("xMatrix is missing")}
  if(missing(yVector)){stop("yVector is missing")}
  if(missing(proportion)){proportion = 0.7}
  if(length(yVector) != nrow(xMatrix)){stop("The number of rows in xMatrix does not equal the length of yVector")}
  if((proportion < 0)||(proportion > 1)){stop("Invalid Proportion, value should be between 0 and 1")}

  n = floor(length(yVector)*proportion)
  index = sample(1:length(yVector),n)

  train.x = xMatrix[index,] # train
  train.y = yVector[index]
  test.x = xMatrix[-index,]  # test
  test.y = yVector[- index]  # real

  train.combined = cbind(train.y, train.x)
  test.combined = cbind(test.y,test.x)

  return(list("train" = data.frame(train.combined), "test" = data.frame(test.combined)))
}
