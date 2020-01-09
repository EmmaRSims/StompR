###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University        #
#     2019/2020                   #
###################################

#Mean Centering Normalisation
#
# x' = x - mean(x)
#

#' centerScale uses the Mean Centering Normalisation method to scale the dataset
#' @details This function uses the equation x' = x - mean(x) to mean center scale all data within the matrix.
#' @param xMatrix matrix of data to be scaled
#' @return returns the variable scalex, which is the scaled matrix
#' @export
centerScale <- function(xMatrix){
  dimx <- dim(xMatrix)
  meanx <- matrix(rep(mean(xMatrix), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])

  scalex <- xMatrix - meanx
  return(scalex)
}
