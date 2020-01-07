###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

#Mean-Centred Normalisation - Range Scaling
#
#         x - mean(x)
# x' = _________________
#       max(x) - min(x)
#
#

#' meannormScale uses the Mean Normalisation method to scale a matrix of data
#' @details This function uses the equation x' = (x - mean(x))/(max(x) - min(x)) to scale the matrix.
#' @param xMatrix matrix of data to be scaled
#' @return returns the variable scalex, which is the scaled matrix
#' @export
meannormScale <- function(xMatrix){
  meanx <- mean(xMatrix)
  minx <- min(xMatrix)
  maxx <- max(xMatrix)
  dimx <- dim(xMatrix)

  meanX <- matrix(rep(meanx, (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])
  minX <- matrix(rep(minx, (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])
  maxX <- matrix(rep(maxx, (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])

  scalex <- (xMatrix - meanX)/(maxX - minX)
  return(scalex)
}
