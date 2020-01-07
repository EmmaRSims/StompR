###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

#Pareto Normalisation
#
#         x - mean(x)
# x' = _______________
#         sqrt(sd(x))
#

#' paretoScale uses the Pareto Normalisation method to scale the input matrix
#' @details This function uses the equation x' = (x - mean(x))/sqrt(sd(x)) to scale the input matrix.
#' @param xMatrix matrix of data to be scaled
#' @return returns the variable scalex, which is the scaled matrix
#' @export
paretoScale <- function(xMatrix){
  dimx <- dim(xMatrix)
  sdx <- sd(xMatrix)

  meanx <- matrix(rep(mean(xMatrix), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])
  sqrtsdx <- matrix(rep(sqrt(sdx), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])

  scalex <- (xMatrix - meanx)/sqrtsdx
  return(scalex)
}


