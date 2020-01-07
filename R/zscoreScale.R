###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

#Z-Score Normalisation
#
#        x - mean(x)
# x' = _____________
#          sd(x)
#

#' zscoreScale uses the Z-Score Normalisation method to scale the input matrix
#' @details This function uses the equation x' = (x - mean(x))/sd(x) to normalise the input matrix
#' @param xMatrix matrix of data to be scaled
#' @return returns the variable scalex, which is the scaled matrix
#' @export
zscoreScale <- function(xMatrix){
  dimx <- dim(xMatrix)
  meanx <- matrix(rep(mean(xMatrix), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])
  sdx <- matrix(rep(sd(xMatrix), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])

  scalex <- (xMatrix - meanx)/sdx
  return(scalex)
}


