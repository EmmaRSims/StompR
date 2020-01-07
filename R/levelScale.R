###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

#Level Normalisation
#
#       x - mean(x)
# x' = _____________
#        mean(x)
#

#' levelScale uses the Level Normalisation method to scale a matrix
#' @details uses the equation x' = (x - mean(x))/mean(x) to level scale the input matrix.
#' @param xMatrix matrix of data to be scaled
#' @return returns the variable scalex, which is the scaled matrix
#' @export
levelScale <- function(xMatrix){
  dimx <- dim(xMatrix)
  meanx <- matrix(rep(mean(xMatrix), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])

  scalex <- (xMatrix - meanx)/meanx
  return(scalex)
}

