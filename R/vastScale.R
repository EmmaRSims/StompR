###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University        #
#     2019/2020                   #
###################################

#Variable Stability Scaling (VAST)
#
#       x - mean(x)       mean(x)
# x' = _____________  .  ________
#          sd(x)           sd(x)
#

#' vastScale uses the Variable Stability Scaling Transformation (VAST) to normalise the input matrix
#' @details This function uses the equation x' = ((x - mean(x)) / sd(x)) . (mean(x)/sd(x)) to normalise the input matrix.
#' @param xMatrix matrix of data to be scaled
#' @return returns the variable scalex, which is the scaled matrix
#' @export
vastScale <- function(xMatrix){
  dimx <- dim(xMatrix)
  meanx <- matrix(rep(mean(xMatrix), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])
  sdx <- matrix(rep(sd(xMatrix), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])

  rhs <- meanx/sdx
  lhs <- (xMatrix - meanx)/sdx

  scalex <- rhs * lhs
  return(scalex)
}


