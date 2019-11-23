#Pareto Normalisation
#
#         x - mean(x)
# x' = _______________
#         sqrt(sd(x))
#

#MATRIX
paretoScale <- function(xMatrix){
  dimx <- dim(xMatrix)
  sdx <- sd(xMatrix)

  meanx <- matrix(rep(mean(xMatrix), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])
  sqrtsdx <- matrix(rep(sqrt(sdx), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])

  scalex <- (xMatrix - meanx)/sqrtsdx
  return(scalex)
}


