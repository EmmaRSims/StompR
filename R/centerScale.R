#Mean Centering Normalisation
#
# x' = x - mean(x)
#

centerScale <- function(xMatrix){
  dimx <- dim(xMatrix)
  meanx <- matrix(rep(mean(xMatrix), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])

  scalex <- xMatrix - meanx
  return(scalex)
}
