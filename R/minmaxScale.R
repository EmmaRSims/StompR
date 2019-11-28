###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

#Min-Max Normalisation
#
#         (x - min)
# x' <- ____________
#        (max - min)
#

minmaxScale <- function(xMatrix){
  minx <- min(xMatrix)
  maxx <- max(xMatrix)
  dimx <- dim(xMatrix)

  ddimx <- (dimx[1]*dimx[2])

  diffAB <- maxx - minx
  minMat <- matrix(rep(minx,ddimx), nrow = dimx[1], ncol=dimx[2])
  maxMat <- matrix(rep(maxx,ddimx), nrow = dimx[1], ncol=dimx[2])
  diffMat <- matrix(rep(diffAB,ddimx), nrow = dimx[1], ncol=dimx[2])

  scalex <- (xMatrix - minx)/(diffMat)
  return(scalex)

}
