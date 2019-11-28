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

#MATRIX
levelScale <- function(xMatrix){
  dimx <- dim(xMatrix)
  meanx <- matrix(rep(mean(xMatrix), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])

  scalex <- (xMatrix - meanx)/meanx
  return(scalex)
}

