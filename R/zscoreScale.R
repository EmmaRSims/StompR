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

zscoreScale <- function(xMatrix){
  dimx <- dim(xMatrix)
  meanx <- matrix(rep(mean(xMatrix), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])
  sdx <- matrix(rep(sd(xMatrix), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])

  scalex <- (xMatrix - meanx)/sdx
  return(scalex)
}


