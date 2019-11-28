###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

#Variable Stability Scaling (VAST)
#
#       x - mean(x)       mean(x)
# x' = _____________  .  ________
#          sd(x)           sd(x)
#

#MATRIX
vastScale <- function(xMatrix){
  dimx <- dim(xMatrix)
  meanx <- matrix(rep(mean(xMatrix), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])
  sdx <- matrix(rep(sd(xMatrix), (dimx[1]*dimx[2])), nrow = dimx[1], ncol = dimx[2])

  rhs <- meanx/sdx
  lhs <- (xMatrix - meanx)/sdx

  scalex <- rhs * lhs
  return(scalex)
}


