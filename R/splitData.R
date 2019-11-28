###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

#
#   Split the Data Sets into Testing, Training, and Reals
#

splitData <- function(xMatrix, yVector, proportion){
  n = floor(length(yVector)*proportion)
  index = sample(1:length(yVector),n)

  train.x = xMatrix[index,] # train
  train.y = yVector[index]
  test.x = xMatrix[-index,]  # test
  test.y = yVector[- index]  # real

  train.combined = cbind(train.y, train.x)
  test.combined = cbind(test.y,test.x)

  return(list("train" = data.frame(train.combined), "test" = data.frame(test.combined)))
}
