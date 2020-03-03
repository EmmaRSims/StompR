###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University        #
#     2019/2020                   #
###################################

#
# Plot Data, png image
#

plotDataPNG <- function(file_path, mat, N, yLab, yMax, meth_names, title){
  png(file_path, width = 1000, height = 1000)
  par(mfrow = c(N,3))

  for(i in 1:length(meth_names)){
    plot(x = 1:dim(mat)[1], y = mat[,i], type="l", ylim=c(0,yMax), xlab = "Iterations", ylab = yLab)
    title(meth_names[i])
  }
  title(title, outer = T)

  dev.off()
}
