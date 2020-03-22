#Correlation Matrix & Scree Plot

#' extractFactors creates a correlogram of the xMatrix data, correlation heatmap, biplot, and a scree plot
#' @details Takes a matrix of data for which each column is a continous factor and generates a scree plot, PCA biplot, correlation heatmap, and a correlogram.
#' @param file_path file path to the folder where the images are stored
#' @param xMatrix matrix of data where each column is a factor
#' @param yVector is a numeric vector containing the response factor
#' @param title name of the matrix to append to title
#' @param group a vector containing the categorical groups for the PCA analysis - if not supplied, they are assigned to the same group
#' @param center_ Boolean value which changes whether the xMatrix is centered during the PCA analysis; set to TRUE by default
#' @param scale_ Boolean value that determines whether the xMatrix is scaled during the PCA analysis; set to TRUE by default
#'
#' @return xMatrix as the original dataset
#'
#' @export
extractFactors <- function(file_path, xMatrix, yVector, title, group, center_, scale_){
  if(missing(center_)){center_ = T}
  if(missing(scale_)){scale_ = T}
  if(missing(group)){group = rep("0",length(xMatrix[,1]))}
  if(missing(title)){title = ""}
  if(missing(file_path)){stop("File Path is missing")}
  if(missing(xMatrix)){stop("xMatrix is missing")}
  if(missing(yVector)){stop("yVector is missing")}

  require(corrgram)
  require(reshape2)
  require(ggplot2)
  require(stats)
  require(ggbiplot)

  #create folder for factor analysis plots
  folder_path <- paste0(file_path, "/Factor_Analysis/")
  suppressWarnings(dir.create(folder_path))


  corrMatrix = cbind(xMatrix, yVector)

  #correlation heatmap
  corMat <- cor(corrMatrix)
  corMat[upper.tri(corMat)] <- 0
  corMat <- melt(corMat)

  png(paste0(folder_path, "corrmap.png"), width = 1200, height = 800)
  p <- ggplot(data = corMat, aes(x = Var1, y = Var2, fill = corMat$value)) + geom_tile(color = "white") + geom_text(aes(label=round(corMat$value,3))) + scale_fill_gradient2(low = "green", high = "green", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed()
  plot(p)
  dev.off()

  #corrgram
  png(paste0(folder_path,"corrgram.png"), width = 800, height = 800)
  corrgram(corrMatrix, order=TRUE, lower.panel=panel.ellipse, upper.panel=panel.pts, text.panel=panel.txt, diag.panel=panel.minmax, main=title)
  dev.off()


  #PCA Plot & Scree Plot
  prcomp_fit <- prcomp(xMatrix, center = center_, scale. = scale_)
  png(paste0(folder_path,"biplot.png"), width = 800, height = 800)
  g <- ggbiplot(prcomp_fit, obs.scale = 1, var.scale = 1, groups = group, ellipse = T, circle = T) + scale_color_discrete(name = '') + theme(legend.direction = 'horizontal', legend.position = 'top')
  plot(g)
  dev.off()


  png(paste0(folder_path,"screeplot.png"), width = 800, height = 800)
  g <- ggscreeplot(prcomp_fit)
  plot(g)
  dev.off()

  return(1)



}
