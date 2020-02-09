#Correlation Matrix & Scree Plot

#' extractFactors creates a correlogram of the xMatrix data, and a scree plot, then returns the factors with the highest eigenvalues
#' @details
#' @export
extractFactors <- function(file_path, xMatrix, title){
  require(corrgram)

  corMat <- cor(xMatrix)
  corMat <- round(corMat, 2)

  x11()
  corrgram(xMatrixs, order=TRUE, lower.panel=panel.ellipse, upper.panel=panel.pts, text.panel=panel.txt, diag.panel=panel.minmax, main=title)
}
