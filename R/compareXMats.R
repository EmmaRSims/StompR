###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

# This is an extra function for those who want to compare different datasets with the same response variable

startStompingMultiple <- function(file_path, xMatrices, yVector, logV, transformV, meth, prop, seed, iter, plsr_ncomp, rfr_ntree, svm_gamma, svm_epsilon, svm_cost, knn_knum, gbm_ntree, gbm_shrink, gbm_dist, gbm_node, rlr_mscale, permission = F){
  #Graphics, you have chosen... poorly.
  if(!is.null(dev.list())){dev.off()}

  #Room of Requirement
  require(ggplot2)

  #Find any missing hobbitses
  if((missing(file_path)) || (!dir.exists(file_path))){stop("Please enter a valid file path to store the output of this function")}
  if(missing(xMatrices)){stop("xMatrices is missing")}
  if(missing(yVector)){stop("yVector is missing")}
  if(missing(transformV)){transformV="raw"}
  if(missing(logV)){logV = F}
  if(missing(meth)){meth = c(1:11)} #1:11 are indices of methods: ["OLSR", "SLR-both", "SLR-forward", "SLR-backward", "PCR", "PLSR", "RFR", "SVM", "KNN", "GBM", "GLMR"]
  if(missing(prop)){prop = 0.7}
  if(missing(seed)){seed = sample(1:500,1)}
  if(missing(plsr_ncomp)){plsr_ncomp = 1}
  if(missing(rfr_ntree)){rfr_ntree = 64}
  if(missing(svm_gamma)){svm_gamma = c(seq(0, 0.06, 0.02), seq(0.1, 0.3, 0.1), 0.6)}
  if(missing(svm_epsilon)){svm_epsilon = c(seq(0,0.1,0.03), 0.2, seq(0.3,0.9,0.3))}
  if(missing(svm_cost)){svm_cost = c(seq(1, 20, 3), seq(30, 110, 20))}
  if(missing(gbm_ntree)){gbm_ntree = 100}
  if(missing(gbm_shrink)){gbm_shrink = 0.001}
  if(missing(gbm_dist)){gbm_dist = "gaussian"}
  if(missing(gbm_node)){gbm_node = 1}
  if(missing(rlr_mscale)){rlr_mscale = 500}
  if(missing(iter)){iter = 10}
  if(missing(permission)){permission == F}

  #Got to test for incorrect inputs, don't want to be sneaking any hobbitses into Mordor
  if(class(xMatrices) != "list"){stop("xMatrices is not a list class")}
  if(class(yVector) != "numeric"){stop("yVector is not a numeric class")}
  if((length(meth)>1)&&(class(meth) != "integer")){stop("Methods vector is not integer")}
  if((length(meth)==1)&&(class(meth) != "numeric")){stop("Methods vector is not numeric")}
  if((min(meth) < 1)||(max(meth)>11)){stop("Methods vector values out of bounds")}
  if((rfr_ntree < 1) || ((class(rfr_ntree) != "numeric") && (class(rfr_ntree) != "integer"))){stop("rfr_ntree must be a positive integer")}
  if(missing(knn_knum)){knn_knum = 1:(ceiling(length(yVector)*0.5))}


  #CONSENT MATTERS.
  if(permission != T){
    cat("\n")
    cat(paste("output path: ",file_path,"\n"))
    permission <- readline(prompt = "Does this package have permission to create and store data in the file path specified? [Y/N]: ")
    if((tolower(permission) != "y") && (tolower(permission) != "yes")){stop("This package needs your explicit permission to alter files on this device.")} else {permission = T}
  }


  method_names = c("OLSR", "SLR-Both", "SLR-For", "SLR-Back", "PCR", "PLSR", "RFR", "SVM", "KNN", "GBM", "GLMR")
  dataset_names <- names(xMatrices)
  if(is.null(dataset_names)){dataset_names = c(1:length(d))}

  #Empty Heatmap Values
  heatmap_methods     <- c()
  heatmap_matrix      <- c()
  heatmap_values_rmse <- c()
  heatmap_values_mape <- c()

  cat("\n-------------------------\n")
  cat("START MULTIPLE STOMPS")
  cat("\n-------------------------\n")


  for(i in 1:length(xMatrices)){

    cat(paste("Matrix: ", dataset_names[i]))

    current_x <- xMatrices[[i]]
    if(class(current_x) != "matrix"){stop(paste("In xMatrices list, element ",i, " is not a matrix."))}
    if(length(current_x[,1]) != length(yVector)){stop(paste("In xMatrices list, matrix ",i, " does not have the same number of rows as yVector."))}

    file_path_mat <- paste0(file_path,"/",dataset_names[i])
    dir.create(file_path_mat)
    stomp_output<-startStomping(file_path_mat, current_x, yVector, logV, transformV, meth, prop, seed, iter, plsr_ncomp, rfr_ntree, svm_gamma, svm_epsilon, svm_cost, knn_knum, gbm_ntree, gbm_shrink, gbm_dist, gbm_node, rlr_mscale, permission)

    heatmap_methods     <- c(heatmap_methods, method_names[meth])
    heatmap_matrix      <- c(heatmap_matrix, rep(dataset_names[i], length(meth)))
    heatmap_values_rmse <- c(heatmap_values_rmse, stomp_output$RMSE_CM)
    heatmap_values_mape <- c(heatmap_values_mape, stomp_output$MAPE_CM)

  }

  rmse_data <- data.frame(heatmap_methods, heatmap_matrix, heatmap_values_rmse)
  hm_rmse_round <- round(heatmap_values_rmse,3)
  png(paste0(file_path, "/heatmap_rmse.png"))
  p <- ggplot(rmse_data, aes(y=heatmap_methods, x=heatmap_matrix, fill=heatmap_values_rmse, label=hm_rmse_round)) + labs(x = "", y = "", fill="RMSE Values") + geom_tile() + scale_fill_gradient(low="green", high="red")  + theme(axis.text.x = element_text(angle=30, hjust = 1, vjust = 1))
  plot(p)
  dev.off()

  mape_data <- data.frame(heatmap_methods, heatmap_matrix, heatmap_values_mape)
  hm_mape_round <- round(heatmap_values_mape,3)
  png(paste0(file_path, "/heatmap_mape.png"))
  p <- ggplot(mape_data, aes(y=heatmap_methods, x=heatmap_matrix, fill=heatmap_values_mape, label=hm_mape_round)) + geom_tile() + scale_fill_gradient(low="white", high="red") + labs(x = "", y = "", fill="MAPE Values (%)") + theme(axis.text.x = element_text(angle=30, hjust = 1, vjust = 1))
  plot(p)
  dev.off()


  return(heatmap_values_rmse)
}
