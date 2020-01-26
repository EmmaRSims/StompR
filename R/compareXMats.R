###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University        #
#     2019/2020                   #
###################################


# This is an extra function for those who want to compare different datasets with the same response variable


#' startStompingMultiple This function compares different statistical methods as well as different xMatrices vs the same yVector response factor.
#' @details
#' A time-saving method of analysing different xMatrix datasets against the same yVector response factor.
#' It also compares each xMatrix versus a variety of different statistical models; the available models to compare are:
#' \itemize{
#' \item Ordinary Least Squares Regression (OLSR)
#' \item Stepwise Linear Regression - Both directions (SLR)
#' \item Stepwise Linear Regression - Forwards direction (SLRf)
#' \item Stepwise Linear Regression - Backwards direction (SLRb)
#' \item Principal Components Regression (PCR)
#' \item Partial Least Squares Regression (PLSR)
#' \item Random Forest Regression (RFR)
#' \item Support Vector Machine (SVM)
#' \item K-Nearest Neighbours Regression (KNN)
#' \item Generalised Boosted Modelling (GBM)
#' \item Robust Linear Regression (RLR)
#' }
#' There are two methods of analysing the performance of each model, these are the Root Mean Square Error (RMSE) and the Mean Absolute Percentage Error (MAPE);
#' there are two heatmaps produced and stored as files in the file path specified, one for each of the perfomance analysis methods.
#' If permission is not specified as true, it will request a user input to confirm consent to create images on the user computer, this step can be skipped/automated by setting that parameter to true.
#'
#' @param file_path string String to a folder where the output plots are stored.
#' @param xMatrices A list of matrices where each column is considered a factor to be modelled. Names of columns will automatically be used if provided, and the names in the list will be used as the name of that dataset.
#' @param yVector A vector containing the response factor to be modelled
#' @param logV Boolean value, if TRUE then transform the xMatrix using log to base 10
#' @param transformV Transformation and scaling method to be applied to xMatrix, raw data is used by default, the options are: raw, center, minmax, meannorm, zscore, pareto, vast, level
#' @param meth Numerical vector of statistical methods to be compared, all of them are used by default if left blank. c(1:11) are indices of methods: "OLSR", "SLR-both", "SLR-forward", "SLR-backward", "PCR", "PLSR", "RFR", "SVM", "KNN", "GBM", "GLMR"
#' @param prop Proportion of data to be used in the training of the model. Value between 0 and 1. For example, 0.7 is 70\% of the data used for training the model.
#' @param seed Initial seed for splitting training and testing datasets. Used for reproducability of results.
#' @param iter How many models are built to assess the overall accuracy of the method
#' @param plsr_ncomp The number of components used for the principal components regression method
#' @param rfr_ntree The number of trees used in the Random Forest Regression method
#' @param svm_gamma The gamma used for the Support Vector Machine method
#' @param svm_epsilon The epsilon used for the Support Vector Machine method
#' @param svm_cost The cost used for the Support Vector Machine Method
#' @param knn_knum The number of K-Centroids used in the K-Nearest Neighbours method
#' @param gbm_ntree The number of trees used in the Generalised Boosted regression method
#' @param gbm_shrink The shrink used in the Generalised Boosted regression method
#' @param gbm_dist The distribution used in the Generalised Boosted regression method
#' @param gbm_node The number of nodes used in the Generalised Boosted regression method
#' @param rlr_mscale The mscale used for the Robust Linear regression method
#' @param permission true,false Permission for this package to create files in the file path specified.
#'
#' @return The performance plots and final heatmap plots are in the file path specified. Also returned is a dataframe with the following:
#' \itemize{
#' \item heatmap_methods - The heatmap methods order
#' \item heatmap_matrix - The heatmap matrix/dataset name order
#' \item heatmap_values_mape - The heatmap MAPE values in corresponding order to heatmap_methods and heatmap_matrix
#' \item heatmap_values_rmse - The heatmap RMSE values in corresponding order to heatmap_methods and heatmap_matrix
#' }
#'
#'@export
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
  if(is.null(dataset_names)){dataset_names = c(1:length(xMatrices))}

  #Empty Heatmap Values
  heatmap_methods     <- c()
  heatmap_matrix      <- c()
  heatmap_values_rmse <- c()
  heatmap_values_mape <- c()

  models <- vector(mode = "list", length = length(xMatrices))

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
    temp_list <- vector(mode = "list", length = length(meth))

    for(meth_ind in 1:ncol(stomp_output$rmse_raw)){
      row_ind = which.min(stomp_output$rmse_raw[,meth_ind])
      model_ind = ((row_ind-1)*ncol(stomp_output$rmse_raw))+meth_ind
      temp_list[[meth_ind]] <- stomp_output$models[[model_ind]]
    }

    models[[i]] <- temp_list

  }

  rmse_data <- data.frame(heatmap_methods, heatmap_matrix, heatmap_values_rmse)
  hm_rmse_round <- round(heatmap_values_rmse,3)
  lim_rmse <- 1.2*max(hm_rmse_round); if(1 > lim_rmse){lim_rmse <- 1}
  png(paste0(file_path, "/heatmap_rmse.png"))
  p <- ggplot(rmse_data, aes(y=heatmap_methods, x=heatmap_matrix, fill=heatmap_values_rmse)) + geom_tile() + geom_text(aes(label=hm_rmse_round)) + scale_fill_gradient(low="green", high="red", limits = c(0,lim_rmse)) + labs(x = "", y = "", fill="RMSE Values") + theme(axis.text.x = element_text(angle=30, hjust = 1, vjust = 1))
  plot(p)
  dev.off()

  mape_data <- data.frame(heatmap_methods, heatmap_matrix, heatmap_values_mape)
  hm_mape_round <- round(heatmap_values_mape,3)
  lim_mape <- 1.2*max(hm_mape_round); if(100 > lim_mape){lim_mape <- 100}
  png(paste0(file_path, "/heatmap_mape.png"))
  p <- ggplot(mape_data, aes(y=heatmap_methods, x=heatmap_matrix, fill=heatmap_values_mape)) + geom_tile() + geom_text(aes(label=hm_mape_round)) + scale_fill_gradient(low="green", high="red", limits = c(0,lim_mape)) + labs(x = "", y = "", fill="MAPE Values (%)") + theme(axis.text.x = element_text(angle=30, hjust = 1, vjust = 1))
  plot(p)
  dev.off()

  output <- data.frame(heatmap_methods, heatmap_matrix, heatmap_values_mape, heatmap_values_rmse)
  output_list <- list("heatmap_dataframe" = output, "best_models" = models)

  return(output_list)

}
