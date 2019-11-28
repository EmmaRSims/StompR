###################################
#     STOMPR                      #
#     Emma Sims                   #
#     Cranfield University 2019   #
###################################

# Hi there!
# This is a package aimed at comparing statistical models to fit your data
# If you're looking at this, you're probably going to judge my programming skills
# I'm not sorry, it's bad, but hopefully readable
# I'll even leave comments for you, because I'm nice like that

# Don't forget to add the argument descriptions pls




startStomping  <- function(file_path, xMatrix, yVector, logV, transformV, meth, prop, seed, iter, plsr_ncomp, rfr_ntree, svm_gamma, svm_epsilon, svm_cost, knn_knum, gbm_ntree, gbm_shrink, gbm_dist, gbm_node, rlr_mscale, permission){
  ##-----PREAMBLE
  #Room of Requirement
  require(pls)
  require(randomForest)
  require(e1071)
  require(FNN)
  require(gbm)
  require(robustbase)

  #Graphics, you have chosen... poorly.
  if(!is.null(dev.list())){dev.off()}

  #Find any missing hobbitses
  if((missing(file_path)) || (!dir.exists(file_path))){stop("Please enter a valid file path to store the output of this function")}
  if(missing(xMatrix)){stop("xMatrix is missing")}
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
  if(class(xMatrix) != "matrix"){stop("xMatrix is not a matrix class")}
  if(class(yVector) != "numeric"){stop("yVector is not a numeric class")}
  if(length(xMatrix[,1]) != length(yVector)){stop("xMatrix does not have the same number of rows as yVector")}
  if((length(meth)>1)&&(class(meth) != "integer")){stop("Methods vector is not integer")}
  if((length(meth)==1)&&(class(meth) != "numeric")){stop("Methods vector is not numeric")}
  if((min(meth) < 1)||(max(meth)>11)){stop("Methods vector values out of bounds")}
  if((rfr_ntree < 1) || ((class(rfr_ntree) != "numeric") && (class(rfr_ntree) != "integer"))){stop("rfr_ntree must be a positive integer")}
  if(missing(knn_knum)){knn_knum = 1:(ceiling(length(yVector)*0.5))}


  ##--------------------------------------------------------------------------------------------------------------------------------------
  cat("\n---------------------")
  cat("\n  STARTING STOMPING")
  cat("\n---------------------\n")

  #CONSENT MATTERS.
  if(permission != T){
    cat("\n")
    cat(paste("output path: ",file_path,"\n"))
    permission <- readline(prompt = "Does this package have permission to create and store data in the file path specified? [Y/N]: ")
    if((tolower(permission) != "y") && (tolower(permission) != "yes")){stop("This package needs your explicit permission to alter files on this device.")}
  }


  ##-----CREATE DIRECTORIES TO STORE PLOTS/FILES
  plot_path = paste0(file_path,"/plots"); dir.create(plot_path)



  ##-----EXTRACT METADATA
  if(is.null(names(yVector))){yName <- "yFactor"} else {yName <- names(yVector)}
  if(is.null(colnames(xMatrix))){xName <- c(1:length(xMatrix[1,]))} else {xName <- colnames(xMatrix)}
  method_names = c("OLSR", "SLR-Both", "SLR-For", "SLR-Back", "PCR", "PLSR", "RFR", "SVM", "KNN", "GBM", "GLMR")


  ##-----TRANSFORM DATA
  cat(paste("\nTransforming Data\n---------------------\nLog Transform: ",logV,"\nScaling Method: ",transformV, "\nIterations: ",iter,"\n"))

  #Log Transform Data
  if(logV==T){
    dataM <-log10(xMatrix)
  } else {dataM <- xMatrix}

  #Combine data for scaling
  dataC <- cbind(dataM, yVector)

  #Scale Data, so scaley, like a ssssssssnake
  if(transformV == "raw"){           dataS <- dataC;                }
  else if(transformV == "center"){   dataS <- centerScale(dataC);   }
  else if(transformV == "minmax"){   dataS <- minmaxScale(dataC);   }
  else if(transformV == "meannorm"){ dataS <- meannormScale(dataC); } #Rangescale
  else if(transformV == "zscore"){   dataS <- zscoreScale(dataC);   }
  else if(transformV == "pareto"){   dataS <- paretoScale(dataC);   }
  else if(transformV == "vast"){     dataS <- vastScale(dataC);     }
  else if(transformV == "level"){    dataS <- levelScale(dataC);    }
  else{stop("transformV not valid")}

  #Seperate Data Again
  dimMat <- dim(dataS)
  dMat <- dataS[,1:(dimMat[2]-1)]
  dVec <- dataS[,dimMat[2]]


  ##-----BUILD MODELS
  #Begin Modelling I guess

  RMSE_MAT <- matrix(nrow = iter, ncol = length(meth))
  MAPE_MAT <- matrix(nrow = iter, ncol = length(meth))

  cat("\nBuilding Models...\n [")
  progress_ratio <- iter/10
  progress_step <- progress_ratio
  for(i in 1:iter){
    #set seed for splitting data - different reproducable seed for each iteration
    set.seed((seed+i-1))
    DATA <- splitData(dMat,dVec,prop) #returns a list [train, test]

    train_ <- DATA$train
    test_ <- DATA$test

    #Time to unleash the Meths
    for(j in 1:length(meth)){
      if(meth[j] == 1){     error <- doOLSR(train_, test_)}
      else if(meth[j]==2){  error <- doSLR(train_, test_) }
      else if(meth[j]==3){  error <- doSLRf(train_, test_)}
      else if(meth[j]==4){  error <- doSLRb(train_, test_)}
      else if(meth[j]==5){  error <- doPCR(train_, test_) }
      else if(meth[j]==6){  error <- doPLSR(train_, test_, plsr_ncomp)}
      else if(meth[j]==7){  error <- doRFR(train_, test_, rfr_ntree)}
      else if(meth[j]==8){  error <- doSVM(train_, test_, svm_gamma, svm_epsilon, svm_cost)}
      else if(meth[j]==9){  error <- doKNN(train_, test_, knn_knum)}
      else if(meth[j]==10){ error <- doGBM(train_, test_, gbm_ntree, gbm_shrink, gbm_dist, gbm_node)}
      else if(meth[j]==11){ error <- doRLR(train_,test_,rlr_mscale)}
      else {stop("Method Vector has an incorrect input")}

      RMSE_MAT[i,j] = error$rmse
      MAPE_MAT[i,j] = error$mape
    }

    if(i >= progress_step){
      cat("*")
      progress_step <- progress_step + progress_ratio
    }

  }
  cat("]\n\n Processing Output Data\n---------------------\n")

  #Extract max values for plotting axis purposes
  N <- ceiling(length(meth)/3)
  mean_divisor <- replicate(length(meth),1:iter)

  ##-----CALCULATE PERFORMANCE Y VALUES
  #RMSE Cumulative Mean
  rmse_cum <- RMSE_MAT[1,]
  for(c in 2:iter){
    rmse_cum <- rbind(rmse_cum, (rmse_cum[c-1] + RMSE_MAT[c,]))
  }

  rmse_cum_mean <- rmse_cum/mean_divisor

  #MAPE Cumulative Mean
  mape_cum <- MAPE_MAT[1,]
  for(c in 2:iter){
    mape_cum <- rbind(mape_cum, (mape_cum[c-1] + MAPE_MAT[c,]))
  }
  mape_cum_mean <- mape_cum/mean_divisor


  #Calculate Maximum y axis Limit for plots
  rmse_max = 1.3*max(RMSE_MAT)                    ; if(rmse_max == 0){rmse_max = max(RMSE_MAT)+0.1}
  mape_max = 1.3*max(MAPE_MAT)                    ; if(mape_max == 0){mape_max = max(MAPE_MAT)+0.1}
  rmse_cum_mean_max <- 1.3*max(rmse_cum_mean)     ; if(rmse_cum_mean_max == 0){rmse_cum_mean_max = max(rmse_cum_mean)+0.1}
  mape_cum_mean_max <- 1.3*max(mape_cum_mean)     ; if(mape_cum_mean_max == 0){mape_cum_mean_max = max(mape_cum_mean)+0.1}


  ##-----CREATE PLOTS

  #RMSE
  plotDataPNG(paste0(plot_path,"/RMSE_raw_performance.png"), RMSE_MAT, N, "RMSE", rmse_max, method_names[meth], "RMSE - Raw Performance")
  plotDataPNG(paste0(plot_path,"/RMSE_Cumulative_mean.png"), rmse_cum_mean, N, expression(paste("RMSE Cumulative ", mu)), rmse_cum_mean_max, method_names[meth], "RMSE - Cumulative Mean")

  #MAPE
  plotDataPNG(paste0(plot_path,"/MAPE_raw_performance.png"), MAPE_MAT, N, "MAPE (%)", mape_max, method_names[meth], "MAPE - Raw Performance")
  plotDataPNG(paste0(plot_path,"/MAPE_Cumulative_mean.png"), mape_cum_mean, N, expression(paste("MAPE Cumulative ", mu, " (%)")), mape_cum_mean_max, method_names[meth], "MAPE - Cumulative Mean")

  #Comparison with final Cumulative Means
  rmse_final <- rmse_cum_mean[iter,]
  mape_final <- mape_cum_mean[iter,]

  png(paste0(plot_path,"/Final_Comparison_rmse.png"))
  plot(y=rmse_final,x=1:length(meth), xaxt='n', xlab="", ylab="Mean RMSE", title=expression(paste("Statistical Method Comparison\n",mu," RMSE")))
  axis(1,at=1:length(meth),labels=method_names[meth],las=2)
  dev.off()

  png(paste0(plot_path,"/Final_Comparison_mape.png"))
  plot(y=mape_final,x=1:length(meth), xaxt='n', xlab="", ylab="Mean MAPE (%)", title=expression(paste("Statistical Method Comparison\n",mu," MAPE (%)")))
  axis(1,at=1:length(meth),labels=method_names[meth],las=2)
  dev.off()

  cat("---------------------\n")

  return(list("RMSE_CM" = rmse_cum_mean[iter,], "MAPE_CM" = mape_cum_mean[iter,], "rmse_raw" = RMSE_MAT, "mape_raw" = MAPE_MAT))

}


