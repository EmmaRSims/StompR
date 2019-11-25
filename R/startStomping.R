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




startStomping  <- function(file_path, xMatrix, yVector, logV, transformV, meth, prop, seed, iter, plsr_ncomp, rfr_ntree, svm_gamma, svm_epsilon, svm_cost, knn_knum, gbm_ntree, gbm_shrink, gbm_dist, gbm_node, rlr_mscale, graph_output){
  #REQUIRE ALL THE THINGS
  require(pls)
  require(randomForest)
  require(e1071)
  require(FNN)
  require(gbm)
  require(robustbase)

  #Find any missing hobbitses
  if(missing(file_path)){stop("Please enter a file path to store the output of this function")}
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
  if(missing(graph_output)){graph_output = "rmse"}

  #Got to test for incorrect inputs, don't want to be sneaking any hobbitses into Mordor
  if(class(xMatrix) != "matrix"){stop("xMatrix is not a matrix class")}
  if(class(yVector) != "numeric"){stop("yVector is not a numeric class")}
  if(length(xMatrix[,1]) != length(yVector)){stop("xMatrix does not have the same number of rows as yVector")}
  if((length(meth)>1)&&(class(meth) != "integer")){stop("Methods vector is not integer")}
  if((length(meth)==1)&&(class(meth) != "numeric")){stop("Methods vector is not numeric")}
  if((min(meth) < 1)||(max(meth)>11)){stop("Methods vector values out of bounds")}
  if((rfr_ntree < 1) || ((class(rfr_ntree) != "numeric") && (class(rfr_ntree) != "integer"))){stop("rfr_ntree must be a positive integer")}
  if(missing(knn_knum)){knn_knum = 1:(ceiling(length(yVector)*0.5))}

  cat("\n---------------------")
  cat("\n  STARTING STOMPING")
  cat("\n---------------------\n")

  cat("\n")
  permission <- readline(prompt = "Does this package have permission to create and store data in the file path specified? [Y/N]: ")
  if((tolower(permission) != "y") && (tolower(permission) != "yes")){stop("This package needs your explicit permission to alter files on this device.")}


  ##-----EXTRACT METADATA
  if(is.null(names(yVector))){yName <- "yFactor"} else {yName <- names(yVector)}
  if(is.null(colnames(xMatrix))){xName <- c(1:length(xMatrix[1,]))} else {xName <- colnames(xMatrix)}
  method_names = c("OLSR", "SLR-both", "SLR-forward", "SLR-backward", "PCR", "PLSR", "RFR", "SVM", "KNN", "GBM", "GLMR")


  ##-----TRANSFORM DATA
  cat(paste("\nTransforming Data\n----------\nLog Transform: ",logV,"\nScaling Method: ",transformV, "\nIterations: ",iter,"\n\n"))

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


  }

  #Extract max values for plotting axis purposes
  rmse_max = max(RMSE_MAT)
  mape_max = max(MAPE_MAT)
  N <- ceiling(length(meth)/3)


  if(graph_output == "rmse"){
    par(mfrow = c(N,3))
    for(n in 1:length(meth)){
      plot(1:iter,RMSE_MAT[,n], type="l", ylim=c(0,(rmse_max*1.5)), xlab = "Iterations", ylab = "RMSE")
      title(method_names[meth[n]])
    }
    title("RMSE", outer = T)
  }
  else if(graph_output == "mape"){
    par(mfrow = c(N,3))
    for(n in 1:length(meth)){
      plot(1:iter,MAPE_MAT[,n], type="l", ylim=c(0,(mape_max*1.5)), xlab = "Iterations", ylab = "MAPE (%)")
      title(method_names[meth[n]])
    }
    title("MAPE", outer = T)
  }

}


