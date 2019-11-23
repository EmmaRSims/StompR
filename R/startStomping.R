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




startStomping  <- function(xMatrix, yVector, logV, transformV, meth, prop, seed, iter, plsr_ncomp, rfr_ntree, svm_gamma, svm_epsilon, svm_cost, knn_knum){
  #REQUIRE ALL THE THINGS
  require(pls)
  require(randomForest)
  require(e1071)
  require(FNN)

  #Find any missing hobbitses
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

  #Got to test for incorrect inputs, don't want to be sneaking any hobbitses into Mordor
  if(class(xMatrix) != "matrix"){stop("xMatrix is not a matrix class")}
  if(class(yVector) != "numeric"){stop("yMatrix is not a numeric class")}
  if(length(xMatrix[,1]) != length(yVector)){stop("xMatrix does not have the same number of rows as yVector")}
  if((length(meth)>1)&&(class(meth) != "integer")){stop("Methods vector is not integer")}
  if((length(meth)==1)&&(class(meth) != "numeric")){stop("Methods vector is not numeric")}
  if((min(meth) < 1)||(max(meth)>11)){stop("Methods vector values out of bounds")}
  if((missing(iter)) && (length(yVector > 0))){iter = ceiling(length(yVector)/20)} else {stop("yVector & xMatrix lengths need to be positive integers")}
  if((rfr_ntree < 1) || ((class(rfr_ntree) != "numeric") && (class(rfr_ntree) != "integer"))){stop("rfr_ntree must be a positive integer")}
  if(missing(knn_knum)){knn_knum = 1:(ceiling(length(yVector)*0.5))}

  cat("\n---------------------")
  cat("\n  STARTING STOMPING")
  cat("\n---------------------")

  ##-----EXTRACT METADATA
  if(is.null(names(yVector))){yName <- "yFactor"} else {yName <- names(yVector)}
  if(is.null(colnames(xMatrix))){xName <- c(1:length(xMatrix[1,]))} else {xName <- colnames(xMatrix)}


  ##-----TRANSFORM DATA
  cat(paste("\nTransforming Data\n----------\nLog Transform: ",logV,"\nScaling Method: ",transformV))

  #Log Transform Data
  if(logV==T){
    dataM <-log10(xMatrix)
  } else {dataM <- xMatrix}

  #Combine data for scaling
  dataC <- cbind(dataM, yVector)

  #Scale Data
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

  for(i in 1:iter){
    #set seed for splitting data - different reproducable seed for each iteration
    set.seed((seed+i-1))
    DATA <- splitData(dMat,dVec,prop) #returns a list [train, test]

    train_ <- DATA$train
    test_ <- DATA$test

    #test models
    olsr_error <- doOLSR(train_, test_)
    slr_error  <- doSLR(train_, test_)
    slrf_error <- doSLRf(train_, test_)
    slrb_error <- doSLRb(train_, test_)
    pcr_error  <- doPCR(train_, test_)
    plsr_error <- doPLSR(train_, test_, plsr_ncomp)
    rfr_error  <- doRFR(train_, test_, rfr_ntree)
    svm_error  <- doSVM(train_, test_, svm_gamma, svm_epsilon, svm_cost)
    knn_error  <- doKNN(train_, test_, knn_knum)


    cat(paste("\nOLSR\n MAPE: ", olsr_error$mape,  "%\nRMSE: ", olsr_error$rmse, "\n~~~"))
    cat(paste("\nSLR\n MAPE: ",  slr_error$mape,   "%\nRMSE: ", slr_error$rmse,  "\n~~~"))
    cat(paste("\nSLRf\n MAPE: ", slrf_error$mape,  "%\nRMSE: ", slrf_error$rmse, "\n~~~"))
    cat(paste("\nSLRb\n MAPE: ", slrb_error$mape,  "%\nRMSE: ", slrb_error$rmse, "\n~~~"))
    cat(paste("\nPCR\n MAPE: ",  pcr_error$mape,   "%\nRMSE: ", pcr_error$rmse,  "\n~~~"))
    cat(paste("\nPLSR\n MAPE: ", plsr_error$mape,  "%\nRMSE: ", plsr_error$rmse, "\n~~~"))
    cat(paste("\nRFR\n MAPE: ",  rfr_error$mape,   "%\nRMSE: ", rfr_error$rmse,  "\n~~~"))
    cat(paste("\nSVM\n MAPE: ",  svm_error$mape,   "%\nRMSE: ", svm_error$rmse,  "\n~~~"))
    cat(paste("\nKNN\n MAPE: ",  knn_error$mape,   "%\nRMSE: ", knn_error$rmse,  "\n~~~"))


  }



}


