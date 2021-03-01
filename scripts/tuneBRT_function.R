

tune.brt <- function(p, a, n.rounds = 256, n.threads = 4){
  #' Uses 10-fold cross-validation to tune the xgboost algorithm to data on environmental
  #' covariates at presence and absence points
  #' This method tunes for model with largest BAT
  #' Requires 'xgboost' library
  #' Requires 'dplyr' library  
  #' Requires 'magrittr' library
  #' Requires 'Matrix' library
  #' Requires 'data.table' library
  #' 
  #' Args:
  #'  p: covariate data at presence/occurrence points
  #'  a: covariate data at documented absence points
  #'  nrounds: the number of decision trees/boosting iterations
  #'  nthreads: the numbers of cores to run on
  # Returns:
  #   A matrix of evaluation metrics and associated parameters
  # Pool presence and absence data together for presence-absence train-test splits
  pTrain <- p %>% mutate(PA = 1)
  aTrain <- a %>% mutate(PA = 0)
  BRTrain <- bind_rows(pTrain[, 3:20], aTrain[3:20]) #use bind_rows because it can handle is there are issues in columns
  #drop unneeded columns
  # In order for model matrix to work properly, need to set na.action to pass
  previous_na_action <- options('na.action')
  options(na.action='na.pass')
  
  sparse_matrix_BRTrain <- Matrix::sparse.model.matrix(PA ~ ., data = BRTrain)[, -1]
  dtrain <- xgb.DMatrix(data = sparse_matrix_BRTrain, label = BRTrain$PA)
  
  k = 5

  evalmetrics <- list("error", "auc")
  
  eval.log <- matrix(0 , ncol = 5)
  colnames(eval.log) <- c("best.auc","best.error", "best.X","best.gamma", "best.alpha")
  for (alpha in seq(0.35, 0.5, by = 0.5) {
    output.BRT <- list()
    for (X in seq(0.025, 0.15, by = 0.025)) {
      for (gamma in seq(0.15, 0.35, by = 0.05)) {
        # 10-fold cross validation to determine best model parameters (chosen by maximizing BAT)
        # set gamma > 0, lowered eta, to help with overfitting
        xgbcv <- xgb.cv(params = list(max.depth = max.depth, alpha = alpha, nthread = n.threads, 
                                      objective = "binary:logistic", gamma = gamma),
                        data = dtrain, 
                        prediction = T,
                        nfold = k, 
                        stratified = TRUE,
                        nrounds = n.rounds,
                        verbose = F,
                        metrics = evalmetrics)
        # for (i in 1:k){
        #   ids <- unlist(xgbcv$folds[i])
        #   in_fold <- BRTrain[ids,]
        #   P <- as.numeric(row.names(subset(in_fold,PA==1)))
        #   A <- as.numeric(row.names(subset(in_fold,PA==0)))
        #   pres <- xgbcv$pred[P]
        #   abs <- xgbcv$pred[A]
        #   output.BRT[[(i)]] <- get_BAT(pres=pres, abs=abs)   # Change this from BAT to AUC, RMSE, and TSS? you can add evaluation log arg to xgb.cv above
        # }
        #mean indices across all models for this value of p
        # mean.boyce<-mean(unlist(lapply(output.BRT, function(x) x["Boyce"])), na.rm = T)      
        # mean.auc<-mean(unlist(lapply(output.BRT, function(x) x["AUC"])), na.rm = T)      
        # mean.tss<-mean(unlist(lapply(output.BRT, function(x) x["TSS"])), na.rm = T)      
        # mean.bat<-mean(unlist(lapply(output.BRT, function(x) x["BAT"])), na.rm = T)
        mean.auc <- tail(xgbcv$evaluation_log$test_auc_mean, 1)
        mean.error <- tail(xgbcv$evaluation_log$test_error_mean, 1)
        # save eval log for each parameter combo
        eval.metrics <- c(mean.auc, mean.error, X, gamma, alpha)
        eval.log <- rbind(eval.log, eval.metrics)
      }    
    }
  }
  return((eval.log)[-1, ])
}

