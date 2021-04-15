
tune.brt <- function(dtrain, n.rounds = 512, n.threads = 4){
  #' Uses 5-fold cross-validation to tune the xgboost algorithm to data 
  #' Requires 'xgboost' library
  #' Requires 'dplyr' library  
  #' Requires 'magrittr' library
  #' Requires 'Matrix' library
  #' Requires 'data.table' library
  #' Requires 'pROC' library
  #' 
  #' Args:
  #'  dtrain: dgCMatrix
  #'  nrounds: the number of decision trees/boosting iterations
  #'  nthreads: the numbers of cores to run on
  # Returns:
  #   A matrix of evaluation metrics and associated parameters
  
  k = 5
  
  #evalmetrics <- list("error", "logloss")
  
  eval.log <- matrix(0 , ncol = 6)
  colnames(eval.log) <- c("TSS", "AUC", "F1", "eta","gamma", "alpha")
  for(alpha in seq(0.35, 0.55, by = 0.05)) {
    output.BRT <- list()
    for (eta in seq(0.01, 0.05, by = 0.01)) {
      for (gamma in seq(0.10, 0.4, by = 0.075)) {
        # 5-fold cross validation to determine best model parameters
        # set gamma > 0, lowered eta, to help with overfitting
        set.seed(2048)
        xgbcv <- xgb.cv(params = list(max.depth = 3, nthread = n.threads, 
                                      eta = eta, 
                                      alpha = alpha, 
                                      gamma = gamma,
                                      objective = "binary:logistic"),
                        data = dtrain, 
                        stratified = TRUE,
                        verbose = F,
                        nfold = k,
                        nrounds = n.rounds,
                        #metrics = evalmetrics,
                        scale_pos_weight = 15,
                        prediction = T
                        )
        # prediction probabilities
        pred <- xgbcv$pred
        
        # Set cutoff threshold
        pred.df <- data.frame(true.zoostat = tmp_prot_Train$zoostat,
                              pred.zoostat = pred)
        # threshold <- filter(pred.df, true.zoostat == 1) %>% 
        #   summarise(min(pred.zoostat)) %>% as.numeric()
        # threshold <- 13/228
        
        # Choose threshold at which gives the largest G-mean score - sqrt(TPR*TNR)
        roc.test <- pROC::roc(response = pred.df$true.zoostat, predictor = pred.df$pred.zoostat)
        threshold <- roc.test$thresholds[which.max(sqrt(roc.test$sensitivities*roc.test$specificities))]
        
        pred.class <- ifelse(pred >= threshold, 1, 0) %>% as.factor()
        # actual zoostat
        real.class <- tmp_prot_Train$zoostat %>% as.factor()
        
        # Create the confusion matrix and get TSS and F1
        conf.mat <- confusionMatrix(pred.class, real.class, positive="1")
        TPR <- conf.mat[["byClass"]][["Sensitivity"]]
        TNR <- conf.mat[["byClass"]][["Specificity"]]
        TSS <- TPR + TNR - 1
        
        F1 <- conf.mat[["byClass"]][["F1"]]
        
        # Get AUC
        
        AUC <- pROC::auc(roc.test)
        
        #mean.logloss <- tail(xgbcv$evaluation_log$test_logloss_mean, 1)
        #mean.error <- tail(xgbcv$evaluation_log$test_error_mean, 1)
        
        # save eval log for each parameter combo
        eval.metrics <- c(TSS, AUC, F1, eta, gamma, alpha)
        eval.log <- rbind(eval.log, eval.metrics) %>% as.data.frame()
      }    
    }
  }
  return((eval.log)[-1, ])
}
