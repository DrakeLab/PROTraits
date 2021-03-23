
tune.brt <- function(dtrain, n.rounds = 256, n.threads = 4){
  #' Uses 5-fold cross-validation to tune the xgboost algorithm to data 
  #' Requires 'xgboost' library
  #' Requires 'dplyr' library  
  #' Requires 'magrittr' library
  #' Requires 'Matrix' library
  #' Requires 'data.table' library
  #' 
  #' Args:
  #'  dtrain: dgCMatrix
  #'  nrounds: the number of decision trees/boosting iterations
  #'  nthreads: the numbers of cores to run on
  # Returns:
  #   A matrix of evaluation metrics and associated parameters
  
  k = 5
  
  evalmetrics <- list("error", "auc")
  
  eval.log <- matrix(0 , ncol = 6)
  colnames(eval.log) <- c("accuracy", "best.auc","best.error", "best.eta","best.gamma", "best.alpha")
  for(alpha in seq(0.35, 0.5, by = 0.05)) {
    output.BRT <- list()
    for (eta in seq(0.025, 0.1, by = 0.025)) {
      for (gamma in seq(0.15, 0.35, by = 0.05)) {
        # 5-fold cross validation to determine best model parameters
        # set gamma > 0, lowered eta, to help with overfitting
        set.seed(2148)
        xgbcv <- xgb.cv(params = list(max.depth = 3, alpha = alpha, nthread = n.threads, 
                                      objective = "binary:logistic", gamma = gamma),
                        data = dtrain, 
                        prediction = T,
                        nfold = k, 
                        stratified = TRUE,
                        nrounds = n.rounds,
                        verbose = F,
                        metrics = evalmetrics,
                        scale_pos_weight = 15
                        )
        # prediction probabilities
        pred <- xgbcv$pred
                # Set cutoff threshold
        pred.class <- ifelse(pred >= 0.5, 1, 0) %>% as.factor()
                # actual zoostat
        real.class <- tmp_prot_Train$zoostat %>% as.factor()
        
        # Create the confusion matrix and get accuracy
        accuracy <- confusionMatrix(pred.class, real.class, positive="1")[["overall"]][["Accuracy"]]
        mean.auc <- tail(xgbcv$evaluation_log$test_auc_mean, 1)
        mean.error <- tail(xgbcv$evaluation_log$test_error_mean, 1)
        # save eval log for each parameter combo
        eval.metrics <- c(accuracy, mean.auc, mean.error, eta, gamma, alpha)
        eval.log <- rbind(eval.log, eval.metrics)
      }    
    }
  }
  return((eval.log)[-1, ])
}
