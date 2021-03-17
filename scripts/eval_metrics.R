# Evaluation Metrics
## MV Evans, D Suh, C Tietelbaum, J Vaz

#' This script is meant to be sourced in order to run evaluation on different models. 
#' It includes an individual function for each metric, a function to aggregate three metrics,
#' as well as a function that will run a subset (or all of them).
#' 
#' Evaluation Metrics Included:
#' 1.  BAT [get_BAT]
#' 2.  Area under the ROC curve [get_AUC]
#' 3.  Boyce Index [get_boyce]
#' 4.  True Skill Statistic [get_TSS] 
#' 5.  Absolute Validation Index [get_AVI]
#' 6.  Contrast Validation Index [get_CVI]
#' 7.  Cohen's Kappa [get_kappa]
#' 8.  Symmetric Extremal Dependence Index [get_SEDI]
#' 9.  F-measure [get_F_measure]
#' 10. All metrics [eval_all]


# Example data to test out functions with [near perfect split]
abs <- rbinom(300,1000,0.46)/1000
pres <- rbinom(300,1000,0.54)/1000
threshold <- 0.5

# Example where there is no discimination between the two
abs <- runif(300,0,1)
pres <-runif(300,0,1)
threshold <- 0.5

#### Calcualte BAT ####

get_BAT <- function(abs,pres){
  #' Calculates a custom metric for tuning purposes
  #' @param abs vector of predictions for pseudo-absences
  #' @param pres vector of predictions for presence points
  #' @return average of of 3 rescaled metrics: Boyce Index, AUC, and TSS
  
  require(dismo)
  
  boyce <- (get_boyce(abs = abs, pres = pres)$Spearman.cor + 1) / 2 #standardize to be between 0 and 1 (not -1 and 1)
  if(is.na(boyce)){
    boyce <- 0.5
  }
  # get AUC and optimal threshold for TSS
  evaluation <- dismo::evaluate(p = as.vector(pres), a = as.vector(abs))
  AUC <- evaluation@auc
  threshold <- threshold(evaluation)$spec_sens
  TSS <- (get_TSS(abs = abs, pres = pres, threshold = threshold) + 1) / 2
  
  return(c(Boyce = boyce , AUC = AUC , TSS = TSS , BAT = mean(c(boyce,AUC,TSS))))
}

#### Individual Functions ####

# Boyce Index

#### internal function calculating predicted-to-expected ratio for each class-interval

boycei <- function(interval, obs, fit) {
  
  fit.bin <- fit
  obs.bin <- obs
  fit.bin[fit[] >= interval[1] & fit[] <= interval[2]] <- "i"
  fit.bin[fit.bin != "i"] <- 0
  obs.bin[obs[] >= interval[1] & obs[] <= interval[2]] <- "i"
  obs.bin[obs.bin != "i"] <- 0
  
  pi <- length(which(obs.bin == "i"))/length(obs)
  ei <- length(which(fit.bin == "i"))/length(fit.bin)
  fi <- pi/ei
  
  return(fi)
}

get_boyce <- function(abs,pres){
  #' Calculates Boyce Index following Hirzel et al. 2006. Based on function in ecospat package.
  #' It is calculated with a moving window that is by default 1/10 of the suitability range 
  #' resuoltion of 100 focals
  #' @param abs vector of predictions for pseudo-absences
  #' @param pres vector of predictions for presence points
  #' @return F.ratio and Spearman correlation (Boyce Index)
  
  fit <- c(abs,pres)
  obs <- pres
  
  window.w <- (max(fit) - min(fit))/10
  res <- 100
  interval <- c(min(fit), max(fit))
  mini <- interval[1]
  maxi <- interval[2]
  
  vec.mov <- seq(from = mini, to = maxi - window.w, by = (maxi - mini - window.w)/res)
  vec.mov[res + 1] <- vec.mov[res + 1] + 1  #Trick to avoid error with closed interval in R
  interval <- cbind(vec.mov, vec.mov + window.w)
  
  f <- apply(interval, 1, boycei, obs, fit)
  to.keep <- which(!is.na(f))  # index to keep no NaN data
  f <- f[to.keep]
  if (length(f) < 2) {
    b <- NA  #at least two points are necessary to draw a correlation
  } else {
    r <- c(1:length(f))[f != c(f[-1], FALSE)]  #index to remove successive duplicates
    b <- cor(f[r], vec.mov[to.keep][r], method = "spearman")  # calculation of the spearman correlation (i.e. Boyce index) after removing successive duplicated values
  }
  
  return(list(F.ratio = f, Spearman.cor = b))
}

# Area under the ROC curve

get_AUC <- function(abs, pres){
  #' Calculates Area Under the Receiver Operating Characteristic curve
  #' @param abs vector of predictions for pseudo-absences
  #' @param pres vector of predictions for presence points
  
  require(dismo)
  
  # Calculate AUC
  AUC <- dismo::evaluate(p = pres, a = abs)@auc
  
  return(AUC)
}

# True Skill Statistic

get_TSS <- function(abs, pres, threshold){
  #' Calculates True Skill Statistic
  #' @param abs vector of predictions for pseudo-absences
  #' @param pres vector of predictions for presence points
  #' @param threshold threshold value to class predictions into binary 0/1
  
  # Create a confusion matrix
  truth.binary <- c(rep(0, length(abs)), rep(1, length(pres))) 
  pred.binary <- ifelse(c(abs,pres)>=threshold,1,0)
  conf.mat <- as.matrix(table(truth.binary, pred.binary)) 
  
  if(ncol(conf.mat)>1){
    # Calculate TSS
    TPR <- conf.mat[2,2]/sum(conf.mat[2,])
    TNR <- conf.mat[1,1]/sum(conf.mat[1,])
    
    TSS <- TPR + TNR - 1
  }else{
    TSS <- 0.5
  }
  
  return(TSS)
}

#Absolute Validation Index

get_AVI <- function(pres, threshold){
  #' Calculates Absolute Validation Index
  #' @param pres vector of predictions for presence points
  #' @param threshold threshold value to class predictions into binary 0/1
  
  # Calculate AVI
  above <- sum(pres>threshold)
  n <- length(pres)
  AVI <- above/n
  
  return(AVI)
}

# Contrast Validation Index

get_CVI <- function(pres,threshold){
  #' Calculates Contrast Validation Index
  #' @param pres vector of predictions for presence points
  #' @param threshold threshold value to class predictions into binary 0/1
  
  # Calculate AVI
  above <- sum(pres>threshold)
  n <- length(pres)
  AVIC <- above/n
  
  # Calculate AVI for chance model
  pres_c <- rbinom(300,1000,0.5)/1000
  above_c <- sum(pres_c>threshold)
  n_c <- sum(pres_c==pres_c)
  AVIC <- above_c/n_c
  
  # Calculate CVI
  CVI <- AVI - AVIC
  
  return(CVI)
}

# Kappa Coefficent

get_kappa <- function(abs, pres, threshold){
  #' Calculates Cohen's kappa coefficient
  #' @param abs vector of predictions for pseudo-absences
  #' @param pres vector of predictions for presence points
  #' @param threshold threshold value to class predictions into binary 0/1
  
  # Create a confusion matrix
  truth.binary <- c(rep(0, length(abs)), rep(1, length(pres))) 
  pred.binary <- ifelse(c(abs,pres)>=threshold,1,0)
  conf.mat <- as.matrix(table(truth.binary, pred.binary)) 
  
  # Calculate kappa
  n <- sum(conf.mat)
  P_o <- sum(diag(conf.mat))/n
  P_1 <- (sum(conf.mat[2,])/n) * (sum(conf.mat[,2])/n)
  P_0 <- (sum(conf.mat[1,])/n) * (sum(conf.mat[,1])/n)
  P_e <- P_0 + P_1
  
  kappa <- (P_o - P_e)/(1-P_e)
  
  return(kappa)
}


# Symmetric Extremal Dependence Index

get_SEDI <- function(abs, pres, threshold){
  #' Calculates Symmetric Extremal Dependence Index following Ferro & Stephenson 2011
  #' @param abs vector of predictions for pseudo-absences
  #' @param pres vector of predictions for presence points
  #' @param threshold threshold value to class predictions into binary 0/1
  
  # Create a confusion matrix
  truth.binary <- c(rep(0, length(abs)), rep(1, length(pres))) 
  pred.binary <- ifelse(c(abs,pres)>=threshold,1,0)
  conf.mat <- as.matrix(table(truth.binary, pred.binary)) 
  
  # Calculate SEDI
  TPR <- conf.mat[2,2]/ sum(conf.mat[2,])
  FPR <- conf.mat[1,2]/ sum(conf.mat[1,])
  
  SEDI <- (log(FPR) - log(TPR) - log(1-FPR) + log(1-TPR))/(log(FPR) + log(TPR) + log(1-FPR) + log(1 - TPR))
  
  return(SEDI)
}

# F-measure

get_F_measure <- function(abs,pres,threshold){
  #' Calculates F-Measure
  #' @param abs vector of predictions for pseudo-absences
  #' @param pres vector of predictions for presence points
  #' @param threshold threshold value to class predictions into binary 0/1
  
  # Create a confusion matrix
  truth.binary <- c(rep(0, length(abs)), rep(1, length(pres))) 
  pred.binary <- ifelse(c(abs,pres)>=threshold,1,0)
  conf.mat <- as.matrix(table(truth.binary, pred.binary)) 
  
  # Calculate f-measure
  precision <- conf.mat[2,2]/sum(conf.mat[,2])
  recall <- conf.mat[2,2]/sum(conf.mat[2,])
  F_measure <- (2*precision*recall)/(precision+recall)
  
  return(F_measure)
}


#### Calculate all Metrics ####

eval_all <- function(abs,pres,threshold){
  #' Calculates all of the different metrics at once
  #' @param abs vector of predictions for pseudo-absences
  #' @param pres vector of predictions for presence points
  #' @param threshold threshold value to class predictions into binary 0/1
  
  require(dismo)
  
  kappa <- get_kappa(abs = abs, pres = pres, threshold = threshold)
  TSS <- get_TSS(abs = abs, pres = pres, threshold = threshold)
  SEDI <- get_SEDI(abs = abs, pres = pres, threshold = threshold)
  AVI <- get_AVI(pres = pres, threshold = threshold)
  F_measure <- get_F_measure(abs = abs, pres = pres, threshold = threshold)
  Boyce <- get_boyce(abs = abs, pres = pres)$Spearman.cor
  AUC <- dismo::evaluate(p = pres, a = abs)@auc
  BAT <- get_BAT(abs = abs, pres = pres)
  
  return(list(kappa = kappa, TSS = TSS, SEDI = SEDI, AVI = AVI, F_measure = F_measure,
              boyceI = Boyce, AUC = AUC, BAT = BAT))
}

eval_all(abs, pres, threshold)