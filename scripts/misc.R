# protraits creation -----

# for when Protraits_analysis.Rmd can't load the fucking csv

# Load packages

library(tidyverse) 
library(magrittr)

library(Matrix)
library(data.table)

protraits <- read_csv("data/modified/protraits_20200225") %>% 
  select(-c(protname, zscore, cscore))

# remove highly correlated vars

protraits <- protraits %>% 
  select(-c(numhosts, numgmpdrecords, 
            weighted.betweenness, bet.uni, deg.uni,
            googlehits))

tmp_prot <- data.table(protraits)


# grid search results log ------------

# Save output from 2020.02.25 grid search. Since it's somehow still in the environment right now. 

write_csv(hyper_grid, "data/modified/gridsearch_results_20200225")

# code has since been changed, and I don't remember if I saved and committed the code used to produce this,
# so may or may not be able to retrieve the code that produced this. parameter rander to

#-------

cv_bst_prot <- xgb.cv(params = list(max.depth = 5, eta = 0.1, nthread = 2, 
                                      +                                   objective = "binary:logistic", gamma = 0.3),
                        +                     data = sparse_matrix_prot,
                        +                     stratified = TRUE,
                        +                     label =  tmp_prot_Train$zoostat,
                        +                     nfold = 5, 
                        +                     nrounds = 7, 
                        +                     metrics = list("error", "auc"))
# [1]	train-error:0.036905+0.008530	train-auc:0.858149+0.093420	test-error:0.067127+0.021102	test-auc:0.627723+0.210645 
# [2]	train-error:0.040266+0.006271	train-auc:0.899382+0.074894	test-error:0.067127+0.021102	test-auc:0.650342+0.210265 
# [3]	train-error:0.040266+0.006271	train-auc:0.898535+0.074004	test-error:0.067127+0.021102	test-auc:0.650342+0.210265 
# [4]	train-error:0.040252+0.008132	train-auc:0.949931+0.030501	test-error:0.067127+0.021102	test-auc:0.650342+0.211710 
# [5]	train-error:0.045294+0.006662	train-auc:0.950642+0.030319	test-error:0.067127+0.021102	test-auc:0.650342+0.211710 
# [6]	train-error:0.045294+0.006662	train-auc:0.963055+0.025000	test-error:0.067127+0.021102	test-auc:0.719985+0.189260 
# [7]	train-error:0.045294+0.006662	train-auc:0.964591+0.025415	test-error:0.067127+0.021102	test-auc:0.719985+0.189260 

#-------

phylacine <- read_csv("data/original/PHYLACINE_datafiles/Trait_data.csv") %>% 
  mutate(hostname = gsub("_", " ", phylacine$Binomial.1.2)) %>% 
  select(-c(Order.1.2, Family.1.2, Genus.1.2, Species.1.2, 
            Life.Habit.Method, Life.Habit.Source, 
            Mass.Method, Mass.Source, Mass.Comparison, Mass.Comparison.Source,
            Added.IUCN.Status.1.2, Diet.Method, Diet.Source)) %>% 
  rename(IUCN.Status = IUCN.Status.1.2)

names(phylacine)

#-------

library(seqinr)
read.fasta("C:/Users/joych/Dowloads/")


# xgboost parameter tuning stuff that i decided not to use --------------------------
 # based on this article


library(xgboost)
library(ggplot2)
library(reshape2)
library(Ecdat)

set.seed(1)
N = 1000
k = 10
x = matrix(rnorm(N*k),N,k)
b = (-1)^(1:k)
yaux=(x%*%b)^2
e = rnorm(N)
y=yaux+e

# = select train and test indexes = #
train=sample(1:N,800)
test=setdiff(1:N,train)

# = parameters = #
# = eta candidates = #
eta=c(0.05,0.1,0.2,0.5,1)
# = colsample_bylevel candidates = #
cs=c(1/3,2/3,1)
# = max_depth candidates = #
md=c(2,4,6,10)
# = sub_sample candidates = #
ss=c(0.25,0.5,0.75,1)

# = standard model is the second value  of each vector above = #
standard=c(2,2,3,2)

# = train and test data = #
xtrain = sparse_matrix_prot[1:99,] %>% as.matrix()
ytrain = tmp_prot_Train[1:99]$zoostat
xtest = sparse_matrix_prot[100:149,] %>% as.matrix()
ytest = tmp_prot_Train[100:149]$zoostat

set.seed(1)
conv_eta = matrix(NA,500,length(eta))
pred_eta = matrix(NA,length(test), length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta
for(i in 1:length(eta)){
  params=list(eta = eta[i], colsample_bylevel=cs[standard[2]],
              subsample = ss[standard[4]], max_depth = md[standard[3]],
              min_child_weigth = 1)
  xgb=xgboost(xtrain, label = ytrain, nrounds = 500, params = params)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, xtest)
}

conv_eta2 = data.frame(iter=1:500, conv_eta)
conv_eta2.5 <- rbind(conv_eta2, conv_eta2)
conv_eta3 = melt(conv_eta2.5, id.vars = "iter")
ggplot(data = conv_eta3) + geom_line(aes(x = iter, y = value, color = variable))


(RMSE_eta = sqrt(colMeans((ytest-pred_eta)^2)))


set.seed(1)
conv_cs = matrix(NA,500,length(cs))
pred_cs = matrix(NA,length(test), length(cs))
colnames(conv_cs) = colnames(pred_cs) = cs
for(i in 1:length(cs)){
  params = list(eta = eta[standard[1]], colsample_bylevel = cs[i],
                subsample = ss[standard[4]], max_depth = md[standard[3]],
                min_child_weigth = 1)
  xgb=xgboost(xtrain, label = ytrain,nrounds = 500, params = params)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = predict(xgb, xtest)
}

conv_cs = data.frame(iter=1:500, conv_cs)
conv_cs = melt(conv_cs, id.vars = "iter")
ggplot(data = conv_cs) + geom_line(aes(x = iter, y = value, color = variable))
