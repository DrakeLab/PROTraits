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