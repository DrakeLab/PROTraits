# Test out glmnet

library(skimr)
library(ggplot2)
library(GGally)

library(dplyr)
pro.df <- read.csv("data/modified/protraits_20200225.csv")
                      
skim(pro.df)                      

#select some covariates that make sense to me?
df.subset <- select(pro.df, zoostat, propprotcommzoon, WOShits, btwn_wt = weighted.betweenness, numhosts, protcommsize) %>%
  #transform some of them
  mutate(log_btwn_wt = log(btwn_wt+0.00001),
         log_WOShits = log(WOShits+1)
         ) %>%
  select(-btwn_wt, -WOShits)

#drop NAs
df.subset <- na.omit(df.subset)

#check out relationships
ggpairs(df.subset)

#make a glm
glm.mod <- glm(zoostat ~., 
               family = binomial(link = 'logit'),
               data = df.subset)

summary(glm.mod)

#confusionmatrix
library(caret)
y.preds <- predict(glm.mod, df.subset, type = "response")
hist(y.preds)
#cut it off at lowest true 1
pred.df <- data.frame(true.y = df.subset$zoostat,
                      pred.y = y.preds)
#lowest true 1 = #0.0357
filter(pred.df, true.y == 1) %>%
  summarise(min(pred.y))

pred.df <- mutate(pred.df, pred.class = ifelse(pred.y>0.04,1,0))

confusionMatrix(as.factor(pred.df$pred.class), as.factor(pred.df$true.y))

#AUC (to explore that threshold)
roc.test <- pROC::roc(response = pred.df$true.y, predictor = pred.df$pred.y)
pROC::auc(roc.test) 
plot(roc.test)
roc.test$thresholds[which.max(roc.test$sensitivities+ roc.test$specificities)] #0.0488
#what is the prevalence in the dataset
mean(df.subset$zoostat) #0.08, could use this too
