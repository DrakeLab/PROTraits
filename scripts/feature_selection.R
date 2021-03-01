

read.csv("https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.csv")

set.seed(7)
# load the library
library(mlbench)
library(caret)
library(tidyverse)

# calculate correlation matrix
data <- select_if(protraits, is.numeric)[-1]
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
corrplot(correlationMatrix, method="color", is.corr=T, tl.col = "black", number.cex = 0.55, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 1)
correlationMatrix[upper.tri(correlationMatrix)] <- NA
correlation.df <- correlationMatrix %>% as.data.frame() %>% mutate(rowID = rownames(correlationMatrix))
corrPairs <- melt(correlation.df) %>% rename(feature1 = rowID, feature2 = variable, PCC = value)

corrPairs <- corrPairs[!duplicated(data.frame(t(apply(corrPairs[, 1:2],1,sort)))),]

# # calculate correlation matrix
# data <- select_if(protraits, is.numeric)[-1]
# correlationMatrix <- cor(data, use = "pairwise.complete.obs")
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
# highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7, names = T)
# # print indexes of highly correlated attributes
# print(highlyCorrelated)
# corrplot(correlationMatrix, method="number", is.corr=T)

# # From Michelle (see slack)
# #turn into long to move around easier
# cor.long <- data.frame(cor.mat, var1 = row.names(cor.mat)) %>%
#   pivot_longer(A:D, names_to = "var2", values_to = "corr")
# #see whcih ones have high correlation
# filter(cor.long, corr>0.7 | corr< (-0.7))
