

set.seed(7)
# load the library
library(tidyverse)
library(magrittr)

# calculate correlation matrix
data <- select_if(protraits, is.numeric)[-1]
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
corrplot(correlationMatrix, method="color", is.corr=T, tl.col = "black", number.cex = 0.55, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 1)
correlationMatrix[upper.tri(correlationMatrix)] <- NA
correlation.df <- correlationMatrix %>% as.data.frame() %>% mutate(rowID = rownames(correlationMatrix))
corrPairs <- melt(correlation.df) %>% rename(feature1 = rowID, feature2 = variable, PCC = value)

corrPairs <- corrPairs[!duplicated(data.frame(t(apply(corrPairs[, 1:2],1,sort)))),]

# look at combinations of variables that have a PCC > 0.75 and choose whether to keep some or not



# # calculate correlation matrix
# data <- select_if(protraits, is.numeric)[-1]
# correlationMatrix <- cor(data, use = "pairwise.complete.obs")
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
# highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7, names = T)
# # print indexes of highly correlated attributes
# print(highlyCorrelated)

# The printed output (these are the variables it recommends that I should take out):
# [1] "numhostzoores"        "normalised.degree"    "deg.uni"              "numhosts"             "bet.uni"             
# [6] "numgmpdrecords"       "betweenness"          "eig.uni"              "weighted.betweenness" "endocrine"           
# [11] "reproductive"         "numsys"               "respiratory"          "lymphatic"            "mpd.obs"             
# [16] "goat"                 "circulatory"          "tm_nonclose"          "closeness"            "numdomhosts"         
# [21] "mpd.obs.rank"         "cattle"               "mpd.obs.z"            "integumentary"        "ocular" 

# corrplot(correlationMatrix, method="number", is.corr=T)

# # From Michelle (see slack)
# #turn into long to move around easier
# cor.long <- data.frame(cor.mat, var1 = row.names(cor.mat)) %>%
#   pivot_longer(A:D, names_to = "var2", values_to = "corr")
# #see whcih ones have high correlation
# filter(cor.long, corr>0.7 | corr< (-0.7))
