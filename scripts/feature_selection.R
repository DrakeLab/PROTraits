

# load the library
library(tidyverse)
library(magrittr)

rm(list = ls())

# LOAD THE DATA ------------

protraits1 <- read.csv("./data/modified/prots228.csv", row.names = 1) %>% 
  select(-c(zscore, cscore, gmpdprotname))
protraits2 <- read.csv("./data/modified/protsentry_clean.csv")[, -(8:16)] # remove domhosts
prothosttraits <- read.csv("./data/modified/allprothosttraits.csv", row.names = 1) %>% select(-X)
protsnettraits <- read.csv("./data/modified/protsnet.csv", row.names = 1) %>% 
  select(-types)
protcommtraits1 <- read.csv("./data/modified/protcomm_prots.csv", row.names = 1)
protcommtraits2 <- read.csv("./data/modified/protcomm_all.csv", row.names = 1)
protecotraits <- read.csv("./data/modified/prots_ecoregions.csv", row.names = 1)
protphylotraits <- read.csv("./data/modified/mpd_prot.csv", row.names = 1)

# BRING IT ALL TOGETHER -------------

protraits <- left_join(protraits1, protraits2)
protraits <- left_join(protraits, prothosttraits)
protraits <- left_join(protraits, protsnettraits)
protraits <- left_join(protraits, protcommtraits1)
protraits <- left_join(protraits, protcommtraits2)
protraits <- left_join(protraits, protecotraits)
# protraits <- left_join(protraits, protphylotraits)

feature_names <- names(protraits)
completeness <- protraits %>% summarise_all(function(x) mean(!is.na(x))) %>% as.numeric()
datacompleteness <- cbind(names(protraits), completeness) %>% as.data.frame()
protraits %>% summarise_all(function(x) mean(!is.na(x))) %>% as.numeric() %>% plot(type = 'h')

dev.off()

# calculate correlation matrix ---------------------

library(corrplot)

data <- select_if(protraits, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
corrplot(correlationMatrix, method="color", is.corr=T, tl.col = "black", tl.cex = 0.6, number.cex = 0.5, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 1)
correlationMatrix[upper.tri(correlationMatrix)] <- NA
correlation.df <- correlationMatrix %>% as.data.frame() %>% mutate(rowID = rownames(correlationMatrix))
corrPairs <- melt(correlation.df) %>% rename(feature1 = rowID, feature2 = variable, PCC = value)

corrPairs <- corrPairs[!duplicated(data.frame(t(apply(corrPairs[, 1:2],1,sort)))),]

highlyCorrelated <- filter(corrPairs, PCC > 0.7 | PCC < (-0.7))



# select features -----------------

# look at combinations of variables from corrPairs that have a PCC > 0.75 and choose whether to keep some or not

protraits <- protraits %>% select(-c(Mass.g, bet, NeonateBodyMass_wEXT, parcommsize, WeaningAge, SexualMaturityAge,
                                     AdultHeadBodyLen))
data <- select_if(protraits, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
correlationMatrix[upper.tri(correlationMatrix)] <- NA
correlation.df <- correlationMatrix %>% as.data.frame() %>% mutate(rowID = rownames(correlationMatrix))
corrPairs <- melt(correlation.df) %>% rename(feature1 = rowID, feature2 = variable, PCC = value)

corrPairs <- corrPairs[!duplicated(data.frame(t(apply(corrPairs[, 1:2],1,sort)))),]

highlyCorrelated <- filter(corrPairs, PCC > 0.7 | PCC < (-0.7))

protraits <- protraits %>% select(-c(deg, eig, MaxLongevity, GestationLen, lymphatic))
data <- select_if(protraits, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
correlationMatrix[upper.tri(correlationMatrix)] <- NA
correlation.df <- correlationMatrix %>% as.data.frame() %>% mutate(rowID = rownames(correlationMatrix))
corrPairs <- melt(correlation.df) %>% rename(feature1 = rowID, feature2 = variable, PCC = value)

corrPairs <- corrPairs[!duplicated(data.frame(t(apply(corrPairs[, 1:2],1,sort)))),]

highlyCorrelated <- filter(corrPairs, PCC > 0.7 | PCC < (-0.7))

table(highlyCorrelated$feature1) %>% View()

protraits <- protraits %>% select(-c(numhostzoons, numcommzoons, numparcommzoons))

# ok it's fine can come back later

# final (for now) corrplot and completeness stuff

data <- select_if(protraits, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
corrplot(correlationMatrix, method="color", is.corr=T, tl.col = "black", tl.cex = 0.6, number.cex = 0.5, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 1)

feature_names <- names(protraits)
completeness <- protraits %>% summarise_all(function(x) mean(!is.na(x))) %>% as.numeric()
datacompleteness <- cbind(names(protraits), completeness) %>% as.data.frame()
protraits %>% summarise_all(function(x) mean(!is.na(x))) %>% as.numeric() %>% plot(type = 'h')

write.csv(protraits, "./data/modified/protraits_210305.csv")

# Extra/old code ---------------

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
