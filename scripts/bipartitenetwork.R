###Host-Prasite Bipartite Network ----------

library(tidyverse) 
library(magrittr)
library(igraph)
library(bipartite)
library(tictoc)
library(BRRR)
library(reshape2)

rm(list = ls())

# create df from which to build network -------

x <- read.csv("./data/modified/gmpd_main_clean.csv", stringsAsFactors = F)[, -1] %>% 
  select(hostname, parname, location) %>% mutate(ID = paste(hostname, parname, location)) %>% 
  na.omit()

y <- x %>% group_by(ID) %>% summarise(freq = n())

z <- x %>% distinct()

hostparnetwork <- left_join(y, z) %>% distinct() %>% select(hostname, parname, 
                                                            location, freq)

# rm(list = c("x", "y", "z"))

## Setup for bipartite ---------

# convert to web format needed for bipartite functions
web <- frame2webs(x, varnames = c("hostname", "parname", "location"))

web <- frame2webs(hostparnetwork, varnames = c("hostname", "parname", "location", "freq"))

# calculate bipartite network indices for all prots and hosts -----------
tic()
parnet <- specieslevel(web[["1"]], level = "lower", index = c("degree", 
                                                              "normalised.degree", 
                                                              "betweenness", 
                                                              "closeness", 
                                                              "PDI")) # a quantitative version of normalised degree
toc()
BRRR::skrrrahh("soulja")


# Create correlation matrix ----------

data <- select_if(parnet, is.numeric)

correlationMatrix <- cor(data, use = "pairwise.complete.obs")
correlation.df <- correlationMatrix %>% as.data.frame() %>% mutate(rowID = rownames(correlationMatrix))

corrPairs <- melt(correlation.df) %>% rename(feature1 = rowID, feature2 = variable, PCC = value)
corrPairs <- corrPairs[!duplicated(data.frame(t(apply(corrPairs[, 1:2],1,sort)))),]

highlyCorrelated <- filter(corrPairs, PCC > 0.7 | PCC < (-0.7))


#Plot

corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 1, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2)

#' keep degree, betweeness (or weighted betweeness), and closeness. Keep all for now

# Clean and save the full parnet --------


allparnet <- parnet %>% rownames_to_column() %>% rename(parname = rowname)

protnames <- read.csv("./data/modified/protnames.csv")[-1]

protnet <- inner_join(protnames, allparnet, by = "parname") # one protname is not in allgmpdpairs

setdiff(protnames$parname, protnet$parname)
setdiff(protnet$parname, protnames$parname)


write.csv(allparnet, "./data/modified/allparnet.csv")
write.csv(protnet, "./data/modified/protnet.csv")

# clean and save prot net traits ------------

protnet <- read.csv("./data/modified/protnet.csv")[-1] 

data <- select_if(protnet, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 0.75, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2)

protnettraits <- protnet %>% select(parname, ProtDegree = degree, ProtBetweenness = weighted.betweenness,
                                    ProtCloseness = weighted.closeness)

data <- select_if(protnettraits, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 0.75, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2)

write.csv(protnettraits, "./data/modified/protraits/protnetraits.csv")

# Host net ----------
tic()
hostnet <- specieslevel(web[["1"]], level = "higher", index = c("degree", 
                                                                 "normalised degree", 
                                                                 "betweenness",
                                                                 "closeness", "PDI", 
                                                                "proportion generality")) # a quantitative version of normalised degree
toc()
BRRR::skrrrahh("biggie")


# Create correlation matrix ----------

data <- select_if(hostnet, is.numeric)

correlationMatrix <- cor(data, use = "pairwise.complete.obs")
correlation.df <- correlationMatrix %>% as.data.frame() %>% mutate(rowID = rownames(correlationMatrix))

corrPairs <- melt(correlation.df) %>% rename(feature1 = rowID, feature2 = variable, PCC = value)
corrPairs <- corrPairs[!duplicated(data.frame(t(apply(corrPairs[, 1:2],1,sort)))),]

highlyCorrelated <- filter(corrPairs, PCC > 0.7 | PCC < (-0.7))


#Plot

corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 1, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2)

# save all hosts net


allhostnet <- hostnet %>% rownames_to_column() %>% rename(hostname = rowname)
write.csv(hostnet, "./data/modified/hostnet.csv")


# make netprotraits
rm(list = ls())

hostparnetwork <- read.csv("./data/modified/allgmpdpairs.csv", stringsAsFactors = F)[, -1] %>% 
  select(parname = gmpdparname, hostname = gmpdhostname) %>% mutate(netID = "1")

hostnet_tmp <- read.csv("./data/modified/hostnet.csv")[-1]
colnames(hostnet_tmp)[2:9] <- paste0("mean_host_", colnames(hostnet_tmp)[2:9])

protnet_tmp <- read.csv("./data/modified/protnet.csv")[-1]
colnames(protnet_tmp)[2:9] <- paste0("prot_", colnames(protnet_tmp)[2:9])

# match the hot net traits to the pairs thing
allparhostnetraits <-  left_join(hostparnetwork, hostnet_tmp) 
# get parasite-level mean of host network measures
parhostnet_agg <- allparhostnetraits %>% group_by(parname) %>% summarise_if(is.numeric, mean, na.rm = TRUE)
#save
write.csv(parhostnet_agg, "./data/modified/parhostnetraits.csv")

# limit to prots
protnames <- read.csv("./data/modified/allprots.csv")[-1]
prothostnet_agg <- parhostnet_agg %>% rename(protname = parname)

# joing for full
protbinettraits_tmp <- inner_join(protnet_tmp, prothostnet_agg, by = "protname") # one protname is not in allgmpdpairs

setdiff(protnames$protname, protbinettraits_tmp$protname)
setdiff(protbinettraits_tmp$protname, protnames$protname) # no diffs!

# write.csv(protbinettraits_tmp, "./data/modified/protraits/bipartiteprotraits.csv")



# UNIPartite ---------------
# postponing this for now?
# convert to bipartite network one-mode network in order to calculate indices like centrality
as.one.mode(web[["1"]])

#_____________________________________________________________________________________________

## Setup for igraph

# convert to graph format needed for igraph functions

g <- graph.data.frame(hostparnetwork[,1:2], directed = F)

bipartite.mapping(g)  # $res tells us if the network meets the criteria for a 2-mode network ($res)
# and $type tells us which nodes fall into each mode - FALSE for prots and TRUE for hosts

# check: number of nodes should equal the total number of unique host + parasite spp.
length(bipartite.mapping(g)$type) - length(unique(union(hostparnetwork$protname, hostparnetwork$hostname))) 

# add the 'type' attribute to the network
V(g)$type <- bipartite_mapping(g)$type

### Visualise (not very useful because network is so large)
#V(g)$label.color <- "black" 
#V(g)$label.cex <- 0.65 
#V(g)$frame.color <-  "gray"
#V(g)$size <- 15
#
#plot(g, layout = layout_with_graphopt)
#
#plot(g, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)

## Analysis

#Used code from this tutorial: https://rpubs.com/pjmurphy/317838

#From the tutorial page:
#Igraph was not designed with two-mode networks in mind. It does, however recognize that the network is two-mode. 
#Keep in mind, however, that when you run centrality measures on a two-mode network, igraph will be treating each 
#of these nodes as though they are in the same mode. Igraph makes no allowance for calculating centralities that 
#are specific to the special case of two-mode networks.
#If you are interested in understanding the relative prominence of nodes in each mode, relative to other nodes in that mode, 
#then the best you will be able to do in igraph will be to analyze each mode separately. 

m <- as.one.mode(web[["1"]])
mgraph <- graph.adjacency(m)

#Calculate centrality
types <- V(g)$type                 # get each vertex `type` in order to sort easily
deg <- degree(m)
bet <- betweenness(m)
clos <- closeness(m)
eig <- eigen_centrality(mgraph)$vector

parnet.uni <- left_join(x = specieslevel(web[["1"]], level = "lower", 
                                       index = c("normalised degree", "betweenness", "closeness")) %>% 
                        rownames_to_column("protname"), 
                      y = data.frame(types, deg, bet, clos, eig) %>% 
                        rownames_to_column("protname") %>% 
                        mutate(types = as.character(types)) %>% 
                        filter(types == "FALSE"))

hostsnet <- left_join(x = specieslevel(web[["1"]], level = "higher", 
                                       index = c("normalised degree", "betweenness", "closeness")) %>% 
                        rownames_to_column("prothostname"), 
                      y = data.frame(types, deg, bet, clos, eig) %>% 
                        rownames_to_column("prothostname") %>% 
                        mutate(types = as.character(types)) %>% 
                        filter(types == "TRUE"))
# Save as csvs
#write.csv(protsnet, "./data/modified/protsnet.csv")
#write.csv(hostsnet, "./data/modified/prothostsnet.csv")

# Add protsnet vars to protratis
protraits <- left_join(protraits, protsnet, by = "protname") #protraits should now have 38 vars




# ###Host-Prot Bipartite Network ----------
# # retired this in favour of "global" parasite network with all host-par pairs, not just prots.
# 
# # create df from which to build network
# hostprotnetwork <- read.csv("./data/modified/allprotpairs.csv", stringsAsFactors = F)[, -1] %>% 
#   select(protname, prothostname) %>% mutate(netID = "1")
# 
# ## Setup for bipartite
# 
# # convert to web format needed for bipartite functions
# web <- frame2webs(hostprotnetwork, varnames = c("protname", "prothostname", "netID"))
# 
# # calculate bipartite network indices for all prots and hosts
# protsnet <- specieslevel(web[["1"]], level = "lower", index = c("degree", 
#                                                                 "normalised degree", 
#                                                                 "betweenness",
#                                                                 "closeness",
#                                                                 "species specificity",
#                                                                 "proportional generality")) # a quantitative version of normalised degree
# 
# hostsnet <- specieslevel(web[["1"]], level = "higher", index = c("degree", 
#                                                                  "normalised degree", 
#                                                                  "betweenness",
#                                                                  "closeness",
#                                                                  "species specificity",
#                                                                  "proportional generality")) # a quantitative version of normalised degree
# 
# # convert to bipartite network one-mode network in order to calculate indices like centrality
# as.one.mode(web[["1"]])
# 
# #_____________________________________________________________________________________________
# 
# ## Setup for igraph
# 
# # convert to graph format needed for igraph functions
# 
# g <- graph.data.frame(hostprotnetwork[,1:2], directed = F)
# 
# bipartite.mapping(g)  # $res tells us if the network meets the criteria for a 2-mode network ($res)
# # and $type tells us which nodes fall into each mode - FALSE for prots and TRUE for hosts
# 
# # check: number of nodes should equal the total number of unique host + parasite spp.
# length(bipartite.mapping(g)$type) - length(unique(union(hostprotnetwork$protname, hostprotnetwork$prothostname))) 
# 
# # add the 'type' attribute to the network
# V(g)$type <- bipartite_mapping(g)$type
# 
# ### Visualise (not very useful because network is so large)
# #V(g)$label.color <- "black" 
# #V(g)$label.cex <- 0.65 
# #V(g)$frame.color <-  "gray"
# #V(g)$size <- 15
# #
# #plot(g, layout = layout_with_graphopt)
# #
# #plot(g, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)
# 
# ## Analysis
# 
# #Used code from this tutorial: https://rpubs.com/pjmurphy/317838
# 
# #From the tutorial page:
# #Igraph was not designed with two-mode networks in mind. It does, however recognize that the network is two-mode. 
# #Keep in mind, however, that when you run centrality measures on a two-mode network, igraph will be treating each 
# #of these nodes as though they are in the same mode. Igraph makes no allowance for calculating centralities that 
# #are specific to the special case of two-mode networks.
# #If you are interested in understanding the relative prominence of nodes in each mode, relative to other nodes in that mode, 
# #then the best you will be able to do in igraph will be to analyze each mode separately. 
# 
# m <- as.one.mode(web[["1"]])
# mgraph <- graph.adjacency(m)
# 
# #Calculate centrality
# types <- V(g)$type                 # get each vertex `type` in order to sort easily
# deg <- degree(m)
# bet <- betweenness(m)
# clos <- closeness(m)
# eig <- eigen_centrality(mgraph)$vector
# 
# protsnet <- left_join(x = specieslevel(web[["1"]], level = "lower", 
#                                    index = c("normalised degree", "betweenness", "closeness")) %>% 
#                         rownames_to_column("protname"), 
#                       y = data.frame(types, deg, bet, clos, eig) %>% 
#                         rownames_to_column("protname") %>% 
#                         mutate(types = as.character(types)) %>% 
#                         filter(types == "FALSE"))
# 
# hostsnet <- left_join(x = specieslevel(web[["1"]], level = "higher", 
#                                        index = c("normalised degree", "betweenness", "closeness")) %>% 
#                         rownames_to_column("prothostname"), 
#                       y = data.frame(types, deg, bet, clos, eig) %>% 
#                         rownames_to_column("prothostname") %>% 
#                         mutate(types = as.character(types)) %>% 
#                         filter(types == "TRUE"))
# # Save as csvs
# #write.csv(protsnet, "./data/modified/protsnet.csv")
# #write.csv(hostsnet, "./data/modified/prothostsnet.csv")
# 
# # Add protsnet vars to protratis
# protraits <- left_join(protraits, protsnet, by = "protname") #protraits should now have 38 vars
