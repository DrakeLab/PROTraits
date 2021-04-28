###Host-Parasite Bipartite Network ----------

library(tidyverse) 
library(magrittr)
library(igraph)
library(bipartite)
library(tictoc)
library(BRRR)
library(reshape2)


rm(list = ls())

# create df from which to build network -------

# # THIS IS ALL A BIG FAILURE ToT BECAUSE MEMORY ERROR, DON'T RUN!!
# x <- read.csv("./data/modified/gmpd_main_clean.csv", stringsAsFactors = F)[, -1] %>% 
#   select(hostname, parname, location) %>% mutate(ID = paste(parname, hostname, location)) %>% 
#   na.omit()
# 
# y <- x %>% group_by(ID) %>% summarise(freq = n())
# 
# z <- x %>% distinct()
# 
# hostparintrx <- x[1:3]
# 
# hostparintrxfreq <- left_join(y, z) %>% distinct() %>% select(hostname, parname, 
#                                                              location, freq)
# 
# # save(hostparintrx, hostparintrxfreq, file = "./scripts/BipartiteNetworkData.Rdata")
# 
# hostparnetwork <- left_join(y, z) %>% distinct() %>% select(hostname, parname, 
#                                                             location, freq)
# rm(list = c("x", "y", "z"))
# 
# ## Setup for bipartite ---------
# 
# # convert to web format needed for bipartite functions
# 
# web <- frame2webs(hostparnetwork, varnames = c("hostname", "parname", "location"))
# 
# # Calculate bipartite network indices -----------
# 
# parnet <- specieslevel(web, level = "lower", index = c("degree", 
#                                                        "betweenness", 
#                                                        "closeness",
#                                                        "species specificity"))
# BRRR::skrrrahh("soulja")

# Okay, so fuck all of the above, let's see if we can use some old saved data to ge the indices we want
# the following CSVs from a month ago (March 24). IDK how I produced them at the time, not interested
# in going back into version history to re-run right now

# All par net traits ------------

allparnet <- read.csv("./data/modified/parnet.csv", row.names = 1, stringsAsFactors = F) %>% 
  rename(parname = protname)

# fix protnames
allparnet$parname <- gsub("Babesia equi", "Theileria equi", allparnet$parname)
allparnet$parname <- gsub("Plasmodium rodhani", "Plasmodium rodhaini", allparnet$parname)
# there's gonna be two P. rodhaini, pick one
allparnet %>% rownames_to_column() %>% filter(grepl("Plasmodium rodhaini", parname)) # picking 1425, less 0s
allparnet <- allparnet[-1424, ]

# Create correlation matrix

data <- select_if(allparnet, is.numeric)

correlationMatrix <- cor(data, use = "pairwise.complete.obs")
correlation.df <- correlationMatrix %>% as.data.frame() %>% mutate(rowID = rownames(correlationMatrix))

corrPairs <- melt(correlation.df) %>% rename(feature1 = rowID, feature2 = variable, PCC = value)
corrPairs <- corrPairs[!duplicated(data.frame(t(apply(corrPairs[, 1:2],1,sort)))),]

highlyCorrelated <- filter(corrPairs, PCC > 0.7 | PCC < (-0.7))

#Plot
corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 1, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2)

# supposedly degree and normalised degree and proportional generality have perfect correlation - let's plot
allparnet$degree <- as.numeric(allparnet$degree/max(allparnet$degree))
plot(allparnet$degree, allparnet$nomalised.degree) # what's with this??
plot(allparnet$degree, allparnet$proportional.generality) # this makes sense

# of these 3 let's get rid of degree and normalised degree and keep proportional generality, 
# (i.e., the number of partner species in relation to the potential number of partner species)
# also get rid of SSI, whcih is strongly correlated with weighted.closeness

# remove those 3 and repeat corr plot

allparnet_tmp <- allparnet %>% select(-c(degree, normalised.degree, species.specificity.index))

data <- select_if(allparnet_tmp, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
#Plot
corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 1, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2)


#' keep proportional generality, weighted betweenness, and closeness. the specieslevel function
#' documentation says that the weighted and non-weighted counterparts of closeness are often similar,
#' but for betweenness they can differ a lot

parnetraits <- allparnet_tmp %>% select(parname, ProportionalGenerality = proportional.generality,
                                        ParasiteCloseness = closeness,
                                        ParasiteBetweenness = weighted.betweenness)

data <- select_if(parnetraits, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
#Plot
corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 1, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2) # NICE!



write.csv(parnetraits, "./data/modified/parnetraits.csv")

# Prot net traits ------------

protnames <- read.csv("./data/modified/protnames.csv", row.names = 1, stringsAsFactors = F)

intersect(protnames$parname, parnetraits$parname) # complete overlap - cool!
setdiff(protnames$parname, parnetraits$parname)

protnet <- left_join(protnames, parnetraits, by = "parname") 

write.csv(protnet, "./data/modified/protnetraits.csv")

# rm(list = ls())

## All host net traits ------------

hostnet_agg <- read.csv("./data/modified/parhostnetraits.csv", row.names = 1, stringsAsFactors = F)
# both have 1998 obs (parasites) of 9 vars:

names(hostnet_agg)

hostnet_agg$parname <- gsub("Babesia equi", "Theileria equi", hostnet_agg$parname)
hostnet_agg$parname <- gsub("Plasmodium rodhani", "Plasmodium rodhaini", hostnet_agg$parname)
# there's gonna be two P. rodhaini, average them
hostnet_agg[1425, 2:9] <- colMeans(hostnet_agg[1424:1425, 2:9])
hostnet_agg <- hostnet_agg[-1424, ]

# Create correlation matrix

data <- select_if(hostnet_agg, is.numeric)

correlationMatrix <- cor(data, use = "pairwise.complete.obs")
correlation.df <- correlationMatrix %>% as.data.frame() %>% mutate(rowID = rownames(correlationMatrix))

corrPairs <- melt(correlation.df) %>% rename(feature1 = rowID, feature2 = variable, PCC = value)
corrPairs <- corrPairs[!duplicated(data.frame(t(apply(corrPairs[, 1:2],1,sort)))),]

highlyCorrelated <- filter(corrPairs, PCC > 0.7 | PCC < (-0.7))

#Plot
corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 1, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2)

# supposedly degree and normalised degree and proportional generality have perfect correlation - let's plot
hostnet_agg$mean_host_degree <- as.numeric(hostnet_agg$mean_host_degree/max(hostnet_agg$mean_host_degree))
plot(hostnet_agg$mean_host_degree, hostnet_agg$mean_host_normalised.degree)
plot(hostnet_agg$mean_host_degree, hostnet_agg$mean_host_proportional.generality) # this makes sense

# of these 3 let's get rid of degree and normalised degree and keep proportional generality, 
# (i.e., the number of partner species in relation to the potential number of partner species)
# also get rid of SSI, which is strongly correlated with weighted.closeness, and prop generality

# remove those 3 and repeat corr plot

hostnet_agg_tmp <- hostnet_agg %>% select(-c(mean_host_degree, mean_host_normalised.degree, 
                                             mean_host_species.specificity.index))

data <- select_if(hostnet_agg_tmp, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
#Plot
corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 1, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2)


#' keep weighted betweenness, and weighted closeness. the specieslevel function
#' documentation says that the weighted and non-weighted counterparts of closeness are often similar,
#' but for betweenness they can differ a lot

parhostnetraits <- hostnet_agg_tmp %>% select(parname, 
                                              HostProportionalGenerality = mean_host_proportional.generality,
                                              HostCloseness = mean_host_closeness,
                                              HostBetweenness = mean_host_weighted.betweenness)

data <- select_if(parhostnetraits, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
#Plot
corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 1, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2) # NICE!

write.csv(parhostnetraits, "./data/modified/allhostnetraits.csv")

# Prot host net traits -------

protnames <- read.csv("./data/modified/protnames.csv", row.names = 1, stringsAsFactors = F)

intersect(protnames$parname, parhostnetraits$parname) # complete overlap - cool!
setdiff(protnames$parname, parhostnetraits$parname)

prothostnet <- left_join(protnames, parhostnetraits, by = "parname") 

write.csv(prothostnet, "./data/modified/prothostnetraits.csv")

# Netprotraits

netprotraits_tmp <- left_join(protnet, prothostnet)

data <- select_if(netprotraits_tmp, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
#Plot
corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 1, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2) # NICE!
# get rid of host closeness 

netprotraits <- netprotraits_tmp %>% select(-HostCloseness)

data <- select_if(netprotraits, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
#Plot
corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 1, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2) # NICE!
# mostly good except for that 0.74 
# not removing par betweenness bc it is akin to SSI, which we want to keep
# could remove proportion generality, which is same as degree pretty much, 
# but will look at final protraits corr before that

write.csv(netprotraits, "./data/modified/protraits/netprotraits.csv")

# Old code -------

# protnet <- read.csv("./data/modified/protnet.csv")[-1] 
# 
# data <- select_if(protnet, is.numeric)
# correlationMatrix <- cor(data, use = "pairwise.complete.obs")
# corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 0.75, number.cex = 1, 
#          na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2)
# 
# protnettraits <- protnet %>% select(parname, ProtDegree = degree, ProtBetweenness = weighted.betweenness,
#                                     ProtCloseness = weighted.closeness)
# 
# data <- select_if(protnettraits, is.numeric)
# correlationMatrix <- cor(data, use = "pairwise.complete.obs")
# corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 0.75, number.cex = 1, 
#          na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2)
# 
# write.csv(protnettraits, "./data/modified/protraits/protnetraits.csv")
# 
# # Host net
# tic()
# hostnet <- specieslevel(web[["1"]], level = "higher", index = c("degree", 
#                                                                  "normalised degree", 
#                                                                  "betweenness",
#                                                                  "closeness", "PDI", 
#                                                                 "proportion generality")) # a quantitative version of normalised degree
# toc()
# BRRR::skrrrahh("biggie")
# 
# 
# # Create correlation matrix
# 
# data <- select_if(hostnet, is.numeric)
# 
# correlationMatrix <- cor(data, use = "pairwise.complete.obs")
# correlation.df <- correlationMatrix %>% as.data.frame() %>% mutate(rowID = rownames(correlationMatrix))
# 
# corrPairs <- melt(correlation.df) %>% rename(feature1 = rowID, feature2 = variable, PCC = value)
# corrPairs <- corrPairs[!duplicated(data.frame(t(apply(corrPairs[, 1:2],1,sort)))),]
# 
# highlyCorrelated <- filter(corrPairs, PCC > 0.7 | PCC < (-0.7))
# 
# 
# #Plot
# 
# corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 1, number.cex = 1, 
#          na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2)
# 
# # save all hosts net
# 
# 
# allhostnet <- hostnet %>% rownames_to_column() %>% rename(hostname = rowname)
# write.csv(hostnet, "./data/modified/hostnet.csv")
# 
# 
# # make netprotraits
# rm(list = ls())
# 
# hostparnetwork <- read.csv("./data/modified/allgmpdpairs.csv", stringsAsFactors = F)[, -1] %>% 
#   select(parname = gmpdparname, hostname = gmpdhostname) %>% mutate(netID = "1")
# 
# hostnet_tmp <- read.csv("./data/modified/hostnet.csv")[-1]
# colnames(hostnet_tmp)[2:9] <- paste0("mean_host_", colnames(hostnet_tmp)[2:9])
# 
# protnet_tmp <- read.csv("./data/modified/protnet.csv")[-1]
# colnames(protnet_tmp)[2:9] <- paste0("prot_", colnames(protnet_tmp)[2:9])
# 
# # match the hot net traits to the pairs thing
# allparhostnetraits <-  left_join(hostparnetwork, hostnet_tmp) 
# # get parasite-level mean of host network measures
# parhostnet_agg <- allparhostnetraits %>% group_by(parname) %>% summarise_if(is.numeric, mean, na.rm = TRUE)
# #save
# write.csv(parhostnet_agg, "./data/modified/parhostnetraits.csv")
# 
# # limit to prots
# protnames <- read.csv("./data/modified/allprots.csv")[-1]
# prothostnet_agg <- parhostnet_agg %>% rename(protname = parname)
# 
# # joing for full
# protbinettraits_tmp <- inner_join(protnet_tmp, prothostnet_agg, by = "protname") # one protname is not in allgmpdpairs
# 
# setdiff(protnames$protname, protbinettraits_tmp$protname)
# setdiff(protbinettraits_tmp$protname, protnames$protname) # no diffs!
# 
# # write.csv(protbinettraits_tmp, "./data/modified/protraits/bipartiteprotraits.csv")
# 
# 
# 
# # UNIPartite
# # postponing this for now?
# # convert to bipartite network one-mode network in order to calculate indices like centrality
# as.one.mode(web[["1"]])
# 
# #_____________________________________________________________________________________________
# 
# ## Setup for igraph
# 
# # convert to graph format needed for igraph functions
# 
# g <- graph.data.frame(hostparnetwork[,1:2], directed = F)
# 
# bipartite.mapping(g)  # $res tells us if the network meets the criteria for a 2-mode network ($res)
# # and $type tells us which nodes fall into each mode - FALSE for prots and TRUE for hosts
# 
# # check: number of nodes should equal the total number of unique host + parasite spp.
# length(bipartite.mapping(g)$type) - length(unique(union(hostparnetwork$protname, hostparnetwork$hostname))) 
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
# parnet.uni <- left_join(x = specieslevel(web[["1"]], level = "lower", 
#                                        index = c("normalised degree", "betweenness", "closeness")) %>% 
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
# 
# 
# 
# 
# ###Host-Prot Bipartite Network
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
