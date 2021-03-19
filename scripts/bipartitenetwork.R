###Host-Prasite Bipartite Network ----------

library(igraph)
library(bipartite)
library(tictoc)
library(BRRR)


# create df from which to build network
hostparnetwork <- read.csv("./data/modified/allpairs.csv", stringsAsFactors = F)[, -1] %>% 
  select(parname = gmpdparname, hostname = gmpdhostname) %>% mutate(netID = "1")

## Setup for bipartite

# convert to web format needed for bipartite functions
web <- frame2webs(hostparnetwork, varnames = c("parname", "hostname", "netID"))

# calculate bipartite network indices for all prots and hosts
tic()
parnet <- specieslevel(web[["1"]], level = "lower", index = c("degree", 
                                                                "normalised degree", 
                                                                "betweenness",
                                                                "closeness",
                                                                "species specificity",
                                                                "proportional generality")) # a quantitative version of normalised degree
toc()
BRRR::skrrrahh("soulja")

parnet <- parnet %>% rownames_to_column() %>% rename(protname = rowname)
protnames <- read.csv("./data/modified/allprots.csv")[-1]
protnet <- inner_join(protnames, parnet, by = "protname")


tic()
hostnet <- specieslevel(web[["1"]], level = "higher", index = c("degree", 
                                                                 "normalised degree", 
                                                                 "betweenness",
                                                                 "closeness",
                                                                 "species specificity",
                                                                 "proportional generality")) # a quantitative version of normalised degree
toc()
BRRR::skrrrahh("soulja")

hostnet <- hostnet %>% rownames_to_column() %>% rename(hostname = rowname)




# UNIPartite ---------------

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



###Host-Prot Bipartite Network ----------

# create df from which to build network
hostprotnetwork <- read.csv("./data/modified/allprotpairs.csv", stringsAsFactors = F)[, -1] %>% 
  select(protname, prothostname) %>% mutate(netID = "1")

## Setup for bipartite

# convert to web format needed for bipartite functions
web <- frame2webs(hostprotnetwork, varnames = c("protname", "prothostname", "netID"))

# calculate bipartite network indices for all prots and hosts
protsnet <- specieslevel(web[["1"]], level = "lower", index = c("degree", 
                                                                "normalised degree", 
                                                                "betweenness",
                                                                "closeness",
                                                                "species specificity",
                                                                "proportional generality")) # a quantitative version of normalised degree

hostsnet <- specieslevel(web[["1"]], level = "higher", index = c("degree", 
                                                                 "normalised degree", 
                                                                 "betweenness",
                                                                 "closeness",
                                                                 "species specificity",
                                                                 "proportional generality")) # a quantitative version of normalised degree

# convert to bipartite network one-mode network in order to calculate indices like centrality
as.one.mode(web[["1"]])

#_____________________________________________________________________________________________

## Setup for igraph

# convert to graph format needed for igraph functions

g <- graph.data.frame(hostprotnetwork[,1:2], directed = F)

bipartite.mapping(g)  # $res tells us if the network meets the criteria for a 2-mode network ($res)
# and $type tells us which nodes fall into each mode - FALSE for prots and TRUE for hosts

# check: number of nodes should equal the total number of unique host + parasite spp.
length(bipartite.mapping(g)$type) - length(unique(union(hostprotnetwork$protname, hostprotnetwork$prothostname))) 

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

protsnet <- left_join(x = specieslevel(web[["1"]], level = "lower", 
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
