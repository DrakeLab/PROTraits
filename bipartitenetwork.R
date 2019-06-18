###Host-Prasite Bipartite Network

#Used code from this tutorial: https://rpubs.com/pjmurphy/317838

library(igraph)

##Setup

#Take only host and parasite columns and put the data in the right format to feed to igraph
carparnetwork <- gmpdpair %>% select(parname, carname)
g <- graph.data.frame(carparnetwork, directed = F)

bipartite.mapping(g)  # this tells us if the network meets the criteria for a 2-mode network ($res)
                      # and which nodes fall into each mode - FALSE is parasite and TRUE is host

#Check: number of nodes should equal the total number of unique host + parasite spp.
length(bipartite.mapping(g)$type) - length(unique(union(carparnetwork$parname, carparnetwork$carname))) 

#Add the 'type' attribute to the network
V(g)$type <- bipartite_mapping(g)$type

#Visualise (not very useful because network is so large)
V(g)$label.color <- "black" 
V(g)$label.cex <- 0.65 
V(g)$frame.color <-  "gray"
V(g)$size <- 15

plot(g, layout = layout_with_graphopt)

plot(g, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)
     
##Analysis

#From the tutorial page:
#Igraph was not designed with two-mode networks in mind. It does, however recognize that the network is two-mode. 
#Keep in mind, however, that when you run centrality measures on a two-mode network, igraph will be treating each 
#of these nodes as though they are in the same mode. Igraph makes no allowance for calculating centralities that 
#are specific to the special case of two-mode networks.
#If you are interested in understanding the relative prominence of nodes in each mode, relative to other nodes in that mode, 
#then the best you will be able to do in igraph will be to analyze each mode separately. 

#Calculate centrality
types <- V(g)$type                 # get each vertex `type` in order to sort easily
deg <- degree(g)
bet <- betweenness(g)
clos <- closeness(g)
eig <- eigen_centrality(g)$vector

cent_df <- data.frame(types, deg, bet, clos, eig)

cent_df[order(cent_df$type, decreasing = TRUE),] # sort w/ `order` by `type`
