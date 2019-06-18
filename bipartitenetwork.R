
library(igraph)

carparnetwork <- gmpdpair %>% select(parname, carname)

g <- graph.data.frame(carparnetwork, directed = F)
bipartite.mapping(g)

V(g)$type <- bipartite_mapping(g)$type

plot(g, vertex.label.cex = 0.6, vertex.label.color = "black")

V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"

plot(g, vertex.label.cex = 0.65, vertex.label.color = "black")

V(g)$label.color <- "black" 
V(g)$label.cex <- 0.65 
V(g)$frame.color <-  "gray"
V(g)$size <- 15

plot(g, layout = layout_with_graphopt)

plot(g, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)
     

types <- V(g)$type                 ## getting each vertex `type` let's us sort easily
deg <- degree(g)
bet <- betweenness(g)
clos <- closeness(g)
eig <- eigen_centrality(g)$vector

cent_df <- data.frame(types, deg, bet, clos, eig)

cent_df[order(cent_df$type, decreasing = TRUE),] ## sort w/ `order` by `type`

