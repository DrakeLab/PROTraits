## 7/30/2019

library(tidyverse) 
library(magrittr)

## create affiliate networks for host-prot pairs

rm(list = ls())

# load data

protzoos <- read.csv("./data/modified/prots228.csv", row.names = 1, stringsAsFactors = F)

allprotpairs <- read.csv("./data/modified/allprotpairs.csv", row.names = 1, stringsAsFactors = F)

# ALL HOSTS ----------

# create empty dfs

# Hosts
allprothosts <- read.csv("./data/modified/allprothosts.csv", row.names = 1, stringsAsFactors = F) %>% 
  add_column(hostprots = NA, 
             numprots = NA,
             protzoos = NA,
             numprotzoons = NA,
             propprotzoon = NA,
             zoores = NA)

# Prots
allprots <- read.csv("./data/modified/allprots.csv", row.names = 1, stringsAsFactors = F) %>% 
  left_join(protzoos[, c(1, 13)], by = "protname") %>% 
  add_column(prothosts = NA, 
             numhosts = NA,
             protcomm = NA,
             protcommsize = NA,
             protcommzoos = NA,
             numcommzoons =NA,
             propcommzoon = NA,
             hostnumprotzoons = NA,
             hostzoos = NA,
             numhostzoons = NA,
             prophostzoon = NA)


# fill in allprothosts

for (i in 1:nrow(allprothosts)) {
  allprothosts$hostprots[i] <- allprotpairs %>% filter(prothostname == allprothosts$prothostname[i]) %>% 
    select(protname) %>% 
    as.vector()
  allprothosts$numprots[i] <- length(allprothosts$hostprots[[i]])
  allprothosts$protzoos[i] <- allprots %>% filter(protname %in% allprothosts$hostprots[[i]]) %>% 
  select(zoostat)
  allprothosts$numprotzoons[i] <- length(which(allprothosts$protzoos[[i]] == 1))
  allprothosts$propprotzoon[i] <- allprothosts$numprotzoons[i]/allprothosts$numprots[i]
  if(allprothosts$numprotzoons[i] > 0){
    allprothosts$zoores[i] <- 1
  } else {
    allprothosts$zoores[i] <- 0
  }
}

# fill in allprots

for (i in 1:nrow(allprots)) {
  allprots$prothosts[i] <- allprotpairs %>% filter(protname == allprots$protname[i]) %>% 
    select(prothostname) %>% as.vector()
  allprots$numhosts[i] <- length(allprots$prothosts[[i]])
  allprots$protcomm[i] <- allprotpairs %>% filter(prothostname %in% allprots$prothosts[[i]], !grepl(allprots$protname[i], protname)) %>% 
    distinct(protname) %>% as.vector()
  allprots$protcommsize[i] <- length(allprots$protcomm[[i]])
  # need to add proportion of protcomm that is zoonotic!
  allprots$protcommzoos[i] <- allprots %>% filter(protname %in% allprots$protcomm[[i]]) %>% 
    select(zoostat)
  allprots$numcommzoons[i] <- length(which(allprots$protcommzoos[[i]] == 1))
  allprots$propcommzoon[i] <- allprots$numcommzoons[i]/allprots$protcommsize[i]
  allprots$hostnumprotzoons[i] <- allprothosts %>% filter(prothostname %in% allprots$prothosts[[i]]) %>% 
    select(numprotzoons)
  allprots$hostzoos[i] <- allprothosts %>% filter(prothostname %in% allprots$prothosts[[i]], numprotzoons > 1) %>% 
    select(prothostname) 
  allprots$numhostzoons[i] <- length(allprots$hostzoos[[i]])
  allprots$prophostzoon[i] <- allprots$numhostzoons[i]/allprots$numhosts[i]
}

allprots$propcommzoon[is.nan(allprots$propcommzoon)] <- 0

protcomm_prots <- allprots %>% select(protname, numhosts, numhostzoons, prophostzoon,
                                      protcommsize, numcommzoons, propcommzoon)

write.csv(protcomm_prots, "./data/modified/protcomm_prots.csv")

# select which vars you want to go into protraits and add join them

# protraits <- protraits %>% left_join(protsnet[,c(1, 3:11)], by ="protname") #protraits should now have 46 vars


# ALL PARS COMMUNITY! ------------


