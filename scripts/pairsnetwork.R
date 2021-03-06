## 7/30/2019

library(tidyverse) 
library(magrittr)

## create affiliate networks for host-prot pairs

# protzoos <- read.csv("./data/modified/protzoos.csv", row.names = 1, stringsAsFactors = F)

protzoos <- read.csv("./data/modified/prots226.csv", row.names = 1, stringsAsFactors = F)

allpairs <- read.csv("./data/modified/allpairs.csv", row.names = 1, stringsAsFactors = F)

allhosts <- read.csv("./data/modified/allhosts.csv", row.names = 1, stringsAsFactors = F) %>% 
  add_column(hostprots = NA, 
             numprots = NA,
             protzoos = NA,
             numprotzoons = NA,
             propprotzoon = NA,
             zoores = NA)

# WHY DOES ALL PROTS HAVE 228 AND NOT 226 PROTS!! ToT - fix this
allprots <- read.csv("./data/modified/allprots.csv", stringsAsFactors = F) %>% 
  left_join(protzoos[, c(1, 13)], by = "protname") %>% 
  add_column(prothosts = NA, 
             numhosts = NA,
             protcomm = NA,
             protcommsize = NA,
             hostnumprotzoons = NA,
             hostzoos = NA,
             numhostzoons = NA,
             prophostzoon = NA)

for (i in 1:nrow(allhosts)) {
  allhosts$hostprots[i] <- allpairs %>% filter(hostname == allhosts$hostname[i]) %>% 
    select(protname) %>% 
    as.vector()
  allhosts$numprots[i] <- length(allhosts$hostprots[[i]])
  allhosts$protzoos[i] <- allprots %>% filter(protname %in% allhosts$hostprots[[i]]) %>% 
  select(zoostat)
  allhosts$numprotzoons[i] <- length(which(allhosts$protzoos[[i]] == 1))
  allhosts$propprotzoon[i] <- allhosts$numprotzoons[i]/allhosts$numprots[i]
  if(allhosts$numprotzoons[i] > 0){
    allhosts$zoores[i] <- 1
  } else {
    allhosts$zoores[i] <- 0
  }
}

for (i in 1:nrow(allprots)) {
  allprots$prothosts[i] <- allpairs %>% filter(protname == allprots$protname[i]) %>% 
    select(hostname) %>% as.vector()
  allprots$numhosts[i] <- length(allprots$prothosts[[i]])
  allprots$protcomm[i] <- allpairs %>% filter(hostname %in% allprots$prothosts[[i]], !grepl(allprots$protname[i], protname)) %>% 
    distinct(protname) %>% as.vector()
  allprots$protcommsize[i] <- length(allprots$protcomm[[i]])
  # need to add proportion of protcomm that is zoonotic!
  allprots$hostnumprotzoons[i] <- allhosts %>% filter(hostname %in% allprots$prothosts[[i]]) %>% 
    select(numprotzoons)
  allprots$hostzoos[i] <- allhosts %>% filter(hostname %in% allprots$prothosts[[i]], numprotzoons > 1) %>% 
    select(hostname) 
  allprots$numhostzoons[i] <- length(allprots$hostzoos[[i]])
  allprots$prophostzoon[i] <- allprots$numhostzoons[i]/allprots$numhosts[i]
}

# select which vars you want to go into protraits and add join them

protraits <- protraits %>% left_join(protsnet[,c(1, 3:11)], by ="protname") #protraits should now have 46 vars


# ALL PARS COMMUNITY! ------------


