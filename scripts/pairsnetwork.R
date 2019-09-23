## 7/30/2019

library(tidyverse) 
library(magrittr) 
library(dplyr) 
library(stringr)

## create affiliate networks for host-prot pairs

protzoos <- read.csv("./data/modified/protzoos.csv", row.names = 1, stringsAsFactors = F)

allpairs <- read.csv("./data/modified/allpairs.csv", row.names = 1, stringsAsFactors = F)

allhosts <- read.csv("./data/modified/allhosts.csv", row.names = 1, stringsAsFactors = F)

allprots <- read.csv("./data/modified/allprots.csv", row.names = 1, stringsAsFactors = F) %>% 
  left_join(protzoos[,1:2], by = "protname")

for (i in 1:length(allhosts$hostname)) {
  allhosts$hostprots[i] <- allpairs %>% filter(hostname == allhosts$hostname[i]) %>% 
    select(protname) %>% 
    as.vector()
  allhosts$numprots[i] <- length(allhosts$hostprots[[i]])
  allhosts$protzoos[i] <- protzoos %>% filter(protname %in% allhosts$hostprots[[i]]) %>% 
  select(zscore)
  allhosts$numprotzoons[i] <- length(which(allhosts$protzoos[[i]] >= 1))
  allhosts$propprotzoon[i] <- allhosts$numprotzoons[i]/allhosts$numprots[i]
  if(allhosts$numprotzoons[i] > 0){
    allhosts$zoores[i] <- 1
  } else {
    allhosts$zoores[i] <- 0
  }
}

for (i in 1:length(allprots$protname)) {
  allprots$prothosts[i] <- allpairs %>% filter(protname == allprots$protname[i]) %>% 
    select(hostname) %>% 
    as.vector()
  allprots$protcomm[i] <- allpairs %>% filter(hostname %in% allprots$prothosts[[i]]) %>% 
    select(protname, -starts_with(allprots$protname[i])) %>% distinct() %>% 
    as.vector()
  allprots$numhosts[i] <- length(allprots$prothosts[[i]])
  allprots$protcommsize[i] <- length(allprots$protcomm[[i]])
  allprots$hostnumprotzoons[i] <- allhosts %>% filter(hostname %in% allprots$prothosts[[i]]) %>% 
    select(numprotzoons)
  allprots$hostzoos[i] <- allhosts %>% filter(hostname %in% allprots$prothosts[[i]]) %>% 
    select(zoonstat)
  allprots$numhostzoons[i] <- length(which(allprots$hostzoos[[i]] >= 1))
  allprots$prophostzoon[i] <- allprots$numhostzoons[i]/allprots$numhosts[i]
}

protraits <- protraits %>% left_join(protsnet[,c(1, 3:11)], by ="protname") #protraits should now have 46 vars
