## 7/30/2019

## create affiliate networks for host-prot pairs

protzoos <- read.csv("./data/modified/protzoos.csv", stringsAsFactors = F)[, -1]

allpairs <- read.csv("./data/modified/allpairs.csv", stringsAsFactors = F)[, -1]

allhosts <- read.csv("./data/modified/allhosts.csv", stringsAsFactors = F)[, -1] %>% 
  mutate(numprots = NA, protzoos = NA, numprotzoons = NA, propprotzoon = NA, zoonstat = NA)

allprots <- read.csv("./data/modified/allprots.csv", stringsAsFactors = F)[, -1] %>% 
  left_join(protzoos[,1:2], by = "protname") %>% 
  mutate(numhosts = NA, numhostprots = NA, hostzoos = NA, numhostzoons = NA, prophostzoon = NA)


for (i in 1:length(allhosts$hostname)) {
  allhosts$hostprots[i] <- allpairs %>% filter(hostname == allhosts$hostname[i]) %>% 
    select(protname) %>% 
    as.vector()
  allhosts$numprots[i] <- length(allhosts$hostprots[[i]])
  allhosts$protzoos[i] <- protzoos %>% filter(protname %in% allhosts$hostprots[[i]]) %>% 
  select(zscore)
  allhosts$numprotzoons[i] <- length(which(allhosts$protzoos[[i]] >= 1))
  allhosts$propprotzoon[i] <- allhosts$numprotzoons[i]/allhosts$numprots[i]
  allhosts$zoonstat[i] <- if(allhosts$numprotzoons[i] > 0){
    print("1")
  } else {
    print("0")
  }
}

for (i in 1:length(allprots$protname)) {
  allprots$prothosts[i] <- allpairs %>% filter(protname == allprots$protname[i]) %>% 
    select(hostname) %>% 
    as.vector()
  allprots$prothostprots[i] <- allpairs %>% filter(hostname %in% allprots$prothosts[[i]]) %>% 
    select(protname) %>% distinct() %>% 
    as.vector()
  allprots$numhosts[i] <- length(allprots$prothosts[[i]])
  allprots$numhostprots[i] <- length(allprots$prothostprots[[i]])
  allprots$hostzoos[i] <- allhosts %>% filter(hostname %in% allprots$prothosts[[i]]) %>% 
    select(zoonstat)
  allprots$numhostzoons[i] <- length(which(allprots$hostzoos[[i]] >= 1))
  allprots$prophostzoon[i] <- allprots$numhostzoons[i]/allprots$numhosts[i]
}
