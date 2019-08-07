## 7/30/2019
## create affiliate networks for host-prot pairs

# create tbl listing all unique prot spp (n = 255)
allprots <- as.tbl(as.data.frame(unique(gmpdprot$protname))) %>% 
  rename(protname = `unique(gmpdprot$protname)`) %>% 
  mutate(prothosts = "empty")

# create tbl listing all unique host spp (n = 251)
allhosts <- as.tbl(as.data.frame(unique(gmpdprot$hostname))) %>% 
  rename(hostname = `unique(gmpdprot$hostname)`) %>% 
  mutate(hostprots = "empty")

# create tbl listing all unique hp pairs (n = 880)
allpairs <- gmpdprot %>% select(hostname, protname) %>% distinct() %>% as.tbl() %>% 
  mutate(pairname = paste(protname, ", ", hostname))

# save as csvs
#write.csv(allhosts, "./data/modified/allhosts.csv")
#write.csv(allprots, "./data/modified/allprots.csv")
#write.csv(allpairs, "./data/modified/allpairs.csv")

allhosts <- read.csv("./data/modified/allhosts.csv", stringsAsFactors = F)[, -1]
allprots <- read.csv("./data/modified/allprots.csv", stringsAsFactors = F)[, -1]
allpairs <- read.csv("./data/modified/allpairs.csv", stringsAsFactors = F)[, -1]

for (i in 1:length(allprots$protname)) {
  allprots$prothosts[i] <- allpairs %>% filter(protname == allprots$protname[i]) %>% 
    select(hostname) %>% 
    as.vector()
  allprots$prothostprots[i] <- allpairs %>% filter(hostname %in% allprots$prothosts[[i]]) %>% 
    select(protname) %>% 
    as.vector()
}

for (i in 1:length(allhosts$hostname)) {
  allhosts$hostprots[i] <- allpairs %>% filter(hostname == allhosts$hostname[i]) %>% 
    select(protname) %>% 
    as.vector()
}

