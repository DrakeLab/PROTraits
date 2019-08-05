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
#write.csv(allprots, "./data/modified/allprots.csv")
#write.csv(allhosts, "./data/modified/allhosts.csv")
#write.csv(allpairs, "./data/modified/allpairs.csv")

for (i in 1:length(allprots$protname)) {
  allprots$prothosts[i] <- allpairs %>% filter(protname == allprots$protname[i]) %>% 
    select(hostname) %>% 
    as.vector()
}

for (i in 1:length(allhosts$hostname)) {
  allhosts$hostprots[i] <- allpairs %>% filter(hostname == allhosts$hostname[i]) %>% 
    select(protname) %>% 
    as.vector()
}


