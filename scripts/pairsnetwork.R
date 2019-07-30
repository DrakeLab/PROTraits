## 7/30/2019
## create affiliate networks for host-prot pairs

# create tbl listing all unique prot spp (n = 255)
prots <- as.tbl(as.data.frame(unique(gmpdprot$protname))) %>% 
  rename(protname = `unique(gmpdprot$protname)`) %>% 
  mutate(prothosts = "empty")

# create tbl listing all unique host spp (n = 251)
hosts <- as.tbl(as.data.frame(unique(gmpdprot$hostname))) %>% 
  rename(hostname = `unique(gmpdprot$hostname)`) %>% 
  mutate(hostprots = "empty")

# create tbl listing all unique hp pairs (n = 880)
pairs <- gmpdprot %>% select(hostname, protname) %>% distinct() %>% as.tbl() %>% 
  mutate(pairname = paste(protname, ", ", hostname))

for (i in 1:length(prots$protname)) {
  prots$prothosts[i] <- pairs %>% filter(protname == prots$protname[i]) %>% 
    select(hostname) %>% 
    as.vector()
}

for (i in 1:length(hosts$hostname)) {
  hosts$hostprots[i] <- pairs %>% filter(hostname == hosts$hostname[i]) %>% 
    select(protname) %>% 
    as.vector()
}


