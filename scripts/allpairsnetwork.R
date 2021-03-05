

library(tidyverse) 
library(magrittr)

## create affiliate networks for all host-par pairs

rm(list = ls())

# load data

gmpd_zooscored <- read.csv("./data/modified/gmpd_zooscored.csv", row.names = 1, stringsAsFactors = F) %>% 
  rename(parname = gmpdparname, hostname = gmpdhostname)

allpairs <- read.csv("./data/modified/allpairs.csv", row.names = 1, stringsAsFactors = F) %>% 
  rename(parname = gmpdparname, hostname = gmpdhostname)

# ALL HOSTS ----------

# create empty dfs

# Hosts
allhosts <- read.csv("./data/modified/allhosts.csv", row.names = 1, stringsAsFactors = F) %>% 
  rename(hostname = gmpdhostname) %>% 
  add_column(hostpars = NA, 
             numpars = NA,
             parzoos = NA,
             numparzoons = NA,
             propparzoon = NA,
             zoores = NA)

# pars
allpars <- gmpd_zooscored %>% select(parname, zoostat) %>% 
  distinct() %>% 
  add_column(parhosts = NA, 
             numhosts = NA,
             parcomm = NA,
             parcommsize = NA,
             parcommzoos = NA,
             numcommzoons =NA,
             propcommzoon = NA,
             hostnumparzoons = NA,
             hostzoos = NA,
             numhostzoons = NA,
             prophostzoon = NA)


# fill in allhosts

for (i in 1:nrow(allhosts)) {
  allhosts$hostpars[i] <- allpairs %>% filter(hostname == allhosts$hostname[i]) %>% 
    select(parname) %>% 
    as.vector()
  allhosts$numpars[i] <- length(allhosts$hostpars[[i]])
  allhosts$parzoos[i] <- allpars %>% filter(parname %in% allhosts$hostpars[[i]]) %>% 
    select(zoostat)
  allhosts$numparzoons[i] <- length(which(allhosts$parzoos[[i]] == 1))
  allhosts$propparzoon[i] <- allhosts$numparzoons[i]/allhosts$numpars[i]
  if(allhosts$numparzoons[i] > 0){
    allhosts$zoores[i] <- 1
  } else {
    allhosts$zoores[i] <- 0
  }
}

hostcomm_allpars <- allhosts %>% select(hostname, numpars, numparzoons, propparzoon, zoores)

# write.csv(hostcomm_allpars, "./data/modified/hostcomm_allpars.csv")

# fill in allpars

for (i in 1:nrow(allpars)) {
  allpars$parhosts[i] <- allpairs %>% filter(parname == allpars$parname[i]) %>% 
    select(hostname) %>% as.vector()
  allpars$numhosts[i] <- length(allpars$parhosts[[i]])
  allpars$parcomm[i] <- allpairs %>% filter(hostname %in% allpars$parhosts[[i]], !grepl(allpars$parname[i], parname)) %>% 
    distinct(parname) %>% as.vector()
  allpars$parcommsize[i] <- length(allpars$parcomm[[i]])
  # need to add proportion of parcomm that is zoonotic!
  allpars$parcommzoos[i] <- allpars %>% filter(parname %in% allpars$parcomm[[i]]) %>% 
    select(zoostat)
  allpars$numcommzoons[i] <- length(which(allpars$parcommzoos[[i]] == 1))
  allpars$propcommzoon[i] <- allpars$numcommzoons[i]/allpars$parcommsize[i]
  allpars$hostnumparzoons[i] <- allhosts %>% filter(hostname %in% allpars$parhosts[[i]]) %>% 
    select(numparzoons)
  allpars$hostzoos[i] <- allhosts %>% filter(hostname %in% allpars$parhosts[[i]], numparzoons > 1) %>% 
    select(hostname) 
  allpars$numhostzoons[i] <- length(allpars$hostzoos[[i]])
  allpars$prophostzoon[i] <- allpars$numhostzoons[i]/allpars$numhosts[i]
}

allpars$propcommzoon[is.nan(allpars$propcommzoon)] <- 0

#limit to protozoa

protnames <- read.csv("./data/modified/allprots.csv", row.names = 1, stringsAsFactors = F) %>% 
  rename(parname = protname)

protcomm_all <- left_join(protnames, allpars, by = "parname") %>% 
  select(parname, parcommsize, numparcommzoons = numcommzoons, propparcommzoon = propcommzoon) %>% 
  rename(protname = parname)

write.csv(protcomm_all, "./data/modified/protcomm_all.csv")

# select which vars you want to go into parraits and add join them

# parraits <- parraits %>% left_join(parsnet[,c(1, 3:11)], by ="parname") #parraits should now have 46 vars


# ALL PARS COMMUNITY! ------------


