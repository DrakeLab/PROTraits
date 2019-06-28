#############################
## Author:  Joy Vaz       ###
## Title:   Network stuff ###
## Project: Protraits     ###
#############################
#find out how many hosts per prot

hostnum <- table(gmpdpair$parname) %>% as.data.frame() %>% rename(parname = Var1, hostnum = Freq)

gmpdpair %<>% semi_join(protzoos)

hostfamnum <- gmpdpair %>%
  group_by(parname) %>%
  summarise(n_distinct(carfamily))

hostordernum <- gmpdpair %>%
  group_by(parname) %>%
  summarise(n_distinct(carorder))

protzoos %<>% left_join(hostnum) %>%  
  left_join(gmpdpair) %>% 
  select(-gmpdparname, -carenv, -prev, -numhosts, -numsamples) %>% 
  distinct(parname, .keep_all = T)

protzoos2 <- protzoos %>% left_join(hostfamnum) %>% left_join(hostordernum)

y <- xtabs(~ parname + carname, gmpdpair)
carparmatrix <- as.matrix(y)

