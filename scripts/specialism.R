
load("./data/original/Park_2016/get_nriABUND.Rda")
protnames <- read.csv("./data/modified/protnames.csv", row.names = 1)

nri <- nri %>% rownames_to_column() %>% rename(protname = rowname)

nri.prot <- left_join(protnames, nri) %>% select(protname, host_phylo_range = mpd.obs)

write.csv(nri.prot, "./data/modified/protraits/phyloprotraits.csv")

