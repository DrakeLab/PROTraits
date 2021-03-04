## MAMMAL TRAITS

load("C:/Users/joych/Downloads/dataForGBM.RData")
allFinalWOS <- allFinalWOS %>% rownames_to_column() %>% rename(hostname = rowname)

allhosts <- read.csv("./data/modified/allhosts.csv")[-1] %>% rename(hostname = gmpdhostname)

setdiff(allFinalWOS$hostname, allhosts$hostname) #44
setdiff(allhosts$hostname, allFinalWOS$hostname) #88
intersect(allFinalWOS$hostname, allhosts$hostname) #295

hosttraits <- left_join(allhosts, allFinalWOS, by = "hostname")

hostpairtraits <- left_join(allprotpairs, hostpairtraits, by = "hostname")

prothosttraits <- hostpairtraits %>% group_by(protname) %>% summarise_if(is.numeric, mean, na.rm = TRUE)



mutate(pairname = paste(protname, ", ", prothostname))