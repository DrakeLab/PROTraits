# Author:  Joy Vaz       #
# Title:   Clean data    #
# Project: Protraits     #

### Attach packages

library(tidyverse) 
library(magrittr)

rm(list = ls())

# ALL GMPD PARS ------------
### Load data

## GMPD data # rows are records of host-par associations. Data from: Stephens et al. 2017, downloaded from https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.1799 on 2018.09.11
gmpdpars_all <- read.csv("./data/original/GMPD_datafiles/GMPD_main.csv") %>% 
  select(hosttype=Group, gmpdhostname=HostCorrectedName, hostorder=HostOrder, hostfamily=HostFamily, hostenv=HostEnvironment, 
         location=LocationName, lat=Latitude, long=Longitude, 
         gmpdparname=ParasiteCorrectedName, HasBinomialName, ParType)
length(unique(gmpdpars_all$gmpdparname)) # 2412 unique pars

gmpdpars_all$gmpdparname <- gsub("Plasmodium malariae", "Plasmodium rodhaini", gmpdpars_all$gmpdparname)

gmpdpars_binomialpars <- gmpdpars_all %>% 
  filter(HasBinomialName == "yes")
length(unique(gmpdpars_binomialpars$gmpdparname)) # 2031 unique pars after filtering out pars with no binomial name

gmpdpars_binomialhostspars <- gmpdpars_all %>% 
  filter(HasBinomialName == "yes", !grepl("no binomial name", gmpdhostname))
length(unique(gmpdpars_binomialhostspars$gmpdparname)) # 1998 unique pars after filtering out hosts and pars with no binomial name


zooscore_all <- read.csv("./data/original/Zooscore_datafiles/ZooScore_GMPD_201906-201908.csv") %>% 
  filter(!Non.GMPD == 1) %>% 
  select(gmpdparname=ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, zscore=XC_ZooScore)
length(unique(zooscore_all$gmpdparname)) # 1992 unique pars, but there are 1993 rows

# find duplicate parasite
table(zooscore_all$gmpdparname) %>% as.data.frame() %>% filter(Freq == 2) # Ascaris suum has two entries (rows 111 and 112) and they are almost identical but the zscores differ. Row 111 says 2 while row 112 says 1. I looked it up and I actually think it should be a 3 (tranmissible to other humans). Source: https://www.cdc.gov/parasites/ascariasis/prevent.html
# remove row with duplicate
zooscore_all <- zooscore_all %>% distinct(gmpdparname, .keep_all = T)
# get rownumber of Ascaris suum
zooscore_all %>% rownames_to_column() %>% filter(gmpdparname == "Ascaris suum") # row number 111
# replace Ascaris suum zscore with the correct one
zooscore_all$zscore[111]  <- 3 

#' Other errors:
#' Entamoeba histolytica is misspelled
#' Trypanosoma brucei is zoonotic (at least two subspecies of it are) Source: CDC https://www.cdc.gov/parasites/sleepingsickness/gen_info/faqs.html
#' Isospora canis and Isospora belli should be Cystoisospora ____ instead

# correct E. histolytica spelling
zooscore_all$gmpdparname <- gsub("Entamoemba histolytica", "Entamoeba histolytica", zooscore_all$gmpdparname)

# correct T. brucei zscore
zooscore_all %>% rownames_to_column() %>% filter(gmpdparname == "Trypanosoma brucei") # row number 1929
# replace zscore with the correct one
zooscore_all$zscore[1929]  <- 3

# Correct genus name
zooscore_all$gmpdparname <- gsub("Isospora canis", "Cystoisospora canis", zooscore_all$gmpdparname)
zooscore_all$gmpdparname <- gsub("Isospora belli", "Cystoisospora belli", zooscore_all$gmpdparname)
zooscore_all$gmpdparname <- gsub("Plasmodium malariae", "Plasmodium rodhaini", zooscore_all$gmpdparname)

#see if any zscores are missing
table(zooscore_all$zscore) %>% as.data.frame() %>% select(Freq) %>% sum() # sum is 1992 which means each row has a zscore

setdiff(gmpdpars_binomialhostspars$gmpdparname, 
        zooscore_all$gmpdparname) %>% sort() # 524
setdiff(zooscore_all$gmpdparname,
        gmpdpars_binomialhostspars$gmpdparname) # 517 (i guess the zooscores file had more pars from outside gmpd???)
intersect(gmpdpars_binomialhostspars$gmpdparname, 
          zooscore_all$gmpdparname) #1474

# add zscores to gmpd
gmpd_zooscored <- left_join(gmpdpars_binomialhostspars, zooscore_all, by = "gmpdparname")

# see how many have zscores
table(gmpd_zooscored$zscore) %>% as.data.frame() %>% select(Freq) %>% sum() # 13681/19333 rows have a zscore

setdiff(gmpd_zooscored$gmpdparname, gmpdpars_binomialhostspars$gmpdparname)

#remove those without zscore
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

gmpd_zooscored <- completeFun(gmpd_zooscored, "zscore") # 13681 obs YAAAY :)

setdiff(gmpd_zooscored$gmpdparname, gmpdpars_binomialhostspars$gmpdparname)

# assign zoostat

table(gmpd_zooscored$zscore)

for (i in 1:length(gmpd_zooscored$gmpdparname)) {
  if(gmpd_zooscored$zscore[i] > 0) {
    gmpd_zooscored$zoostat[i] <- 1
  } else {
    gmpd_zooscored$zoostat[i] <- 0
  }
}

table(gmpd_zooscored$zoostat)

# SAVE THIS MF

# write.csv(gmpd_zooscored, "./data/modified/gmpd_zooscored.csv")

gmpd_zooscored %>% distinct(gmpdparname, .keep_all = T) %>% filter(ParType == "Protozoa") %>% select(gmpdparname)
#

# PROTS ------
gmpd_zooscored <- read.csv("./data/modified/gmpd_zooscored.csv")[-1]

gmpdprot <- gmpd_zooscored %>% filter(ParType == "Protozoa") %>% rename(gmpdprotname = gmpdparname, prothostname = gmpdhostname)

#
gmpdtaxo <- read.csv("./data/original/GMPD_datafiles/GMPD_parasite_taxonomy.csv") %>% # rows are unique parasite species, columns are taxonomic classifications of each species
  filter(ParType == "Protozoa", HasBinomialName == "yes") %>% 
  select(gmpdprotname=ParasiteCorrectedName, 
         parphylum=ParPhylum, parclass=ParClass, parorder=ParOrder, parfamily=ParFamily) %>% 
  distinct()

# Save as csvs
#write.csv(gmpdprot, "./data/modified/gmpdprot.csv")
#write.csv(gmpdtaxo, "./data/modified/gmpdtaxo.csv")




## Load protozoa zooscores data and subset relevant portions

# 
prots178 <- read.csv("./data/original/Zooscore_datafiles/Zooscore_trait_Protozoa.csv") %>% # rows are unique protozoa species (ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final), each is given a zooscore (see Coding Flowchart). Data (unpublished) from: Han lab, Cary Institute of Ecosystem Studies, recieved via email from Barbara Han on 2018.08.06
  select(protname=ï..ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, 
         zscore=XC_ZooScore, cscore=XC_CScore, 
         gmpdprotname=ParasiteCorrectedName.updated, 
         tm_close=close, tm_nonclose=nonclose, tm_vector=vector, tm_intermediate=intermediate, 
         parphylum=ParPhylum, parclass=ParClass, parorder=ParOrder, parfamily=ParFamily, protWOS = WOShits..As.of.2.6.2017.)

#write.csv(prots178, "./data/modified/prots178.csv")

# 
prots051 <- read.csv("./data/modified/prots051.csv") %>% # 51 additional protozoa that have zooscores but no tranmission mode traits recorded in GMPD_parasite_traits.csv, plus E. histolytica which got added to this list instead of the original 178.
  select(protname, 
         zscore, cscore,
         gmpdprotname, 
         tm_close, tm_nonclose, tm_vector, tm_intermediate,
         parphylum, parclass, parorder, parfamily, protWOS = WOShits)

#write.csv(prots049, "./data/modified/prots049.csv")

# join the two datasets
names(prots178) == names(prots051)

prots229 <- rbind(prots178, prots051)

# assign zoostat

table(prots229$zscore)

for (i in 1:length(prots229$protname)) {
  if(prots229$zscore[i] > 0) {
    prots229$zoostat[i] <- 1
  } else {
    prots229$zoostat[i] <- 0
  }
}

table(prots229$zoostat)

### Clean data

## Names

# Check  for discrepencies in gmpdprotnames between the two dataframes 
setdiff(prots229$protname, gmpdprot$gmpdprotname) 

#' 3 spp in prots227 does is not listed gmpdprot: "Cystoisospora canis"  "Trypanosoma brimonti" "Cystoisospora belli"
#' gmpdprot has outdated names. replace Isospora with Cystoisospora
#' Trypanosoma brimonti has one host with no binomial name and was thus excluded
  
prots229$protname <- gsub("Isospora canis", "Cystoisospora canis", prots229$protname) 

# Remove T. brimonti from prots229
prots228 <- prots229 %>% filter(!grepl("Trypanosoma brimonti", protname))

# remove unecessary vars
prots228 <- prots228 %>% select(-c(zscore, cscore, gmpdprotname))

# Check for discrepencies between final protnames and gmpdprotnames
setdiff(prots228$protname, gmpdprot$gmpdprotname) # the final protnames in prots226 contains 2 corrected protnames that have been updated from the original gmpdprotname

# Create protname variable in gmpd for final protnames
gmpdprot <- gmpdprot %>% mutate(protname = gmpdprotname)

# Verify that the protnames in both datasets are now matching
intersect(prots228$protname, gmpdprot$protname) %>% length()

# write.csv(prots228, "./data/modified/prots228.csv")

### Create

# Add prots228 data to gmpdprot to create protraits (idk why this is a thing??)
protraits <- prots228 %>% 
  select(-c(zscore, cscore, zoostat, gmpdprotname)) %>% 
  left_join(gmpdprot, by = "protname")


# Check if protraits_zooscored has 228 prot species
length(unique(protraits$protname))

protraits %>% distinct(protname, .keep_all = T) %>% select(zoostat) %>% table()

# create tbl listing all unique prot spp (n = 228)
allprots <- protraits %>% distinct(protname)

# create tbl listing all unique prot host spp (n = 245)
allprothosts <- protraits %>% distinct(prothostname)

# create tbl listing all unique host-prot pairs (n = 840)
allprotpairs <- protraits %>% select(prothostname, protname) %>% distinct() %>% as.tbl() %>% 
  mutate(pairname = paste(protname, ", ", prothostname))

# # Split my mammal order
# 
# # Ungulates (n = 103)
# ung_protraits <- protraits %>% filter(hosttype == "ungulates") %>% distinct(protname, .keep_all = T)
# table(ung_protraits$zoostat) # 6/103 zoonotic, 97/103 non-zoonotic
# 
# # Carnivores (n = 54)
# car_protraits <- protraits %>% filter(hosttype == "carnivores") %>% distinct(protname, .keep_all = T)
# table(car_protraits$zoostat) # 3/54 zoonotic, 51/54 non-zoonotic
# 
# # Primates (n = 90)
# pri_protraits <- protraits %>% filter(hosttype == "primates") %>% distinct(protname, .keep_all = T)
# table(pri_protraits$zoostat) # 12/90 zoonotic, 78/90 non-zoonotic
# 
# # check if we got the right number
# sum(nrow(ung_protraits), nrow(car_protraits), nrow(pri_protraits)) == protraits %>% select(protname, hosttype) %>% distinct() %>% nrow()
# 
# # Save as csvs
# # write.csv(allprots, "./data/modified/allprots.csv")
# # write.csv(allprotpairs, "./data/modified/allprotpairs.csv")
# # write.csv(allprothosts, "./data/modified/allprothosts.csv")


# gmpd_zooscored <- read.csv("./data/modified/gmpd_zooscored.csv")[-1]
# 
# table(gmpd_zooscored$zoostat)
# 
# # create tbl listing all unique par spp (n = 226)
# allpars <- gmpd_zooscored %>% select(gmpdparname) %>% distinct()
# 
# # create tbl listing all unique prot host spp (n = 245)
# allhosts <- gmpd_zooscored %>% select(gmpdhostname) %>% distinct()
# 
# # create tbl listing all unique host-prot pairs (n = 840)
# allpairs <- gmpd_zooscored %>% select(gmpdhostname, gmpdparname) %>% distinct() %>% as.tbl() %>% 
#   mutate(pairname = paste(gmpdparname, ", ", gmpdhostname))
# 
# ung_partraits <- gmpd_zooscored %>% filter(hosttype == "ungulates") %>% distinct(gmpdparname, .keep_all = T)
# table(ung_partraits$zoostat) %>% print() # 6/103 zoonotic, 97/103 non-zoonotic
# 
# car_partraits <- gmpd_zooscored %>% filter(hosttype == "carnivores") %>% distinct(gmpdparname, .keep_all = T)
# table(car_partraits$zoostat) %>% print() # 3/54 zoonotic, 51/54 non-zoonotic
# 
# pri_partraits <- gmpd_zooscored %>% filter(hosttype == "primates") %>% distinct(gmpdparname, .keep_all = T)
# table(pri_partraits$zoostat) %>% print() # 12/90 zoonotic, 78/90 non-zoonotic
# 
# 
# # Save as csvs
# write.csv(allpars, "./data/modified/allpars.csv")
# write.csv(allpairs, "./data/modified/allpairs.csv")
# write.csv(allhosts, "./data/modified/allhosts.csv")
