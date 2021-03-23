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

# Entire GMPD minus species with no binomial name ---------
gmpd_allpars <- read.csv("./data/original/GMPD_datafiles/GMPD_main.csv") %>% 
  filter(HasBinomialName == "yes", !grepl("no binomial name", HostCorrectedName))

# Select only desired vars
gmpd_allpars <- gmpd_allpars  %>% 
  select(gmpdparname=ParasiteCorrectedName, -HasBinomialName, partype = ParType, 
         gmpdhostname=HostCorrectedName, hosttype=Group, 
         hostorder=HostOrder, hostfamily=HostFamily, hostenv=HostEnvironment, 
         location=LocationName, lat=Latitude, long=Longitude)

# Update this prot spp name
gmpd_allpars$gmpdparname <- gsub("Plasmodium malariae", "Plasmodium rodhaini", gmpd_allpars$gmpdparname)
  
length(unique(gmpd_allpars$gmpdparname)) # 1998 unique pars
table(gmpd_allpars %>% distinct(gmpdparname, .keep_all = T) %>% select(partype)) # 255 protozoa

# save all pairs
gmpd_allpairs <- gmpd_allpars %>% 
  select(gmpdparname, gmpdhostname) %>% 
  distinct()

write.csv(gmpd_allpairs, "./data/modified/allgmpdpairs.csv")

# Add zoostat to zscores to GMPD ---------

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
table(zooscore_all$zscore) %>% as.data.frame() %>% select(Freq) %>% sum() 
# sum is 1992 which means each row has a zscore

setdiff(gmpd_allpars$gmpdparname, 
        zooscore_all$gmpdparname) %>% sort() # 524
setdiff(zooscore_all$gmpdparname,
        gmpd_allpars$gmpdparname) # 517 (i guess the zooscores file had more pars from outside gmpd???)
intersect(gmpd_allpars$gmpdparname, 
          zooscore_all$gmpdparname) #1474

# add zscores to gmpd
gmpd_zooscored <- left_join(gmpd_allpars, zooscore_all, by = "gmpdparname")

# see how many have zscores
table(gmpd_zooscored$zscore) %>% as.data.frame() %>% select(Freq) %>% sum() # 13681/19333 rows have a zscore

setdiff(gmpd_zooscored$gmpdparname, gmpd_allpars$gmpdparname)

#remove those without zscore
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

gmpd_zooscored <- completeFun(gmpd_zooscored, "zscore") # 13681 obs YAAAY :)

setdiff(gmpd_zooscored$gmpdparname, gmpd_allpars$gmpdparname) # should be 0

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

# write.csv(gmpd_zooscored, "./data/modified/gmpd_zooscored.csv") # does not have parasite taxo or tm_mode

# Prots -------

gmpdprot <- gmpd_zooscored %>% filter(partype == "Protozoa") 


length(unique(gmpdprot$gmpdparname)) # 228 prots
gmpdprot %>% distinct(gmpdparname, .keep_all = T) %>% select(zoostat) %>% table() # 13 zoonotic
gmpdprot %>% distinct(gmpdparname, .keep_all = T) %>% filter(zoostat == 1) %>% select(gmpdparname)


# add prot taxonomy data ------
gmpdtaxo <- read.csv("./data/original/GMPD_datafiles/GMPD_parasite_taxonomy.csv") %>% # rows are unique parasite species, columns are taxonomic classifications of each species
  filter(HasBinomialName == "yes") 

gmpdtaxo <- gmpdtaxo %>% select(gmpdparname=ParasiteCorrectedName, partype = ParType, parphylum=ParPhylum, 
                                parclass=ParClass, parorder=ParOrder, parfamily=ParFamily) %>% 
  distinct()

# Update this prot spp name
gmpdtaxo$gmpdparname <- gsub("Plasmodium malariae", "Plasmodium rodhaini", gmpdtaxo$gmpdparname)
# fix spelling
gmpdtaxo$gmpdparname <- gsub("Plasmodium rodhani", "Plasmodium rodhaini", gmpdtaxo$gmpdparname)

length(unique(gmpdtaxo$gmpdparname)) # 2031 unique pars but the df has 2048 rows?

# find duplicate parasites
gmpdtaxo %>% select(gmpdparname) %>% 
  table() %>% as.data.frame() %>% filter(Freq > 1) 
#' UGHH there are 16 that appear more than once. Does this matter? 
#' Yes because the protozoa order, fam, etc. are going to be predictors. 
#' How many of these duplicates are actually protozoa?

gmpdtaxo %>% filter(partype == "Protozoa") %>% select(gmpdparname) %>% 
  table() %>% as.data.frame() %>% filter(Freq > 1) 
# Just the one - Cytauxzoon felis appears twice

gmpdtaxo %>% rownames_to_column() %>% filter(gmpdparname == "Cytauxzoon felis") 
# It has two Orders, Achromatorida and Piroplasmida. Wikipedia says Piroplasmida so I'm going to go with that.

gmpdtaxo <- gmpdtaxo[-445, ] %>% 
  distinct()

# check for duplicate protozoa again
gmpdtaxo %>% filter(partype == "Protozoa") %>% select(gmpdparname) %>% 
  table() %>% as.data.frame() %>% filter(Freq > 1) 
# None! Nice. The other partypes have duplicates but that's not my problem right now.

# Subset to just prots
gmpdprottaxo <- gmpdtaxo %>% filter(partype == "Protozoa")
length(unique(gmpdprottaxo$gmpdparname)) # 255
# WHY are there more than 228? Is it because 27 of those were not zooscored?

setdiff(gmpdprottaxo$gmpdparname, gmpdprot$gmpdparname) # 27, one of which is T. brimonti which we don't want anyway
setdiff(gmpdprot$gmpdparname, gmpdprottaxo$gmpdparname) # 0
intersect(gmpdprottaxo$gmpdparname, zooscore_all$gmpdparname) # HOW CAN THIS BE 229 WHEN TAXO ONLY HAS 255 PARS ASDFASDLFKJ?

## Merge GMPD prots with GMPD prots taxo
gmpdprottaxo <- left_join(gmpdprot, gmpdprottaxo)

# Save as csvs
#write.csv(gmpdprot, "./data/modified/gmpdprot.csv")
#write.csv(gmpdprottaxo, "./data/modified/gmpdprottaxo.csv")


# PROTS from raw data------

# I think the point of this is just to add the tm modes.

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

# correct T. brucei zscore
prots229 %>% rownames_to_column() %>% filter(protname == "Trypanosoma brucei") # row number 156
# replace zscore with the correct one
prots229$zscore[156]  <- 3

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
setdiff(prots229$protname, gmpdprot$gmpdparname) 

#' 2 spp in prots229 are not listed gmpdprot: "Isospora canis"  "Trypanosoma brimonti"
#' Replace Isospora with Cystoisospora
#' Trypanosoma brimonti has one host with no binomial name and was thus excluded
  
prots229$protname <- gsub("Isospora canis", "Cystoisospora canis", prots229$protname) 

# Remove T. brimonti from prots229
prots228 <- prots229 %>% filter(!grepl("Trypanosoma brimonti", protname)) %>% 
  select(-c(zscore, cscore, gmpdprotname, zoostat)) # remove unecessary vars

# write.csv(prots228, "./data/modified/prots228.csv")

# Merge prots 228 with gmpdprottaxo ----

# Rename to match
gmpdprottaxo <- gmpdprottaxo %>% rename(protname = gmpdparname)

# Check for discrepencies between final protnames and gmpdparnames
setdiff(prots228$protname, gmpdprottaxo$protname) # 0

# Verify that the protnames in both datasets are now matching
intersect(prots228$protname, gmpdprottaxo$protname) %>% length()


## Merge

# Add prots228 data to gmpdprottaxo to create gmpdprotraits
gmpdprotraits <- left_join(prots228, gmpdprottaxo)

# Check if gmpdprotraits has 228 prot species
length(unique(gmpdprotraits$protname))

gmpdprotraits %>% distinct(protname, .keep_all = T) %>% select(zoostat) %>% table()

# write.csv(gmpdprotraits, "./data/modified/protraits/gmpdprotraits.csv")

# create tbl listing all unique prot spp (n = 228)
allprots <- gmpdprotraits %>% distinct(protname)

# create tbl listing all unique prot host spp (n = 246)
allprothosts <- gmpdprotraits %>% distinct(gmpdhostname)

# create tbl listing all unique host-prot pairs (n = 840)
allprotpairs <- gmpdprotraits %>% select(gmpdhostname, protname) %>% distinct() %>% as.tbl() %>% 
  mutate(pairname = paste(protname, ", ", gmpdhostname))

# write.csv(allprots, "./data/modified/protnames.csv")
# write.csv(allprothosts, "./data/modified/prothostnames.csv")
# write.csv(allprotpairs, "./data/modified/prothostpairs.csv")
