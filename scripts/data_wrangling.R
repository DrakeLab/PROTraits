# Author:  Joy Vaz       #
# Title:   Clean data    #
# Project: Protraits     #

### Attach packages ------

library(tidyverse) 
library(magrittr)
library(here)

rm(list = ls())

### Load data -------

## GMPD data 
# rows are records of host-par associations. Data from: Stephens et al. 2017, downloaded from https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.1799 on 2018.09.11

# Entire GMPD minus species with no binomial name and remove arthropods

gmpd_main <- read.csv("./data/original/GMPD_datafiles/GMPD_main.csv")

gmpd_allpars <- gmpd_main %>% 
  filter(HasBinomialName == "yes", !grepl("no binomial name", HostCorrectedName), !ParType == "Arthropod")

# Select only desired vars
gmpd_allpars <- gmpd_allpars  %>% 
  select(parname=ParasiteCorrectedName, -HasBinomialName, partype = ParType, 
         hostname=HostCorrectedName, hosttype=Group, 
         hostorder=HostOrder, hostfamily=HostFamily, hostenv=HostEnvironment, 
         location=LocationName, lat=Latitude, long=Longitude)

# Correct prot spp name ------
gmpd_allpars$parname <- gsub("Plasmodium malariae", "Plasmodium rodhaini", gmpd_allpars$parname)
gmpd_allpars$parname <- gsub("Plasmodium rodhani", "Plasmodium rodhaini", gmpd_allpars$parname)
gmpd_allpars$parname <- gsub("Plasmodium praefalciparum", "Plasmodium falciparum", gmpd_allpars$parname)
gmpd_allpars$parname <- gsub("Babesia equi", "Theileria equi", gmpd_allpars$parname)

length(unique(gmpd_allpars$parname)) # 1594 unique pars
length(unique(gmpd_allpars$hostname)) # 406 unique hosts
table(gmpd_allpars %>% distinct(parname, .keep_all = T) %>% select(partype)) # 253 protozoa

# save cleaned gmpd
write.csv(gmpd_allpars, "./data/modified/gmpd_main_clean.csv")

# Save all associations (every combo of host-par interaction)
gmpd_allobs <- gmpd_allpars %>% 
  select(parname, hostname)

write.csv(gmpd_allobs, "./data/modified/allgmpdobs.csv")

# Save all uniquie pairs (every unique combo of host-par interaction)
gmpd_allpairs <- gmpd_allpars %>% 
  select(parname, hostname) %>% 
  distinct()

write.csv(gmpd_allpairs, "./data/modified/allgmpdpairs.csv")

# Save prots
gmpd_allprots <- gmpd_allpars %>% 
  filter(partype == "Protozoa")

write.csv(gmpd_allprots, "./data/modified/gmpd_main_prot.csv")

# Add zooscores to GMPD ---------

# this is from Tao - a CSV of all the zooscored gmpd parasites
zooscore_data <- read.csv("./data/modified/Zooscore_GMPD_JV_21042021.csv")
names(zooscore_data)

zooscore_all <- zooscore_data %>% 
  select(rawparname=ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, zscore=XC_ZooScore, 
         gmpdmatch = Match, wOShits = WOShits..As.of.2.6.2017.)


length(unique(zooscore_all$rawparname)) # 1850 unique pars
length(unique(zooscore_all$gmpdmatch)) #1753 because there are 98 NAs - zooscore parnames that did not match GMPD names

# #' Other errors:
# #' Entamoeba histolytica is misspelled
# #' Isospora canis and Isospora belli should be Cystoisospora ____ instead
# 
# # correct E. histolytica spelling
# zooscore_all$parname <- gsub("Entamoemba histolytica", "Entamoeba histolytica", zooscore_all$parname)
# 
# # Correct genus name
# zooscore_all$parname <- gsub("Isospora canis", "Cystoisospora canis", zooscore_all$parname)
# zooscore_all$parname <- gsub("Isospora belli", "Cystoisospora belli", zooscore_all$parname)
# zooscore_all$parname <- gsub("Plasmodium malariae", "Plasmodium rodhaini", zooscore_all$parname)

#see if any zscores are missing
table(zooscore_all$zscore) %>% as.data.frame() %>% select(Freq) %>% sum() 
# sum is 1850 which means each row has a zscore

setdiff(gmpd_allpars$parname, 
        zooscore_all$gmpdmatch) %>% sort() # 141
setdiff(zooscore_all$gmpdmatch,
        gmpd_allpars$parname) %>% sort() # 299 (i guess the zooscores file has names that don't match up or parasites outside the GMPD?)
intersect(gmpd_allpars$parname, 
          zooscore_all$gmpdmatch) #1453

zooscore_all <- zooscore_all %>% select(parname = gmpdmatch, zscore)
setdiff(gmpd_zooscored$parname, gmpd_allpars$parname) # should be 0

# check if all rows have zscore
table(zooscore_all$zscore) %>% as.data.frame() %>% select(Freq) %>% sum() - nrow(zooscore_all) 

# add zscores to gmpd
gmpd_zooscored <- left_join(gmpd_allpars, zooscore_all, by = "parname")

# see how many have zscores
table(gmpd_zooscored$zscore) %>% as.data.frame() %>% select(Freq) %>% sum() # 15563/16595 rows have a zscore

setdiff(gmpd_zooscored$parname, gmpd_allpars$parname) # should be 0

#remove those without zscore
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

gmpd_zooscored <- completeFun(gmpd_zooscored, "zscore") # 15563 obs YAAAY :)

setdiff(gmpd_zooscored$parname, gmpd_allpars$parname) # should be 0


# assign zoostat

table(gmpd_zooscored$zscore)

for (i in 1:length(gmpd_zooscored$parname)) {
  if(gmpd_zooscored$zscore[i] > 0) {
    gmpd_zooscored$zoostat[i] <- 1
  } else {
    gmpd_zooscored$zoostat[i] <- 0
  }
}

table(gmpd_zooscored$zoostat)

# SAVE THIS MF

write.csv(gmpd_zooscored, "./data/modified/gmpd_zooscored.csv") # does not have parasite taxo or tm_mode

# Prots -------
rm(list = ls())

gmpd_zooscored <- read.csv("./data/modified/gmpd_zooscored.csv")[-1]

gmpdprot_zooscored <- gmpd_zooscored %>% filter(partype == "Protozoa")
# check if all rows have zscore
table(gmpdprot_zooscored$zscore) %>% as.data.frame() %>% select(Freq) %>% sum() - nrow(gmpdprot_zooscored)

length(unique(gmpdprot_zooscored$parname)) # 226 prots

protnames <- gmpdprot_zooscored$parname %>% unique() %>% as.vector() %>% as.data.frame() %>% 
  rename("parname" = ".") %>% arrange(parname) 

write.csv(protnames, "./data/modified/protnames.csv")

gmpdprot_zooscored %>% distinct(parname, .keep_all = T) %>% select(zoostat) %>% table() # 20 zoonotic
gmpdprot_zooscored %>% distinct(parname, .keep_all = T) %>% filter(zoostat == 1) %>% select(parname)

# Save as csv
write.csv(gmpdprot_zooscored, "./data/modified/gmpd_zooscored_prot.csv")

# add prot taxonomy data ------
gmpdtaxo <- read.csv("./data/original/GMPD_datafiles/GMPD_parasite_taxonomy.csv") %>% # rows are unique parasite species, columns are taxonomic classifications of each species
  filter(HasBinomialName == "yes") 

gmpdtaxo <- gmpdtaxo %>% select(parname=ParasiteCorrectedName, partype = ParType, parphylum=ParPhylum, 
                                parclass=ParClass, parorder=ParOrder, parfamily=ParFamily) %>% 
  distinct()

# Update this prot spp name
gmpdtaxo$parname <- gsub("Plasmodium malariae", "Plasmodium rodhaini", gmpdtaxo$parname)# fix spelling
gmpdtaxo$parname <- gsub("Plasmodium rodhani", "Plasmodium rodhaini", gmpdtaxo$parname)
gmpdtaxo$parname <- gsub("Plasmodium praefalciparum", "Plasmodium falciparum", gmpdtaxo$parname)
gmpdtaxo$parname <- gsub("Babesia equi", "Theileria equi", gmpdtaxo$parname)

gmpdtaxo <- gmpdtaxo %>% distinct()

length(unique(gmpdtaxo$parname)) # 2029 unique pars but the df has 2046 rows?

# find duplicate parasites
gmpdtaxo %>% select(parname) %>% 
  table() %>% as.data.frame() %>% filter(Freq > 1) 
#' UGHH there are 15 that appear more than once, and one that appears 3 times. Does this matter? 
#' Yes because the protozoa order, fam, etc. are going to be predictors. 
#' How many of these duplicates are actually protozoa?
gmpdtaxo %>% filter(partype == "Protozoa") %>% select(parname) %>% 
  table() %>% as.data.frame() %>% filter(Freq > 1) 
# Just the one - Cytauxzoon felis appears twice

gmpdtaxo %>% rownames_to_column() %>% filter(parname == "Cytauxzoon felis") 
# It has two Orders, Achromatorida and Piroplasmida. Wikipedia says Piroplasmida so I'm going to go with that.

gmpdtaxo <- gmpdtaxo[-445, ] %>% 
  distinct()

# check for duplicate protozoa again
gmpdtaxo %>% filter(partype == "Protozoa") %>% select(parname) %>% 
  table() %>% as.data.frame() %>% filter(Freq > 1) 
# None! Nice. The other partypes have duplicates but that's not my problem right now.

# Subset to just prots
gmpdprottaxo <- gmpdtaxo %>% filter(partype == "Protozoa")
length(unique(gmpdprottaxo$parname)) # 254
# 28 of those were not zooscored, I guess, since we have only 226 prots

setdiff(gmpdprottaxo$parname, protnames$parname) # 28, one of which is T. brimonti which we don't want anyway, plus the 8 hepatozoons.
setdiff(protnames$parname, gmpdprottaxo$parname) # 0
intersect(gmpdprottaxo$parname, zooscore_all$parname) # 227 - the extra one is T. brimonti

## get df of prots with taxo
gmpdprottaxo <- left_join(protnames, gmpdprottaxo) 

gmpdprottaxo %>% na.omit() %>% nrow() - nrow(gmpdprottaxo) # no prots w/o taxonomic data

# Save as csv
write.csv(gmpdprottaxo, "./data/modified/gmpd_taxo_prot.csv")

# add tranmission mode data --------------
rm(list = ls())

protnames <- read.csv("./data/modified/protnames.csv")
gmpd_zooscored <- read.csv("./data/modified/gmpd_zooscored.csv")
gmpdtm <- read.csv("./data/original/GMPD_datafiles/GMPD_parasite_traits.csv") %>% # rows are unique parasite species, columns are taxonomic classifications of each species
  select(-ParasiteTraitsCitation) %>% rename(parname = ParasiteCorrectedName)

# get df of prots with transmission modes
gmpdtm$parname <- gsub("Plasmodium malariae", "Plasmodium rodhaini", gmpdtm$parname)# fix spelling
gmpdtm$parname <- gsub("Plasmodium rodhani", "Plasmodium rodhaini", gmpdtm$parname)
gmpdtm$parname <- gsub("Plasmodium praefalciparum", "Plasmodium falciparum", gmpdtm$parname)
gmpdtm$parname <- gsub("Babesia equi", "Theileria equi", gmpdtm$parname)

unique(gmpd_zooscored$parname) #1453
unique(gmpdtm$parname) #1640
setdiff(protnames$parname, gmpdtm$parname) # 52
intersect(protnames$parname, gmpdtm$parname) # 174

gmpdtm_prot <- left_join(protnames, gmpdtm) %>% distinct() # 226
setdiff(protnames$parname, gmpdtm_prot$parname) # Nice

# Save as csv
write.csv(gmpdtm_prot, "./data/modified/gmpd_tm_prot.csv")


# Merge all to create GMPDprotraits

rm(list = ls())

gmpd_zoostat_prot <- read.csv("./data/modified/gmpd_zooscored_prot.csv") %>% 
  select(parname, zoostat) %>% distinct()
gmpd_taxo_prot <- read.csv("./data/modified/gmpd_taxo_prot.csv")[-1]
gmpd_tm_prot <- read.csv("./data/modified/gmpd_tm_prot")[-1]


gmpdprotraits <- gmpd_zoostat_prot %>% left_join(gmpd_taxo_prot) %>% left_join(gmpd_tm_prot)

# Save as csv
write.csv(gmpdprotraits, "./data/modified/protraits/gmpdprotraits.csv")
