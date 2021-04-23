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
table(gmpd_allpars %>% distinct(parname, .keep_all = T) %>% select(partype)) # 253 protozoa

# Save all associations (every combo of host-par interaction)
gmpd_allobs <- gmpd_allpars %>% 
  select(parname, hostname)

write.csv(gmpd_allobs, "./data/modified/allgmpdobs.csv")

# Save all uniquie pairs (every unique combo of host-par interaction)
gmpd_allpairs <- gmpd_allpars %>% 
  select(parname, hostname) %>% 
  distinct()

write.csv(gmpd_allpairs, "./data/modified/allgmpdpairs.csv")


# Add zooscores to GMPD ---------

# this is from Tao - a CSV of all the zooscored gmpd parasites
zooscore_data <- read.csv("./data/modified/Zooscore_GMPD_JV_21042021.csv")

zooscore_all <- zooscore_data %>% 
  select(rawparname=ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, zscore=XC_ZooScore, gmpdmatch = Match)

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

gmpd_zooscored <- read.csv("./data/modified/gmpd_zooscored.csv")[-1]

gmpdprot <- gmpd_zooscored %>% filter(partype == "Protozoa") 


length(unique(gmpdprot$parname)) # 226 prots
gmpdprot %>% distinct(parname, .keep_all = T) %>% select(zoostat) %>% table() # 20 zoonotic
gmpdprot %>% distinct(parname, .keep_all = T) %>% filter(zoostat == 1) %>% select(parname)


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
#' UGHH there are 16 that appear more than once. Does this matter? 
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

setdiff(gmpdprottaxo$parname, gmpdprot$parname) # 28, one of which is T. brimonti which we don't want anyway, plus the 8 hepatozoons.
setdiff(gmpdprot$parname, gmpdprottaxo$parname) # 0
intersect(gmpdprottaxo$parname, zooscore_all$parname) # 227 - the extra one is T. brimonti

## Merge GMPD prots with GMPD prots taxo
gmpdprot_taxo <- left_join(gmpdprot, gmpdprottaxo) # do i need this?



# Save as csvs
#write.csv(gmpdprot, "./data/modified/gmpdprot.csv")
#write.csv(gmpdprottaxo, "./data/modified/gmpdprottaxo.csv")

# add tranmission mode data --------------
gmpdtm <- read.csv("./data/original/GMPD_datafiles/GMPD_parasite_traits.csv") %>% # rows are unique parasite species, columns are taxonomic classifications of each species
  select(-ParasiteTraitsCitation) %>% rename(parname = ParasiteCorrectedName)

unique(gmpdtm$parname)

# Update this prot spp name
gmpdtaxo$parname <- gsub("Plasmodium malariae", "Plasmodium rodhaini", gmpdtaxo$parname)# fix spelling
gmpdtaxo$parname <- gsub("Plasmodium rodhani", "Plasmodium rodhaini", gmpdtaxo$parname)
gmpdtaxo$parname <- gsub("Plasmodium praefalciparum", "Plasmodium falciparum", gmpdtaxo$parname)
gmpdtaxo$parname <- gsub("Babesia equi", "Theileria equi", gmpdtaxo$parname)

gmpdtaxo <- gmpdtaxo %>% distinct()

length(unique(gmpdtaxo$parname)) # 2029 unique pars but the df has 2046 rows?




















# Clean data from zooscore files ------

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
setdiff(prots229$protname, gmpdprot$parname) 

#' 2 spp in prots229 are not listed gmpdprot: "Isospora canis"  "Trypanosoma brimonti"
#' Replace Isospora with Cystoisospora
#' Trypanosoma brimonti has one host with no binomial name and was thus excluded
  
prots229$protname <- gsub("Isospora canis", "Cystoisospora canis", prots229$protname) 

# Remove T. brimonti from prots229
prots228 <- prots229 %>% filter(!grepl("Trypanosoma brimonti", protname)) %>% 
  mutate(num_tm = tm_close + tm_nonclose + tm_vector + tm_intermediate) %>%
  select(-c(zscore, cscore, gmpdprotname)) # remove unecessary vars



# write.csv(prots228, "./data/modified/prots228.csv")
# write.csv(prots228, "./data/modified/protraits/gmpdprotraits.csv")

# Merge prots 228 with gmpdprottaxo ----

# Rename to match
gmpdprottaxo <- gmpdprottaxo %>% rename(protname = parname)

# Check for discrepencies between final protnames and parnames
setdiff(prots228$protname, gmpdprottaxo$protname) # 0

# Verify that the protnames in both datasets are now matching
intersect(prots228$protname, gmpdprottaxo$protname) %>% length()


## Merge

# Add prots228 data to gmpdprottaxo to create gmpdprotraits
gmpdprotraits <- left_join(prots228, gmpdprottaxo)

# Check if gmpdprotraits has 228 prot species
length(unique(gmpdprotraits$protname))

gmpdprotraits %>% distinct(protname, .keep_all = T) %>% select(zoostat) %>% table()

# write.csv(gmpdprotraits, "./data/modified/gmpdprotraits.csv")

# create tbl listing all unique prot spp (n = 228)
allprots <- gmpdprotraits %>% distinct(protname)

# create tbl listing all unique prot host spp (n = 246)
allprothosts <- gmpdprotraits %>% distinct(hostname)

# create tbl listing all unique host-prot pairs (n = 840)
allprotpairs <- gmpdprotraits %>% select(hostname, protname) %>% distinct() %>% as.tbl() %>% 
  mutate(pairname = paste(protname, ", ", hostname))

# write.csv(allprots, "./data/modified/protnames.csv")
# write.csv(allprothosts, "./data/modified/prothostnames.csv")
# write.csv(allprotpairs, "./data/modified/prothostpairs.csv")

# Extra/old code ----------

# # Remove marine and domestic hosts a la Dallas et al 2018 --------
#  
# remove seals and walruses since they are not terrestrial
# gmpd_allpars <- gmpd_allpars[-which(gmpd_allpars$HostFamily %in% c("Otariidae","Phocidae","Odobenidae")), ]
# 
# # these are the 33 species I was goin to remove because they removed domestic animals in
# gmpddomhosts <- c('Felis catus', #domestic cat (no gmpd records)
#                   'Micropotamogale lamottei','Micropotamogale ruwenzorii','Potamogale velox', # otter shrews
#                   'Rhagamys orthodon', # extinct rodent
#                   'Myocastor coypus', # semiaquatic rodent
#                   'Cynogale bennettii', # otter civet (semiaquatic civet)
# 
#                   # whole bunch of otters:
#                   'Aonyx capensis',  'Aonyx capensis','Aonyx cinerea','Enhydra lutris','Hydrictis maculicollis',
#                   'Lontra canadensis','Lontra felina','Lontra longicaudis','Lontra provocax','Lutra lutra',
#                   'Lutra nippon','Lutra sumatrana','Lutrogale perspicillata','Pteronura brasiliensis',
# 
#                   "Ovis aries", # domestic sheep (a bunch of records, but actually for all of these except one, the original host reported name is Ovis aries musimon or Ovis musimon, which is a wild goat)
#                   "Bos taurus",  # domestic cattle (no records)
#                   "Capra hircus",  # domestic goat (1 record, but the original reported name was Capra aegagrus which is a whole!! different!! WILD!! species!!)
#                   "Sus scrofa",  # wild boar
#                   "Equus caballus", # domestic horse (all records from 1 study - Mathee et al. 2004, re: helminths)
#                   "Equus asinus",  # wild ass or domestic donkey (all records from 1 study - Mathee et al. 2004, re: helminths)
#                   "Bubalis bubalis",  # domestic water buffalo (no gmpd records)
#                   "Camelus dromedarius",  # domestic dromedary camel (two records from 1 study - Mihok et al. 1994, re: T. simiae)
#                   "Camelus bactrianus", # domestic bactrian camel (no gmpd records)
#                   "Llama glama", # domestic llama (first of all: typo. Genus is spelled Lama, not Llama. 4 records but the origianl host reported name is Lama guanicoe, which is a whole!! another!! wild!! species!!)
#                   "Llama pacos", # domestic alpaca (same typo in genus. no gmpd records)
#                   "Ursus maritimus" # polar bear 
#                   )
# # this all doesn't make sense, screw it and just use GMPD as is. the dromedary and one ovis aries records are probably the only ones i'd change. idk, maybe the marine things too?

