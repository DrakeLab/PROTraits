# Author:  Joy Vaz       #
# Title:   Clean data    #
# Project: Protraits     #

### attach packages
library("tidyverse", "magrittr", "dplyr", "stringr")

## load protozoa zooscores data and subset relevant portions
#prots178 <- read.csv("./data/original/Zooscore_datafiles/Zooscore_trait_Protozoa.csv") %>% # rows are unique protozoa species (ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final), each is given a zooscore (see Coding Flowchart). Data (unpublished) from: Han lab, Cary Institute of Ecosystem Studies, recieved via email from Barbara Han on 8.6.2018
#  select(protname=ï..ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, 
#         zscore=XC_ZooScore, cscore=XC_CScore, 
#         gmpdprotname=ParasiteCorrectedName.updated, 
#         tm_close=close, tm_nonclose=nonclose, tm_vector=vector, tm_intermediate=intermediate, 
#         parphylum=ParPhylum, parclass=ParClass, parorder=ParOrder, parfamily=ParFamily)
#  
#gmpd_tax <- read.csv("./data/original/GMPD_datafiles/GMPD_parasite_taxonomy.csv") %>% # rows are unique parasite species, columns are taxonomic classifications of each species
#  filter(ParType == "Protozoa", HasBinomialName == "yes") %>% 
#  select(gmpdprotname=ParasiteCorrectedName, parphylum=ParPhylum, parclass=ParClass, parorder=ParOrder, parfamily=ParFamily) %>% 
#  distinct()
#
#prots048 <- read.csv("./data/modified/48prots.csv") %>% # 48 additional protozoa that have zooscores but no tranmission mode traits recorded in GMPD_parasite_traits.csv
#  rename(protname=ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, zscore=XC_ZooScore) %>% 
#  mutate(cscore=NA, gmpdprotname=protname, tm_close=NA, tm_nonclose=NA, tm_vector=NA, tm_intermediate=NA) %>% 
#  left_join(gmpd_tax) 
#
#protzoos <- rbind(prots178, prots048)
#
#write.csv(protzoos, "./data/modified/protzoos.csv")

#__________________________________________________________________________________

protzoos <- read.csv("./data/modified/protzoos.csv")[, 2:13]

gmpdprot <- read.csv("./data/original/GMPD_datafiles/GMPD_main.csv") %>% # rows are observations of parasite(ParasiteCorrectedName) occurance in a host(HostCorrectedName) for wild primates, carnivores and ungulates. Data from: Stephens et al. 2017, downloaded from https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.1799 on 9.11.2018
  filter(ParType == "Protozoa", HasBinomialName == "yes", !grepl("no binomial name", HostCorrectedName)) %>% 
  select(hosttype=Group, hostname=HostCorrectedName, hostorder=HostOrder, hostfamily=HostFamily, hostenv=HostEnvironment, 
         location=LocationName, lat=Latitude, long=Longitude, 
         gmpdprotname=ParasiteCorrectedName, 
         prev=Prevalence, 
         numhosts=HostsSampled, numsamples=NumSamples) %>% 
  mutate(ID = seq(len=2484)) # give unique ID to each GMPD record

## update gmpdprot with correct prot spp names

# check  for discrepencies in gmpdprotnames between the two dataframes 
setdiff(protzoos$gmpdprotname, gmpdprot$gmpdprotname) # 1 spp in protzoos does is not listed gmpdprot - this is because its host does not have a binomial name and was filtered out above. 
# remove T. brimonti from protzoos
protzoos <- protzoos %>% filter(!grepl("Trypanosoma brimonti", protname))

# check for discrepencies between final protnames and gmpdprotnames
setdiff(protzoos$protname, gmpdprot$gmpdprotname) # the final protnames in protzoos contains 2 corrected protnames that have been updated from the original gmpdprotname

# create protname variable in gmpd for final protnames
gmpdprot <- gmpdprot %>% mutate(protname=gmpdprotname)

# update the 2 spp names to match zooscore
gmpdprot$protname <- gsub("Cystoisospora canis", "Isospora canis", gmpdprot$protname)
gmpdprot$protname <- gsub("Plasmodium malariae", "Plasmodium rodhaini", gmpdprot$protname)

# verify that the protnames in both datasets are now matching
setdiff(protzoos$protname, gmpdprot$protname)

# check if that gmpdprot has the same number of prot spp as protzoos
length(unique(gmpdprot$protname)) - length(protzoos$protname) # there are 30 protozoa spp in gmpd that have not been scored yet

# save a list of the prot spp that were not in protzoos
noscores <- as.data.frame(setdiff(gmpdprot$protname, protzoos$protname)) %>% rename(protname = `setdiff(gmpdprot$protname, protzoos$protname)`)

#__________________________________________________________________________________

# add protzoos data to gmpdprot
gmpdprotraits <- left_join(gmpdprot, protzoos, by = "protname")

# subset tbl of all gmpd protozoa-host pairs and combine with zooscore data
gmpdzooscored <- gmpdprotraits %>% filter(!is.na(zscore))

# check if gmpdzooscored has 225 prot species
length(unique(gmpdzooscored$protname))

#__________________________________________________________________________________

# create tbl listing all unique protozoa spp (n = 255)
protname <- as.tbl(as.data.frame(unique(gmpdprot$protname))) %>% rename(protname = `unique(gmpdprot$protname)`)

# create tbl listing all unique host spp from all datasets (n = 251)
hostname <- as.tbl(as.data.frame(unique(gmpdprot$hostname))) %>% rename(hostname = `unique(gmpdprot$hostname)`)

#__________________________________________________________________________________

#__________________________________________________________________________________


#__________________________________________________________________________________
  
