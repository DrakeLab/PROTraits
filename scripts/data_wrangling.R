#############################
## Author:  Joy Vaz       ###
## Title:   Clean data    ###
## Project: Protraits     ###
#############################

library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)

# load datasets and subset data to only include relevant variables and protozoa obs.
protzoos <- read.csv("./data/original/Zooscore_datafiles/Zooscore_trait_Protozoa.csv") %>% 
  select(protname=ï..ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, 
         zscore=XC_ZooScore, cscore=XC_CScore, 
         gmpdprotname=ParasiteCorrectedName.updated, 
         tm_close=close, tm_nonclose=nonclose, tm_vector=vector, tm_intermediate=intermediate, 
         parphylum=ParPhylum, parclass=ParClass, parorder=ParOrder, parfamily=ParFamily)
  # Rows are unique protozoa species (ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final),  is given a zooscore (see Coding Flowchart). Data (unpublished) from: Han lab, Cary Institute of Ecosystem Studies, recieved via email from Barbara Han on 8.6.2018
gmpdprot <- read.csv("./data/original/GMPD_datafiles/GMPD_main.csv") %>% 
  filter(ParType == "Protozoa", HasBinomialName == "yes", !grepl("no binomial name", HostCorrectedName)) %>% 
  select(hosttype=Group, hostname=HostCorrectedName, hostorder=HostOrder, hostfamily=HostFamily, hostenv=HostEnvironment, 
         location=LocationName, lat=Latitude, long=Longitude, 
         gmpdprotname=ParasiteCorrectedName, 
         prev=Prevalence, 
         numhosts=HostsSampled, numsamples=NumSamples) %>% 
  mutate(ID = seq.int(nrow(gmpdprot))) # give unique ID to each GMPD record

  # Rows are unique observations of parasite(ParasiteCorrectedName) occurance in a host(HostCorrectedName) for wild primates, carnivores and ungulates. Data from: Stephens et al. 2017, downloaded from https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.1799 on 9.11.2018

gmpd_tax <- read.csv("./data/original/GMPD_datafiles/GMPD_parasite_taxonomy.csv") %>% 
  filter(ParType == "Protozoa", HasBinomialName == "yes") %>% 
  rename(gmpdprotname=ParasiteCorrectedName, parphylum=ParPhylum, parclass=ParClass, parorder=ParOrder, parfamily=ParFamily)

prots_48 <- read.csv("./data/modified/48prots.csv") %>% 
  rename(protname=ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, zscore=XC_ZooScore) %>% 
  mutate(cscore=NA, gmpdprotname=protname, tm_close=NA, tm_nonclose=NA, tm_vector=NA, tm_intermediate=NA)

#  select(parphylum=ParPhylum, parclass=ParClass, parorder=ParOrder, parfamily=ParFamily)

protzoos_2 <- left_join(prots_48, gmpd_tax) 

#%>% write.csv("./data/modified/48prots_tax.csv")

#__________________________________________________________________________________
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
length(unique(gmpdprot$protname)) - length(protzoos$protname) # there are 78 protozoa spp in gmpd that have not been scored in zooscore

# save a list of the prot spp that were not in protzoos
noscores <- as.data.frame(setdiff(gmpdprot$protname, protzoos$protname)) %>% rename(protname = `setdiff(gmpdprot$protname, protzoos$protname)`)

#__________________________________________________________________________________

# add protzoos data to gmpdprot
gmpdprotraits <- left_join(gmpdprot, protzoos %>% select(protname, zscore, cscore, tm_close, tm_nonclose, tm_vector, tm_intermediate), by = "protname")

# subset tbl of all gmpd protozoa-host pairs and combine with zooscore data
gmpdzooscored <- gmpdprotraits %>% filter(zscore >= -1)

# check if gmpdzooscored has 177 prot species
length(unique(gmpdzooscored$protname))

gmpdprot <- gmpdprot %>% mutate(prothostpair = paste(protname, hostname, sep = ", "))

gmpdpair <- gmpdprot %>% select(prothostpair, protname, hostname, hostfamily, hostorder, hosttype) %>% distinct()

#__________________________________________________________________________________

# create tbl listing all unique protozoa spp (n = 255)
protname <- as.tbl(as.data.frame(unique(gmpdprot$protname))) %>% rename(protname = `unique(gmpdprot$protname)`)

# create tbl listing all unique host spp from all datasets (n = )
hostname <- as.tbl(as.data.frame(unique(gmpdprot$hostname))) %>% rename(hostname = `unique(gmpdprot$hostname)`)

#__________________________________________________________________________________

#__________________________________________________________________________________


#__________________________________________________________________________________
  
