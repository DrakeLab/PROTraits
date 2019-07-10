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
  select(parname=ï..ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, 
         zscore=XC_ZooScore, cscore=XC_CScore, 
         gmpdparname=ParasiteCorrectedName.updated, 
         tm_close=close, tm_nonclose=nonclose, tm_vector=vector, tm_intermediate=intermediate, 
         parphylum=ParPhylum, parclass=ParClass, parorder=ParOrder, parfamily=ParFamily)
  # Rows are unique protozoa species (ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final),  is given a zooscore (see Coding Flowchart). Data (unpublished) from: Han lab, Cary Institute of Ecosystem Studies, recieved via email from Barbara Han on 8.6.2018
gmpdprot <- read.csv("./data/original/GMPD_datafiles/GMPD_main.csv") %>% 
  filter(ParType == "Protozoa", HasBinomialName == "yes") %>% 
  select(hosttype=Group, hostname=HostCorrectedName, hostorder=HostOrder, hostfamily=HostFamily, hostenv=HostEnvironment, 
         location=LocationName, lat=Latitude, long=Longitude, 
         gmpdparname=ParasiteCorrectedName, 
         prev=Prevalence, 
         numhosts=HostsSampled, numsamples=NumSamples)
  # Rows are unique observations of parasite(ParasiteCorrectedName) occurance in a host(HostCorrectedName) for wild primates, carnivores and ungulates. Data from: Stephens et al. 2017, downloaded from https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.1799 on 9.11.2018
prots_48 <- read.csv("./data/modified/48prots.csv") %>% 
  mutate(parname=ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final)
#  rename(parname=ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, 
#         zscore=XC_ZooScore) %>% 
#  mutate(cscore=NA, 
#         gmpdparname=parname,
#         tm_close=NA, tm_nonclose=NA, tm_vector=NA, tm_intermediate=NA)
gmpd_tax <- read.csv("./data/original/GMPD_datafiles/GMPD_parasite_taxonomy.csv") %>% 
  filter(ParType == "Protozoa", HasBinomialName == "yes") %>% 
  mutate(parname=ParasiteCorrectedName)
#  select(parphylum=ParPhylum, parclass=ParClass, parorder=ParOrder, parfamily=ParFamily)

left_join(prots_48, gmpd_tax) %>% write.csv("./data/modified/48prots_tax.csv")



#__________________________________________________________________________________
## update gmpdprot with correct prot spp names

# all prot spp in protzoos should match to spp in gmpdprot since protzoos spp were taken from gmpd 
setdiff(protzoos$gmpdparname, gmpdprot$gmpdparname) # comparing gmpdparname shows no difference
setdiff(protzoos$parname, gmpdprot$gmpdparname) # however the final spp names protzoos parname contains 2 updated spp names that differ from gmpdparname

# create new parname variable in gmpd 
gmpdprot %<>% mutate(parname=gmpdparname)

# update the 2 spp names to match zooscore
gmpdprot$parname <- gsub("Cystoisospora canis", "Isospora canis", gmpdprot$parname)
gmpdprot$parname <- gsub("Plasmodium malariae", "Plasmodium rodhaini", gmpdprot$parname)

# check that the parnames in both datasets are now matching
setdiff(protzoos$parname, gmpdprot$parname)
#__________________________________________________________________________________

# there are 78 protozoa spp in gmpd that have not been scored in zooscore
noscores <- as.data.frame(setdiff(gmpdprot$parname, protzoos$parname))
#__________________________________________________________________________________

# create tbl listing all unique protozoa spp from all datasets (n = 294)
parname <- unique(gmpdprot$parname)
parnames <- as.tbl(as.data.frame(parname))

# create tbl listing all unique carrier spp from all datasets (n = )
carname <- unique(gmpdprot$carname)
carnames <- as.tbl(as.data.frame(carname))
#__________________________________________________________________________________

# add protzoos data to gmpdprot
gmpdprot %<>% left_join(protzoos %>% select(parname, zscore,tm_close, tm_nonclose, tm_vector, tm_intermediate), by = "parname")

# subset tbl of all gmpd protozoa-host pairs and combine with zooscore data
gmpdzooscored <- gmpdprotraits %>% filter(zscore >= -1)

protrait <- left_join(parnames, gmpdprot)

#subset tbl of all gmpd protozoa-host pairs and combine with zooscore data
allprotscored <- protrait %>% filter(zscore >= -1)
#__________________________________________________________________________________

gmpdprot %<>% mutate(parcarpair = paste(parname, carname, sep = ", "))

gmpdpair <- distinct(gmpdprot, parcarpair, .keep_all = T) 

#__________________________________________________________________________________
  
