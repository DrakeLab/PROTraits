#############################
## Author:  Joy Vaz       ###
## Title:   Clean data    ###
## Project: Protraits     ###
#############################

library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)

# load datasets
protzoos <- read.csv("./Data/Zooscore_datafiles/Zooscore_trait_Protozoa.csv") # Rows are unique protozoa species (ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final),  is given a zooscore (see Coding Flowchart). Data (unpublished) from: Han lab, Cary Institute of Ecosystem Studies, recieved via email from Barbara Han on 8.6.2018
#eid2prot <- read.csv("./Data/Wardeh_datafiles/SpeciesInteractions_EID2.csv") # Rows are unique host(Carrier)-parasite(Cargo) associations. Data from: Wardeh et al. 2015, downloaded from https://www.nature.com/articles/sdata201549 on 9.11.2018
gmpdprot <- read.csv("./Data/GMPD_datafiles/GMPD_main.csv") # Rows are unique observations of parasite(ParasiteCorrectedName) occurance in a host(HostCorrectedName) for wild primates, carnivores and ungulates. Data from: Stephens et al. 2017, downloaded from https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.1799 on 9.11.2018

# subset datasets to only include protozoa rows and relevant columns
protzoos %<>% select(parname=ï..ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, zscore=XC_ZooScore, cscore=XC_CScore, gmpdparname=ParasiteCorrectedName.updated, tm_close=close, tm_nonclose=nonclose, tm_vector=vector, tm_intermediate=intermediate, parphylum=ParPhylum, parclass=ParClass, parorder=ParOrder, parfamily=ParFamily)
#eid2prot %<>% filter(Cargo.classification == "Protozoa", Carrier.classification == c("Human", "Mammal", "Domestic", "Primate", "Rodent"), Publications.count >= 1) %<>% select(parname=Cargo, carname=Carrier, cartype=Carrier.classification, seqcount=Sequences.count, seqID=Sequences, pubcount=Publications.count, pubID=Publications)
gmpdprot %<>% filter(ParType == "Protozoa", HasBinomialName == "yes") %<>% select(mamtype=Group, carname=HostCorrectedName, carorder=HostOrder, carfamily=HostFamily, carenv=HostEnvironment, gmpdparname=ParasiteCorrectedName, prev=Prevalence, numhosts=HostsSampled, numsamples=NumSamples)

# capitalise first letter of binomial names in eid2prot to match zooscore and gmpdprot
#eid2prot$parname <- gsub("(^[a-z])", "\\U\\1", tolower(eid2prot$parname), perl = T)
#eid2prot$carname <- gsub("(^[a-z])", "\\U\\1", tolower(eid2prot$carname), perl = T)

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
#parname <- unique(union(eid2prot$parname, gmpdprot$parname))
parname <- unique(gmpdprot$parname)
parnames <- as.tbl(as.data.frame(parname))

# create tbl listing all unique carrier spp from all datasets (n = )
#carname <- unique(union(eid2prot$carname, gmpdprot$carname))
carname <- unique(gmpdprot$carname)
carnames <- as.tbl(as.data.frame(carname))
#__________________________________________________________________________________

# add protzoos data to gmpdprot
#gmpdprot %<>% left_join(protzoos %>% select(parname, zscore,tm_close, tm_nonclose, tm_vector, tm_intermediate), by = "parname")

# subset tbl of all gmpd protozoa-host pairs and combine with zooscore data
#gmpdzooscored <- gmpdprotraits %>% filter(zscore >= -1)

#protrait <- left_join(parnames, gmpdprot)

# subset tbl of all gmpd protozoa-host pairs and combine with zooscore data
#allprotscored <- protrait %>% filter(zscore >= -1)
#__________________________________________________________________________________

# compare and contrast unique prot spp of the two main datasets
eid2xtra <- setdiff(unique(eid2prot$parname), unique(gmpdprot$parname))
gmpdxtra <- setdiff(unique(gmpdprot$parname), unique(eid2prot$parname))
sameprot <- intersect(unique(gmpdprot$parname), unique(eid2prot$parname)) # 30 prots shared by both databases, 3 of them in noscore
# check if zero
sum(length(eid2xtra), length(gmpdxtra), length(sameprot)) - length(parnames$parname)
#__________________________________________________________________________________

eid2pair <- eid2prot %>% 
  select(parname, carname, cartype) %>% 
  mutate(parcarpair = paste(parname, carname, sep = ", "))

gmpdprot %<>% mutate(parcarpair = paste(parname, carname, sep = ", "))

gmpdpair <- distinct(gmpdprot, parcarpair, .keep_all = T) 

parcarpair <- setdiff(eid2pair$parcarpair, gmpdpair$parcarpair)
xtrapairs <- as.tbl(as.data.frame(parcarpair)) %>% 
  left_join(eid2pair)
  
allpairs <- as.tbl(as.data.frame(table(union_all(eid2pair$parcarpair, gmpdpair$parcarpair)))) %>% 
  select(parcarpair=Var1, shared=Freq) %>%
  left_join(full_join(eid2pair, gmpdpair))
#__________________________________________________________________________________
  
