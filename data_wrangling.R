#############################
## Author:  Joy Vaz       ###
## Date:    11 Sep 2018   ###
## Project: Protraits     ###
#############################

library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)

#load data
zooscore <- read.csv("./Data/Zooscore_datafiles/Zooscore_trait_Protozoa.csv") # Rows are unique protozoa species (ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final),  is given a zooscore (see Coding Flowchart). Data (unpublished) from: Han lab, Cary Institute of Ecosystem Studies, recieved via email from Barbara Han on 8.6.2018
sppintrx <- read.csv("./Data/Wardeh_datafiles/SpeciesInteractions_EID2.csv") # Rows are unique host(Carrier)-parasite(Cargo) associations. Data from: Wardeh et al. 2015, downloaded from https://www.nature.com/articles/sdata201549 on 9.11.2018
gmpdprot <- read.csv("./Data/GMPD_datafiles/GMPD_main.csv") # Rows are unique observations of parasite(ParasiteCorrectedName) occurance in a host(HostCorrectedName) for wild primates, carnivores and ungulates. Data from: Stephens et al. 2017, downloaded from https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.1799 on 9.11.2018

#subset datasets to only include protozoa rows and relevant columns
zooscore %<>% select(parname=ï..ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, zscore=XC_ZooScore, cscore=XC_CScore, gmpdparname=ParasiteCorrectedName.updated, tm_close=close, tm_nonclose=nonclose, tm_vector=vector, tm_intermediate=intermediate, parphylum=ParPhylum, parclass=ParClass, parorder=ParOrder, parfamily=ParFamily)
sppintrx %<>% filter(Cargo.classification == "Protozoa") %<>% select(parname=Cargo, carname=Carrier, cartype=Carrier.classification, seqcount=Sequences.count, pubcount=Publications.count)
gmpdprot %<>% filter(ParType == "Protozoa", HasBinomialName == "yes") %<>% select(mamtype=Group, carname=HostCorrectedName, carorder=HostOrder, carfamily=HostFamily, carenv=HostEnvironment, gmpdparname=ParasiteCorrectedName, prev=Prevalence, numhosts=HostsSampled, numsamples=NumSamples)

#capitalise first letter of binomial names in sppintrx to match zooscore and gmpdprot
sppintrx$parname <- gsub("(^[a-z])", "\\U\\1", tolower(sppintrx$parname), perl = T)
sppintrx$carname <- gsub("(^[a-z])", "\\U\\1", tolower(sppintrx$carname), perl = T)

#remname the two corrected protozoan names in gmpd to match parname in zooscore
gmpdprot %<>% mutate(parname = gsub("Cystoisospora canis", "Isospora canis", gmpdprot$gmpdparname))
#"Plasmodium malariae", "Plasmodium rodhaini"


#compile list of all unique protozoan spp
allprots <- as.data.frame(zooscore$parname)
allprots <- as.tbl(allprots)



#allprots <- as.data.frame(unique(union(sppintrx$parname, zooscore$parname)))
#allprots %<>% rename(parname=`unique(union(sppintrx$parname, zooscore$parname))`) 

protcartraits <- left_join(allprots, sppintrx)
protcartraits <- left_join(protcartraits, zooscore)
protcartraits2 <- protcartraits %>% group_by(parname)
protraits <- left_join(allprots, zooscore)

parrange <- protcartraits2 %>% summarise(n_distinct(carname), n_distinct(cartype), sum(seqcount), sum(pubcount))
parrange %<>% rename(carsppcount=`n_distinct(carname)`, cartypecount=`n_distinct(cartype)`, protseqcount=`sum(seqcount)`, protpubcount=`sum(pubcount)`) 
protraits <- left_join(protraits, parrange)




