#############################
## Author:  Joy Vaz       ###
## Date:    11 Sep 2018   ###
## Project: Protraits     ###
#############################

library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)

# load datasets
zooscore <- read.csv("./Data/Zooscore_datafiles/Zooscore_trait_Protozoa.csv") # Rows are unique protozoa species (ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final),  is given a zooscore (see Coding Flowchart). Data (unpublished) from: Han lab, Cary Institute of Ecosystem Studies, recieved via email from Barbara Han on 8.6.2018
sppintrx <- read.csv("./Data/Wardeh_datafiles/SpeciesInteractions_EID2.csv") # Rows are unique host(Carrier)-parasite(Cargo) associations. Data from: Wardeh et al. 2015, downloaded from https://www.nature.com/articles/sdata201549 on 9.11.2018
gmpdprot <- read.csv("./Data/GMPD_datafiles/GMPD_main.csv") # Rows are unique observations of parasite(ParasiteCorrectedName) occurance in a host(HostCorrectedName) for wild primates, carnivores and ungulates. Data from: Stephens et al. 2017, downloaded from https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.1799 on 9.11.2018

# subset datasets to only include protozoa rows and relevant columns
zooscore %<>% select(parname=ï..ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, zscore=XC_ZooScore, cscore=XC_CScore, gmpdparname=ParasiteCorrectedName.updated, tm_close=close, tm_nonclose=nonclose, tm_vector=vector, tm_intermediate=intermediate, parphylum=ParPhylum, parclass=ParClass, parorder=ParOrder, parfamily=ParFamily)
sppintrx %<>% filter(Cargo.classification == "Protozoa") %<>% select(parname=Cargo, carname=Carrier, cartype=Carrier.classification, seqcount=Sequences.count, pubcount=Publications.count)
gmpdprot %<>% filter(ParType == "Protozoa", HasBinomialName == "yes") %<>% select(mamtype=Group, carname=HostCorrectedName, carorder=HostOrder, carfamily=HostFamily, carenv=HostEnvironment, gmpdparname=ParasiteCorrectedName, prev=Prevalence, numhosts=HostsSampled, numsamples=NumSamples)

# capitalise first letter of binomial names in sppintrx to match zooscore and gmpdprot
sppintrx$parname <- gsub("(^[a-z])", "\\U\\1", tolower(sppintrx$parname), perl = T)
sppintrx$carname <- gsub("(^[a-z])", "\\U\\1", tolower(sppintrx$carname), perl = T)

# gmpdparname does not match up with zooscore parname because two binomials have been updated in zooscore 
setdiff(zooscore$parname, gmpdprot$gmpdparname)

# create new parname variable in gmpd 
gmpdprot %<>% mutate(parname=gmpdparname)

# update binomials to match parname in zooscore
gmpdprot$parname <- gsub("Cystoisospora canis", "Isospora canis", gmpdprot$parname)
gmpdprot$parname <- gsub("Plasmodium malariae", "Plasmodium rodhaini", gmpdprot$parname)

# check the parname binomials now match
setdiff(zooscore$parname, gmpdprot$parname)

#there are 78 protozoa spp in gmpd that have not been scored in zooscore
tobescored <- setdiff(gmpdprot$parname, zooscore$parname)

# create tbl listing all unique protozoa spp from all datasets (n = 742)
parname <- unique(union(sppintrx$parname, gmpdprot$parname))
allprot <- as.tbl(as.data.frame(parname))

# create tbl of all gmpd protozoa-host pairs and combine with zooscore data
gmpdprotraits <- left_join(gmpdprot, zooscore, by = "parname")

# subset tbl of all gmpd protozoa-host pairs and combine with zooscore data
gmpdzooscored <- protcartraits %>% filter(zscore >= -1)

allprotraits <- left_join(allprot, gmpdprotraits)






protfreq <- as.data.frame(table(gmpdprot$parname))
protfreq <- as.tbl(protfreq)

protfre2 <- as.data.frame(table(protcartraits2$parname))
protfre2 <- as.tbl(protfre2)

protfre3 <- left_join(protfreq, protfre2, by = "Var1")


#protcartraits2 <- protcartraits %>% group_by(parname)
#protraits <- left_join(allprots, zooscore)



parrange <- protcartraits2 %>% summarise(n_distinct(carname), n_distinct(cartype), sum(seqcount), sum(pubcount))
parrange %<>% rename(carsppcount=`n_distinct(carname)`, cartypecount=`n_distinct(cartype)`, protseqcount=`sum(seqcount)`, protpubcount=`sum(pubcount)`) 
protraits <- left_join(protraits, parrange)




