# Author:  Joy Vaz       #
# Title:   Clean data    #
# Project: Protraits     #

### Attach packages

library(tidyverse) 
library(magrittr)



# ALL GMPD PARS ------------
### Load data

## GMPD data # rows are records of host-par associations. Data from: Stephens et al. 2017, downloaded from https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.1799 on 2018.09.11
gmpdpars_all <- read.csv("./data/original/GMPD_datafiles/GMPD_main.csv") %>% 
  select(hosttype=Group, gmpdhostname=HostCorrectedName, hostorder=HostOrder, hostfamily=HostFamily, hostenv=HostEnvironment, 
         location=LocationName, lat=Latitude, long=Longitude, 
         gmpdparname=ParasiteCorrectedName, HasBinomialName, ParType)
length(unique(gmpdpars_all$gmpdparname)) # 2412 unique pars


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

# correct E. histolytica spelling

zooscore_all$gmpdparname <- gsub("Entamoemba histolytica", "Entamoeba histolytica", zooscore_all$gmpdparname)

# correct T. brucei zscore
zooscore_all %>% rownames_to_column() %>% filter(gmpdparname == "Trypanosoma brucei") # row number 1929
# replace Ascaris suum zscore with the correct one
zooscore_all$zscore[1929]  <- 3


#see if any zscores are missing
table(zooscore_all$zscore) %>% as.data.frame() %>% select(Freq) %>% sum() # sum is 1992 which means each row has a zscore

setdiff(gmpdpars_binomialhostspars$gmpdparname, 
        zooscore_all$gmpdparname) %>% sort() # 526
setdiff(zooscore_all$gmpdparname,
        gmpdpars_binomialhostspars$gmpdparname) # 520 (i guess the zooscores file had more pars from outside gmpd???)
intersect(gmpdpars_binomialhostspars$gmpdparname, 
          zooscore_all$gmpdparname) #1472 

# add zscores to gmpd
gmpd_zooscored <- left_join(gmpdpars_binomialhostspars, zooscore_all, by = "gmpdparname")

# see how many have zscores
table(gmpd_zooscored$zscore) %>% as.data.frame() %>% select(Freq) %>% sum() # 13677/19333 rows have a zscore

setdiff(gmpd_zooscored$gmpdparname, gmpdpars_binomialhostspars$gmpdparname)

#remove those without zscore
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

gmpd_zooscored <- completeFun(gmpd_zooscored, "zscore") # 13677 obs YAAAY :)

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


#

gmpdprot <- gmpd_zooscored %>% filter(ParType == "Protozoa") %>% rename(gmpdprotname = gmpdparname)

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
         parphylum=ParPhylum, parclass=ParClass, parorder=ParOrder, parfamily=ParFamily)

#write.csv(prots178, "./data/modified/prots178.csv")

# 
prots049 <- read.csv("./data/modified/48prots.csv") %>% # 49 additional protozoa that have zooscores but no tranmission mode traits recorded in GMPD_parasite_traits.csv, plus E. histolytica which got added to this list instead of the original 178.
  rename(protname=ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, 
         zscore=XC_ZooScore, cscore=XC_CScore) %>% 
  mutate(gmpdprotname=protname, tm_close=NA, tm_nonclose=NA, 
         tm_vector=NA, tm_intermediate=NA) %>% 
  left_join(gmpdtaxo)

#write.csv(prots049, "./data/modified/prots049.csv")

# join the two datasets
names(prots178) == names(prots049)

prots227 <- rbind(prots178, prots049)

# assign zoostat

table(prots227$zscore)

for (i in 1:length(prots227$protname)) {
  if(prots227$zscore[i] > 0) {
    prots227$zoostat[i] <- 1
  } else {
    prots227$zoostat[i] <- 0
  }
}

table(prots227$zoostat)

### Clean data

## Names

# Check  for discrepencies in gmpdprotnames between the two dataframes 
setdiff(prots227$gmpdprotname, gmpdprot$gmpdprotname) 

#' 3 spp in prots227 does is not listed gmpdprot: "Cystoisospora canis"   "Trypanosoma brimonti"  "Entamoeba histolytica"
#' Cystoisospora canis is missing from gmpd_zooscored for some reason,
#' Trypanosoma brimonti has one host with no binomial name and was thus excluded
#' Entamoeba histolytical has a typo in 
#  

# Remove T. brimonti from prots226
prots226 <- prots227 %>% filter(!grepl("Trypanosoma brimonti", protname))

# Check for discrepencies between final protnames and gmpdprotnames
setdiff(prots226$protname, gmpdprot$gmpdprotname) # the final protnames in prots226 contains 2 corrected protnames that have been updated from the original gmpdprotname

# Create protname variable in gmpd for final protnames
gmpdprot <- gmpdprot %>% mutate(protname = gmpdprotname)

# Update the 2 spp names to match zooscore
gmpdprot$protname <- gsub("Cystoisospora canis", "Isospora canis", gmpdprot$protname)
gmpdprot$protname <- gsub("Plasmodium malariae", "Plasmodium rodhaini", gmpdprot$protname)

# Verify that the protnames in both datasets are now matching
setdiff(prots226$protname, gmpdprot$protname)

# write.csv(prots226, "./data/modified/prots226.csv")

## Completeness

# Save a list of the 29 gmpdprot spp that are not in prots226
noscores <- as.data.frame(setdiff(gmpdprot$protname, prots226$protname)) %>% 
  rename(protname = `setdiff(gmpdprot$protname, prots226$protname)`)


### Create

# Add prots226 data to gmpdprot to create protraits
protraits <- left_join(prots226, gmpdprot, by = "protname") %>% 
  select(ID, protname, hostname, zoostat, zscore, cscore, 
         tm_close, tm_nonclose, tm_vector, tm_intermediate, 
         parphylum, parclass, parorder, parfamily,
         hosttype, hostorder, hostfamily, hostenv,
         lat, long, location)




# Check if protraits_zooscored has 226 prot species
length(unique(protraits$protname))

# create tbl listing all unique prot spp (n = 226)
allprots <- protraits %>% distinct(protname)

# create tbl listing all unique prot host spp (n = 245)
allprothosts <- protraits %>% distinct(hostname)

# create tbl listing all unique host-prot pairs (n = 840)
allprotpairs <- protraits %>% select(hostname, protname) %>% distinct() %>% as.tbl() %>% 
  mutate(pairname = paste(protname, ", ", hostname))

# Split my mammal order

# Ungulates (n = 103)
ung_protraits <- protraits %>% filter(hosttype == "ungulates") %>% distinct(protname, .keep_all = T)
table(ung_protraits$zoostat) # 6/103 zoonotic, 97/103 non-zoonotic

# Carnivores (n = 54)
car_protraits <- protraits %>% filter(hosttype == "carnivores") %>% distinct(protname, .keep_all = T)
table(car_protraits$zoostat) # 3/54 zoonotic, 51/54 non-zoonotic

# Primates (n = 90)
pri_protraits <- protraits %>% filter(hosttype == "primates") %>% distinct(protname, .keep_all = T)
table(pri_protraits$zoostat) # 12/90 zoonotic, 78/90 non-zoonotic

# check if we got the right number
sum(nrow(ung_protraits), nrow(car_protraits), nrow(pri_protraits)) == protraits %>% select(protname, hosttype) %>% distinct() %>% nrow()

# Save as csvs
# write.csv(allprots, "./data/modified/allprots.csv")
# write.csv(allprotpairs, "./data/modified/allprotpairs.csv")
# write.csv(allprothosts, "./data/modified/allprothosts.csv")

## Commented the following section out bc manually entered raw data (protsentry) has been processed in clean_raw_data.R

# ## Manually entered data
# 
# rawprots <- read.csv("./data/original/protsentry.csv") %>% 
#   select(protname = ï..ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, Type,
#          intra = intra_extra, bodysystem = site_system, continent = geo_dist,
#          domestic = dom_host, domestic_name = dom_hostname, flagella, sexual)
# 
# View(rawprots)
# 
# bodysystems <- c("muscular", "skeletal", "circulatory", "respiratory", "digestive", "immune", "urinary", 
#                  "nervous", "endocrine", "reproductive", "lymphatic", "integumentary", "ocular")
# 
# recode(rawprots$domestic, "yes" = 1, "no" = 0)
# recode(rawprots$flagella, "yes" = 1, "no" = 0)
# recode(rawprots$sexual, "yes" = 1, "both" = 1, "no" = 0)
# 
# library(BRRR)
# skrrrahh("flava")

## Mammal traits

allFinalWOS <- allFinalWOS %>% rownames_to_column()

allFinalWOS <- allFinalWOS %>% rename(hostname = rowname)

hostpairtraits <- allFinalWOS

hostpairtraits <- left_join(allprotpairs, hostpairtraits, by = "hostname")

prothosttraits <- hostpairtraits %>% group_by(protname) %>% summarise_if(is.numeric, mean, na.rm = TRUE)
