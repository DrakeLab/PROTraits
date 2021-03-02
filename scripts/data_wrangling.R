# Author:  Joy Vaz       #
# Title:   Clean data    #
# Project: Protraits     #

### Attach packages

library(tidyverse) 
library(magrittr)


### Load data

## GMPD data # rows are records of host-par associations. Data from: Stephens et al. 2017, downloaded from https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.1799 on 2018.09.11
gmpdpars_all <- read.csv("./data/original/GMPD_datafiles/GMPD_main.csv")
length(unique(gmpdpars_all$ParasiteCorrectedName)) # 2412 unique pars

gmpdpars_binomialpars <- gmpdpars_all %>% 
  filter(HasBinomialName == "yes")
length(unique(gmpdpars_binomialpars$ParasiteCorrectedName)) # 2031 unique pars after filtering out pars with no binomial name

gmpdpars_binomialhostspars <- gmpdpars_all %>% 
  filter(HasBinomialName == "yes", !grepl("no binomial name", HostCorrectedName))
length(unique(gmpdpars_binomialhostspars$ParasiteCorrectedName)) # 1988 unique pars after filtering out hosts and pars with no binomial name


zooscore_all <- read.csv("./data/original/Zooscore_datafiles/ZooScore_GMPD_201906-201908.csv") %>% 
  rename(ParasiteCorrectedName=ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final)
length(unique(zooscore_all$ParasiteCorrectedName)) # 2022 unique pars

levels(zooscore_all$Non.GMPD)
table(zooscore_all$Non.GMPD) # 1047 pars are in GMPD, 31 pars are not, 945 pars have no value

zooscore_GMPD <- zooscore_all %>% 
  filter(!Non.GMPD == "1")
zooscore_GMPD <- zooscore_GMPD[-985, ] # remove row that has "Meningonema_peruzzii_transmission" in the Non.GMPD column
length(unique(zooscore_GMPD$ParasiteCorrectedName)) # 1991 unique pars

setdiff(gmpdpars_binomialpars$ParasiteCorrectedName, 
        zooscore_GMPD$ParasiteCorrectedName) # 547 pars are in GMPD but not zooscored (?)
setdiff(zooscore_GMPD$ParasiteCorrectedName,
        gmpdpars_binomialpars$ParasiteCorrectedName) # 538 pars are in GMPD but not zooscored (?)
intersect(gmpdpars_binomialpars$ParasiteCorrectedName, 
          zooscore_GMPD$ParasiteCorrectedName) #1482 zooscored out of 2031



#
gmpdprot <- read.csv("./data/original/GMPD_datafiles/GMPD_main.csv") %>% # rows are observations of parasite(ParasiteCorrectedName) occurance in a host(HostCorrectedName) for wild primates, carnivores and ungulates. Data from: Stephens et al. 2017, downloaded from https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.1799 on 2018.09.11
  filter(ParType == "Protozoa", HasBinomialName == "yes", !grepl("no binomial name", HostCorrectedName)) %>% 
  select(hosttype=Group, hostname=HostCorrectedName, hostorder=HostOrder, hostfamily=HostFamily, hostenv=HostEnvironment, 
         location=LocationName, lat=Latitude, long=Longitude, 
         gmpdprotname=ParasiteCorrectedName, 
         prev=Prevalence, 
         numhosts=HostsSampled, numsamples=NumSamples) %>% 
  mutate(ID = seq(len=2484))  # give unique ID to each GMPD record 

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

# 
prots226 <- rbind(prots178, prots049)

for (i in 1:length(prots226$protname)) {
  if(prots226$zscore[i] > 0){
    prots226$zoostat[i] <- 1
  } else {
    prots226$zoostat[i] <- 0
  }
}


### Clean data

## Names

# Check  for discrepencies in gmpdprotnames between the two dataframes 
setdiff(prots226$gmpdprotname, gmpdprot$gmpdprotname) # 1 spp in prots226 does is not listed gmpdprot - this is because its host does not have a binomial name and was filtered out above. 

# Remove T. brimonti from prots226
prots226 <- prots226 %>% filter(!grepl("Trypanosoma brimonti", protname))

# Check for discrepencies between final protnames and gmpdprotnames
setdiff(prots226$protname, gmpdprot$gmpdprotname) # the final protnames in prots226 contains 2 corrected protnames that have been updated from the original gmpdprotname

# Create protname variable in gmpd for final protnames
gmpdprot <- gmpdprot %>% mutate(protname = gmpdprotname)

# Update the 2 spp names to match zooscore
gmpdprot$protname <- gsub("Cystoisospora canis", "Isospora canis", gmpdprot$protname)
gmpdprot$protname <- gsub("Plasmodium malariae", "Plasmodium rodhaini", gmpdprot$protname)

# Verify that the protnames in both datasets are now matching
setdiff(prots226$protname, gmpdprot$protname)

## Completeness

# Save a list of the gmpdprot spp that are not in prots226
noscores <- as.data.frame(setdiff(gmpdprot$protname, prots226$protname)) %>% 
  rename(protname = `setdiff(gmpdprot$protname, prots226$protname)`)


### Create

# Add prots226 data to gmpdprot to create protraits
protraits <- left_join(prots226, gmpdprot, by = "protname") %>% 
  select(ID, protname, hostname, zoostat, zscore, cscore, 
         tm_close, tm_nonclose, tm_vector, tm_intermediate, 
         parphylum, parclass, parorder, parfamily,
         hosttype, hostorder, hostfamily, hostenv,
         lat, long, location, numhosts, numsamples, prev)

ung_protraits <- protraits %>% filter(hosttype == "ungulates") %>% distinct(protname, .keep_all = T)
table(ung_protraits$zoostat) %>% print() # 6/103 zoonotic, 97/103 non-zoonotic

car_protraits <- protraits %>% filter(hosttype == "carnivores") %>% distinct(protname, .keep_all = T)
table(car_protraits$zoostat) %>% print() # 3/54 zoonotic, 51/54 non-zoonotic

pri_protraits <- protraits %>% filter(hosttype == "primates") %>% distinct(protname, .keep_all = T)
table(pri_protraits$zoostat) %>% print() # 12/90 zoonotic, 78/90 non-zoonotic


# Check if protraits_zooscored has 226 prot species
length(unique(protraits$protname))

# create tbl listing all unique prot spp (n = 226)
allprots <- as.tbl(as.data.frame(unique(protraits$protname))) %>% 
  rename(protname = `unique(protraits$protname)`)

# create tbl listing all unique host spp (n = 245)
allhosts <- as.tbl(as.data.frame(unique(protraits$hostname))) %>% 
  rename(hostname = `unique(protraits$hostname)`)

# create tbl listing all unique hp pairs (n = 840)
allpairs <- protraits %>% select(hostname, protname) %>% distinct() %>% as.tbl() %>% 
  mutate(pairname = paste(protname, ", ", hostname))

ung_protraits <- protraits %>% filter(hosttype == "ungulates") %>% distinct(protname, .keep_all = T)
table(ung_protraits$zoostat) %>% print() # 6/103 zoonotic, 97/103 non-zoonotic

car_protraits <- protraits %>% filter(hosttype == "carnivores") %>% distinct(protname, .keep_all = T)
table(car_protraits$zoostat) %>% print() # 3/54 zoonotic, 51/54 non-zoonotic

pri_protraits <- protraits %>% filter(hosttype == "primates") %>% distinct(protname, .keep_all = T)
table(pri_protraits$zoostat) %>% print() # 12/90 zoonotic, 78/90 non-zoonotic


# Save as csvs
#write.csv(allprots, "./data/modified/allprots.csv")
#write.csv(allpairs, "./data/modified/allpairs.csv")
#write.csv(allhosts, "./data/modified/allhosts.csv")

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

hostpairtraits <- left_join(allpairs, hostpairtraits, by = "hostname")

prothosttraits <- hostpairtraits %>% group_by(protname) %>% summarise_if(is.numeric, mean, na.rm = TRUE)
