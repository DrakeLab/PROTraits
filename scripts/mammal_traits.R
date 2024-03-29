## MAMMAL TRAITS

library(tidyverse)
library(magrittr)
library(reshape2)
library(corrplot)

rm(list = ls())

# my host species
allhosts <- read.csv("./data/modified/allhosts.csv")[-1] %>% rename(hostname = gmpdhostname)

# PanTHERIA -----------
pantheria <- read.delim("./data/original/PanTHERIA/PanTHERIA_1-0_WR05_Aug2008.txt") %>% 
  select(hostname = MSW05_Binomial, Order =	MSW05_Order, Family =	MSW05_Family, 
         ActivityCycle =	X1.1_ActivityCycle, 
         AdultBodyMass_g =	X5.5_AdultBodyMass_g_EXT, 
         AdultHeadBodyLen_mm =	X13.1_AdultHeadBodyLen_mm, 
         DietBreadth =	X6.1_DietBreadth, 
         GestationLen =	X9.1_GestationLen_d, 
         HabitatBreadth =	X12.1_HabitatBreadth, 
         HomeRange =	X22.1_HomeRange_km2, 
         InterbirthInterval =	X14.1_InterbirthInterval_d, 
         LitterSize =	X15.1_LitterSize, 
         LittersPerYear_wEXT =	X16.2_LittersPerYear_EXT, 
         MaxLongevity =	X17.1_MaxLongevity_m, 
         NeonateBodyMass_wEXT =	X5.6_NeonateBodyMass_g_EXT, 
         PopulationDensity =	X21.1_PopulationDensity_n.km2, 
         SexualMaturityAge =	X23.1_SexualMaturityAge_d, 
         Terrestriality =	X12.2_Terrestriality, 
         TrophicLevel =	X6.2_TrophicLevel, 
         WeaningAge =	X25.1_WeaningAge_d,
         AgeatEyeOpening_d = X2.1_AgeatEyeOpening_d,
         AgeatFirstBirth_d = X3.1_AgeatFirstBirth_d,
         BasalMetRate_mLO2hr = X18.1_BasalMetRate_mLO2hr,
         BasalMetRateMass_g = X5.2_BasalMetRateMass_g,
         DispersalAge_d = X7.1_DispersalAge_d,
         HomeRange_Indiv_km2 = X22.2_HomeRange_Indiv_km2,
         SocialGrpSize = X10.2_SocialGrpSize, 
         PopulationGrpSize = X10.1_PopulationGrpSize,
         HuPopDen_Mean_n.km2 =X27.2_HuPopDen_Mean_n.km2,
         HuPopDen_Change = X27.4_HuPopDen_Change,
         Precip_Mean_mm = X28.1_Precip_Mean_mm,
         Temp_Mean_01degC = X28.2_Temp_Mean_01degC)

pantheria[pantheria == -999] <- NA

pantheria %>% summarise_all(function(x) mean(!is.na(x))) ## hmmmmmm

intersect(pantheria$hostname, allhosts$hostname) # 383
setdiff(allhosts$hostname, pantheria$hostname) # 0!!! (all my hosts are in panteria)

allhostsPantraits <- left_join(allhosts, pantheria, by = "hostname")
completenessPantraits <- allhostsPantraits %>% summarise_all(function(x) mean(! is.na(x))) %>% transpose()
# completenessPantraits_df <- completenessPantraits[[1]] %>% unlist() %>% as.data.frame() %>% rownames_to_column()

## Some of them are SUPER INCOMPLETE! compare to Dallas data and choose which ones give more complete data



# Dallas et al. 2018 -------

#' "Trait-based prediction of host species roles in parasite sharing networks". 
#' Mammal trait data from figshare. Dataset. https://doi.org/10.6084/m9.figshare.5129980.v2 

# the .Rdata file
load("./data/original/Dallas_2018/dataForGBM.RData")

# Finaltraits

Dallas2018Finaltraits <- allFinalWOS %>% rownames_to_column() %>% rename(hostname = rowname)

Dallas2018Finaltraits$AdultBodyMass_wEXT[is.nan(Dallas2018Finaltraits$AdultBodyMass_wEXT)] <- NA

intersect(Dallas2018Finaltraits$hostname, allhosts$hostname) # 295 - 295/383 is not great...
setdiff(Dallas2018Finaltraits$hostname, allhosts$hostname) # 44
setdiff(allhosts$hostname, Dallas2018Finaltraits$hostname) # 88 mimatches - some of these are domestic hosts (Dromedary camels, Sheep), with Dallas et al. removed. Others idk.
 

allhostsDallas2018Finaltraits <- left_join(allhosts, Dallas2018Finaltraits)
completenessDallas2018Finaltraits <- allhostsDallas2018Finaltraits %>% summarise_all(function(x) mean(! is.na(x))) %>% transpose()

# GMPD traits 

Dallas2018GMPDtraits <- read.csv("./data/original/Dallas_2018/GMPD_traits.csv") %>% 
  rename(hostname = binomial) 
Dallas2018GMPDtraits$hostname <- gsub("_", " ", Dallas2018GMPDtraits$hostname)

intersect(Dallas2018GMPDtraits$hostname, allhosts$hostname) # 350 - close!
setdiff(allhosts$hostname, Dallas2018GMPDtraits$hostname) # 33 missing/mismatching -- they are all primates??? 
setdiff(Dallas2018Finaltraits$hostname, Dallas2018GMPDtraits$hostname) %>% sort() # 30 Similar list. It seems the GMPD one is missing a bunch of primates

## looks promising... see what it is like after merge with allhosts

#' NOTES:
#' Orders: 1 = Artiodactyla (even-toed ungulates), 2 = Carnivora, 3 = Perissodactyla (Odd-toed ungulates), 4 = Primates
#' There are bunch of NAs and I think all of them are Primates?
#' This has only 295 of my 383 species. Most of their traits are from PanTHERIA. 
#' PanTHERIA has all my 383 hosts - see above. But completeness is really low.
#' So I'm going to add add all variables I want from all three datasets
#' The variables from Dallas datasets that are not from PanTHERIA:
#' Network measures (degree, closeness, betweenness, and eigenvector centrality)
#' Brain weight (GMPD one has two versions idk why), 
#' the fair proportion measure of evolutionary distinctiveness,
#' GR_Area_combined (is this geographic range?), and wos


allhostsDallas2018GMPDtraits <- left_join(allhosts, Dallas2018GMPDtraits)
completenessDallas2018GMPDtraits <- allhostsDallas2018GMPDtraits %>% summarise_all(function(x) mean(! is.na(x))) # so-so. Brain weight is low.

# compare the completenesses across the three trait datasets and select the best variables from each. use a cutoff of 75%
# see LNB for how these decisions were made

completenessPantraits
completenessDallas2018Finaltraits
completenessDallas2018GMPDtraits

FinalPantraits <- allhostsPantraits %>% select(hostname, 
                                               ActivityCycle, 
                                               GestationLen, 
                                               HabitatBreadth, 
                                               InterbirthInterval, 
                                               LitterSize,
                                               PopulationDensity, 
                                               Terrestriality, 
                                               WeaningAge, 
                                               SocialGrpSize, 
                                               HuPopDen_Mean_n.km2, 
                                               HuPopDen_Change, 
                                               Precip_Mean_mm, 
                                               Temp_Mean_01degC)

FinalDallas2018traits <- allhostsDallas2018Finaltraits %>% select(hostname, 
                                                                  degree, 
                                                                  close, 
                                                                  between, 
                                                                  eigen, 
                                                                  fairProp, 
                                                                  wos)

FinalDallas2018GMPDtraits <- Dallas2018GMPDtraits %>% select(hostname, 
                                                             AdultBodyMass_wEXT, 
                                                             AdultHeadBodyLen, 
                                                             DietBreadth, 
                                                             HomeRange, 
                                                             NeonateBodyMass_wEXT, 
                                                             LittersPerYear_wEXT, 
                                                             MaxLongevity, 
                                                             SexualMaturityAge, 
                                                             TrophicLevel, 
                                                             GR_Area_Combined_IUCN_preferred)

# PHYLACINE -----------

phylacine <- read.csv("./data/original/PHYLACINE_datafiles/Trait_data.csv") %>% 
  select(hostname = Binomial.1.2, 
         # Terrestrial, Marine, Freshwater, Aerial, 
         Mass.g, Island.Endemicity, IUCN.Status = IUCN.Status.1.2,
         Diet.Plant, Diet.Vertebrate, Diet.Invertebrate) 


phylacine$hostname <- gsub("_", " ", phylacine$hostname)

intersect(allhosts$hostname, phylacine$hostname) # 358 ... not bad, only missing 25
setdiff(allhosts$hostname, phylacine$hostname) # 25 - MOST MISMATCHES ARE SYNONYMNS - COME FIX LATER. The GMPD really needs an upgrade...

allhostsPHYLAtraits <- left_join(allhosts, phylacine)
completenessallhostsPHYLAtraits <- allhostsPHYLAtraits %>% summarise_all(function(x) mean(! is.na(x))) # Very good!

FinalPHYLAtraits <- allhostsPHYLAtraits


# FINAL HOST TRAITS ---------

# JOIN EVERYTHING

Finalhosttraits <- allhosts %>% left_join(FinalDallas2018traits)

Finalhosttraits <- left_join(Finalhosttraits, FinalDallas2018GMPDtraits)

Finalhosttraits <- left_join(Finalhosttraits, FinalPantraits)

Finalhosttraits <- left_join(Finalhosttraits, FinalPHYLAtraits)

completenessFinalhosttraits <- Finalhosttraits %>% summarise_all(function(x) mean(!is.na(x))) %>% transpose()
completenessFinalhosttraits_df <- completenessFinalhosttraits[[1]] %>% unlist() %>% as.data.frame() %>% rownames_to_column()

# HomeRange and SocialGrpSize are low but the rest are pretty good!

# SAVE THIS MOFO!!!!!!

# write.csv(Finalhosttraits, "./data/modified/allhosttraits.csv")



# now just for prots ----------------

rm(list = ls())

allhosttraits <- read.csv("./data/modified/allhosttraits.csv") 

hosttraits <- allhosttraits %>% 
  select(hostname, HostDietBreadth = DietBreadth,
         HostDietInvertebrate = Diet.Invertebrate,
         HostHabitatBreadth = HabitatBreadth,
         HostGeographicRange = GR_Area_Combined_IUCN_preferred, 
         HostInterbirthInterval = InterbirthInterval,
         HostTrophicLevel = TrophicLevel, 
         MeanHumanPopDensity = HuPopDen_Mean_n.km2, 
         MeanTemp = Temp_Mean_01degC, 
         MeanPrecipitation = Precip_Mean_mm,
         # HostSocialGrpSize = SocialGrpSize, 
         HostIslandEndemicity = Island.Endemicity,
         HostPopDensity = PopulationDensity,
         HostEvolutionaryDistinctiveness = fairProp)

prothosts <- read.csv("./data/modified/gmpd_zooscored_prot.csv") %>% 
  select(hostname) %>% distinct()
protpairs<- read.csv("./data/modified/gmpd_zooscored_prot.csv") %>% 
  select(parname, hostname) %>% distinct()

prothosttraits <- left_join(prothosts, hosttraits)

# Change host trait vars of the factor class into numeric

select_if(prothosttraits, is.numeric) %>% names()
select_if(prothosttraits, is.factor) %>% names() # "HostIslandEndemicity" 

#Island
prothosttraits$HostIslandEndemicity %>% levels()
prothosttraits$HostIslandEndemicity %>% table()
levels(prothosttraits$HostIslandEndemicity) <- c(0, 1, 0, 2)
prothosttraits$HostIslandEndemicity %>% table()

prothosttraits$HostIslandEndemicity <- prothosttraits$HostIslandEndemicity %>% as.numeric()

# check if all numeric
select_if(prothosttraits, is.numeric) %>% names() # 12 are numeric - all except for protname, which is good

# Correlation analysis

# Create correlation matrix

data <- select_if(prothosttraits, is.numeric)

correlationMatrix <- cor(data, use = "pairwise.complete.obs")
correlation.df <- correlationMatrix %>% as.data.frame() %>% mutate(rowID = rownames(correlationMatrix))

corrPairs <- melt(correlation.df) %>% rename(feature1 = rowID, feature2 = variable, PCC = value)
corrPairs <- corrPairs[!duplicated(data.frame(t(apply(corrPairs[, 1:2],1,sort)))),]

highlyCorrelated <- filter(corrPairs, PCC > 0.7 | PCC < (-0.7))

#Plot

corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 0.75, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 3)
# cool, keep them all for next step - aggregate to the prot spp level

hostprotpairtraits <- left_join(protpairs, prothosttraits)

# aggregate by mean and median
# mean
prothosttraits_agg <- hostprotpairtraits %>% group_by(parname) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) # gives NaNs

# replace NaNs

is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))
}

prothosttraits_agg[is.nan.data.frame(prothosttraits_agg)] <- NA

# median
prothosttraits_agg2 <- hostprotpairtraits %>% group_by(parname) %>% 
  summarise_if(is.numeric, median, na.rm = TRUE)

# compare
plot(prothosttraits_agg$HostHomeRange, prothosttraits_agg2$HostHomeRange)
plot(prothosttraits_agg$HostGeographicRange, prothosttraits_agg2$HostGeographicRange)
plot(prothosttraits_agg$MeanHumanPopDensity, prothosttraits_agg2$MeanHumanPopDensity)
plot(prothosttraits_agg$HostInterbirthInterval, prothosttraits_agg2$HostInterbirthInterval)

#' from these plots, can see that means are influenced by extreme values, so often higher than medians,
#' but i think it's close enough to just use mean

#Check for completeness
completenessprothosttraits_agg  <- prothosttraits_agg %>% as.data.frame() %>% 
  summarise_all(function(x) mean(!is.na(x))) %>% transpose()
completenessprothosttraits_agg_df <- completenessprothosttraits_agg[[1]] %>% unlist() %>% 
  as.data.frame() %>% rownames_to_column()

prothosttraits_agg %>% summarise_all(function(x) mean(!is.na(x))) %>% as.numeric() %>% plot(type = 'h')
prothosttraits_agg %>% summarise_all(function(x) mean(!is.na(x))) %>% min() # Good enough!

# Correlation analysis

# Create correlation matrix

data <- select_if(prothosttraits_agg, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
#Plot
corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 0.75, number.cex = 0.7, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 3)
# cool, keep them all


# Add host community vars -------
hostcommtraits <- read.csv("./data/modified/hostcomm_allpars.csv")[-1] %>% 
  select(hostname, HostNumPars = numpars, HostPropParZoonootic = propparzoon)

setdiff(protpairs$hostname, hostcommtraits$hostname) # no diff!

prothostcommtraits <- left_join(protpairs, hostcommtraits)

prothostcommtraits_agg1 <- prothostcommtraits %>% group_by(parname) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)
prothostcommtraits_agg2 <- prothostcommtraits %>% group_by(parname) %>% 
  summarise_if(is.numeric, median, na.rm = TRUE)

plot(prothostcommtraits_agg1$HostNumPars, prothostcommtraits_agg2$HostNumPars)
plot(prothostcommtraits_agg1$HostPropParZoonootic, prothostcommtraits_agg2$HostPropParZoonootic)
# gonna stick with median as it avoids the effect of extremes

prothostcommtraits_agg <- prothostcommtraits_agg2

# Correlation analysis

# Create correlation matrix
data <- select_if(prothostcommtraits_agg, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
#Plot
corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 0.75, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 3)
# cool, keep both

prothosttraits2 <- left_join(prothosttraits_agg, prothostcommtraits_agg)

# Full correlation analysis

# Create correlation matrix
data <- select_if(prothosttraits2, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
#Plot
corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 0.75, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 2)

# remove numpars

hostprotraits <- prothosttraits2 %>% select(-HostNumPars)


# Save CSV

write.csv(hostprotraits, "./data/modified/protraits/hostprotraits.csv")



