# Author:  Joy Vaz       #
# Title:   Clean data    #
# Project: Protraits     #

### Attach packages


library(tidyverse)
library(magrittr)
library(reshape2)
library(corrplot)

rm(list = ls())

### Load data

# Select appropriate columns from protsdataentry (leave out refs, other_host, notes, etc.)

protsentry <- read_csv("data/original/protsentry.csv") %>% 
  rename(parname=ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, zscore=XC_ZooScore) %>% 
  select(-c(starts_with("ref"), starts_with("tax"), other_host, note,
            ParasiteCorrectedName.updated, ParasiteReportedName,
            ParPhylum, ParClass, ParOrder, ParFamily))

protnames <- read.csv("./data/modified/protnames.csv")[-1]

# check to see if they match up
intersect(protnames$parname, protsentry$parname) # 216 - uh oh
setdiff(protnames$parname, protsentry$parname) # 10  
setdiff(protsentry$parname, protnames$parname) # 11 

# replace Isospora with Cystoisospora
protsentry$parname <- gsub("Isospora canis", "Cystoisospora canis", protsentry$parname) 
protsentry$parname <- gsub("Babesia equi", "Theileria equi", protsentry$parname) 

# Trypanosoma brimonti has one host with no binomial name and was thus excluded
# Remove T. brimonti from prots229
protsentry <- protsentry %>% filter(!grepl("Trypanosoma brimonti", parname))

# check again

intersect(protnames$parname, protsentry$parname) #218 - that's better
setdiff(protnames$parname, protsentry$parname) # 8 - "Cyclospora cayetanensis" "Cystoisospora belli"
setdiff(protsentry$parname, protnames$parname) # the Hepatozoon JM variants, which we don't want

# This area commented out bc resulting dfs have been saved as csvs, don't need to run all over agian ---

### Clean up variable columns --------------

## Fill in NAs for "NF"

protsentry[protsentry == "NF"] <- NA


## Turn "yes" and "no" into 0 and 1 respectively (intracellular, dom_host, flagella, cyst, sexual, etc.)

protsentry[protsentry == "yes"] <- 1
protsentry[protsentry == "both"] <- 1
protsentry[protsentry == "no"] <- 0

# ## Create individual columns for the factor variables (site_system, geo_dist, dom_host_name, tramission, etc.) ------------
# 
# Body Systems (14 vars)

protraits_site <- as.data.frame(protsentry %>% select(parname, site_system))

sys_names <- c(
  "muscular", 
  "skeletal", 
  "circulatory", 
  "respiratory", 
  "digestive", 
  "immune", 
  "urinary", 
  "nervous", 
  "endocrine", 
  "reproductive", 
  "lymphatic", 
  "integumentary", 
  "ocular", 
  "multi" 
)

protraits_site[, 3:16] <- 0
colnames(protraits_site)[3:16] <- sys_names


for (x in 1:length(sys_names)) {
  for (i in 1:nrow(protraits_site)) {
    protraits_site[, sys_names[x]][i][grep(sys_names[x], protraits_site$site_system[i])] <- 1
    if(is.na(protraits_site$site_system[i])){
      protraits_site[i, 3:16] <- NA
    }
  }
}

protraits_site$numsys  <- rowSums(protraits_site[, 3:15])

# 
# write_csv(protraits_site, "data/modified/protraits_site.csv")
# 
# # Geographic distribution (X variables - continents? subconinental divisions? column for specificity?)
# 
# # There are only 60 lines that have this entered, so I am not including it as a variable right now.
# # Many of them are also at the genus level, so I my confidence in them is low. 
# # Instead of this, can use the GMPD lat longs to count how many ADMs at different levels? Or just continents? IDK.
# # Ask Barbara about this?
# 
# Domestic hosts (X variables - cattle, sheep, goats, dogs, cats, pigs, horses, poultry, other bovids?)

dom_host_names <- c('cattle', 'sheep', 'goat', 'dog', 'cat', 'pig', 'horse', 'chicken')

protraits_dom_host_names <- as.data.frame(protsentry %>% select(parname, dom_hostname))

protraits_dom_host_names[, 3:10] <- 0
colnames(protraits_dom_host_names)[3:10] <- dom_host_names

for (x in 1:length(dom_host_names)) {
  for (i in 1:nrow(protraits_dom_host_names)) {
    protraits_dom_host_names[, dom_host_names[x]][i][grep(dom_host_names[x], protraits_dom_host_names$dom_hostname[i])] <- 1
    if(is.na(protraits_dom_host_names$dom_hostname[i])){
      protraits_dom_host_names[i, 3:10] <- NA
    }
  }
}

protraits_dom_host_names$numdomhosts  <- rowSums(protraits_dom_host_names[, 3:10])

# 

# write_csv(protraits_dom_host_names, "data/modified/protraits_domhosts.csv")

# Later? ---

# Transmission modes (X vars - fecal-oral, sexual, environment, ingestion, cysts, ticks, waterborne, etc.)
# Filter data based on specificity (genus vs. species) - LATER


# Select all clean columns, remove raw data entered, save a a final copy that can be merged with the main protraits df in data_wrangling.R

protraits_sitesys <- read_csv("data/modified/protraits_site.csv") %>% 
  select(parname, numsys)

protraits_domhosts <- read_csv("data/modified/protraits_domhosts.csv") %>% 
  select(parname, numdomhosts)

joytraits_01 <- protsentry %>% 
  select(c(parname, type = Type, intra_extra, dom_host, flagella, cyst, sexual)) # 7 vars

joytraits_02 <- left_join(joytraits_01, protraits_domhosts) # 8 vars
joytraits_03 <- left_join(joytraits_02, protraits_sitesys) # 9 vars

# Check for completeness
joytraits_03 %>% summarise_all(function(x) mean(!is.na(x))) %>% as.numeric() %>% plot(type = 'h')
joytraits_03 %>% summarise_all(function(x) mean(!is.na(x))) %>% min() # Good enough!

# remove vars under 40% coverage

protsentry_clean <- joytraits_03 %>% select(-numdomhosts) %>% 
  rename(ProtType = type, Intracellular = intra_extra, HasDomesticHost = dom_host,
         Flagella = flagella, Cyst = cyst, SexualReproduction = sexual, NumOrganSystems = numsys)

protsentry_clean$Intracellular <- as.numeric(protsentry_clean$Intracellular)
protsentry_clean$HasDomesticHost <- as.numeric(protsentry_clean$HasDomesticHost)
protsentry_clean$Flagella <- as.numeric(protsentry_clean$Flagella)
protsentry_clean$Cyst <- as.numeric(protsentry_clean$Cyst)
protsentry_clean$SexualReproduction <- as.numeric(protsentry_clean$SexualReproduction)

# Correlation analysis

# Create correlation matrix
data <- select_if(protsentry_clean, is.numeric) %>% select(-c(Cyst, Flagella))

correlationMatrix <- cor(data, use = "pairwise.complete.obs")

#Plot

corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 0.75, number.cex = 1, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 3)
# hmmm

correlation.df <- correlationMatrix %>% as.data.frame() %>% mutate(rowID = rownames(correlationMatrix))

corrPairs <- melt(correlation.df) %>% rename(feature1 = rowID, feature2 = variable, PCC = value)
corrPairs <- corrPairs[!duplicated(data.frame(t(apply(corrPairs[, 1:2],1,sort)))),]

highlyCorrelated <- filter(corrPairs, PCC > 0.7 | PCC < (-0.7))

otherprotraits <- protsentry_clean %>% select(-c(Cyst, Flagella))

### Save final df

# write.csv(otherprotraits, "data/modified/protraits/otherprotraits.csv")





