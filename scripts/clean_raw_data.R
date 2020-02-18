# Author:  Joy Vaz       #
# Title:   Clean data    #
# Project: Protraits     #

### Attach packages


library(tidyverse) 
library(magrittr)


### Load data

# Select appropriate columns from protsdataentry (leave out refs, other_host, notes, etc.)

protsentry <- read_csv("data/original/protsentry.csv") %>% 
  rename(protname=ParasiteCorrectedName_Zooscores_VR_Ver5.0_Final, zscore=XC_ZooScore) %>% 
  select(-c(starts_with("ref"), starts_with("tax"), other_host, note,
            ParasiteCorrectedName.updated, ParasiteReportedName,
            ParPhylum, ParClass, ParOrder, ParFamily))


### Clean up variable columns --------------

## Fill in NAs for "NF"

protsentry[protsentry == "NF"] <- NA


## Turn "yes" and "no" into 0 and 1 respectively (intracellular, dom_host, flagella, cyst, sexual, etc.)

protsentry[protsentry == "yes"] <- 1
protsentry[protsentry == "no"] <- 0

## Create individual columns for the factor variables (site_system, geo_dist, dom_host_name, tramission, etc.) ------------

# Body Systems (14 vars)

protraits_site <- as.data.frame(protsentry %>% select(protname, site_system))

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
  }
}


# Geographic distribution (X variables - continents? subconinental divisions? column for specificity?)



# Domestic hosts (X variables - cattle, sheep, goats, dogs, cats, pigs, horses, poultry, other bovids?)



# Transmission modes (X vars - fecal-oral, sexual, environment, ingestion, cysts, ticks, waterborne, etc.)



# Columns to add up factor vars (num dom hosts, num sites, num continents, num tms etc.)


# Filter data based on specificity (genus vs. species)


### Save final df

# Select all clean columns, remove raw data entered, save a a final copy that can be merged with the main protraits df in data_wrangling.R


