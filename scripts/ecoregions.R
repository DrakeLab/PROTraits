
library(sf)
library(tidyverse) 
library(magrittr)
library(tictoc)
library(BRRR)
library(corrplot)
library(here)


# rm(list = ls())

# assign a ecoregion data  -------
gmpd_zooscored_prot <- read.csv("./data/modified/gmpd_zooscored_prot.csv")


host_par_points_df <- gmpd_zooscored_prot %>%
  select(ID = X, parname, location, long, lat) %>% 
  na.omit() # extract lat longs and remove empty rows

protnames <- read.csv("./data/modified/protnames.csv")

nrow(gmpd_zooscored_prot) - nrow(host_par_points_df) # 88 obs don't have lat/longs
length(unique(gmpd_zooscored_prot$parname)) - length(unique(host_par_points_df$parname))  # lost 2 prots
setdiff(protnames$parname, host_par_points_df$parname)

# convert to sf
host_par_points_sf <- host_par_points_df %>% st_as_sf(coords = c("long","lat"), crs=4326) 

# load wwf terrestrial ecoregion data
teow_sf <- st_read("./data/original/WWF_ecoregions_datafiles/wwf_terr_ecos.shp") # downloaded from https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world on 

# replace NAs in realm with "RL" for "Rock and ice or Lake area" (see metadata)
teow_sf$REALM <- factor(teow_sf$REALM, levels = c(levels(teow_sf$REALM), "RL"))
teow_sf <- teow_sf %>% dplyr::mutate(REALM = replace_na(REALM, "RL"))

# create df with each point and corresponding TEOW vars
tic()
teowprot <- st_intersection(teow_sf, host_par_points_sf) %>% 
  as.data.frame()
toc()
skrrrahh("soulja")

ecoprotraits_tmp <- left_join(host_par_points_df, 
                              teowprot %>% select(ID, 
                                                  ecoregion = ECO_NUM, 
                                                  realm = REALM, 
                                                  biome = BIOME, 
                                                  eco_area = AREA), 
                              by = "ID")

unique(ecoprotraits_tmp$parname)



ecoprotraits_grp <- ecoprotraits_tmp[-1] %>% group_by(parname) 

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

ecoprotraits_agg <- summarise(ecoprotraits_grp, 
                              n_realms = n_distinct(realm),
                              n_ecoregions = n_distinct(ecoregion), 
                              n_biomes = n_distinct(biome),
                              eco_range = sum(eco_area), 
                              main_biome = getmode(biome),
                              main_realm = getmode(realm)) 

table(ecoprotraits_agg$main_realm)

ecoprotraits_agg$main_realm <- factor(ecoprotraits_agg$main_realm, levels = c("AT", "IM", "NA", "NT", "PA"))


# Create and plot correlation matrix
data <- select_if(ecoprotraits_agg, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 0.75, number.cex = 2, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 1)


# correlation.df <- correlationMatrix %>% as.data.frame() %>% mutate(rowID = rownames(correlationMatrix))
# corrPairs <- melt(correlation.df) %>% rename(feature1 = rowID, feature2 = variable, PCC = value)
# corrPairs <- corrPairs[!duplicated(data.frame(t(apply(corrPairs[, 1:2],1,sort)))),]
# 
# highlyCorrelated <- filter(corrPairs, PCC > 0.7 | PCC < (-0.7))



ecoprotraits <- left_join(protnames, ecoprotraits_agg) %>% select(parname, main_realm, n_ecoregions)


# Save csv
# write.csv(ecoprotraits, "./data/modified/protraits/ecoprotraits.csv")


# create df of the GMPD prot records that did NOT overlap with TEOW polygons
noteows <- anti_join(gmpd_zooscored_prot, teowprot, by = "parname") 
# 5 records did not match, 4 of them don't have lat longs
noteows %>% na.omit()
#' Hepatocystis taiwanensis has lat/long coordinates but still did not overlap with TEOW polygons 
#' because the point falls in the ocean off the coast of Taiwan.

# plot for fun

# plot global distribution of GMPD protozoa records across TEOW biomes ------

biome_names <- c("Tropical & Subtropical Moist Broadleaf Forests", 
                 "Tropical & Subtropical Dry Broadleaf Forests", 
                 "Tropical & Subtropical Coniferous Forests", 
                 "Temperate Broadleaf & Mixed Forests", 
                 "Temperate Conifer Forests", 
                 "Boreal Forests/Taiga", 
                 "Tropical & Subtropical Grasslands, Savannas & Shrublands", 
                 "Temperate Grasslands, Savannas & Shrublands", 
                 "Flooded Grasslands & Savannas", 
                 "Montane Grasslands & Shrublands", 
                 "Tundra", 
                 "Mediterranean Forests, Woodlands & Scrub", 
                 "Deserts & Xeric Shrublands", 
                 "Mangroves", 
                 "Lake", 
                 "Rock & Ice")

biome_colors <- c("#2c3100",
                  "#707d37",
                  "#1a4910",
                  "#437c44",
                  "#00765a",
                  "#008fac",
                  "#843a29",
                  "#9f8075",
                  "#6e480e",
                  "#312c1e",
                  "#015281",
                  "#5c2f00",
                  "#a68052",
                  "#7b6f29",
                  "#017db5",
                  "#00325f")
# view colours
pie(rep(1,16), col = biome_colors, labels = biome_names)

biome_map <- ggplot(teow_sf) +
  geom_sf(aes(fill = as.factor(BIOME))) +
  scale_fill_manual(values = biome_colors, name = "Terrestrial Biome", labels = biome_names)

biome_map +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Global distribution of protozoa records in GMPD") +
  geom_point(data = gmpd_zooscored_prot, aes(x = long, y = lat, 
                                             color = factor(zoostat)), alpha = 0.5, size = 1) +
  scale_colour_manual(values=c("white", "red")) +
  #theme(panel.background = element_rect(fill = "azure")) +
  coord_sf(crs = 4326)

# I used realm for the model though. Realm map: https://commons.wikimedia.org/wiki/File:Ecozones.svg


# plot global distribution of GMPD protozoa records across TEOW realms ------

realm_names <- c("Austalasia", 
                 "Antarctic", 
                 "Afrotropics", 
                 "Indomalaya", 
                 "Nearctic", 
                 "Neotropics", 
                 "Oceania", 
                 "Palearctic", 
                 "Rock & Ice")

realm_colors <- c("#a68052", # beige-y
                  "#008fac", # sky blue-ish
                  "#707d37", # olive green
                  "#7b6f29", # khaki green?
                  "#015281", 
                  "#437c44", 
                  "#6e480e", 
                  "#5c2f00", 
                  "#00325f")
# view colours
pie(rep(1,9), col = realm_colors, labels = realm_names)

realm_map <- ggplot(teow_sf) +
  geom_sf(aes(fill = REALM)) +
  scale_fill_manual(values = realm_colors, name = "Terrestrial Realm", labels = realm_names)

realm_map +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Global distribution of protozoa records in GMPD") +
  geom_point(data = gmpd_zooscored_prot, aes(x = long, y = lat, 
                                             color = factor(zoostat)), alpha = 0.5, size = 1) +
  scale_colour_manual(values=c("white", "red")) +
  #theme(panel.background = element_rect(fill = "azure")) +
  coord_sf(crs = 4326)

# plot global distribution of GMPD protozoa records by country -----

library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(!admin == "Antarctica", !sovereignt == "Vatican") %>% 
  mutate(gdp_per_cap = gdp_md_est/pop_est)
class(world)

ggplot() +
  geom_sf(data = world, aes(fill = log(gdp_per_cap)), color = "black") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Global distribution of protozoa records in GMPD") +
  scale_fill_viridis_c() +
  geom_point(data = gmpd_zooscored_prot, aes(x = long, y = lat, 
                                            color = factor(zoostat)), alpha = 0.7, size = 1.2) +
  scale_colour_manual(values=c("white", "red")) +
  theme(panel.background = element_rect(fill = "azure")) +
  coord_sf(crs = 4326)

# factorize economy

world <- world %>% mutate(DevelopmentIndex = as.factor(economy))

world$DevelopmentIndex %>% levels()
world$DevelopmentIndex %>% table()
levels(world$DevelopmentIndex) <- c("Developed", "Developed", 
                                    "Emerging", "Emerging", "Emerging", 
                                    "Developing", "LeastDeveloped")
world$DevelopmentIndex %>% table()

# extract gdp data -----

GDPworld <- world %>% select(gdp_per_cap, DevelopmentIndex)

GDPprot <- st_intersection(st_make_valid(GDPworld), host_par_points_sf)
class(GDPprot) <- "data.frame"

GDPprot_grp <- GDPprot %>% select(parname, gdp_per_cap) %>% 
  mutate(DevelopmentIndex = as.numeric(GDPprot$DevelopmentIndex)) %>% 
  group_by(parname)
GDPprot_agg <- GDPprot_grp %>% summarise(meanGDP_per_cap = mean(gdp_per_cap), 
                                         DevelopmentIndex = mean(DevelopmentIndex))

# Create and plot correlation matrix
data <- select_if(GDPprot_agg, is.numeric)
correlationMatrix <- cor(data, use = "pairwise.complete.obs")
corrplot(correlationMatrix, method="color", tl.col = "black", tl.cex = 0.75, number.cex = 2, 
         na.label = "NA", na.label.col = "darkgray", addCoef.col = "darkgray", number.digits = 1)

GDPprotraits <- left_join(protnames, GDPprot_agg) %>% select(parname, meanGDP_per_cap)

# Save csv
# write.csv(GDPprotraits, "./data/modified/protraits/gdpprotraits.csv")


