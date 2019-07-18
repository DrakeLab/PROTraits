library(sf)

# assign a ecoregion data to each host-parasite location

host_par_points_df <- as.data.frame(gmpdprotraits[, c(1, 18, 19)]) %>% na.omit() # extract lat longs and remove empty rows
host_par_points_sf <- host_par_points_df %>% st_as_sf(coords = c("long","lat"), crs=4326) # convert to sf
#host_par_points_df$geometry <- host_par_points_sf$geometry # add geometry column to original dataframe

# load wwf terrestrial ecoregion data
teow_sf <- st_read("./data/original/WWF_ecoregions_datafiles/wwf_terr_ecos.shp") # downloaded from https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world on 


# create df with each point and corresponding TEOW vars
gmpdprot_teow <- st_intersection(ecoregions_sf, host_par_points_sf) %>% 
  as.data.frame()

gmpdprotraits <- left_join(gmpdprotraits, gmpdprot_teow %>% select(ID, ECO_NAME, REALM, BIOME), by = "ID")

# create df of the GMPD prot records that did NOT overlap with TEOW polygons
gmpdprot_no_teow <- anti_join(gmpdprotraits, gmpdprot_teow, by = "ID") # 171 records did not match, mostly because they did not have lat/longs in GMPD
length(gmpdprot_no_teow$lat %>% na.omit()) # 75 records have lat/longs but still did not overlap with TEOW polygons because they were off the coast in Marine ecoregions. 26 of these are records of zoonotic species. 

# plot for fun

library(ggplot2)

# plot global distribution of GMPD protozoa records across TEOW biomes

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
#pie(rep(1,16), col = biome_colors, labels = biome_names)

biome_map <- ggplot(ecoregions_sf) +
  geom_sf(aes(fill = as.factor(BIOME))) +
  scale_fill_manual(values = biome_colors, name = "Terrestrial Biome", labels = biome_names)

biome_map +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Global distribution of protozoa records in GMPD") +
  geom_point(data = host_par_points_df, aes(x = long, y = lat),  color = "#fffffa", alpha = 0.5, size = 1) +
  theme(panel.background = element_rect(fill = "azure")) +
  coord_sf(crs = 4326)

# plot global distribution of GMPD protozoa records by country 

library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot() +
  geom_sf(data = world, aes(fill = gdp_md_est/pop_est), color = "black") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Global distribution of protozoa records in GMPD") +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  geom_point(data = host_par_points_df, aes(x = long, y = lat)) 

