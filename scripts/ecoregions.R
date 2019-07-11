library(sf)

# assign a ecoregion data to each host-parasite location

host_par_points_df <- as.data.frame(gmpdprot[, c(8,7)]) %>% na.omit() # extract lat longs and remove empty rows
host_par_points_sf <- host_par_points_df %>% st_as_sf(coords = c("long","lat"), crs=4326) # convert to sf
host_par_points_df$geometry <- host_par_points_sf$geometry # add geometry column to original dataframe

ecoregions_sf <- st_read("./data/original/WWF_ecoregions_datafiles/wwf_terr_ecos.shp") # load wwf terrestrial ecoregion data

hp_pnts_ecoregion <- st_intersection(ecoregions_sf, host_par_points_sf) %>% as.data.frame() # create df with each point and corresponding ecoregion values


# map for fun

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot() +
  geom_sf(data = world, aes(fill = gdp_md_est/pop_est), color = "black") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)")) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  geom_point(data = host_par_points_df, aes(x = long, y = lat)) +
  coord_sf(crs = 4326)

