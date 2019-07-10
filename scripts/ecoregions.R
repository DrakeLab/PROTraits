library(sf)
library(raster)
library(fasterize)

host_par_points <- as.matrix(gmpdprot[, c(8,7)]) %>% na.omit()

ecoregions_sf <- st_read("./data/original/WWF_ecoregions_datafiles/wwf_terr_ecos.shp")
ecoregions_sf$BIOME <- gsub(98, 15, ecoregions_sf$BIOME)
ecoregions_sf$BIOME <- gsub(99, 16, ecoregions_sf$BIOME)
ecoregions_sf$BIOME <- as.numeric(ecoregions_sf$BIOME)
biome_colors <- rev(terrain.colors(18))

st_geometry_type(ecoregions_sf)

r <- raster(ecoregions_sf, res = 1)

ecoregions_all <- fasterize(ecoregions_sf, r, field = "BIOME")
plot(ecoregions_all, col = biome_colors[3:18])
points(host_par_points, cex =5)
plot

ecoregions_biome <- fasterize(ecoregions_sf, r, field = "OBJECTID", by = "BIOME")
plot(ecoregions_biome)

raster::extract(ecoregions_biome, host_par_points, method='simple', na.rm = TRUE)

x <- raster::extract(ecoregions_biome, host_par_points, method='simple', df = TRUE)
print(x)
