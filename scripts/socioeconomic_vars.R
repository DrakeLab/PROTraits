# Socioeconomic data

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting


nc_data <- nc_open("./data/original/Kummu_2018/GDP_per_capita_PPP_1990_2015_v2.nc")

GDP_data <- raster("./data/original/Kummu_2018/GDP_per_capita_PPP_1990_2015_v2.nc", layer = 26) # choose 2015 data

GDP_data %>% plot()

points <-  gmpdprot %>% dplyr::select(long, lat)

protdata <- extract(GDP_data, points)
protdata %>% head()



extract(GDP_data, matrix(c(25.7, -33.48), ncol = 2))
