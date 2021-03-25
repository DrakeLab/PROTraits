# Socioeconomic data

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(magrittr)
library(tidyverse)

rm(list = ls())

# Import data

#' Kummu, Matti; Taka, Maija; Guillaume, Joseph H. A. (2018), 
#' Gridded global datasets for Gross Domestic Product and Human Development Index over 1990–2015, 
#' Scientific Data, Article-journal, https://doi.org/10.1038/sdata.2018.4
#' Spatial data accessed from Dryad https://doi.org/10.5061/dryad.dk1j0 on (check download date)

# Choose layer 26 (2015)
GDP_HDI_2015 <- raster::stack(raster("./data/original/Kummu_2018/GDP_per_capita_PPP_1990_2015_v2.nc", layer = 26),
                              raster("./data/original/Kummu_2018/HDI_1990_2015_v2.nc", layer = 26))

summary(GDP_HDI_2015)

gmpdprot <- read.csv("./data/modified/gmpd_zooscored.csv") %>% 
  filter(partype == "Protozoa") %>% 
  dplyr::select(ID = X, protname = gmpdparname, long, lat)
points <- gmpdprot %>% dplyr::select(long, lat)

protdata <- raster::extract(GDP_HDI_2015, points) %>% as.data.frame()
protdata %>% head()

prots_GDP_HDI <- cbind(gmpdprot$protname, protdata) 

colnames(prots_GDP_HDI) <- c("protname", "GDP_2015", "HDI_2015")

prots_GDP_HDI_mean <- prots_GDP_HDI %>% group_by(protname) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)
colnames(prots_GDP_HDI_mean)[2:3] <- paste("mean_", colnames(prots_GDP_HDI_mean)[2:3])

prots_GDP_HDI_mean %>% head()

write.csv(prots_GDP_HDI_mean, "./data/modified/protraits/socioeconprotraits.csv")

# coverage

completenessprots_GDP_HDI_mean <- prots_GDP_HDI_mean %>% summarise_all(function(x) mean(!is.na(x))) %>% 
  transpose()

completenessprots_GDP_HDI_mean_df <- completenessprots_GDP_HDI_mean[[1]] %>% unlist() %>% 
  as.data.frame() %>% rownames_to_column()
# write.csv(completenessprots_GDP_HDI_mean_df, "./data/modified/completenesssocioeconprotraits.csv")
