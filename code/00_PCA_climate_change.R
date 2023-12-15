# Script to study relation between the 19 bioclimatic variables within the species range (figure )
# A principal composant analysis allows us to identify the two most informative bioclimatic variables. 
# We then calculate their evolution over two periods: 1965-2015, as a reference, and 2005-2014.
# Skip directly to the part "calculate change" to extract the data used in the analysis.

# Author: Etienne Henry and Pablo M. Lucas
# Date: 10/2022

# Load packages ----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)

library(sf)
sf::sf_use_s2(FALSE)

library(raster)
library(factoextra)
library(exactextractr)
library(readxl)


# Birdlife distribution maps --------------------------------------------------------

# Raw distribution maps

distributions <- st_read(dsn = "birdlife/BOTW/BOTW.gdb/a00000009.gdbtable")
#' source BirdLife International and Handbook of the Birds of the World (2021). Data from: Bird species distribution maps of the world. Version 2021.1
#' Available at http://datazone.birdlife.org/species/requestdis.

# Filtering 

distributions.flt <-subset(distributions,
                           distributions$presenc %not in% c(3,4,5) & # Remove possibly extant, extinct and possibly extinct
                             distributions$seasonl %in% c(1,2) &  # Keep resident and breeding grounds  (remove non breeding, passage and seasonal occurrence uncertain)
                             distributions$origin %in% c(1,2,5,6)) # Keep native, reintroduced, uncertain, assisted colonization (remove introduced,  vagrant)

# Fuse the polygons to make 1 multipolygons for each species 

dist.agg.ws84 <- distributions.flt %>% dplyr::group_by(sci_nam) %>% dplyr::summarise(N= n())

rm(distributions, distributions.flt)

# Import bioclimatic variables --------------------------------------------------------

#' source Karger, D.N. (2018). Data from: CHELSAcruts - High resolution temperature and precipitation 
#' timeseries for the 20th century and beyond
#' \doi{/10.16904/ENVIDAT.159}

# 1965-1995 1 Mean annual air temperature
bc.1965.1 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_1.rst")
bc.1965.1 <- replace(bc.1965.1, bc.1965.1==-3.4e+38, NA)
#hist(bc.1965.1, main = "1965-1995 1 Mean annual air temperature",col = "springgreen")
crs(bc.1965.1) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 2 Mean diurnal air temperature range
bc.1965.2 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_2.rst")
bc.1965.2 <- replace(bc.1965.2, bc.1965.2==-3.4e+38, NA)
#hist(bc.1965.2, main = "1965-1995 2 Mean diurnal air temperature range", col = "springgreen")
crs(bc.1965.2) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 3 Isothermality 
bc.1965.3 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_3.rst")
#hist(bc.1965.3, main = "1965-1995 3 Isothermality", col = "springgreen")
crs(bc.1965.3) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 4 Temperature seasonality 
bc.1965.4 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_4.rst")
#hist(bc.1965.4, main = "1965-1995 4 Temperature seasonality", col = "springgreen")
crs(bc.1965.4) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 


# 1965-1995 5 Mean daily maximum air temperature of the warmest month
bc.1965.5 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_5.rst")
#hist(bc.1965.5, main = "1965-1995 5 Mean daily maximum air temperature of the warmest month", col = "springgreen")
crs(bc.1965.5) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 


# 1965-1995 6 Mean daily maximum air temperature of the coldest month
bc.1965.6 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_6.rst")
#hist(bc.1965.6, main = "1965-1995 6 Mean daily maximum air temperature of the coldest month", col = "springgreen")
crs(bc.1965.6) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 7 Annual range of air temperature
bc.1965.7 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_7.rst")
#hist(bc.1965.7, main = "1965-1995 7 Annual range of air temperature", col = "springgreen")
crs(bc.1965.7) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 8 Mean daily mean air temperatures of the wettest quarter
bc.1965.8 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_8.rst")
#hist(bc.1965.8, main = "1965-1995 8 Mean daily mean air temperatures of the wettest quarter", col = "springgreen")
crs(bc.1965.8) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 9 Mean daily mean air temperatures of the driest quarter
bc.1965.9 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_9.rst")
#hist(bc.1965.9, main = "1965-1995 9 Mean daily mean air temperatures of the driest quarter", col = "springgreen")
crs(bc.1965.9) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 10 Mean daily mean air temperatures of the warmest quarter
bc.1965.10 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_10.rst")
#hist(bc.1965.10, main = "1965-1995 10 Mean daily mean air temperatures of the warmest quarter", col = "springgreen")
crs(bc.1965.10) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 11 Mean daily mean air temperatures of the coldest quarter
bc.1965.11 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_11.rst")
#hist(bc.1965.11, main = "1965-1995 11 Mean daily mean air temperatures of the coldest quarter", col = "springgreen")
crs(bc.1965.11) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 12 Annual precipitation amount
bc.1965.12 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_12.rst")
#hist(bc.1965.12, main = "1965-1995 12 Annual precipitation amount", col = "springgreen")
crs(bc.1965.12) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 13 Precipitation amount of the wettest month
bc.1965.13 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_13.rst")
#hist(bc.1965.13, main = "1965-1995 13 Precipitation amount of the wettest month", col = "springgreen")
crs(bc.1965.13) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 14 Precipitation amount of the driest month
bc.1965.14 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_14.rst")
#hist(bc.1965.14, main = "1965-1995 14 Precipitation amount of the driest month", col = "springgreen")
crs(bc.1965.14) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 15 Precipitation seasonality
bc.1965.15 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_15.rst")
#hist(bc.1965.15, main = "1965-1995 15 Precipitation seasonality", col = "springgreen")
crs(bc.1965.15) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 16 Mean monthly precipitation amount of the wettest quarter
bc.1965.16 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_16.rst")
#hist(bc.1965.16, main = "1965-1995 16 Mean monthly precipitation amount of the wettest quarter", col = "springgreen")
crs(bc.1965.16) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 17 Mean monthly precipitation amount of the driest quarter
bc.1965.17 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_17.rst")
#hist(bc.1965.17, main = "1965-1995 17 Mean monthly precipitation amount of the driest quarter", col = "springgreen")
crs(bc.1965.17) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 18 Mean monthly precipitation amount of the warmest quarter
bc.1965.18 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_18.rst")
#hist(bc.1965.18, main = "1965-1995 18 Mean monthly precipitation amount of the warmest quarter", col = "springgreen")
crs(bc.1965.18) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 19 Mean monthly precipitation amount of the coldest quarter
bc.1965.19 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_19.rst")
#hist(bc.1965.19, main = "1965-1995 19 Mean monthly precipitation amount of the coldest quarter", col = "springgreen")
crs(bc.1965.19) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 


# 2005-2014 1 Mean annual air temperature
bc.2005.1 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_1.rst")
bc.2005.1 <- replace(bc.2005.1, bc.2005.1==-3.4e+38, NA)
#hist(bc.2005.1, main = "2005-2014 1 Mean annual air temperature",col = "springgreen")
crs(bc.2005.1) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 2 Mean diurnal air temperature range
bc.2005.2 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_2.rst")
#hist(bc.2005.2, main = "2005-2014 2 Mean diurnal air temperature range", col = "springgreen")
crs(bc.2005.2) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 3 Isothermality 
bc.2005.3 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_3.rst")
#hist(bc.2005.3, main = "2005-2014 3 Isothermality", col = "springgreen")
crs(bc.2005.3) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 4 Temperature seasonality 
bc.2005.4 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_4.rst")
#hist(bc.2005.4, main = "2005-2014 4 Temperature seasonality", col = "springgreen")
crs(bc.2005.4) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 5 Mean daily maximum air temperature of the warmest month
bc.2005.5 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_5.rst")
#hist(bc.2005.5, main = "2005-2014 5 Mean daily maximum air temperature of the warmest month", col = "springgreen")
crs(bc.2005.5) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 6 Mean daily maximum air temperature of the coldest month
bc.2005.6 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_6.rst")
#hist(bc.2005.6, main = "2005-2014 6 Mean daily maximum air temperature of the coldest month", col = "springgreen")
crs(bc.2005.6) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 7 Annual range of air temperature
bc.2005.7 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_7.rst")
#hist(bc.2005.7, main = "2005-2014 7 Annual range of air temperature", col = "springgreen")
crs(bc.2005.7) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 8 Mean daily mean air temperatures of the wettest quarter
bc.2005.8 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_8.rst")
#hist(bc.2005.8, main = "2005-2014 8 Mean daily mean air temperatures of the wettest quarter", col = "springgreen")
crs(bc.2005.8) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 9 Mean daily mean air temperatures of the driest quarter
bc.2005.9 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_9.rst")
#hist(bc.2005.9, main = "2005-2014 9 Mean daily mean air temperatures of the driest quarter", col = "springgreen")
crs(bc.2005.9) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 10 Mean daily mean air temperatures of the warmest quarter
bc.2005.10 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_10.rst")
#hist(bc.2005.10, main = "2005-2014 10 Mean daily mean air temperatures of the warmest quarter", col = "springgreen")
crs(bc.2005.10) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 11 Mean daily mean air temperatures of the coldest quarter
bc.2005.11 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_11.rst")
#hist(bc.2005.11, main = "2005-2014 11 Mean daily mean air temperatures of the coldest quarter", col = "springgreen")
crs(bc.2005.11) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 12 Annual precipitation amount
bc.2005.12 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_12.rst")
#hist(bc.2005.12, main = "2005-2014 12 Annual precipitation amount", col = "springgreen")
crs(bc.2005.12) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 13 Precipitation amount of the wettest month
bc.2005.13 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_13.rst")
#hist(bc.2005.13, main = "2005-2014 13 Precipitation amount of the wettest month", col = "springgreen")
crs(bc.2005.13) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 14 Precipitation amount of the driest month
bc.2005.14 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_14.rst")
#hist(bc.2005.14, main = "2005-2014 14 Precipitation amount of the driest month", col = "springgreen")
crs(bc.2005.14) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 15 Precipitation seasonality
bc.2005.15 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_15.rst")
#hist(bc.2005.15, main = "2005-2014 15 Precipitation seasonality", col = "springgreen")
crs(bc.2005.15) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 16 Mean monthly precipitation amount of the wettest quarter
bc.2005.16 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_16.rst")
#hist(bc.2005.16, main = "2005-2014 16 Mean monthly precipitation amount of the wettest quarter", col = "springgreen")
crs(bc.2005.16) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 17 Mean monthly precipitation amount of the driest quarter
bc.2005.17 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_17.rst")
#hist(bc.2005.17, main = "2005-2014 17 Mean monthly precipitation amount of the driest quarter", col = "springgreen")
crs(bc.2005.17) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 18 Mean monthly precipitation amount of the warmest quarter
bc.2005.18 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_18.rst")
#hist(bc.2005.18, main = "2005-2014 18 Mean monthly precipitation amount of the warmest quarter", col = "springgreen")
crs(bc.2005.18) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 19 Mean monthly precipitation amount of the coldest quarter
bc.2005.19 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_19.rst")
#hist(bc.2005.19, main = "2005-2014 19 Mean monthly precipitation amount of the coldest quarter", col = "springgreen")
crs(bc.2005.19) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

dist.cea <- st_transform(dist.agg.ws84, crs="+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs") 

dist.bc <- dist.cea %>% dplyr::select(sci_nam)

bc <- stack(bc.1965.1,bc.1965.2,bc.1965.3,bc.1965.4,bc.1965.5,bc.1965.6,bc.1965.7,bc.1965.8,bc.1965.9,bc.1965.10,bc.1965.11,
            bc.1965.12,bc.1965.13,bc.1965.14,bc.1965.15,bc.1965.16,bc.1965.17,bc.1965.18,bc.1965.19,
            bc.2005.1,bc.2005.2,bc.2005.3,bc.2005.4,bc.2005.5,bc.2005.6,bc.2005.7,bc.2005.8,bc.2005.9,
            bc.2005.10,bc.2005.11,bc.2005.12,bc.2005.13,bc.2005.14,bc.2005.15,bc.2005.16,bc.2005.17,bc.2005.18,
            bc.2005.19)

# Extraction of the mean value of each 1965 climate variable within the species range
# 1. Stack
bc.1965 <- stack(bc.1965.1,bc.1965.2,bc.1965.3,bc.1965.4,bc.1965.5,bc.1965.6,bc.1965.7,bc.1965.8,bc.1965.9,bc.1965.10,bc.1965.11,
            bc.1965.12,bc.1965.13,bc.1965.14,bc.1965.15,bc.1965.16,bc.1965.17,bc.1965.18,bc.1965.19)
# 2. Extract
dist.bc[,c("bc_1965_1","bc_1965_2","bc_1965_3","bc_1965_4","bc_1965_5","bc_1965_6","bc_1965_7","bc_1965_8",
           "bc_1965_9","bc_1965_10","bc_1965_11","bc_1965_12","bc_1965_13","bc_1965_14","bc_1965_15",
           "bc_1965_16","bc_1965_17","bc_1965_18","bc_1965_19")] <- exact_extract(bc.1965, dist.bc, "mean")

# Extraction of the mean value of each 2005 climate variable within the species range
# 1. Stack
bc.2005 <- stack( bc.2005.1,bc.2005.2,bc.2005.3,bc.2005.4,bc.2005.5,bc.2005.6,bc.2005.7,bc.2005.8,bc.2005.9,
            bc.2005.10,bc.2005.11,bc.2005.12,bc.2005.13,bc.2005.14,bc.2005.15,bc.2005.16,bc.2005.17,bc.2005.18,
            bc.2005.19)
# 2. Extract 
dist.bc[,c("bc_2005_1","bc_2005_2","bc_2005_3","bc_2005_4","bc_2005_5","bc_2005_6","bc_2005_7","bc_2005_8","bc_2005_9",
        "bc_2005_10","bc_2005_11","bc_2005_12","bc_2005_13","bc_2005_14","bc_2005_15",
        "bc_2005_16","bc_2005_17","bc_2005_18","bc_2005_19")] <- exact_extract(bc.2005, dist.bc, "mean")


dist.bc.s <- dist.bc %>% st_drop_geometry() %>%
  dplyr::select(sci_nam,bc_1965_1,bc_1965_2,bc_1965_3,bc_1965_4,bc_1965_5,bc_1965_6,bc_1965_7,bc_1965_8,
                bc_1965_9,bc_1965_10,bc_1965_11,bc_1965_12,bc_1965_13,bc_1965_14,bc_1965_15,
                bc_1965_16,bc_1965_17,bc_1965_18,bc_1965_19,
                bc_2005_1,bc_2005_2,bc_2005_3,bc_2005_4,bc_2005_5,bc_2005_6,bc_2005_7,bc_2005_8,bc_2005_9,
                bc_2005_10,bc_2005_11,bc_2005_12,bc_2005_13,bc_2005_14,bc_2005_15,
                bc_2005_16,bc_2005_17,bc_2005_18,bc_2005_19)

# Save data

#write.table(dist.bc.s,file="dataset/external_factors/bioclim/intermediate_bioclim/bioclim_birds.csv")

rm(bc.1965.1,bc.1965.2,bc.1965.3,bc.1965.4,bc.1965.5,bc.1965.6,bc.1965.7,bc.1965.8,bc.1965.9,bc.1965.10,bc.1965.11,
   bc.1965.12,bc.1965.13,bc.1965.14,bc.1965.15,bc.1965.16,bc.1965.17,bc.1965.18,bc.1965.19,
   bc.2005.1,bc.2005.2,bc.2005.3,bc.2005.4,bc.2005.5,bc.2005.6,bc.2005.7,bc.2005.8,bc.2005.9,
   bc.2005.10,bc.2005.11,bc.2005.12,bc.2005.13,bc.2005.14,bc.2005.15,bc.2005.16,bc.2005.17,bc.2005.18,
   bc.2005.19)

# PCA --------------------------------------------------------

dist.bc.s <- read.table(file="dataset/external_factors/bioclim/intermediate_bioclim/bioclim_birds.csv")

dist.bc.s <- dist.bc.s %>%
  filter(sci_nam %not in% sps.marine$x)


sub_1965_all <- dist.bc.s %>%
  dplyr::select(sci_nam,bc_1=bc_1965_1,bc_2=bc_1965_2,bc_3=bc_1965_3,bc_4=bc_1965_4,bc_5=bc_1965_5,bc_6=bc_1965_6,bc_7=bc_1965_7,bc_8=bc_1965_8,
                bc_9=bc_1965_9,bc_10=bc_1965_10,bc_11=bc_1965_11,bc_12=bc_1965_12,bc_13=bc_1965_13,bc_14=bc_1965_14,bc_15=bc_1965_15,
                bc_16=bc_1965_16,bc_17=bc_1965_17,bc_18=bc_1965_18,bc_19=bc_1965_19) %>% 
  mutate(period="1965")

sub_2005_all <- dist.bc.s %>%
  dplyr::select(sci_nam,bc_1=bc_2005_1,bc_2=bc_2005_2,bc_3=bc_2005_3,bc_4=bc_2005_4,bc_5=bc_2005_5,bc_6=bc_2005_6,bc_7=bc_2005_7,bc_8=bc_2005_8,
                bc_9=bc_2005_9,bc_10=bc_2005_10,bc_11=bc_2005_11,bc_12=bc_2005_12,bc_13=bc_2005_13,bc_14=bc_2005_14,bc_15=bc_2005_15,
                bc_16=bc_2005_16,bc_17=bc_2005_17,bc_18=bc_2005_18,bc_19=bc_2005_19) %>% 
  mutate(period="2005")

sub_1965_2005<-rbind(sub_1965_all,sub_2005_all) 

# Omit the species with NA 

bioclimatic_variables_pre<-na.omit(sub_1965_2005)

# PCA
bioclimatic_variables_pca<-prcomp(bioclimatic_variables, center = TRUE,scale = TRUE)

# Figure S4.A

fviz_pca_var(bioclimatic_variables_pca,
             col.var = "cos2", # Color by contributions to the PC
             gradient.cols = c("#FEFCFB", "#D18977", "#8E1806"),
             repel = TRUE     # Avoid text overlapping
) +   theme_minimal()

# Correlation plot: figure S4.b
col_cor  <- colorRampPalette(c("goldenrod3","white","#8B1202"))(100)

M = cor(bioclimatic_variables ,method = c("pearson"))

corrplot(M, type = 'lower', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45, col = col_cor)

# Calculate change --------------------------------------------------------

# Select the 2 variables that are the most informative in the PCA (& minimizing correlation):
# variable 1 = Mean annual air temperature
# variable 12 = Annual precipitation amount

dist.cea <- st_transform(dist.agg.ws84, crs="+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs") 

dist.bc <- dist.cea %>% dplyr::select(sci_nam)

bc <- stack(bc.1965.1,
            bc.1965.12,
            bc.2005.1,
            bc.2005.12)


dist.bc[,c("bc_1965_1_mean","bc_1965_12_mean","bc_2005_1_mean","bc_2005_12_mean")] <- exact_extract(bc, dist.bc, "mean")
dist.bc[, c("bc_1965_1_med", "bc_1965_12_med","bc_2005_1_med","bc_2005_12_med")] <- exact_extract(bc , dist.bc, "quantile", quantiles=c(0.5))

# Extract median
delta_bc_imp_med <- dist.bc %>% st_drop_geometry() %>% 
  mutate(Delta_BC_1_med=bc_2005_1_med-bc_1965_1_med,
         Delta_BC_12_med=bc_2005_12_med-bc_1965_12_med) %>% 
  dplyr::select(sci_nam,Temp_change_med = Delta_BC_1_med,Ppt_change_med = Delta_BC_12_med)

# Save data
write.table(delta_bc_imp_med, file="dataset/external_factors/bioclim/delta_2bc_imp_med.csv")#output

# Extract geometric mean
dist.bc[,c("bc_1965_1_gmean")] <- exact_extract(bc.1965.1, dist.bc, function(values, coverage_fraction)
  (exp(mean(log(values[values>0]*coverage_fraction),na.rm=T))))
dist.bc[,c("bc_1965_12_gmean")] <- exact_extract(bc.1965.12, dist.bc, function(values, coverage_fraction)
  (exp(mean(log(values[values>0]*coverage_fraction),na.rm=T))))
dist.bc[,c("bc_2005_1_gmean")] <- exact_extract(bc.2005.1, dist.bc, function(values, coverage_fraction)
  (exp(mean(log(values[values>0]*coverage_fraction),na.rm=T))))
dist.bc[,c("bc_2005_12_gmean")] <- exact_extract(bc.2005.12, dist.bc, function(values, coverage_fraction)
  (exp(mean(log(values[values>0]*coverage_fraction),na.rm=T))))

# Calculate change 
delta_bc_imp_gmean <- dist.bc %>% st_drop_geometry() %>% 
  mutate(Delta_BC_1_gmean=bc_2005_1_gmean-bc_1965_1_gmean,
         Delta_BC_12_gmean=bc_2005_12_gmean-bc_1965_12_gmean) %>% 
  dplyr::select(sci_nam, Temp_change_gmean = Delta_BC_1_gmean,Ppt_change_gmean = Delta_BC_12_gmean)

# Save data
write.table(delta_bc_imp_gmean, file="dataset/external_factors/bioclim/delta_2bc_imp_gmean.csv")

# END -----
