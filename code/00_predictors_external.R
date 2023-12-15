# Script to compile all data from different sources on bird extrinsic predictors of extinction.
# This code extracts and filters IUCN bird distribution maps, calculates their range, 
# and then overlays the polygons with raster data on external factors. (e.g.,population density, cropland extent...)
# It also calculates for each species the median GDP per capita between the countries of occurrence. 

# Author: Etienne Henry and Victor Cazalis for GDP
# Date: 10/2022

# Load packages ----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(sf)
sf::sf_use_s2(FALSE)
library(plyr)
library(ggpubr)
library(purrr)
library(reshape2)
library('rnaturalearth')
library(raster)
library(rgdal)
library(exactextractr)
library(readxl)

'%not in%' <- Negate('%in%')

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

# Save data in 2 part

#st_write(dist.agg.ws84[1:5500,], "dataset/birds/bird_distributions_BL/shapefile/dist_agg_ws84_1.shp")
#st_write(dist.agg.ws84[5501:nrow(dist.agg.ws84),], "dataset/birds/bird_distributions_BL/shapefile/dist_agg_ws84_2.shp")

# Save list of selected species

list.sps <- dist.agg %>% dplyr::select(sci_nam) %>%  st_drop_geometry()
#write.table(list.sps, "dataset/birds/list_sps_birds.txt")
  

# Birdlife distribution maps processed --------------------------------------------------------

dist_agg1 <- st_read("dataset/birds/bird_distributions_BL/shapefile/dist_agg_ws84_1.shp")
dist_agg2 <- st_read("dataset/birds/bird_distributions_BL/shapefile/dist_agg_ws84_2.shp")
dist.agg.ws84 <- bind_rows(dist_agg1,dist_agg2)

# Correction for globally distributed species: crop overlapping latitudes (for polygon projection)

box = c(xmin = -180.00000, ymin = -65.15322, xmax = 179.999999, ymax = -14.68439)
dist.agg.ws84[dist.agg.ws84$sci_nam =="Diomedea sanfordi",] <- st_crop(dist.agg.ws84[dist.agg.ws84$sci_nam =="Diomedea sanfordi",], box)

box2 = c(xmin = -180.00000, ymin = 50.73316, xmax = 179.999999, ymax = 84.11185)
dist.agg.ws84[dist.agg.ws84$sci_nam =="Pagophila eburnea",] <- st_crop(dist.agg.ws84[dist.agg.ws84$sci_nam =="Pagophila eburnea",], box2) 

rm(dist_agg1, dist_agg2)
rm(box,box2)

# Calculate range size --------------------------------------------------------

dist.range <- dist.agg.ws84

# Projection in Mollweid: equal area 
dist.range.moll <- st_transform(dist.range, crs="+proj=moll")

dist.range.moll$range_km2 <- st_area(dist.range.moll)
dist.range.moll$range_km2 <- units::set_units(dist.range.moll$range_km2, "km^2")

dist.range.s <- dist.range.moll %>% dplyr::select(sci_nam,range_km2) %>% st_drop_geometry()

# Save data
#write.table(dist.range.s,"dataset/birds/traits_data_birds/range_km2.csv")

# Canopy cover --------------------------------------------------------
#' source Remelgado, R. & Meyer, C (2023). Data from: Systematic losses in tree-canopy cover over three decades 
#' revealed by integrating complementary data sources (preprint)
#' \doi{10.31223/X5T68Z}

dist.forest <- dist.agg.ws84
canopy.change <- raster("dataset/external_factors/iGFC/iGFC-canopyChange_20000000-20180000_300m.tif")
canopy.2018 <- raster("dataset/external_factors/iGFC/iGFC-canopyDensity_20180000_300m.tif")

# Projection of bird distribution polygons to match the coordinate system of the raster

dist.forest <- st_transform(dist.forest,crs(canopy.change))

# Extract raster quantiles and/or geometric mean within birds range areas

dist.forest[, c("canopy_change_Q10", "canopy_change_med","canopy_change_Q90")] <- exact_extract(canopy.change , dist.forest, "quantile", quantiles=c(0.1, 0.5,0.9))


dist.forest[, c("canopy_density_Q10", "canopy_density_med","canopy_density_Q90")] <- exact_extract(canopy.2018 , dist.forest, "quantile", quantiles=c(0.1, 0.5,0.9))


dist.forest.s <- dist.forest %>% 
  dplyr::select(sci_nam,canopy_change_Q10,canopy_change_med,canopy_change_Q90) %>% 
  st_drop_geometry()

dist.forest.t <- dist.forest %>% 
  dplyr::select(sci_nam,canopy_density_Q10,canopy_density_med,canopy_density_Q90) %>% 
  st_drop_geometry()

# Save data 

write.table(dist.forest.s,"dataset/external_factors/iGFC/canopy_change_Q10_med_Q90.csv")
write.table(dist.forest.t,"dataset/external_factors/iGFC/canopy_density_Q10_med_Q9.csv")

# Crop land change --------------------------------------------------------
#' source Potapov, P. (2022). Data from: Global maps of cropland extent and change show accelerated cropland 
#' expansion in the twenty-first century. Nat Food 3, 19–
#' \doi{10.1038/s43016-021-00429-z}

dist.crop <- dist.agg.ws84

crop.gain <- raster("dataset/external_factors/cropland_Potapov2021/Global_cropland_3km_netgain.tif")
crop.loss <- raster("dataset/external_factors/cropland_Potapov2021/Global_cropland_3km_netloss.tif")
crop.loss <- - crop.loss

# Calculate net gain/loss per pixel by addition 

net.crop.gl.stack <- stack(crop.gain,crop.loss)
net.crop.gl <- calc(net.crop.gl.stack, sum)

rm(crop.gain,crop.loss,net.crop.gl.stack)

# % of cropland per pixel

crop.cover.2019 <- raster("dataset/external_factors/cropland_Potapov2021/Global_cropland_3km_2019.tif")
crop.cover.2019 <- replace(crop.cover.2019, crop.cover.2019==255, 100)

# Projection of bird distribution polygons to match the coordinate system of the raster

dist.crop <- st_transform(dist.crop,crs(net.crop.gl))

# Extract raster quantiles and/or geometric mean within birds range areas

dist.crop[, c("crop_change_Q10", "crop_change_med","crop_change_Q90")]<- exact_extract(net.crop.gl, dist.crop, "quantile", quantiles=c(0.1, 0.5,0.9))
dist.crop[, c("crop_change_geom_mean")] <- exact_extract(net.crop.gl, dist.crop, function(values, coverage_fraction)
  (exp(mean(log(values[values>0]*coverage_fraction),na.rm=T))))

dist.crop[, c("crop_cover_Q10", "crop_cover_med","crop_cover_Q90")]<- exact_extract(crop.cover.2019, dist.crop, "quantile", quantiles=c(0.1, 0.5,0.9))
dist.crop[, c("crop_cover_geom_mean")] <- exact_extract(crop.cover.2019, dist.crop, function(values, coverage_fraction)
  (exp(mean(log(values[values>0]*coverage_fraction),na.rm=T))))


dist.crop.t <- dist.crop %>% 
  dplyr::select(sci_nam,crop_change_geom_mean,
                crop_cover_geom_mean) %>% 
  st_drop_geometry()

dist.crop.s <- dist.crop %>% 
  dplyr::select(sci_nam,crop_change_Q10,crop_change_med,crop_change_Q90,crop_change_geom_mean,
                crop_cover_Q10,crop_cover_med,crop_cover_Q90,crop_cover_geom_mean) %>% 
  st_drop_geometry()

# Save data 

# write.table(dist.crop.s,"dataset/external_factors/cropland_Potapov2021/crop_cover_change_Q10_med_Q90.csv")
# write.table(dist.crop.t,"dataset/external_factors/cropland_Potapov2021/crop_cover_change_geom_mean.csv")

# Accessibility to cities Weiss 2018 --------------------------------------------------------
#' source Weiss, D.J. (2018). Data from: A global map of travel time to cities to assess inequalities 
#' in accessibility in 2015. Na-ture 553, 333–336
#' \doi{10.1038/nature25181}

dist.cities <- dist.agg.ws84

cities <- raster( "dataset/external_factors/cities_Weiss2018/accessibility_to_cities_2015_v1.0.tif" )
cities <- replace(cities, cities==-9999, NA)

# Projection of bird distribution polygons to match the coordinate system of the raster

dist.cities <- st_transform(dist.cities,crs(cities))

# Extract raster quantiles and/or geometric mean within birds range areas

dist.cities[, c("cities_Q10", "cities_med","cities_Q90")] <- exact_extract(cities,dist.cities, "quantile", quantiles=c(0.1, 0.5,0.9))
dist.cities[, c("cities_geom_mean")] <- exact_extract(cities, dist.cities, function(values, coverage_fraction)
  (exp(mean(log(values[values>0]*coverage_fraction),na.rm=T))))

dist.cities.s <- dist.cities %>% 
  dplyr::select(sci_nam,cities_Q10,cities_med,cities_Q90,cities_geom_mean) %>% 
  st_drop_geometry() 

# Save data

write.table(dist.cities.s,"dataset/external_factors/cities_Weiss2018/cities_Q10_med_Q90_gmean.csv")

rm(cities, dist.cities,dist.cities.s)

# Human population density  --------------------------------------------------------
#' source Schiavina, M. (2019). Data from: GHS-POP R2019A - GHS population grid multitemporal (1975-1990-2000-2015).
#' \doi{10.2905/0C6B9751-A71F-4062-830B-43C9F432370F1}

# 1. Population density 2000 

dist.pop.2000 <- dist.agg.ws84

pop00 <-raster("dataset/external_factors/pop_dens/GHS_POP_E2000_GLOBE_R2019A_54009_1K_V1_0.tif")

# Projection of bird distribution polygons to match the coordinate system of the raster

dist.pop.2000 <- st_transform(dist.pop.2000,crs(pop00)) 

# Extract raster quantiles and/or geometric mean within birds range areas

dist.pop.2000[, c("p00_Q10","p00_med", "p00_Q90")]<-exact_extract(pop00, dist.pop.2000, "quantile", quantiles=c(0.1,0.5, 0.9))
dist.pop.2000[, c("p00_geom_mean")] <- exact_extract(pop00, dist.pop.2000, function(values, coverage_fraction)
  (exp(mean(log(values[values>0]*coverage_fraction),na.rm=T))))

dist.pop.2000.s  <- dist.pop.2000 %>% 
  dplyr::select(sci_nam,p00_Q10,p00_med,p00_Q90,p00_geom_mean) %>% st_drop_geometry() 

# 2. Population density 2015

dist.pop.2015 <- dist.agg.ws84

pop15 <-raster("dataset/external_factors/pop_dens/GHS_POP_E2015_GLOBE_R2019A_54009_1K_V1_0.tif")

# Projection of bird distribution polygons to match the coordinate system of the raster

dist.pop.2015 <- st_transform(dist.pop.2015,crs(pop15)) 

# Extract raster quantiles, geometric mean and sum within birds range areas

dist.pop.2015[, c("p15_Q10","p15_med", "p15_Q90")]<-exact_extract(pop15, dist.pop.2015, "quantile", quantiles=c(0.1,0.5, 0.9))
dist.pop.2015$p15_sum <- exact_extract(pop15, dist.pop.2015, function(values, coverage_fraction)
  sum(values * coverage_fraction, na.rm=TRUE))
dist.pop.2015[, c("p15_geom_mean")] <- exact_extract(pop15, dist.pop.2015, function(values, coverage_fraction)
  (exp(mean(log(values[values>0]*coverage_fraction),na.rm=T))))

dist.pop.2015.s <- dist.pop.2015 %>% 
  dplyr::select(sci_nam,p15_Q10,p15_med,p15_Q90,p15_sum,p15_geom_mean) %>% st_drop_geometry() 

# Save data

write.table(dist.pop.2000.s,"dataset/external_factors/pop_dens/dist_pop_2000_Q10_med_Q90_gmean.csv")
write.table(dist.pop.2015.s,"dataset/external_factors/pop_dens/dist_pop_2015_Q10_med_Q90_sum_gmean.csv")

rm(dist.pop.2000,dist.pop.2000.s,dist.pop.2015,dist.pop.2015.s,pop00,pop15)

# Proportion of rural people  --------------------------------------------------------
#' source Schiavina, M. (2019). Data from: GHS-POP R2019A - GHS population grid multitemporal (1975-1990-2000-2015).
#' \doi{10.2905/0C6B9751-A71F-4062-830B-43C9F432370F1}

dist.rural <- dist.agg.ws84

human_r_pop <-raster("dataset/external_factors/human_r_pop/Calculated_human_rural_population.tif")

# Projection of bird distribution polygons to match the coordinate system of the raster

dist.rural <- st_transform(dist.rural,crs(human_r_pop)) 

# Extract raster quantiles, geometric mean and sum within birds range areas

dist.rural[, c("rural_Q10","rural_med", "rural_Q90")]<-exact_extract(human_r_pop, dist.rural, "quantile", quantiles=c(0.1,0.5, 0.9))
dist.rural$rural_sum <- exact_extract(human_r_pop, dist.rural, function(values, coverage_fraction)
  sum(values * coverage_fraction, na.rm=TRUE))

dist.rural[, c("rural_geom_mean")] <- exact_extract(human_r_pop, dist.rural, function(values, coverage_fraction)
  (exp(mean(log(values[values>0]*coverage_fraction),na.rm=T))))

dist.rural.s  <- dist.rural %>% 
  dplyr::select(sci_nam,rural_Q10,rural_med,rural_Q90,rural_sum,rural_geom_mean) %>% st_drop_geometry() 

# Save data

write.table(dist.rural.s, "dataset/external_factors/human_r_pop/rural_Q10_med_Q90_sum_gmean.csv")

rm(dist.rural,dist.rural.s,human_r_pop)

# GDP  --------------------------------------------------------
#' source Worldbank (2021). Data from: GDP per Capita. World Bank Development Indicators, The World Bank Group
#' Accessed at https://data.worldbank.org/indicator/NY.GDP.CAP.CD 01-11-2021.

# IUCN dataframe, to match IUCN countries with those of worldbank
#' source BirdLife International (2022). Data from: IUCN Red List for birds. 
#' Downloaded from http://www.birdlife.org on 15/06/2022.

sps.iucn <- readRDS("dataset/Species.merged.april22.rds")

# Birds selected species
sps.birds <- read.table("dataset/birds/list_sps_birds.txt") #see above
sps.birds <- sps.birds$sci_nam

species <- sps.iucn %>% 
  filter(class=="AVES" & scientific_name %in% sps.birds)

# Countries stats (GDP, Corruption, Research capacity)

countriesTOT<-readRDS("dataset/external_factors/gdp/Countries_adaptedfromSIS_COUNTRYOCCURRENCESUBFIELD.rds")

# Prepare GDP per capita data (take the most recent GDP for each country, see e.g. Syria)

gdp <- read.csv("dataset/external_factors/gdp/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_3159040.csv", sep="\t")

gdp$GDP <- NA

for(i in 1:nrow(gdp)){
  v<-gdp[i,grepl("X", names(gdp))]
  v<-v[is.na(v)==F]
  if(length(v)>0){gdp$GDP[i]<-v[length(v)]}
}

gdp<-gdp[, c("Country.Name", "Country.Code", "GDP")]

# Match 4 variables with IUCN countries 
ctr<-read.csv("dataset/external_factors/gdp/Countries_crosswalk.csv")

# GDP

ctr$GDP<-gdp$GDP[match(ctr$Countries_WB, gdp$Country.Name)]
ctr$GDP[ctr$Countries_WB=="Korea, Dem. People's Rep."]<-18000000000/25778810 # Only country with missing data (for Gibraltar and St Martin I took UK and France), estimate taken from this site: https://tradingeconomics.com/north-korea/gdp and population size here: https://data.worldbank.org/indicator/SP.POP.TOTL?view=map
table(is.na(ctr$GDP)) # Antarctica is NA

# Create empty variables in species data frame
species$gdpOK<-species$nb_countries<-species$med_GDP<-NA

#  Loop to calculate the 4 variables

for(i in 1:nrow(species)){
  
  # Create countries table
  countries <- subset(countriesTOT, countriesTOT$taxonid==species$taxonid[i])
  #countries<-subset(countries, countries$origin %not in% c("Introduced", "Vagrant")) # Keep native, reintroduced, uncertain, assisted colonisation (remove introduced and vagrant)
  countriesUNIQ <- ddply(countries, .(Countries_IUCN), function(x){data.frame(N=nrow(x))})
  
  # GDP
  countriesUNIQ$gdp <- ctr$GDP[match(countriesUNIQ$Countries_IUCN, ctr$Countries_IUCN)]
  
  species$med_GDP[i] <- median(countriesUNIQ$gdp, na.rm=T)
  species$nb_countries[i] <- nrow(countriesUNIQ)
  species$gdpOK[i] <- table(factor(is.na(countriesUNIQ$gdp), levels=c("TRUE", "FALSE")))["FALSE"]
}

species.gdp.s <- species %>% dplyr::select(sps_name = scientific_name, med_GDP)

# Save data

write.table(species.gdp.s,"dataset/external_factors/gdp/sps_gdp.csv")


# Bioclimatic variables  --------------------------------------------------------
#' source Karger, D.N. (2018). Data from: CHELSAcruts - High resolution temperature and precipitation 
#' timeseries for the 20th century and beyond
#' \doi{/10.16904/ENVIDAT.159}

# 1965-1995 1 Mean annual air temperature
bc.1965.1 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_1.rst")
bc.1965.1 <- replace(bc.1965.1, bc.1965.1==-3.4e+38, NA)
crs(bc.1965.1) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 1965-1995 12 Annual precipitation amount
bc.1965.12 <-raster("dataset/external_factors/bioclim/1965_1995_bioclim/rast_bioclim_1965_1995_12.rst")
crs(bc.1965.12) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 1 Mean annual air temperature
bc.2005.1 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_1.rst")
bc.2005.1 <- replace(bc.2005.1, bc.2005.1==-3.4e+38, NA)
crs(bc.2005.1) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 

# 2005-2014 12 Annual precipitation amount
bc.2005.12 <-raster("dataset/external_factors/bioclim/2005_2014_bioclim/rast_bioclim_2005_2014_12.rst")
crs(bc.2005.12) <- "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs" 


# Projection of bird distribution polygons to match the coordinate system of the raster

dist.cea <- st_transform(dist.agg.ws84, crs="+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs") 

dist.bc <- dist.cea %>% dplyr::select(sci_nam)

bc <- stack(bc.1965.1,bc.1965.12,
            bc.2005.1,bc.2005.12)

# Extract raster median within birds range areas

dist.bc[, c("bc_1965_1_med", "bc_1965_12_med","bc_2005_1_med","bc_2005_12_med")] <- exact_extract(bc , dist.bc, "quantile", quantiles=c(0.5))

# Calculate difference

delta_bc_imp_med <- dist.bc %>% st_drop_geometry() %>% 
  mutate(Delta_BC_1_med=bc_2005_1_med-bc_1965_1_med,
         Delta_BC_12_med=bc_2005_12_med-bc_1965_12_med) %>% 
  dplyr::select(sci_nam,Temp_change_med = Delta_BC_1_med,Ppt_change_med = Delta_BC_12_med)

# Save data

write.table(delta_bc_imp_med, file="dataset/external_factors/bioclim/delta_2bc_imp_med.csv")

# Extract raster geometric mean within birds range areas

dist.bc[,c("bc_1965_1_gmean")] <- exact_extract(bc.1965.1, dist.bc, function(values, coverage_fraction)
  (exp(mean(log(values[values>0]*coverage_fraction),na.rm=T))))
dist.bc[,c("bc_1965_12_gmean")] <- exact_extract(bc.1965.12, dist.bc, function(values, coverage_fraction)
  (exp(mean(log(values[values>0]*coverage_fraction),na.rm=T))))
dist.bc[,c("bc_2005_1_gmean")] <- exact_extract(bc.2005.1, dist.bc, function(values, coverage_fraction)
  (exp(mean(log(values[values>0]*coverage_fraction),na.rm=T))))
dist.bc[,c("bc_2005_12_gmean")] <- exact_extract(bc.2005.12, dist.bc, function(values, coverage_fraction)
  (exp(mean(log(values[values>0]*coverage_fraction),na.rm=T))))

# Calculate difference

delta_bc_imp_gmean <- dist.bc %>% st_drop_geometry() %>% 
  mutate(Delta_BC_1_gmean=bc_2005_1_gmean-bc_1965_1_gmean,
         Delta_BC_12_gmean=bc_2005_12_gmean-bc_1965_12_gmean) %>% 
  dplyr::select(sci_nam, Temp_change_gmean = Delta_BC_1_gmean,Ppt_change_gmean = Delta_BC_12_gmean)

# Save data

write.table(delta_bc_imp_gmean, file="dataset/external_factors/bioclim/delta_2bc_imp_gmean.csv")

# END ----


