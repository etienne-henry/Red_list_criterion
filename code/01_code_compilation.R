# Script to compile all the information required for the analysis into a single data frame: 
# - IUCN category of species
# - category under each criterion
# - predictors of extinction

# Author: Etienne Henry
# Date: 10/2022

# Load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)


# Initialization -------------------------------------------------------- 

# Import functions 

source("code/01_function_RL_cat_extract.R")

# Lists

categories <- c("CR","EN","VU","NT")
list.criteria <- c('A1','A2',"A3","A4","B1","B2","C1","C2","D1","D2")

# Birdlife  Categorie and Criteria dataframe

BL.cat.init <- read.csv2("dataset/birds/BL_Category_Criteria_Rationale_2021.csv")
#' source BirdLife International (2022). Data from: IUCN Red List for birds. 
#' Downloaded from http://www.birdlife.org on 15/06/2022.

# Select marine (later removed from the analysis)

sps.marine <- BL.cat.init %>% 
  filter(Seabird == "Yes" & Landbird == "No") %>%
  dplyr::select(sci_nam=Scientific.name)

sps_marine <- sps.marine$sci_nam

# Save list
#write.table(sps_marine, "dataset/birds/sps_marine.txt")

rm(sps.marine, sps_marine)

# List of the 10,964 species for which we have a distribution area

sps.birds <- read.table("dataset/birds/list_sps_birds.txt")
sps.birds <- sps.birds$sci_nam

# Filter for birds species with distribution areas (see code 00_predictors_external)

BL.cat <- BL.cat.init %>% 
  dplyr::select(sci_nam=Scientific.name,RL_cat=RL.Category,Manual=Assessment.set.to.manual,
                crit_highest = Criteria.met.at.highest.level, 
                CR_criteria= CR.criteria, EN_criteria=EN.criteria,VU_criteria=VU.criteria) %>% 
  filter(sci_nam %in% sps.birds & RL_cat != "DD")

BL.cat[BL.cat == ""] <- NA

BL.cat$NT_criteria <- NA

# Reorganize manually assessed species

for (k in 1:nrow(BL.cat)) {
  if (BL.cat$Manual[k]== "VRAI"){
    if (grepl(BL.cat$RL_cat[k], "CR_criteria")){
      BL.cat$CR_criteria[k] <- BL.cat$crit_highest[k]
    }
    if (grepl(BL.cat$RL_cat[k], "EN_criteria")) {
      BL.cat$EN_criteria[k] <- BL.cat$crit_highest[k]
      BL.cat$CR_criteria[k] <- NA
    }
    if (grepl(BL.cat$RL_cat[k], "VU_criteria")) {
      BL.cat$VU_criteria[k] <- BL.cat$crit_highest[k]
      BL.cat$CR_criteria[k] <- NA
      BL.cat$EN_criteria[k] <- NA
    }

    if (grepl(BL.cat$RL_cat[k], "NT_criteria")) {
      BL.cat$NT_criteria[k] <- BL.cat$crit_highest[k]
      BL.cat$CR_criteria[k] <- NA
      BL.cat$EN_criteria[k] <- NA
      BL.cat$VU_criteria[k] <- NA
    }
  }
}


# Species x criteria matrix -------------------------------------------------------- 

# The function class_criteria_BL constructs a data frame with RL criteria in columns and classes for each criterion the category reached by a species. 

df <- data.frame(matrix(ncol = 13, nrow = nrow(BL.cat)))
colnames(df) <- c('sps','RL_cat', 'A1', 'A2',"A3","A4","B1","B2","C1","C2","D","D1","D2")
species.tab <- class_criteria_BL(BL.cat,df,categories)

# Fuse D column with D1

for (k in 1:nrow(species.tab)) {
  if (!is.na(species.tab$D[k]) & is.na(species.tab$D1[k])){
    species.tab$D1[k] <- species.tab$D[k]
  } else if (!is.na(species.tab$D[k]) & !is.na(species.tab$D1[k])) {
    if (from_cat_to_num(species.tab$D[k]) > from_cat_to_num(species.tab$D1[k])) {
      species.tab$D1[k] <- species.tab$D[k]
    }
  }
}
    
species.tab <- species.tab %>% dplyr::select(-D)

# Correction to ensure that no criterion reaches a higher category than the one that was finally evaluated (when manual assessment)

birds.criteria.cat.correct <- correction_lower_cat(species.tab,list.criteria)

# 1st hypothesis: we assume that a LC species is classified as LC under each criterion -> LC_inf

birds.criteria.cat.LCinf <- LC_inf(birds.criteria.cat.correct,list.criteria)

# 2nd hypothesis: we assume that a species that does not trigger a criterion is LC under that specific criterion -> LC_maybe
# (this implies that each model has the same number of species)

birds.criteria.cat.LCall <- LC_all(birds.criteria.cat.LCinf,list.criteria)

bird.iucn <- birds.criteria.cat.LCall

names(bird.iucn)[1] <- "sci_nam"


# Save data

#write.table(bird.iucn,"dataset/birds/processed_data/birds_criteria_cat.csv")

rm(BL.cat.init,NT.sps,df,species.tab,k,birds.criteria.cat.LCinf,birds.criteria.cat.LCall,
   class_criteria_BL,from_cat_to_num,LC_all,LC_inf,sps.birds,list.criteria,categories,BL.cat)


# I. Intrinsic traits -------------------------------------------------------- 

## 1. Ecological traits: Birdlife traits
#' source BirdLife International (2022). Data from: IUCN Red List for birds. 
#' Downloaded from http://www.birdlife.org on 15/06/2022.

bl.traits <- read.table("dataset/birds/traits_data_birds/birdlife_traits.csv")
#see 00_predictors_intrinsic.R for construction of the csv
bird.iucn$mig_BL <- bl.traits$mig_BL[match(bird.iucn$sci_nam, bl.traits$sci_name)]
bird.iucn$gen_len <- bl.traits$gen_len[match(bird.iucn$sci_nam, bl.traits$sci_name)]
bird.iucn$endemic <- bl.traits$endemic[match(bird.iucn$sci_nam, bl.traits$sci_name)]
bird.iucn$insularity <- bl.traits$insularity[match(bird.iucn$sci_nam, bl.traits$sci_name)]
bird.iucn$forest_dep <- bl.traits$forest_dep[match(bird.iucn$sci_nam, bl.traits$sci_name)]

# Log transformation 

bird.iucn$gen_len <- as.numeric(bird.iucn$gen_len) # +1 NA
bird.iucn$gen_len_log <- log(bird.iucn$gen_len) 

# Corrections 

bl.traits$endemic[is.na(bl.traits$endemic)] <- "Non endemic"
bl.traits$endemic[bl.traits$endemic=="Yes"] <- "Endemic"

# Consider unknown forest dependancy as non-forest (4 species)

bird.iucn$forest_dep[bird.iucn$forest_dep=="Unknown"] <- "Non-forest"

# Consider only migrant or no migrant (Unknown as no migrant, 16 species)

bird.iucn$mig_BL[bird.iucn$mig_BL=="Nomadic"] <- "Migrant"
bird.iucn$mig_BL[bird.iucn$mig_BL=="Altitudinal Migrant"] <- "Migrant"
bird.iucn$mig_BL[bird.iucn$mig_BL=="Full Migrant"] <- "Migrant"
bird.iucn$mig_BL[bird.iucn$mig_BL=="Unknown"] <- "No_Migrant"
bird.iucn$mig_BL[bird.iucn$mig_BL=="Not a Migrant"] <- "No_Migrant"

bird.iucn$mig_BL <- factor(bird.iucn$mig_BL, levels=c('No_Migrant','Migrant'))

rm(bl.traits) 

# Family

sps.fam <- readRDS("dataset/Species.merged.april22.rds")

sps.birds <- read.table("dataset/birds/list_sps_birds.txt")
sps.birds <- sps.birds$sci_nam

sps.fam <- sps.fam %>% 
  filter(class=="AVES" & scientific_name %in% sps.birds) %>%
  dplyr::select(sci_nam=scientific_name, family)

bird.iucn$family <- sps.fam$family[match(bird.iucn$sci_nam, sps.fam$sci_nam)]

rm(sps.birds,sps.fam)

## 2. Range size km2: distribution map 
#' source BirdLife International and Handbook of the Birds of the World (2021). Data from: Bird species distribution maps of the world. Version 2021.1
#' Available at http://datazone.birdlife.org/species/requestdis.

range.birds <- read.table("dataset/birds/traits_data_birds/range_km2.csv")
#see 00_predictors_external.R for construction of the csv

# Log transformation 

bird.iucn$range_km2 <- range.birds$range_km2[match(bird.iucn$sci_nam, range.birds$sci_nam)]
bird.iucn$range_km2_log  <- log(bird.iucn$range_km2 +1)

rm(range.birds)

## 3. Habitat breath

habitats.b <- read.table("dataset/birds/traits_data_birds/habitat_breadth.csv")
#see 00_predictors_intrinsic.R for construction of the csv

bird.iucn$habitats_b <- habitats.b$n[match(bird.iucn$sci_nam, habitats.b$sps)]
bird.iucn$habitats_b <- as.numeric(bird.iucn$habitats_b)

rm(habitats.b) 

## 4. Morphological: AVONET
#' source Tobias, J.A. (2022). Data from: AVONET: morphological, ecological and geographical data for all birds. 
#' Ecology Letters 25, 581–597. 
#' \doi{10.1111/ele.13898}

avo.traits <- read.table("dataset/birds/traits_data_birds/AVONET/avo_traits.csv")
#see 00_predictors_intrinsic.R for construction of the csv

bird.iucn$beak_len <- avo.traits$beak.len[match(bird.iucn$sci_nam, avo.traits$sps)]
bird.iucn$hwi <- avo.traits$hwi[match(bird.iucn$sci_nam, avo.traits$sps)]
bird.iucn$body_mass <- avo.traits$mass[match(bird.iucn$sci_nam, avo.traits$sps)]
bird.iucn$trophic_niche <- avo.traits$trophic_niche[match(bird.iucn$sci_nam, avo.traits$sps)]

# Log transformation 

bird.iucn$beak_len_log <- log(bird.iucn$beak_len)
bird.iucn$body_mass_log <- log(bird.iucn$body_mass+1)

# Reclassification of trophic niche from AVONET  (to lower number of categories)

bird.iucn$trophic_niche[bird.iucn$trophic_niche=="Omnivore"] <- "Omnivore"
bird.iucn$trophic_niche[bird.iucn$trophic_niche=="Frugivore"] <- "Herbivore"
bird.iucn$trophic_niche[bird.iucn$trophic_niche=="Invertivore"] <- "Invertivore"
bird.iucn$trophic_niche[bird.iucn$trophic_niche=="Herbivore terrestrial"] <- "Herbivore"
bird.iucn$trophic_niche[bird.iucn$trophic_niche=="Herbivore aquatic"] <- "Herbivore"
bird.iucn$trophic_niche[bird.iucn$trophic_niche=="Aquatic predator"] <- "Predator"
bird.iucn$trophic_niche[bird.iucn$trophic_niche=="Vertivore"] <- "Predator"
bird.iucn$trophic_niche[bird.iucn$trophic_niche=="Nectarivore"] <- "Herbivore"
bird.iucn$trophic_niche[bird.iucn$trophic_niche=="Granivore"] <- "Herbivore"
bird.iucn$trophic_niche[bird.iucn$trophic_niche=="Scavenger"] <- "Predator"
  
rm(avo.traits)

## 5. Behavioural traits: Tobias & Pigot 2019
#' source Tobias, J.A. &  Pigot, A.L. (2019). Data from: Integrating behaviour and ecology into global biodiversity 
#' conservation strategies. Philosophical Transactions of the Royal Society B: Biological Sciences 374, 20190012
#' \doi{10.1098/rstb.2019.0012}

behaviour.traits <- read.table("dataset/birds/traits_data_birds/behaviour_traits.csv")
bird.iucn$LogClutchSize <- behaviour.traits$LogClutchSize[match(bird.iucn$sci_nam, behaviour.traits$Species)]
bird.iucn$IslandDwelling <- behaviour.traits$IslandDwelling[match(bird.iucn$sci_nam, behaviour.traits$Species)]

rm(behaviour.traits)

## 6. Nocturnality: Wilman et al. 2014
#' source Wilman, H. (2014). Data from: EltonTraits 1.0: Species-level foraging attributes of 
#' the world’s birds and mammals. Ecology 95, 2027–2027.
#' \doi{10.1890/13-1917.1}

noc.traits <- read.table("dataset/birds/traits_data_birds/nocturnal_traits.csv")
bird.iucn$nocturnal <- noc.traits$nocturnal[match(bird.iucn$sci_nam, noc.traits$Species)]

bird.iucn$nocturnal <- factor(bird.iucn$nocturnal,levels=c(0,1),labels=c("non-nocturnal","nocturnal"))

rm(noc.traits)

# II. External factors -------------------------------------------------------- 

### Habitat alteration

## Canopy density extent
#' source Remelgado, R. & Meyer, C (2023). Data from: Systematic losses in tree-canopy cover over three decades 
#' revealed by integrating complementary data sources (preprint)
#' \doi{10.31223/X5T68Z}

canopy.cover <- read.table("dataset/external_factors/iGFC/canopy_density_Q10_med_Q9_gmean.csv")

bird.iucn$canopy_density_Q10 <- canopy.cover$canopy_density_Q10[match(bird.iucn$sci_nam, canopy.cover$sci_nam)]
bird.iucn$canopy_density_med <- canopy.cover$canopy_density_med[match(bird.iucn$sci_nam, canopy.cover$sci_nam)]
bird.iucn$canopy_density_Q90 <- canopy.cover$canopy_density_Q90[match(bird.iucn$sci_nam, canopy.cover$sci_nam)]

## Canopy change

canopy.change <- read.table("dataset/external_factors/iGFC/canopy_change_Q10_med_Q90.csv")

bird.iucn$canopy_change_med <- canopy.change$canopy_change_med[match(bird.iucn$sci_nam, canopy.change$sci_nam)]

rm(canopy.cover,canopy.change)

# Log transformation 

bird.iucn$canopy_density_med_log <- log(bird.iucn$canopy_density_med+1)
bird.iucn$canopy_change_med_log <- sign(bird.iucn$canopy_change_med)*log(abs(bird.iucn$canopy_change_med)+1)

## Cropland extent and changes
#' source Potapov, P. (2022). Data from: Global maps of cropland extent and change show accelerated cropland 
#' expansion in the twenty-first century. Nat Food 3, 19–
#' \doi{10.1038/s43016-021-00429-z}

cropland <- read.table("dataset/external_factors/cropland_Potapov2021/crop_cover_change_Q10_med_Q90.csv")
cropland.gmean <- read.table("dataset/external_factors/cropland_Potapov2021/crop_cover_change_geom_mean.csv")

bird.iucn$crop_change_Q10 <- cropland$crop_change_Q10[match(bird.iucn$sci_nam, cropland$sci_nam)]
bird.iucn$crop_change_med <- cropland$crop_change_med[match(bird.iucn$sci_nam, cropland$sci_nam)]
bird.iucn$crop_change_Q90 <- cropland$crop_change_Q90[match(bird.iucn$sci_nam, cropland$sci_nam)]
bird.iucn$crop_change_gmean <- cropland.gmean$crop_change_geom_mean[match(bird.iucn$sci_nam, cropland.gmean$sci_nam)]

bird.iucn$crop_cover_Q10 <- cropland$crop_cover_Q10[match(bird.iucn$sci_nam, cropland$sci_nam)]
bird.iucn$crop_cover_med <- cropland$crop_cover_med[match(bird.iucn$sci_nam, cropland$sci_nam)]
bird.iucn$crop_cover_Q90 <- cropland$crop_cover_Q90[match(bird.iucn$sci_nam, cropland$sci_nam)]
bird.iucn$crop_cover_gmean <- cropland.gmean$crop_cover_geom_mean[match(bird.iucn$sci_nam, cropland.gmean$sci_nam)]

rm(cropland,cropland.gmean)

# Log transformation 

bird.iucn$crop_change_med_log <- sign(bird.iucn$crop_change_med)*log(abs(bird.iucn$crop_change_med)+1)
bird.iucn$crop_change_gmean_log <- sign(bird.iucn$crop_change_gmean)*log(abs(bird.iucn$crop_change_gmean)+1)

bird.iucn$crop_cover_med_log <- log(bird.iucn$crop_cover_med+1)
bird.iucn$crop_cover_gmean_log <- log(bird.iucn$crop_cover_gmean+1)

### Human encroachment

## Median GDP 
#' source Worldbank (2021). Data from: GDP per Capita. World Bank Development Indicators, The World Bank Group
#' Accessed at https://data.worldbank.org/indicator/NY.GDP.CAP.CD 01-11-2021.

sps.gdp <- read.table("dataset/external_factors/gdp/sps_gdp.csv")

bird.iucn$gdp_med <- sps.gdp$med_GDP[match(bird.iucn$sci_nam, sps.gdp$sps_name)]

rm(sps.gdp)

bird.iucn$gdp_med_log <- log(bird.iucn$gdp_med + 1)

## Human population density and change 
#' source Schiavina, M. (2019). Data from: GHS-POP R2019A - GHS population grid multitemporal (1975-1990-2000-2015).
#' \doi{10.2905/0C6B9751-A71F-4062-830B-43C9F432370F1}

dist.pop.2000 <- read.table("dataset/external_factors/pop_dens/dist_pop_2000_Q10_med_Q90_gmean.csv")
dist.pop.2015 <- read.table("dataset/external_factors/pop_dens/dist_pop_2015_Q10_med_Q90_sum_gmean.csv")

# 2000

bird.iucn$p00_Q10<-dist.pop.2000$p00_Q10[match(bird.iucn$sci_nam, dist.pop.2000$sci_nam)]
bird.iucn$p00_med<-dist.pop.2000$p00_med[match(bird.iucn$sci_nam, dist.pop.2000$sci_nam)]
bird.iucn$p00_Q90<-dist.pop.2000$p00_Q90[match(bird.iucn$sci_nam, dist.pop.2000$sci_nam)]
bird.iucn$p00_gmean<-dist.pop.2000$p00_geom_mean[match(bird.iucn$sci_nam, dist.pop.2000$sci_nam)]

# 2015

bird.iucn$p15_Q10<-dist.pop.2015$p15_Q10[match(bird.iucn$sci_nam, dist.pop.2015$sci_nam)]
bird.iucn$p15_med<-dist.pop.2015$p15_med[match(bird.iucn$sci_nam, dist.pop.2015$sci_nam)]
bird.iucn$p15_Q90<-dist.pop.2015$p15_Q90[match(bird.iucn$sci_nam, dist.pop.2015$sci_nam)]
bird.iucn$p15_gmean<-dist.pop.2015$p15_geom_mean[match(bird.iucn$sci_nam, dist.pop.2015$sci_nam)]
bird.iucn$p15_sum<-dist.pop.2015$p15_sum[match(bird.iucn$sci_nam, dist.pop.2015$sci_nam)]

# Population density change
# Formula: (Population 2015-Population 2000)/Population 2000 and:
#   - when pop_2000 = 0 et pop_2015 > 0 --> +1
#   - when pop_2000 = 0 et pop_2015 = 0 --> 0

bird.iucn <- bird.iucn %>% 
  mutate(var_pop_Q10 = ifelse(p00_Q10 > 0, (p15_Q10 - p00_Q10)/p00_Q10, ifelse(p00_Q10 == 0 & p15_Q10 > 0, 1, 0)),
         var_pop_med = ifelse(p00_med > 0, (p15_med - p00_med)/p00_med, ifelse(p00_med == 0 & p15_med > 0, 1, 0)), 
         var_pop_Q90 = ifelse(p00_Q90 > 0, (p15_Q90 - p00_Q90)/p00_Q90, ifelse(p00_Q90 == 0 & p15_Q90 > 0, 1, 0)),
         var_pop_gmean = ifelse(p00_gmean > 0, (p15_gmean - p00_gmean)/p00_gmean, ifelse(p00_gmean == 0 & p15_gmean > 0, 1, 0)))  %>% 
  dplyr::select(- p00_Q10, - p00_med, - p00_Q90, - p00_gmean)

rm(dist.pop.2000,dist.pop.2015)

# Log transformation 

bird.iucn$p15_med_log <- log(bird.iucn$p15_med +1)
bird.iucn$p15_gmean_log <- log(bird.iucn$p15_gmean +1)
bird.iucn$var_pop_med_log <- sign(bird.iucn$var_pop_med)*log(abs(bird.iucn$var_pop_med)+1)
bird.iucn$var_pop_gmean_log <- sign(bird.iucn$var_pop_gmean)*log(abs(bird.iucn$var_pop_gmean)+1)

## Proportion of rural people 

human_r_pop <- read.table("dataset/external_factors/human_r_pop/rural_sum.csv")

bird.iucn$rural_sum <- human_r_pop$rural_sum[match(bird.iucn$sci_nam, human_r_pop$sci_nam)]

# Calculation of the proportion

bird.iucn <- bird.iucn %>% 
  mutate(prop_rural = ifelse(p15_sum > 0, (rural_sum / p15_sum)*100, 0)) %>% 
  dplyr::select(- rural_sum, - p15_sum)
  
# Log transformation 

bird.iucn$prop_rural_log <- log(bird.iucn$prop_rural + 1)

rm(human_r_pop)

## Time travel to cities
#' source Weiss, D.J. (2018). Data from: A global map of travel time to cities to assess inequalities 
#' in accessibility in 2015. Na-ture 553, 333–336
#' \doi{10.1038/nature25181}

dist.cities <- read.table("dataset/external_factors/cities_Weiss2018/cities_Q10_med_Q90_gmean.csv")
bird.iucn$cities_Q10 <- dist.cities$cities_Q10[match(bird.iucn$sci_nam, dist.cities$sci_nam)]
bird.iucn$cities_med <- dist.cities$cities_med[match(bird.iucn$sci_nam, dist.cities$sci_nam)]
bird.iucn$cities_Q90 <- dist.cities$cities_Q90[match(bird.iucn$sci_nam, dist.cities$sci_nam)]
bird.iucn$cities_gmean <- dist.cities$cities_geom_mean[match(bird.iucn$sci_nam, dist.cities$sci_nam)]

rm(dist.cities)

# Log transformation 

bird.iucn$cities_med_log <- log(bird.iucn$cities_med + 1)
bird.iucn$cities_gmean_log <- log(bird.iucn$cities_gmean + 1)

### Climate change

## Bioclimatic variables
#' source Karger, D.N. (2018). Data from: CHELSAcruts - High resolution temperature and precipitation 
#' timeseries for the 20th century and beyond
#' \doi{/10.16904/ENVIDAT.159}

# Median

bc.delta.med <- read.table("dataset/external_factors/bioclim/delta_2bc_imp_med.csv")

bird.iucn$Temp_change_med <- bc.delta.med$Temp_change_med[match(bird.iucn$sci_nam, bc.delta.med$sci_nam)]
bird.iucn$Ppt_change_med <- bc.delta.med$Ppt_change_med[match(bird.iucn$sci_nam, bc.delta.med$sci_nam)]

# Geometric mean

bc.delta.gmean <- read.table("dataset/external_factors/bioclim/delta_2bc_imp_gmean.csv")

bird.iucn$Temp_change_gmean <- bc.delta.gmean$Temp_change_gmean[match(bird.iucn$sci_nam, bc.delta.gmean$sci_nam)]
bird.iucn$Ppt_change_gmean <- bc.delta.gmean$Ppt_change_gmean[match(bird.iucn$sci_nam, bc.delta.gmean$sci_nam)]

rm(bc.delta.med,bc.delta.gmean)

# Save final dataset  -------------------------------------------------------- 

write.table(bird.iucn, "dataset/birds/processed_data/birds_ER_predictors_range_noAUTO.csv")

# END ----

