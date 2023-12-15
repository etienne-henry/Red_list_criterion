# Script to compile all data from different sources on bird intrinsic predictors. 
# It also matches the taxonomies. 

# Author: Etienne Henry
# Date: 10/2022

# Load packages ----------------------------------------------------------------

library("ggplot2")
library("dplyr")
library("tidyr")
library("tidyverse")
library("readxl")


# Taxonomic crosswalk -------------------------------------------------------- 

# Functions 

source("code/00_function_taxo_crosswalk.R")

# Taxonomic crosswalk table between BirdTree (Jetz) and Birdlife taxonomy


corres.taxo <- read.csv("dataset/birds/traits_data_birds/Jetz_BirdLife_crosswalk.csv")
names(corres.taxo) <- c("BL_name","J_name","Match_type")

# 1. Morphological traits: AVONET 2022 -------------------------------------------------------- 

avonet <- read.csv(file="dataset/birds/traits_data_birds/AVONET/ELEData/TraitData/AVONET1_BirdLife.csv",head=T)
#' source Tobias, J.A. (2022). Data from: AVONET: morphological, ecological and geographical data for all birds. 
#' Ecology Letters 25, 581–597. 
#' \doi{10.1111/ele.13898}

# Select for variables of interest

avo.traits <- avonet %>% dplyr::select(sps=Species1,beak.len=Beak.Length_Nares,
                                hwi=Hand.Wing.Index,
                                mass=Mass,mig_avonet=Migration,
                                trophic_lvl=Trophic.Level,trophic_niche=Trophic.Niche,
                                prim_ls=Primary.Lifestyle,
                                habitat=Habitat)


# Taxonomic crosswalk from AVONET 2022 to Birdlife:

avo.traits <- crosswalk_avonet_BL(avo.traits)
# 14 mismatchs after correction: 
# 3 species for which no distribution map is available, 10 news species, 1 lump (same species)
# see 00_function_taxo_crosswalk.R for more information

# Save data
# write.table( noc.traits, "dataset/birds/traits_data_birds/avo_traits.csv")

# 2. Behavioural traits: Tobias Pigot 2019 -------------------------------------------------------- 

behaviour <- read_excel("dataset/birds/behaviour_Tobias_Pigot_2019.xlsx")
#' source Tobias, J.A. & Pigot, A.L. (2019). Data from: Integrating behaviour and ecology into global biodiversity 
#' conservation strategies. Philosophical Transactions of the Royal Society B: Biological Sciences 374, 20190012
#' \doi{10.1098/rstb.2019.0012}

# Select for variables of interest

behaviour <- behaviour %>% dplyr::select(Species, NestPlacement,Territoriality,LogClutchSize,IslandDwelling) %>%
   mutate(Species = str_replace(Species, "_", " "))

# Taxonomic crosswalk from Tobias et Pigot 2019 to Birdlife

behaviour.mod <- crosswalk_Tobias19_BL(corres.taxo,behaviour)
# 25 mismatchs after correction: 15 extinct species, 10 with missing information (range not in Extent...)
# see 00_function_taxo_crosswalk.R for more information

# Save data
# write.table( noc.traits, "dataset/birds/traits_data_birds/behaviour_traits.csv")

# 3. Ecological traits: Birdlife traits -------------------------------------------------------- 

bl.attributes <- read_excel("dataset/birds/BL_Species_Attributes_2021.xlsx")
#' source BirdLife International (2022). Data from: IUCN Red List for birds. 
#' Downloaded from http://www.birdlife.org on 15/06/2022.

# Select for variables of interest

bl.traits <- bl.attributes %>% dplyr::select(sci_name=`Scientific name`,mig_BL=`Migratory status`,
                                         gen_len=`Generation length (years)`,endemic=`Endemic (breeds only in a single country)`,
                                         alt_min=`Min altitude (m)` , alt_max=`Max altitude (m)`,
                                         range_size= `EOO breeding`, forest_dep=`Forest dependency`)

# Save data
# write.table( noc.traits, "dataset/birds/traits_data_birds/birdlife_traits.csv")

# 4. Habitat breath: SIS connect from IUCN Red List -------------------------------------------------------- 

habitats.sis <- read.csv(file= "dataset/birds/habitats.csv")
#' source BirdLife International (2022). Data from: IUCN Red List for birds. 
#' Downloaded from http://www.birdlife.org on 15/06/2022.

# Species from IUCN 
sps.iucn <- readRDS("dataset/birds/Species.merged.april22.rds")
#' source BirdLife International (2022). Data from: IUCN Red List for birds. 
#' Downloaded from http://www.birdlife.org on 15/06/2022.

#List of species for which we have a distribution range 
sps.birds <- read.table("dataset/birds/list_sps_birds.txt")
sps.birds <- sps.birds$sci_nam

# SIS connect assessment df

assess <- read.csv(file= "dataset/birds/assessments.csv")
#' source BirdLife International (2022). Data from: IUCN Red List for birds. 
#' Downloaded from http://www.birdlife.org on 15/06/2022.

# Change the date to select the last assessment

sps.iucn$NewDate <- format(as.Date(sps.iucn$assessment_date), "%d/%m/%Y")
sps.iucn$id_date <- paste(sps.iucn$taxonid,"_",sps.iucn$NewDate)

assess$id_date <- paste(assess$internal_taxon_id,"_",assess$RedListAssessmentDate.value)

# Filter for the corresponding assessment

assess.f <- subset(assess, id_date %in% sps.iucn$id_date)
taxon_id <- assess.f$internal_taxon_id
assess_id <- assess.f$assessment_id

rm(assess,sps.iucn,sps.birds,assess.f)

# Select for the studied species and effective assessment

habitats.flt <- habitats.sis %>% 
  filter(internal_taxon_id %in% taxon_id) %>% 
  filter(assessment_id %in% assess_id) %>% 
  select(sps = internal_taxon_name,
         importance= GeneralHabitats.GeneralHabitatsSubfield.majorImportance,
         suitability= GeneralHabitats.GeneralHabitatsSubfield.suitability,
         season = GeneralHabitats.GeneralHabitatsSubfield.season ,
         class_habitats = GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsLookup)

# Select for Breeding and Resident habitat + Suitable habitat

habitats.flt <- habitats.flt %>% 
  filter(season == "Breeding Season" | season == "Resident" ) %>% 
  filter(suitability == "Suitable") 

# Count the number of main class habitats to define an index of habitat breadth

detach(package:plyr)

habitats.flt[,c("habitat_main","habitat_subfield")] <- str_split_fixed(habitats.flt$class_habitats, "[.]", 2)

habitats.b <- habitats.flt %>% dplyr::group_by(sps) %>% 
  summarize(n=n_distinct(habitat_main))

# Save data
# write.table( noc.traits, "dataset/birds/traits_data_birds/habitat_breadth.csv")

# 5. Nocturnality: Wilman et al. 2014 -------------------------------------------------------- 
#' source Wilman, H. (2014). Data from: EltonTraits 1.0: Species-level foraging attributes of 
#' the world’s birds and mammals. Ecology 95, 2027–2027.
#' \doi{10.1890/13-1917.1}

wilman.traits <- read_excel("dataset/birds/traits_data_birds/Wilman_2014_data_15_Jan2015.xlsx",sheet = 6 ) 

noc.traits <- wilman.traits[,c(1,26)]
names(noc.traits) <- c("Species","nocturnal")
noc.traits <- noc.traits[-1,]

# Taxonomic crosswalk from Wilman 2014 to Birdlife

noc.traits <- crosswalk_Wilman2014_BL(corres.taxo,noc.traits)
# 46 mismatchs after correction: 21 extinct species, 25 with missing information (range not in Extent...)

wilman.sps <- noc.traits$Species

# Save data
# write.table( noc.traits, "dataset/birds/traits_data_birds/nocturnal_traits.csv")


# END ----






   
   
