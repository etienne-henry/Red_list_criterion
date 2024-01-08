# Script to plot Figure 3

# Author: Etienne Henry
# Date: 10/2022

# Load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(RColorBrewer)

# Function color

f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)



# Birds predictors of extinction + estimates dataframe -------------------------------------------------------- 

df.crit.plot <- read.table("dataset/birds/processed_data/predictors_estimates_CLM_TH_nTH_TempPpt_noMarine.csv")

df.crit.plot <- df.crit.plot %>%  filter(predictor != "LC|NT" & predictor != "NT|VU" & predictor != "VU|EN" & predictor != "EN|CR" )

df.crit.plot$predictor <- factor(df.crit.plot$predictor,
                                 levels=c("beak_len_log" , "hwi" , "body_mass_log" ,
                                          "gen_len_log" , "LogClutchSize" ,  "mig_BLMigrant" ,"mig_BLNo_Migrant" ,
                                          "nocturnalnocturnal",  "nocturnalnon-nocturnal"  ,
                                          "IslandDwellingYes" ,"IslandDwellingNo",
                                          "trophic_nicheHerbivore" ,  "trophic_nicheOmnivore" ,  "trophic_nicheInvertivore" ,  "trophic_nichePredator" , 
                                          "forest_depNon-forest" , "forest_depLow" , "forest_depMedium" , "forest_depHigh" ,
                                          "habitats_b" , 
                                          "range_km2_log" , 
                                          "canopy_density_med_log" , 
                                          "canopy_change_med_log" , 
                                          "crop_cover_med_log" ,
                                          "crop_change_med_log", 
                                          "gdp_med_log" , 
                                          "p15_med_log" , 
                                          "var_pop_med_log" ,  "prop_rural" , 
                                          "cities_med_log" , 
                                          "Temp_change_med",
                                          "Ppt_change_med"),
                                 labels= c("Beak Length" , "HWI" , "Body Mass" ,
                                           "Generation Length" , "Clutch Size" ,  "Migration Yes" ,  "Migration No", 
                                           "Nocturnal Yes"  , "Nocturnal No" , "Insularity Yes" , "Insularity No" , 
                                           "Trophic Niche Herbivore" , "Trophic Niche Omnivore" , "Trophic Niche Invertivore" , "Trophic Niche Carnivor" , 
                                           "Forest Depedency None" ,  "Forest Depedency Low" ,  "Forest Depedency Medium" ,  "Forest Depedency High" , 
                                           "Habitats Breadth" ,
                                           "Range size" , 
                                           "Canopy density cover" , "Canopy density change" , "Cropland cover" , "Cropland change",
                                           "GDP" , "Population density" , "Population density change" ,  "Proportion rural" , 
                                           "Distance to cities" , 
                                           "Mean Annual Temperature Change","Annual Precipitation Amount Change"))

df.crit.plot <- df.crit.plot %>% mutate( predictor = factor(predictor, levels = rev(levels(predictor))))

df.crit.plot$criteria <- factor(df.crit.plot$criteria,
                                levels=c("RL_cat","A2","A3","A4","B1","B2","C1","C2","D1","D2"),
                                labels=c("All","A2","A3","A4","B1","B2","C1","C2","D1","D2"))



estimate_pos <- df.crit.plot %>% 
  filter(estimate >= 0)

quantile(estimate_pos$estimate, probs = seq(0, 1, 1/5))

# Cut according to quantile

table(cut(estimate_pos$estimate, 
          breaks = c(0.03502383,0.16132958,0.23598375,0.41905968,0.89319352,1.75892986)))
estimate_pos$cut_est <- cut(estimate_pos$estimate, 
                            breaks = c(0,0.16132958,0.23598375,0.41905968,0.89319352,1.75892986))


estimate_neg <- df.crit.plot %>% 
  filter(estimate < 0)

quantile(estimate_neg$estimate, probs = seq(0, 1, 1/5))

table(cut(estimate_neg$estimate, 
          breaks = c(-3.70761082 -0.97411637 -0.46433357 -0.26870494 -0.14458973,0.0)))
estimate_neg$cut_est <- cut(estimate_neg$estimate, 
                            breaks = c(-3.70661082,-0.97411637,-0.46433357,-0.26870494,-0.14458973,0))

a <- bind_rows(estimate_neg,estimate_pos)

df.crit.plot <- left_join(df.crit.plot, a)

factor(df.crit.plot$cut_est)

df.crit.plot$cut_est <- factor(df.crit.plot$cut_est, 
      levels=c("(0.893,1.76]", "(0.419,0.893]", "(0.236,0.419]","(0.161,0.236]", "(0,0.161]",
               "(-0.145,0]","(-0.269,-0.145]","(-0.464,-0.269]", "(-0.974,-0.464]","(-3.71,-0.974]"))


col <- c("#cc8d02","#FFB103","#ffc035","#ffd881","#ffefcc","#ccdaef","#99b5e0","#6690d1","#326bc2","#0047B3")


# Heat Map

ggplot(df.crit.plot , aes(x = criteria, y = predictor, fill = cut_est)) +
  geom_tile(color = "black")+
  scale_fill_manual(values = col,name = "CLMs Estimates") +
  coord_fixed() + xlab("Red List Criteria") + ylab("Predictors")+
  theme_test() + ylab("")+xlab("")

# END ---- 
