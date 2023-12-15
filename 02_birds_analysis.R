# Script that evaluates the performance of the criteria-specific approach and compares it to the category-specific approach. 
# It first selects the predictors of importance for each criterion, then runs a block taxonomic validation with the selected model 
# and finally predicts the RL category of each species under each criterion.


# Author: Etienne Henry

# Load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(corrplot)
library(ordinal)
library(lme4)
library(MuMIn)
library("MESS")
library(MASS)
'%not in%' <- Negate('%in%')

# Directory -------------------------------------------------------- 

#setwd(dir="~/share/homes/mq21utob/")
#setwd("~/share/homes/mq21utob/Criteria_specific_sRedList")
setwd(dir="/Volumes/homes/mq21utob/Criteria_specific_sRedList/")

# Initialization -------------------------------------------------------- 

# Import functions 

source("code/02_function_analysis.R")

# Birds predictors of extinction dataframe

#birds.predict <- read.table("dataset/birds/processed_data/birds_ER_predictors_range.csv")

birds.predict <- read.table("dataset/birds/processed_data/birds_ER_predictors_range_noAUTO.csv")

# Filter for marine species 

sps.marine <- read.table("dataset/birds/sps_marine.txt")

birds.predict <- birds.predict %>%
  filter(sci_nam %not in% sps.marine$x)

# Criteria

list_criteria <- c("RL_cat","A2","A3","A4","B1","B2","C1","C2","D1","D2")

# Categories

# No hypothesis 
#lc <- c("LC","LC_inf")

# Hypothesis 
lc <- c("LC","LC_inf","LC_maybe")
vu <- c("VU")
en <- c("EN")
cr <- c("CR")

non_threat <- c("LC","NT")
threat <- c("VU","EN","CR")


# Data processing  -------------------------------------------------------- 

# Scale numerical variables and factor categorial ones

birds.predict.p <- birds.predict %>% 
  dplyr::select(-RL_cat, -A1,-A2,-A3,-A4,-B1,-B2,-C1,-C2,-D1,-D2) 

birds.predict.s <- birds.predict.p %>% 
  mutate_if(is.numeric,scale) %>% 
  mutate_if(is.numeric,as.numeric) %>% 
  mutate_if(is.character,factor)

birds.predict.s$forest_dep <- factor(birds.predict.s$forest_dep, levels=c('Non-forest','Low','Medium','High'))
birds.predict.s$mig_BL <- factor(birds.predict.s$mig_BL, levels=c('No_Migrant','Migrant'))

birds.predict.join <- birds.predict %>% 
  dplyr::select(sci_nam, RL_cat,A2,A3,A4,B1,B2,C1,C2,D1,D2)

df.analysis <- left_join(birds.predict.join,birds.predict.s)

# Ordinate level of threat 

for (criteria in list_criteria){
  df.analysis[,c(criteria)][df.analysis[,c(criteria)] %in% lc] <- "LC"
  df.analysis[,c(criteria)][df.analysis[,c(criteria)] %in% vu] <- "VU"
  df.analysis[,c(criteria)][df.analysis[,c(criteria)] %in% en] <- "EN"
  df.analysis[,c(criteria)][df.analysis[,c(criteria)] %in% cr] <- "CR"
  df.analysis[,c(criteria)] <- factor(df.analysis[,c(criteria)], levels=c("LC","NT","VU","EN","CR"), ordered = TRUE)
  
}

rm(birds.predict,birds.predict.join,birds.predict.p,birds.predict.s)

# Investigate correlations between predictors

# a. Between numeric predictors

df <- df.analysis[variable.full]
corr.num <- df %>% 
  dplyr::select_if(is.numeric)

M.flt <- na.omit(corr.num)
M = cor(M.flt ,method = c("pearson"))
M.df <- as.data.frame(M)
#rite.csv(M.df,"/sDiv/Etienne/Criteria_specific_sRedList/dataset/correlation_num.csv")
corrplot(M, method = 'circle')
corrplot(M, method = 'number')

rm(corr.num, M.flt,M,df)

# b. Between factorial predictors

corr.fac <- df.analysis %>% 
  dplyr::select_if(is.factor)

mytable <- table(corr.fac$nocturnal, corr.fac$IslandDwelling)

# Goodman-Kruskal's gamma
MESS::gkgamma(mytable)

# Khi 2 test
chisq.test(mytable)

rm(corr.fac,mytable)

# I. Modelisation  -------------------------------------------------------

df.analysis <- df.analysis %>% 
  dplyr::select(-endemic)

# List of predictors 

variable.full <- c("beak_len_log" , "hwi" , "body_mass_log",
                   "gen_len_log" , "LogClutchSize" , "mig_BL" , "nocturnal" , "IslandDwelling" ,
                   "trophic_niche" , "forest_dep" , "habitats_b" ,
                   "range_km2_log" ,
                   "canopy_density_med_log" , "canopy_change_med_log" ,
                   "crop_cover_med_log" , "crop_change_med_log" ,
                   "gdp_med_log" ,
                   "p15_med_log" , "var_pop_med_log" , "prop_rural",
                   "cities_med_log" ,
                   "Temp_change_med","Ppt_change_med")

list.model.formula <- list()
list.model.df <- list()
predictors.estimates <- data.frame()
summary.model <- data.frame()

# Model selection 

for (k in 1:length(list_criteria)){
  
  # Function that selects the predictors of importance for each criterion and returns a list of : 
  model.select  <- per_criteria_model_selection(df.analysis,list_criteria[k])
  # 1. The model summary (n, AIC, BIC)
  summary.model <- bind_rows(summary.model,model.select[1]) 
  # 2. The selected predictors, their estimate and their p-value.
  predictors.estimates <- bind_rows(predictors.estimates,model.select[2])
  # 3. The model formula
  list.model.formula[[k]] <- model.select[[3]]
  # 4. A dataframe without NA for each criterion
  list.model.df[[k]] <- model.select[4]
  
  print(list_criteria[k])
}

rm(model.select)


# Save data predictors estimates

## CLMs

# Weight CLM Threatened VS non Threatened
#write.table(predictors.estimates,"dataset/birds/processed_data/predictors_estimates_CLM_TH_nTH_TempPpt.csv")
# Weight CLM Threatened VS non Threatened no marine
#write.table(predictors.estimates,"dataset/birds/processed_data/predictors_estimates_CLM_TH_nTH_TempPpt_noMarine.csv")
# Weight CLM per category
#write.table(predictors.estimates,"dataset/birds/processed_data/predictors_estimates_CLM_category.csv")
# Weight CLM LC VS non LC
#write.table(predictors.estimates,"dataset/birds/processed_data/predictors_estimates_CLM_LC_nLC.csv")

## GLMs
#write.table(predictors.estimates,"dataset/birds/processed_data/predictors_estimates_glm.csv")

# II. Block taxonomic validation -------------------------------------------------------

# II.1. Validation with class

df.model.validation <- df.analysis %>% dplyr::select(family,sci_nam,RL_cat,A2,A3,A4,B1,B2,C1,C2,D1,D2)

for (k in 1:length(list_criteria)){
  
  criteria <- list_criteria[k]
  
  # Function that return a dataframe with the predicted classes of the test dataset for each criteria
  
  df.validation.output <- per_criteria_taxo_validation(model.formula=list.model.formula[[k]],
                                                       model.df=data.frame(list.model.df[[k]]),
                                                       criteria )

  # Join the actual categories and the predicted ones during the validation for each criterion
  
  df.model.validation <- left_join(df.model.validation, df.validation.output)

  print(list_criteria[k])
}

# II.2. Validation with class and probabilities 
#
## Return list of one df for each criterion: df contains probabilities of prediction for each category  
# 
# df.proba.validation <- list()
# 
# for (k in 1:length(list_criteria)){
#   
#   criteria <- list_criteria[k]
#   df.validation.output <- per_criteria_taxo_validation_probas(model.formula=list.model.formula[[k]],
#                                                        model.df=data.frame(list.model.df[[k]]),
#                                                        criteria )
#   
#   df.proba.validation[[k]] <- df.validation.output
#   
#   print(list_criteria[k])
# }
# 
# rm(criteria, k, df.validation.output)


## II.A. Binary (Threat/non_Threat) validation -------------------------------------------------------

# Validation table

df.accuracy <- data.frame()

for (criteria in list_criteria){
  
  # Function that returns for each criterion the performance indexes of the model (accuracy, specificity, sensitiy and TSS)
  # Binary validation (i.e., considering Threat vq Non-threat categories)
  
  df.data <- criteria_binary_validation_table(df.model.validation, criteria )
  df.accuracy <- bind_rows(df.accuracy,df.data)
  
}

rm(criteria, k, df.data)

# Combined validation: selecting for the highest category of threat predicted among all criteria

df.general.validation <- df.model.validation %>% 
  dplyr::select(sci_nam,RL_cat,Predict_validation.RL_cat,Predict_validation.A2,Predict_validation.A3,
                Predict_validation.A4,Predict_validation.B1,Predict_validation.B2,Predict_validation.C1,Predict_validation.C2,
                Predict_validation.D1,Predict_validation.D2)

df.general.validation$CS <- NA

for (k in 1:nrow(df.general.validation)){

  predicted.cat <- as.numeric(df.general.validation[k,4:12])

  worst.class <- ifelse(length(na.omit(predicted.cat)) > 0, max(na.omit(predicted.cat)),NA)

  df.general.validation$CS[k] <- worst.class

}

df.general.validation$CS <- factor(df.general.validation$CS, 
                                                          levels=c(1,2,3,4,5),
                                                          labels=c("LC","NT","VU","EN","CR"))


df.acc <- df.general.validation %>% 
  dplyr::select(RL_cat,CS)

df.acc.flt <- na.omit(df.acc) %>% mutate_if(is.factor, as.character)

df.acc.flt$RL_cat[df.acc.flt$RL_cat %in% non_threat] <-  "None Threat"
df.acc.flt$RL_cat[df.acc.flt$RL_cat %in% threat] <- "Threat"
df.acc.flt$RL_cat <- factor( df.acc.flt$RL_cat,levels=c("None Threat","Threat"))

df.acc.flt$CS[df.acc.flt$CS %in% non_threat] <-  "None Threat"
df.acc.flt$CS[df.acc.flt$CS %in% threat] <- "Threat"
df.acc.flt$CS <- factor( df.acc.flt$CS,levels=c("None Threat","Threat"))

table.criteria <- table( df.acc.flt$RL_cat,df.acc.flt$CS)

True_threat <- as.numeric(table.criteria[2,2])
True_nonthreat  <- as.numeric(table.criteria[1,1]) 

nb.sps.non.th <- as.numeric(table(df.acc.flt$RL_cat)[1])
nb.sps.th <- as.numeric(table(df.acc.flt$RL_cat)[2])
pour.sps.th <- (nb.sps.th/(nb.sps.non.th+nb.sps.th))*100

# Indexes

accuracy <- (True_nonthreat+True_threat)/nrow(df.acc.flt)
sensitivity <- (True_threat / nb.sps.th)
specificity <- (True_nonthreat / nb.sps.non.th)

TSS <- (specificity + sensitivity - 1)

df.data <- data.frame(criteria="CS",n=nrow(df.general.validation %>% filter(!is.na(CS))),
                      pourcentage_threat = pour.sps.th ,
                      accuracy,sensitivity,
                      specificity,TSS) 

df.accuracy <- bind_rows(df.accuracy,df.data)


### Average error

df.validation.average.err <- left_join(df.analysis %>% 
                                      dplyr::select(sci_nam, A2,A3,A4,B1,B2,C1,C2,D1,D2), df.general.validation) %>% 
  mutate_if(is.factor, as.character)

list_criteria.cs <- c(list_criteria,"CS")

# Calculate average number of difference between the actual and predicted category
df.average.err <- average_error(list_criteria.cs ,df.validation.average.err )

a <- data.frame(RL_cat = mean(df.average.err$average_err_RL_cat,na.rm = T), 
                A2 = mean(df.average.err$average_err_A2,na.rm = T),
                A3 = mean(df.average.err$average_err_A3,na.rm = T),
                A4 = mean(df.average.err$average_err_A4,na.rm = T),
                B1 = mean(df.average.err$average_err_B1,na.rm = T),
                B2 = mean(df.average.err$average_err_B2,na.rm = T),
                C1 = mean(df.average.err$average_err_C1,na.rm = T),
                C2 = mean(df.average.err$average_err_C2,na.rm = T),
                D1 = mean(df.average.err$average_err_D1,na.rm = T),
                D2 = mean(df.average.err$average_err_D2,na.rm = T),
                CS = mean(df.average.err$average_err_CS,na.rm = T)) %>% 
  pivot_longer( everything(),names_to = "criteria",values_to = "average_error")

df.accuracy.f <- left_join(df.accuracy,a)

rm(list_criteria.cs,df.validation.average.err,df.average.err,a)

## Save data

# Weight CLM Threatened VS non Threatened
#write.table(df.accuracy.f,"dataset/birds/processed_data/accuracy_validation_binary_TH_nTH_noLCmaybe.csv")
# Weight CLM Threatened VS non Threatened
#write.table(df.accuracy,"dataset/birds/processed_data/accuracy_validation_binary_TH_nTH_noLCmaybe_nomarine.csv")
# Weight CLM per category
#write.table(df.accuracy.f,"dataset/birds/processed_data/accuracy_validation_binary_category.csv")
# Weight CLM LC VS non LC
#write.table(df.accuracy.f,"dataset/birds/processed_data/accuracy_validation_binary_LC_nLC.csv")

# GLM 
#write.table(df.accuracy,"dataset/birds/processed_data/accuracy_validation_binary_glm.csv")

## II.B. Per category validation -------------------------------------------------------

# Validation table

df.accuracy.cat <- data.frame()

for (criteria in list_criteria){
  
  # Function that returns for each criterion the performance indexes of the model (accuracy, specificity, sensitiy and TSS)
  # per category validation (i.e., considering each category independantly)
  
  df.data.cat <- criteria_category_validation_table(df.model.validation, criteria )
  df.accuracy.cat <- bind_rows(df.accuracy.cat,df.data.cat)
  
}

rm(criteria, k, df.data.cat)

# Combined validation: selecting for the higheest cateogry of threat predicted amoung all criteria

df.cat.validation <- df.model.validation %>% 
  dplyr::select(sci_nam,RL_cat,Predict_validation.RL_cat,Predict_validation.A2,Predict_validation.A3,
                Predict_validation.A4,Predict_validation.B1,Predict_validation.B2,Predict_validation.C1,Predict_validation.C2,
                Predict_validation.D1,Predict_validation.D2)

df.cat.validation$Prediction_worst <- NA


for (k in 1:nrow(df.cat.validation)){
  
  predicted.cat <- as.numeric(df.cat.validation[k,3:12])
  
  worst.class <- ifelse(length(na.omit(predicted.cat)) > 0, max(na.omit(predicted.cat)),NA)
  
  df.cat.validation$Prediction_worst[k] <- worst.class
  
}

df.cat.validation$Prediction_worst <- factor(df.cat.validation$Prediction_worst, 
                                                 levels=c(1,2,3,4,5),
                                                 labels=c("LC","NT","VU","EN","CR"))

df.acc.cat <- df.cat.validation %>% 
  dplyr::select(RL_cat,Prediction_worst)

df.acc.cat.flt <- na.omit(df.acc.cat) %>% mutate_if(is.factor, as.character)

m <- matrix(0, ncol =8, nrow = 1)
df.data <- data.frame(m)
colnames(df.data) <- c("accuracy.CR","accuracy.EN","accuracy.VU","accuracy.NT","accuracy.LC","sensitivity","specificity","TSS")

list.category <- list("CR","EN","VU","NT","LC")

table.criteria <- as.data.frame(table( df.acc.cat.flt$RL_cat,df.acc.cat.flt$Prediction_worst))
names(table.criteria) <- c("actual", "predicted","freq")

for (k in 1:length(list.category)){
  
  category <- list.category[k]
  
  accuracy <- ifelse(sum(table.criteria$actual== category) > 0 & sum(table.criteria$predicted==category) > 0, 
                     (table.criteria$freq[table.criteria$actual==category & table.criteria$predicted == category] ) /
                       sum(table.criteria$freq[table.criteria$actual == category] ) ,0)
  
  df.data[1,k] <- accuracy
  
}


sensitivity <- ( ifelse(sum(table.criteria$actual=="CR") > 0 & sum(table.criteria$predicted=="CR") > 0, 
                        table.criteria$freq[table.criteria$actual=="CR" & table.criteria$predicted == "CR"],0) +
                   ifelse(sum(table.criteria$actual=="EN") > 0 & sum(table.criteria$predicted=="EN") > 0, 
                          table.criteria$freq[table.criteria$actual=="EN" & table.criteria$predicted == "EN"],0) +
                   ifelse(sum(table.criteria$actual=="VU") > 0 & sum(table.criteria$predicted=="VU") > 0, 
                          table.criteria$freq[table.criteria$actual=="VU" & table.criteria$predicted == "VU"],0) ) /
  sum(table.criteria$freq[table.criteria$actual %in% threat])


specificity <- ( ifelse(sum(table.criteria$actual=="LC") > 0 & sum(table.criteria$predicted=="LC") > 0, 
                        table.criteria$freq[table.criteria$actual=="LC" & table.criteria$predicted == "LC"],0) +
                   ifelse(sum(table.criteria$actual=="NT") > 0 & sum(table.criteria$predicted=="NT") > 0, 
                          table.criteria$freq[table.criteria$actual=="NT" & table.criteria$predicted == "NT"],0)  ) /
  sum(table.criteria$freq[table.criteria$actual %in% non_threat])

TSS <- (specificity + sensitivity - 1)

df.data$sensitivity[1] <- sensitivity
df.data$specificity[1] <- specificity
df.data$TSS[1] <- TSS

df.data <- df.data %>% mutate(criteria="CS")

df.accuracy.cat <- bind_rows(df.accuracy.cat,df.data)


# Save data

# Per category validation, CLM weight Threatened vs non Threatened 

# write.table(df.accuracy.cat,"dataset/birds/processed_data/accuracy_validation_per_cat_TH_nTH.csv")

# III. Model Prediction -------------------------------------------------------

# 1. Prediction per criteria 

df.predict <-  df.analysis %>%
  dplyr::select(family,sci_nam,RL_cat,
                beak_len_log , hwi , body_mass_log,
                gen_len_log , LogClutchSize , mig_BL , nocturnal , IslandDwelling ,
                trophic_niche , forest_dep , habitats_b ,
                range_km2_log ,
                canopy_density_med_log , canopy_change_med_log ,
                crop_cover_med_log , crop_change_med_log ,
                gdp_med_log ,
                p15_med_log , var_pop_med_log , prop_rural,
                cities_med_log ,
                Temp_change_med,Ppt_change_med)

# Select species with 

list_sps_model <- data.frame(list.model.df[1])$sci_nam


df.predict <- df.predict %>% 
  filter(sci_nam %in% list_sps_model)

a <- df.analysis %>% 
  filter(sci_nam %in% list_sps_model)

names(a)



hist(a$body_mass_log, xlab = "Log Body Mass", ylab = "Numbers of species", 
     main = "Distribution of log(Bodymass) for the 8,695 species included in the analysis")


ggplot(a, aes(x=body_mass_log)) + 
  geom_bar() +
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle("Distribution of RedList categories for the 8,695 species included in the analysis") +
  xlab("RedList categories") +
  ylab("Number of species") 


b <- df.analysis %>% 
  filter(sci_nam %not in% list_sps_model)

names(a)

ggplot(b, aes(x=RL_cat)) + 
  geom_bar()


ggplot(b, aes(x=RL_cat, fill=RL_cat)) + 
  geom_bar() +
  scale_fill_manual(values=c("#01AF50","#92D04F","#FEFD32","#FFC024","#FE2905"))+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle("Distribution of RedList categories for the 1,864 species excluded from the analysis") +
  xlab("RedList categories") +
  ylab("Number of species") 

hist(b$body_mass_log, xlab = "Log Body Mass", ylab = "Numbers of species", 
     main = "Distribution of log(Bodymass) for the 1,864 species excluded from the analysis")


# Function that return all the RL categories predicted by the model for each species under each criterion 
df.predict <- model_prediction(df.predict, list.model.formula,list.model.df,list_criteria)


df.predict <- df.predict %>% 
  dplyr::select(family, sci_nam,RL_cat,prediction_RL_cat,prediction_A2,prediction_A3,
  prediction_A4,prediction_B1,prediction_B2,prediction_C1,prediction_C2,
  prediction_D1,prediction_D2)


# 2. Confusion matrix

df.predict.mat <- df.predict

df.predict.mat$RL_cat <- as.character(df.predict.mat$RL_cat)
df.predict.mat$RL_cat <- factor(df.predict.mat$RL_cat, 
                            levels=c("LC","NT","VU","EN","CR"))
                            #labels=c("None Threat","None Threat","Threat","Threat","Threat"))

# 2.a. Prediction category-specific approach vs criteria-specific

# Extract highest category predicted among criteria 

for (k in 1:nrow(df.predict.mat)){
  
  #head(df.predict.mat)

  predicted.cat <- as.numeric(df.predict.mat[k,5:13])

  #predicted.cat <- df.predict.mat[k,5:13]

  #for (i in 1:ncol(predicted.cat)){
  #  i=1
  #  predicted.cat[1,i] <- as.numeric(from_cat_to_num(predicted.cat[1,i]))
  #}
  #worst.class <- max(apply(predicted.cat, 2, function(x) max(x, na.rm = TRUE)))
  
  worst.class <- max(predicted.cat)
  
  df.predict.mat$Prediction_worst[k] <- worst.class
  
}

rm(predicted.cat,worst.class)

df.predict.mat$Prediction_worst <- factor(df.predict.mat$Prediction_worst, 
                                             levels=c(1,2,3,4,5),
                                             labels=c("LC","NT","VU","EN","CR"))

## 

criterion_spe <- df.predict.mat %>% 
  dplyr::select(sci_nam,Prediction_worst)

df.predict <- left_join(df.predict,criterion_spe )

# Save data

#write.table(df.predict,"dataset/birds/processed_data/df_prediction_per_criteria.csv")

write.table(df.predict,"dataset/birds/processed_data/df_prediction_per_criteria_nomarine.csv")


##

df.predict.mat$Prediction_worst <- factor(df.predict.mat$Prediction_worst, 
                                           levels=c("LC","NT","VU","EN","CR"),
                                          labels=c("None Threat","None Threat","Threat","Threat","Threat"))

df.predict.mat$RL_cat <- factor(df.predict.mat$RL_cat, 
                                          levels=c("LC","NT","VU","EN","CR"),
                                          labels=c("None Threat","None Threat","Threat","Threat","Threat"))


# Confusion matrix criteria-specific approach vs actual RL categories

table(df.predict.mat$RL_cat, df.predict.mat$Prediction_worst)

# Confusion matrix criterion-blind approach vs actual RL categories

df.predict.mat$prediction_RL_cat <- as.character(df.predict.mat$prediction_RL_cat)
df.predict.mat$prediction_RL_cat <- factor(df.predict.mat$prediction_RL_cat, 
                            levels=c("LC","NT","VU","EN","CR"),
                            labels=c("None Threat","None Threat","Threat","Threat","Threat"))

table(df.predict.mat$RL_cat, df.predict.mat$prediction_RL_cat)

# 3. Probabilities of prediction per criteria 

df.proba.cat <- data.frame()

for (k in 1:length(list_criteria)){
  
  criteria <- list_criteria[[k]]
  model.formula <- list.model.formula[[k]]
  model.df <- data.frame(list.model.df[[k]])
  proba.cat <- prediction_proba_category(model.formula,model.df,criteria)
  
  df.proba.cat <- bind_rows(df.proba.cat,proba.cat)
  print(criteria)
  
}

rownames(df.proba.cat) <- NULL

# Save data

# write.table(df.proba.cat,"dataset/birds/processed_data/df_prediction_proba_category.csv")

rm(criteria,model.formula,model.df)






