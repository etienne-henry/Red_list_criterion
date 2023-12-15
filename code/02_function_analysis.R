# Script that compiles the functions needed for the analysis
# See code 02_birds_analysis.R

# Author: Etienne Henry
# Date: 10/2022

# Load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(plyr)
library(ordinal)
library(lme4)
library(car)
library(MuMIn)
library("MESS")
library(MASS)

# Initialization ----------------------------------------------------------------

list_criteria <- c("RL_cat","A2","A3","A4","B1","B2","C1","C2","D1","D2")

lc <- c("LC","LC_calc","LC_assumed","LC_maybe")
vu <- c("VU","VU_calc")
en <- c("EN","EN_calc")
cr <- c("CR","CR_calc")

non_threat <- c("LC","NT")
threat <- c("VU","EN","CR")


#  ----------------------------------------------------------------
# Function that transforms the RL category into a numerical scale to compare them. 
#  ----------------------------------------------------------------

from_cat_to_num <- function(category){
  if (category == "CR"){num <- 5}
  else if(category== "EN") {num <- 4}
  else if(category== "VU") {num <- 3}
  else if(category== "NT") {num <- 2}
  else {num <- 1}
  return(num)
}


#  ----------------------------------------------------------------
# I. Model selection
#
# Function that runs for each criterion a CLM with all the predictors and then run a backward variable selection (minimizing the AIC) 
# It returns a list of: 
#
# 1. The model summary (n, AIC, BIC)
# 2. The selected predictors, their estimate and their p-value.
# 3. The model formula
# 4. A dataframe without NA for each criterion
#  ----------------------------------------------------------------

per_criteria_model_selection <-  function(df.analysis,criteria){

  criteria.df <- df.analysis[,c("family", "sci_nam", criteria, 
                                variable.full)]
  
  
  # Filtering
  
  criteria.df.flt <- na.omit(criteria.df)
  
  ### WEIGTH Threatened/non_Threatened species
  
  criteria.df.flt$threat <- ifelse(criteria.df.flt[,c(criteria)] %in% threat, "TH","NTH")
  criteria.df.flt$threat <- factor(criteria.df.flt$threat)

  weight <- as.data.frame(table(criteria.df.flt$threat))
  weight.nt <- weight$Freq[weight$Var1 == "NTH"]
  weight.t <- weight$Freq[weight$Var1 == "TH"]

  criteria.df.flt$weight <- ifelse(criteria.df.flt$threat == "NTH",
                                   1-(weight.nt/nrow(criteria.df.flt)),
                                   1-(weight.t/nrow(criteria.df.flt)))

  #### CLMs with all the predictors
  
  eval(parse(text=paste0("CLM_Full<-clm(", criteria,"~", paste(variable.full, collapse="+"), ",
                         weights=weight,
                         data=criteria.df.flt)")))

  # Backward stepwise model selection
  
  step.model <- step(CLM_Full,direction = "backward",trace=0)

  # Formula
  model.formula <- step.model$formula
  
  # Summary
  df.data <- data.frame(criteria,nrow(criteria.df.flt),AIC(step.model),BIC(step.model)) 
  names(df.data)<- c("criteria","n","AIC","BIC")
  
  # Estimates and p-values of selected predictors
  sum.model <- summary(step.model)
  df.coef <- as.data.frame(sum.model$coefficients[,c(1,4)])
  df.coef$predictor <- row.names(df.coef)
  row.names(df.coef) <- NULL
  df.coef <- df.coef %>% mutate(criteria= criteria) %>% dplyr::select(criteria,predictor,estimate=Estimate,pvalue='Pr(>|z|)')
  
  list_return <- list(df.data,df.coef,model.formula,criteria.df.flt)
  
  return(list_return)
  
}


#  ----------------------------------------------------------------
# II. Block phylogenetic validation
# Function that iteratively excludes one family from the data used to train the model, and then used the model to predict 
# the Red List category of the species in the left-out family
# It returns a dataframe with the predicted class for the corresponding criterion
#  ----------------------------------------------------------------

per_criteria_taxo_validation <- function(model.formula,model.df,criteria) {
  
  df.validation <- data.frame()
  
  #For each family in the dataset
  
  for(FAM in levels(model.df$family)){
    
    tryCatch({

      cat(paste0("Starting family: ", FAM, "(N=", nrow(model.df[model.df$family==FAM,]), ")", "\n"))
      
      species.train <- subset(model.df, model.df$family != FAM)
      
      ### CLMs
      
      # Run model without the family excluded

      model.train  <- clm( model.formula  , weights=weight, data=species.train)
      
      # Predict the Red List category of the species in the left-out family
      
      model.output <- data.frame(predict(model.train, newdata=model.df[model.df$family==FAM,], type="class"))
      model.output$sci_nam <-  model.df$sci_nam[model.df$family==FAM]
      
      colnames(model.output) <- c(paste("Predict_validation",criteria,sep = "."),"sci_nam")
      
      df.validation <- bind_rows(df.validation,model.output)
      
    }, error=function(e){cat(paste0("Bug at family", FAM, "\n"))})
      
  }
    
  return(df.validation)
}


#  ----------------------------------------------------------------
# In that case, the function returns a dataframe with the predicted class and the probability of prediction for each category for the corresponding criterion 
#  ----------------------------------------------------------------

per_criteria_taxo_validation_probas <- function(model.formula,model.df,criteria) {
  
  df.validation <- data.frame()
  
  for(FAM in levels(model.df$family)){
    
    tryCatch({
      
      cat(paste0("Starting family: ", FAM, "(N=", nrow(model.df[model.df$family==FAM,]), ")", "\n"))
      
      species.train <- subset(model.df, model.df$family != FAM)
      
      ### CLMs
      
      model.pred.polr  <- polr( model.formula , weights=weight, data=species.train)
      prob.cat <- data.frame(predict(model.pred.polr,newdata=model.df[model.df$family==FAM,], type = "probs"))
      model.output <- data.frame(prob.cat,
                                 class=predict(model.pred.polr, newdata=model.df[model.df$family==FAM,], type="class"))
      
  
      model.output$sci_nam <-  model.df$sci_nam[model.df$family==FAM]
      
      model.output <- model.output %>% mutate(criteria=criteria)
      
      rownames(model.output) <- NULL
      
      df.validation <- bind_rows(df.validation,model.output)
      
    }, error=function(e){cat(paste0("Bug at family", FAM, "\n"))})
    
    
  }
  
  return(df.validation)
}



#  ----------------------------------------------------------------
# II.A. Binary Validation: Threat/non_Threat 
# Function that calculate for each criterion the accuracy, specificity, sensitiy and TSS of the model, by considering only threat categories (VU, EN, CR) vs non-threat categories (LC, NT)
#  ----------------------------------------------------------------

criteria_binary_validation_table <- function(df.model.validation,critera) {
  
  # Initialisation 
  
  True_nonthreat <- 0
  True_threat <- 0
  
  nb.sps.non.th <- 0
  nb.sps.th <- 0
  pour.sps.th <- 0
  
  accuracy <- 0
  sensitivity <- 0
  specificity <- 0
  TSS <- 0

  # Select the categories of the criterion along side the one that have been predicted by the block taxonomic validation 
  
  eval(parse(text=paste0("def.acc <- df.model.validation %>% dplyr::select(sci_nam,", criteria,",", paste("Predict_validation",criteria,sep = "."), ") ")))
  
  df.acc.flt <- na.omit(def.acc) %>% mutate_if(is.factor, as.character)
  
  # Binary classification of actual categories 
  
  df.acc.flt[,c(criteria)][df.acc.flt[,c(criteria)] %in% non_threat] <-  "None Threat"
  df.acc.flt[,c(criteria)][df.acc.flt[,c(criteria)] %in% threat] <- "Threat"
  df.acc.flt[,c(criteria)] <- factor( df.acc.flt[,c(criteria)],levels=c("None Threat","Threat"))
  
  # Binary classification of predicted categories 
  
  df.acc.flt[,c(paste("Predict_validation",criteria,sep = "."))][df.acc.flt[,c(paste("Predict_validation",criteria,sep = "."))] %in% non_threat ] <- "None Threat"
  df.acc.flt[,c(paste("Predict_validation",criteria,sep = "."))][df.acc.flt[,c(paste("Predict_validation",criteria,sep = "."))] %in% threat ] <- "Threat"
  df.acc.flt[,c(paste("Predict_validation",criteria,sep = "."))] <- factor(df.acc.flt[,c(paste("Predict_validation",criteria,sep = "."))],levels=c("None Threat","Threat"))
  
  # Confusion matrix
  
  table.criteria <- table( df.acc.flt[,c(criteria)],df.acc.flt[,c(paste("Predict_validation",criteria,sep = "."))])
  
  True_threat <- as.numeric(table.criteria[2,2])
  True_nonthreat  <- as.numeric(table.criteria[1,1]) 
  
  nb.sps.non.th <- as.numeric(table(df.acc.flt[,c(criteria)])[1])
  nb.sps.th <- as.numeric(table(df.acc.flt[,c(criteria)])[2])
  pour.sps.th <- (nb.sps.th/(nb.sps.non.th+nb.sps.th))*100
  
  # Calculate indexes of the model performance
  
  accuracy <- ((True_nonthreat+True_threat)/nrow(df.acc.flt))
  sensitivity <- (True_threat / nb.sps.th)
  specificity <- (True_nonthreat / nb.sps.non.th)
  TSS <- (specificity + sensitivity - 1)
  
  # Compile data 
  
  df.data <- data.frame(criteria,n=nrow(df.acc.flt),pourcentage_threat = pour.sps.th ,accuracy,sensitivity,specificity,TSS) 
  
  return(df.data)
  
}


#  ----------------------------------------------------------------
# II.A.b. Average error
# Function that calculate for each criterion average error of prediction (i.e., the average number of difference between the actual and predicted category)
#  ----------------------------------------------------------------

average_error <- function(list_criteria.general,df){
  
  for (criteria in list_criteria.general) {
    
    df[,c(paste("average_err",criteria,sep = "_"))] <- NA
    
    for (k in 1:nrow(df)){
      
      original <- 0
      predicted <- 0
      
      # CS corresponding to the combined model
      if (criteria != "CS"){
        
        if (!is.na(df[,c(criteria)][k]) & !is.na(df[,c(paste("Predict_validation",criteria,sep = "."))][k])){
          
          original <- from_cat_to_num(df[,c(criteria)][k])
          predicted <- from_cat_to_num(df[,c(paste("Predict_validation",criteria,sep = "."))][k])
          
          # Absolute difference between original and predicted category
          
          average.error <- abs(original-predicted)
          df[,c(paste("average_err",criteria,sep = "_"))][k] <- average.error
        } 
      } else if (criteria == "CS") {
        
        if (!is.na(df[,c("RL_cat")][k]) & !is.na(df[,c("CS")][k])){
          
          original <- from_cat_to_num(df[,c("RL_cat")][k])
          predicted <- from_cat_to_num(df[,c("CS")][k])
          average.error <- abs(original-predicted)
          df[,c(paste("average_err",criteria,sep = "_"))][k] <- average.error
        }
      }
    }
    print(criteria)
  }
  return(df)
}

#  ----------------------------------------------------------------
# II.B. Per category validation
# Function that calculate for each criterion the accuracy, specificity, sensitivity and TSS of the model, by considering all categories (LC, NT, VU, EN, CR)
#  ----------------------------------------------------------------

criteria_category_validation_table <- function(df.model.validation,criteria) {
  
  # Initialization 
  
  m <- matrix(0, ncol =8, nrow = 1)
  df.data <- data.frame(m)
  colnames(df.data) <- c("accuracy.CR","accuracy.EN","accuracy.VU","accuracy.NT","accuracy.LC","sensitivity","specificity","TSS")
  
  list.category <- list("CR","EN","VU","NT","LC")

  sensitivity <- 0
  specificity <- 0
  TSS <- 0
  
  eval(parse(text=paste0("def.acc.cat <- df.model.validation %>% dplyr::select(sci_nam,", criteria,",", paste("Predict_validation",criteria,sep = "."), ") ")))
  
  def.acc.cat[,c(criteria)] <- factor(def.acc.cat[,c(criteria)], levels=c("LC","NT","VU","EN","CR"), ordered = TRUE)
  
  df.acc.cat.flt <- na.omit(def.acc.cat) %>% mutate_if(is.factor, as.character)

  table.criteria <- as.data.frame(table( df.acc.cat.flt[,c(criteria)],df.acc.cat.flt[,c(paste("Predict_validation",criteria,sep = "."))]))
  names(table.criteria) <- c("actual", "predicted","freq")

  # Accuracy per category 
  
  for (k in 1:length(list.category)){

    category <- list.category[k]
    
    accuracy <- ifelse(sum(table.criteria$actual== category) > 0 & sum(table.criteria$predicted==category) > 0, 
                       (table.criteria$freq[table.criteria$actual==category & table.criteria$predicted == category] ) /
                         sum(table.criteria$freq[table.criteria$actual == category] ) ,0)
    
    df.data[1,k] <- accuracy

  }

  # Global sensitivity of the model
  
  sensitivity <- ( ifelse(sum(table.criteria$actual=="CR") > 0 & sum(table.criteria$predicted=="CR") > 0, 
                        table.criteria$freq[table.criteria$actual=="CR" & table.criteria$predicted == "CR"],0) +
    ifelse(sum(table.criteria$actual=="EN") > 0 & sum(table.criteria$predicted=="EN") > 0, 
           table.criteria$freq[table.criteria$actual=="EN" & table.criteria$predicted == "EN"],0) +
    ifelse(sum(table.criteria$actual=="VU") > 0 & sum(table.criteria$predicted=="VU") > 0, 
           table.criteria$freq[table.criteria$actual=="VU" & table.criteria$predicted == "VU"],0) ) /
                    sum(table.criteria$freq[table.criteria$actual %in% threat])
  
  # Global specificity of the model
  
  specificity <- ( ifelse(sum(table.criteria$actual=="LC") > 0 & sum(table.criteria$predicted=="LC") > 0, 
                          table.criteria$freq[table.criteria$actual=="LC" & table.criteria$predicted == "LC"],0) +
                     ifelse(sum(table.criteria$actual=="NT") > 0 & sum(table.criteria$predicted=="NT") > 0, 
                            table.criteria$freq[table.criteria$actual=="NT" & table.criteria$predicted == "NT"],0)  ) /
    sum(table.criteria$freq[table.criteria$actual %in% non_threat])
  
  # Global TSS of the model
  
  TSS <- (specificity + sensitivity - 1)
  
  df.data$sensitivity[1] <- sensitivity
  df.data$specificity[1] <- specificity
  df.data$TSS[1] <- TSS
  
  df.data <- df.data %>% mutate(criteria=criteria)
  
  return(df.data)
  
}


#  ----------------------------------------------------------------
# III. Model  Prediction
# A. Class prediction
# Function that return all the RL categories predicted by the model for each species under each criterion 
#  ----------------------------------------------------------------

model_prediction <- function(df.predict,list.model.formula,list.model.df,list_criteria) {
  for (k in 1:length(list_criteria)){
    
    criteria <- list_criteria[[k]]
    model.formula <- list.model.formula[[k]]
    model.df <- data.frame(list.model.df[[k]])
    
    model.pred  <- clm( model.formula , weights=weight, data=model.df)
    
    prediction <- data.frame(predict(object=model.pred,newdata=df.predict, type = "class"))

    names(prediction) <- paste("prediction",criteria,sep = "_")

    prediction$sci_nam <- df.predict$sci_nam

    
    df.predict <- left_join(df.predict,prediction)
    print(criteria)
  }
  
  return(df.predict)
}

#  ----------------------------------------------------------------
# III. Model Prediction
# B. Probabilities prediction
# Function that return all the RL categories and the probabilities predicted by the model for each species under each criterion 
#  ----------------------------------------------------------------

prediction_proba_category <- function(list.model.formula,list.model.df,list_criteria) {
  
  model.pred.polr  <- polr( model.formula , weights=weight, data=model.df)
  
  prediction <- data.frame(predict(model.pred.polr, type = "probs"))

  prediction$class <- data.frame(prediction=predict(model.pred.polr, type="class"))
  
  prediction$sci_nam <- model.df$sci_nam
  
  prediction <- prediction %>% mutate(criteria=criteria) 

return(rev(prediction))
}

# END -----





