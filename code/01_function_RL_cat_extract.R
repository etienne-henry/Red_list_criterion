# Script that compiles the functions necessary to extract information on the RL category reached by each criterion

# Author: Etienne Henry
# Date: 10/2022

# Load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(plyr)

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
# Function that classes for each species the category to the corresponding RL criteria It takes as arguments: 
# - BL_cat: a df with
#  - name of the species, 
#  - whether the assessment was done manually or automatically
#  - the criteria triggered for the categories CR, EN, VU and NT. 
# - df: an empty data frame with the species in the first column and the 11 criteria considered in the other columns.
# - category: list (CR, EN, VU, NT)
#  ----------------------------------------------------------------

class_criteria_BL <- function(BL_cat,df,categories){
  
  for(k in categories){
    
    df.cat <- BL_cat[,c("sci_nam","Manual","RL_cat",paste0(k,"_criteria",sep=""))]
    
    # Remove subsubcriterias
    
    for(X in c("a", "b", "c", "d", "e", "v", "i", ",", "[()]", " ")){
      
      df.cat[,c(paste0(k,"_criteria",sep=""))] <- gsub(X, "", df.cat[,c(paste0(k,"_criteria",sep=""))])
      
    }
    
    df.cat[,c(paste0(k,"_criteria",sep=""))] <- plyr::revalue(df.cat[,c(paste0(k,"_criteria",sep=""))], c(
      "A1+2B1+2"="A1+2;B1+2","A1+2B1+2C1"="A1+2;B1+2;C1","A1+2B1+2C1+2"="A1+2;B1+2;C1+2",
      "A1+2B1+2C1+2D"="A1+2;B1+2;C1+2;D","A1+2B1+2C1D"="A1+2;B1+2;C1;D,","A1+2B1+2D"="A1+2;B1+2;D",
      "A1+2B1+2D1"="A1+2;B1+2;D1","A1+2C1D"="A1+2;C1;D","A1+2C2"="A1+2;C2","A1+2D2"="A1+2;D2",
      "A1B1+2"="A1;B1+2","A1B1+2C1"="A1;B1+2;C1","A1B1+2C1+2"="A1;B1+2;C1+2","A1B1+2C1+2D"="A1;B1+2;C1+2;D",
      "A1B1+2C1D"="A1;B1+2;C1;D","A1B1+2C2"="A1;B1+2;C2","A1B1+2C2D"="A1;B1+2;C2;D","A1B1+2D"="A1;B1+2;D",
      "A1C1"="A1;C1","A1C1+2"="A1;C1+2","A1C2"="A1;C2","A1C2D"="A1;C2;D","A1D"="A1;D","A1D2"="A1;D2",
      "A2B1+2"="A2;B1+2","A2B1+2C1+2"="A2;B1+2;C1+2","A2B1+2C2"="A2;B1+2;C2","A2B1+2D1"="A2;B1+2;D1",
      "A2B1+3"="A2;B1+3","A2D"="A2;D","B1+2C1"="B1+2;C1","B1+2C1+2D"="B1+2;C1+2;D","B1+2C1D"="B1+2;C1;D",
      "B1+2C2"="B1+2;C2","B1+2C2D"="B1+2;C2;D","B1+2C2D2"="B1+2;C2;D2","B1+2D"="B1+2;D","B1+2D1"="B1+2;D1",
      "B1+2D2"="B1+2;D2","B1+3D2"="B1+3;D2","C1+2D"="C1+2;D","C2D"="C2;D","B1+2C1D"="B1+2;C1;D",
      "B1+2C1+2"="B1+2;C1+2","C1+2D2"="C1+2;D2","A2C2D"="A2;C2;D","A2D2"="A2;D2","C2D2"="C2;D2"
    ))
    
    # Split criterias
    
    df.cat$Crit8 <- df.cat$Crit7 <- df.cat$Crit6 <- df.cat$Crit5 <- df.cat$Crit4 <- df.cat$Crit3 <- df.cat$Crit2 <- df.cat$Crit1 <- NA
    
    for(i in 1:nrow(df.cat)){

      Crits1 <- unlist(strsplit(as.character(df.cat[,c(paste0(k,"_criteria",sep=""))][i]), c(";")))
      if (length(Crits1)>0){
        Crits2 <- unlist(strsplit(Crits1, "[+]"))
        df.cat[i, which(names(df.cat)=="Crit1"):(which(names(df.cat)=="Crit1")+length(Crits2)-1)] <- Crits2
        df.cat[i,]
      }
    }
    
    # Add letters in the ones with only figures
    
    for(L in 1:nrow(df.cat)){

      for(C in which(names(df.cat)=="Crit1"):ncol(df.cat)){

        if(df.cat[L,C] %in% as.character(1:9)){df.cat[L,C]<-paste0(substr(df.cat[L,(C-1)],1,1), df.cat[L,C])}
      }
    }
    
    # Re-organized the criteria triggered 
    
    for (i in 1:nrow(df.cat)){
      if (k == "CR"){
        df$sps[i] <- df.cat$sci_nam[i]
        df$RL_cat[i] <- df.cat$RL_cat[i]
      }
      
      crits <-  df.cat[i,5:ncol(df.cat)] %>% select_if(~sum(!is.na(.)) > 0)
      
      for (j in 1:11){
        
        criteria_df <- names(df)[j+2]
        
        # If manual evaluation, make sure that we do not consider higher categories of threat of what has been evaluated.
        
        if(df.cat$Manual[i] == "VRAI" & from_cat_to_num(k) > from_cat_to_num(df.cat$RL_cat[i]) ){
          ifelse(criteria_df %in% crits & is.na(df[i,j+2]) ,
                 df[i,j+2] <- df.cat$RL_cat[i], NA)
        } else if (df.cat$Manual[i] == "VRAI" & from_cat_to_num(k) <= from_cat_to_num(df.cat$RL_cat[i]) ){
          ifelse(criteria_df %in% crits & is.na(df[i,j+2]) ,
                 df[i,j+2] <- k, NA)
        } else if (df.cat$Manual[i] == "FAUX") {
          ifelse(criteria_df %in% crits & is.na(df[i,j+2]) ,
                 df[i,j+2] <- k, NA)
          
        }
      }
    }
    
    print(k)
  }
  
  return(df)
}

#  ----------------------------------------------------------------
# Function that fills with 'LC_inf' all criteria for LC species 
#  ----------------------------------------------------------------


LC_inf <- function(df,list.criteria) {
  for (k in list.criteria) {
    for (i in 1:nrow(df)) {
      
      if ( df$RL_cat[i] == "LC" ) {
        df[i,k] <- "LC_inf"
      }
    }
    print(k)
  }
  return(df)
}

#  ----------------------------------------------------------------
# Function that fills with 'LC_maybe' empty criteria (which do not trigger a category)
#  ----------------------------------------------------------------

LC_all <- function(df,list.criteria) {
  for (k in list.criteria) {
    for (i in 1:nrow(df)) {
      if ( is.na(df[i,k]) ) {
        df[i,k] <- "LC_maybe"
      }
    }
    print(k)
  }
  return(df)
}


#  ----------------------------------------------------------------
# Function that corrects the dataframe to ensure that no criterion reaches a higher category than the one that was finally evaluated
#  ----------------------------------------------------------------

correction_lower_cat <- function(df,list.criteria){
  cat_RL <- 0
  cat_crit <- 0
  for (k in 1:nrow(df)){
    # Extract level of threat as defined by the Red List
    if(!is.na(df[k,"RL_cat"])) {
      cat_RL <- from_cat_to_num(df[k,"RL_cat"])
      }
    for (i in list.criteria){
      if(!is.na(df[k,i])) {
        cat_crit <- from_cat_to_num(df[k,i])
        }
      if(cat_RL > 0 & cat_crit > 0 & cat_RL < cat_crit){
        df[k,i] <- df[k,2]
      }
      cat_crit <- 0
    }
    cat_RL <- 0
  }
  return(df)
}

# END -----


