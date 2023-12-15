# Script to plot Figure 1

# Author: Etienne Henry
# Date: 10/2022

# Load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot)
library(RColorBrewer)
library(ggthemes)

# Import data ----------------------------------------------------------------

birds.criteria.cat.hyp <- read.table("dataset/birds/processed_data/birds_ER_predictors_range_noAUTO.csv")


# Filter for marine species 

sps.marine <- read.table("dataset/birds/sps_marine.txt")

birds.predict <- birds.predict %>%
  filter(sci_nam %not in% sps.marine$x)

rm(sps.marine)

birds.criteria.cat.hyp <- birds.criteria.cat.hyp %>%
  dplyr::select(sci_nam,RL_cat,A1,A2,A3,A4,B1,B2,C1,C2,D1,D2)

birds.criteria.cat.hyp[is.na(birds.criteria.cat.hyp)] <- "NA"

birds.criteria.cat.hyp[birds.criteria.cat.hyp=="LC_maybe"] <- "NA"

# Categories

list_criteria <- c("RL_cat","A1","A2","A3","A4","B1","B2","C1","C2","D1","D2")



# Ordered level of threat 

criteria.coverage <- data.frame()

for (criteria in list_criteria){

  
  birds.criteria.cat.hyp[,c(criteria)] <- factor(birds.criteria.cat.hyp[,c(criteria)],
                                                 levels=c("NA",
                                                          "LC_inf","LC",
                                                          "NT",
                                                          "VU",
                                                          "EN",
                                                          "CR"),
                                                 labels=c("Missing",
                                                          "LC","LC",
                                                          "NT",
                                                          "VU",
                                                          "EN",
                                                          "CR"),)

  table.criteria <- birds.criteria.cat.hyp %>% count(criteria) %>%  mutate(crit=criteria)
  names(table.criteria)[1] <- "category"
  criteria.coverage <- bind_rows(criteria.coverage,table.criteria)
}


criteria.coverage$crit <- factor(criteria.coverage$crit, 
                                     levels= c("RL_cat","A1","A2","A3","A4","B1","B2","C1","C2","D1","D2"),
                                     labels= c("All","A1","A2","A3","A4","B1","B2","C1","C2","D1","D2"))

# Red List category color
col <- c('#BDBDBD',
         '#6fcd6c',
         '#d5e431',
         '#fae915',
         '#ff934f',
         '#e23400')

# Plot

ggplot(criteria.coverage, aes(fill=category, y=freq, x=crit)) + 
  geom_bar(position="fill", stat="identity",width=0.7)+
  scale_fill_manual(values = col,name = "Information origin") + xlab("Red List Criteria") + ylab("") + theme_tufte()+
  theme(       legend.title = element_text(size=12, face="bold"),
               axis.text.x = element_text(size=12),             
               axis.text.y = element_text(vjust=0.2),          
               axis.ticks = element_line(size=0.4),               
               axis.title = element_text(size=12, face="bold"))

# END ---- 


